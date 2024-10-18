#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cstdlib>
#include <sstream>

// Definición expandida de un AST para xBase
namespace AST {
    class Expression {
    public:
        virtual ~Expression() = default;
        virtual llvm::Value *codegen() = 0;
    };

    class NumberExpr : public Expression {
        double Val;
    public:
        NumberExpr(double Val) : Val(Val) {}
        llvm::Value *codegen() override;
        double getVal() const { return Val; }
    };

    class StringExpr : public Expression {
        std::string Val;
    public:
        StringExpr(const std::string &Val) : Val(Val) {}
        llvm::Value *codegen() override;
        const std::string &getVal() const { return Val; }
    };

    class VariableExpr : public Expression {
        std::string Name;
    public:
        VariableExpr(const std::string &Name) : Name(Name) {}
        llvm::Value *codegen() override;
        const std::string &getName() const { return Name; }
    };

    class BinaryExpr : public Expression {
        char Op;
        std::unique_ptr<Expression> LHS, RHS;
    public:
        BinaryExpr(char op, std::unique_ptr<Expression> LHS,
                   std::unique_ptr<Expression> RHS)
            : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
        llvm::Value *codegen() override;
    };
}

// Contexto global de LLVM
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;

// Estructura para almacenar valores de variables
struct Value {
    enum Type { Number, String } type;
    union {
        double numVal;
        std::string* strVal;
    };
    Value() : type(Number), numVal(0.0) {}
    Value(double val) : type(Number), numVal(val) {}
    Value(const std::string& val) : type(String), strVal(new std::string(val)) {}
    Value(const Value& other) : type(other.type) {
        if (type == Number) {
            numVal = other.numVal;
        } else {
            strVal = new std::string(*other.strVal);
        }
    }
    ~Value() {
        if (type == String) delete strVal;
    }
    Value& operator=(const Value& other) {
        if (this != &other) {
            if (type == String) delete strVal;
            type = other.type;
            if (type == Number) {
                numVal = other.numVal;
            } else {
                strVal = new std::string(*other.strVal);
            }
        }
        return *this;
    }
};

static std::map<std::string, Value> NamedValues;
static std::unique_ptr<llvm::ExecutionEngine> TheExecutionEngine;

extern "C" {
    char* concat_strings(const char* s1, const char* s2) {
        std::string result = std::string(s1) + std::string(s2);
        char* cstr = new char[result.length() + 1];
        std::strcpy(cstr, result.c_str());
        return cstr;
    }

    void print_result(const char* s) {
        std::cout << "Resultado: " << s << std::endl;
    }
}

extern "C" {
    double evaluateExpression(const char* op, const char* lhs, const char* rhs);
    void printResult(double result);
}

// Implementaciones de codegen
llvm::Value *AST::NumberExpr::codegen() {
    return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
}

llvm::Value *AST::StringExpr::codegen() {
    return Builder.CreateGlobalStringPtr(Val);
}

llvm::Value *AST::VariableExpr::codegen() {
    if (NamedValues.find(Name) == NamedValues.end()) {
        throw std::runtime_error("Unknown variable name: " + Name);
    }
    const Value& val = NamedValues[Name];
    if (val.type == Value::Number) {
        return llvm::ConstantFP::get(TheContext, llvm::APFloat(val.numVal));
    } else {
        return Builder.CreateGlobalStringPtr(*val.strVal);
    }
}

llvm::Value *AST::BinaryExpr::codegen() {
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

    // Si ambos son double, realizar operación aritmética
    if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy()) {
        switch (Op) {
        case '+':
            return Builder.CreateFAdd(L, R, "addtmp");
        case '-':
            return Builder.CreateFSub(L, R, "subtmp");
        case '*':
            return Builder.CreateFMul(L, R, "multmp");
        case '/':
            return Builder.CreateFDiv(L, R, "divtmp");
        default:
            throw std::runtime_error("invalid binary operator for numbers");
        }
    }
    // Si al menos uno es string, realizar concatenación
    else if (L->getType()->isPointerTy() || R->getType()->isPointerTy()) {
        if (Op != '+') {
            throw std::runtime_error("only '+' operator is supported for strings");
        }
        // Llamar a una función de runtime para concatenar strings
        llvm::Function *ConcatFunc = TheModule->getFunction("concat_strings");
        if (!ConcatFunc) {
            std::vector<llvm::Type*> ArgTypes(2, llvm::Type::getInt8PtrTy(TheContext));
            llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(TheContext), ArgTypes, false);
            ConcatFunc = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "concat_strings", TheModule.get());
        }
        return Builder.CreateCall(ConcatFunc, {L, R}, "concattmp");
    }
    else {
        throw std::runtime_error("incompatible types for binary operation");
    }
}

// Función para procesar expresiones
void processLine(const std::string &line) {
    if (line[0] == '?') {
        // Process expression
        std::istringstream iss(line.substr(1));
        std::string expr;
        std::getline(iss >> std::ws, expr);

        // Split the expression into parts
        std::istringstream expr_stream(expr);
        std::string lhs, rhs;
        char op = 0;
        expr_stream >> lhs;
        if (expr_stream >> op >> rhs) {
            // Binary expression
            if (NamedValues.find(lhs) == NamedValues.end() || NamedValues.find(rhs) == NamedValues.end()) {
                std::cerr << "Undefined variable.\n";
                return;
            }

            // Create AST for the binary expression
            auto LHS = std::make_unique<AST::VariableExpr>(lhs);
            auto RHS = std::make_unique<AST::VariableExpr>(rhs);
            auto Expr = std::make_unique<AST::BinaryExpr>(op, std::move(LHS), std::move(RHS));

            // Generate code for the expression
            llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), false);
            llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "example", TheModule.get());

            llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
            Builder.SetInsertPoint(BB);

            llvm::Value *Result = Expr->codegen();
            
if (Result) {
        // Call runtime function to print the result
        llvm::Function *PrintFunc = TheModule->getFunction("print_result");
        if (!PrintFunc) {
            llvm::FunctionType *PrintFT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::Type::getInt8PtrTy(TheContext)}, false);
            PrintFunc = llvm::Function::Create(PrintFT, llvm::Function::ExternalLinkage, "print_result", TheModule.get());
        }
        Builder.CreateCall(PrintFunc, {Result});
        Builder.CreateRetVoid();

        // Verify the generated function
        if (llvm::verifyFunction(*F, &llvm::errs())) {
            std::cerr << "Error: Invalid function generated\n";
            F->eraseFromParent();
            return;
        }

        // Print the generated IR
        TheModule->print(llvm::outs(), nullptr);

        std::cout << "Finalizing object..." << std::endl;
        TheExecutionEngine->finalizeObject();

        std::cout << "Retrieving function..." << std::endl;
        llvm::Function *CompiledF = TheExecutionEngine->FindFunctionNamed("example");
        if (!CompiledF) {
            std::cerr << "Error: Failed to retrieve compiled function\n";
            
            // Print all function names in the module
            std::cout << "Functions in the module:" << std::endl;
            for (auto &Func : TheModule->getFunctionList()) {
                std::cout << Func.getName().str() << std::endl;
            }
            
            return;
        }

        std::cout << "Running function..." << std::endl;
        std::vector<llvm::GenericValue> NoArgs;
        TheExecutionEngine->runFunction(CompiledF, NoArgs);

        // Remove the function from the module to avoid name conflicts in future compilations
        F->eraseFromParent();
    } else {
        std::cerr << "Error: Failed to generate code for expression\n";
        F->eraseFromParent();
    }
        } else {
            // Single variable expression
            if (NamedValues.find(lhs) == NamedValues.end()) {
                std::cerr << "Undefined variable.\n";
                return;
            }

            // Print the variable's value
            const Value& val = NamedValues[lhs];
            if (val.type == Value::Number) {
                std::cout << "Value of " << lhs << ": " << val.numVal << std::endl;
            } else {
                std::cout << "Value of " << lhs << ": \"" << *val.strVal << "\"" << std::endl;
            }
        }
    } else {
        // Variable assignment
        std::istringstream iss(line);
        std::string name;
        std::string equals;
        std::string value;
        
        if (!(iss >> name >> equals >> value) || equals != "=") {
            std::cerr << "Invalid assignment format.\n";
            return;
        }

        // Determine if the value is a number or a string
        try {
            double numVal = std::stod(value);
            NamedValues[name] = Value(numVal);
            std::cout << "Assigned " << name << " = " << numVal << std::endl;
        } catch (const std::invalid_argument&) {
            // If not a number, assume it's a string
            if (value.front() == '"' && value.back() == '"') {
                // Remove the quotes
                value = value.substr(1, value.length() - 2);
                NamedValues[name] = Value(value);
                std::cout << "Assigned " << name << " = \"" << value << "\"" << std::endl;
            } else {
                std::cerr << "Invalid string value. Must be enclosed in quotes.\n";
            }
        } catch (const std::out_of_range&) {
            std::cerr << "Number out of range.\n";
        }
    }
}

// Función principal del compilador
int main(int argc, char *argv[]) {
    llvm::InitLLVM X(argc, argv);
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);
    
    std::string ErrorStr;
    llvm::EngineBuilder builder(std::move(TheModule));
    builder.setErrorStr(&ErrorStr);
    TheExecutionEngine.reset(builder.create());
    
    if (!TheExecutionEngine) {
        std::cerr << "Could not create ExecutionEngine: " << ErrorStr << std::endl;
        return 1;
    }

    // Recreate the module
    TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);
    
    // Add global mappings for runtime functions
    TheExecutionEngine->addGlobalMapping("concat_strings", reinterpret_cast<uint64_t>(concat_strings));
    TheExecutionEngine->addGlobalMapping("print_result", reinterpret_cast<uint64_t>(print_result));

    std::string line;
    std::cout << "Escribe expresiones como 'x = 5', 'y = \"Hola\"' o '? x + y'. Escribe 'exit' para salir.\n";
    while (true) {
        std::cout << "> ";
        std::getline(std::cin, line);

        if (line == "exit") {
            break;
        }

        try {
            processLine(line);
        } catch (const std::exception &e) {
            std::cerr << "Error: " << e.what() << "\n";
        }
    }

    return 0;
}