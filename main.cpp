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
        return Builder.CreateCall(ConcatFunc, {L, R}, "concat_strings");
    }
    else {
        throw std::runtime_error("incompatible types for binary operation");
    }
}

// Función para procesar expresiones
void processLine(const std::string &line) {
    if (line[0] == '?') {
        std::istringstream iss(line.substr(1));
        std::string expr;
        std::getline(iss >> std::ws, expr);

        std::istringstream expr_stream(expr);
        std::string lhs, rhs;
        char op = 0;
        expr_stream >> lhs;
        if (expr_stream >> op >> rhs) {
            if (NamedValues.find(lhs) == NamedValues.end() || NamedValues.find(rhs) == NamedValues.end()) {
                std::cerr << "Variable no definida.\n";
                return;
            }

            // Crear un nuevo módulo para esta expresión
            TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);
            
            // Recrear las declaraciones de las funciones externas
            llvm::FunctionType *ConcatFT = llvm::FunctionType::get(
                llvm::Type::getInt8PtrTy(TheContext),
                {llvm::Type::getInt8PtrTy(TheContext), llvm::Type::getInt8PtrTy(TheContext)},
                false);
            llvm::Function::Create(ConcatFT, llvm::Function::ExternalLinkage,
                                 "concat_strings", TheModule.get());

            llvm::FunctionType *PrintFT = llvm::FunctionType::get(
                llvm::Type::getVoidTy(TheContext),
                {llvm::Type::getInt8PtrTy(TheContext)},
                false);
            llvm::Function::Create(PrintFT, llvm::Function::ExternalLinkage,
                                 "print_result", TheModule.get());

            // Crear AST y generar código
            auto LHS = std::make_unique<AST::VariableExpr>(lhs);
            auto RHS = std::make_unique<AST::VariableExpr>(rhs);
            auto Expr = std::make_unique<AST::BinaryExpr>(op, std::move(LHS), std::move(RHS));

            llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), false);
            llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                "example_func", TheModule.get());

            llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
            Builder.SetInsertPoint(BB);

            llvm::Value *Result = Expr->codegen();
            Builder.CreateCall(TheModule->getFunction("print_result"), {Result});
            Builder.CreateRetVoid();

            // Verificar y mostrar el IR
            llvm::verifyFunction(*F);
            TheModule->print(llvm::outs(), nullptr);

            // Añadir el módulo al motor de ejecución
            TheExecutionEngine->addModule(std::move(TheModule));
            TheExecutionEngine->finalizeObject();

            // Obtener y ejecutar la función
            uint64_t FPtr = TheExecutionEngine->getFunctionAddress("print_result");
            if (FPtr) {
                void (*FP)() = (void (*)())(intptr_t)FPtr;
                FP();
                
                // Remover el módulo después de la ejecución
                // TheExecutionEngine->removeModule(H);
            } else {
                std::cerr << "Error: No se pudo obtener la función compilada\n";
            }

            // Crear un nuevo módulo para la siguiente operación
            TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);

        } else {
            // Código para expresión de una sola variable (sin cambios)
            if (NamedValues.find(lhs) == NamedValues.end()) {
                std::cerr << "Variable no definida.\n";
                return;
            }

            const Value& val = NamedValues[lhs];
            if (val.type == Value::Number) {
                std::cout << "Valor de " << lhs << ": " << val.numVal << std::endl;
            } else {
                std::cout << "Valor de " << lhs << ": \"" << *val.strVal << "\"" << std::endl;
            }
        }
    } else {
        // Código para asignación de variables (sin cambios)
        std::istringstream iss(line);
        std::string name;
        std::string equals;
        std::string value;
        
        if (!(iss >> name >> equals >> value) || equals != "=") {
            std::cerr << "Formato de asignación inválido.\n";
            return;
        }

        try {
            double numVal = std::stod(value);
            NamedValues[name] = Value(numVal);
            std::cout << "Asignado " << name << " = " << numVal << std::endl;
        } catch (const std::invalid_argument&) {
            if (value.front() == '"' && value.back() == '"') {
                value = value.substr(1, value.length() - 2);
                NamedValues[name] = Value(value);
                std::cout << "Asignado " << name << " = \"" << value << "\"" << std::endl;
            } else {
                std::cerr << "Valor de cadena inválido. Debe estar entre comillas.\n";
            }
        } catch (const std::out_of_range&) {
            std::cerr << "Número fuera de rango.\n";
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