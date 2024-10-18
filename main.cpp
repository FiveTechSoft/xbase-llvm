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

// Función auxiliar para eliminar espacios en blanco al inicio y final de una cadena
std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first) {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}

void processLine(const std::string &line) {
    if (line[0] == '?') {
        std::string expr = trim(line.substr(1));
        
        // Tokenizar la expresión
        std::vector<std::string> tokens;
        std::string current_token;
        for (char c : expr) {
            if (c == ' ') {
                if (!current_token.empty()) {
                    tokens.push_back(current_token);
                    current_token.clear();
                }
            } else if (c == '+' || c == '-' || c == '*' || c == '/') {
                if (!current_token.empty()) {
                    tokens.push_back(current_token);
                    current_token.clear();
                }
                tokens.push_back(std::string(1, c));
            } else {
                current_token += c;
            }
        }
        if (!current_token.empty()) {
            tokens.push_back(current_token);
        }

        std::cout << "Tokens: ";
        for (const auto& token : tokens) {
            std::cout << "'" << token << "' ";
        }
        std::cout << std::endl;

        // Verificar que todas las variables estén definidas
        for (const auto& token : tokens) {
            if (token != "+" && token != "-" && token != "*" && token != "/" &&
                (token.front() != '"' || token.back() != '"') &&
                NamedValues.find(token) == NamedValues.end()) {
                std::cerr << "Variable no definida: " << token << std::endl;
                return;
            }
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
        std::unique_ptr<AST::Expression> Expr;
        for (size_t i = 0; i < tokens.size(); i++) {
            if (tokens[i] == "+" || tokens[i] == "-" || tokens[i] == "*" || tokens[i] == "/") {
                if (!Expr || i + 1 >= tokens.size()) {
                    std::cerr << "Error de sintaxis: operador '" << tokens[i] << "' mal posicionado" << std::endl;
                    return;
                }
                char op = tokens[i][0];
                i++; // Avanzar al siguiente token
                std::unique_ptr<AST::Expression> RHS;
                if (tokens[i].front() == '"' && tokens[i].back() == '"') {
                    RHS = std::make_unique<AST::StringExpr>(tokens[i].substr(1, tokens[i].length() - 2));
                } else if (NamedValues.find(tokens[i]) != NamedValues.end()) {
                    RHS = std::make_unique<AST::VariableExpr>(tokens[i]);
                } else {
                    try {
                        double val = std::stod(tokens[i]);
                        RHS = std::make_unique<AST::NumberExpr>(val);
                    } catch (const std::invalid_argument&) {
                        std::cerr << "Token inválido: " << tokens[i] << std::endl;
                        return;
                    }
                }
                Expr = std::make_unique<AST::BinaryExpr>(op, std::move(Expr), std::move(RHS));
            } else {
                std::unique_ptr<AST::Expression> Current;
                if (tokens[i].front() == '"' && tokens[i].back() == '"') {
                    Current = std::make_unique<AST::StringExpr>(tokens[i].substr(1, tokens[i].length() - 2));
                } else if (NamedValues.find(tokens[i]) != NamedValues.end()) {
                    Current = std::make_unique<AST::VariableExpr>(tokens[i]);
                } else {
                    try {
                        double val = std::stod(tokens[i]);
                        Current = std::make_unique<AST::NumberExpr>(val);
                    } catch (const std::invalid_argument&) {
                        std::cerr << "Token inválido: " << tokens[i] << std::endl;
                        return;
                    }
                }
                if (!Expr) {
                    Expr = std::move(Current);
                } else {
                    std::cerr << "Error de sintaxis: se esperaba un operador" << std::endl;
                    return;
                }
            }
        }

        if (!Expr) {
            std::cerr << "Error: expresión vacía" << std::endl;
            return;
        }

        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
            "example_func", TheModule.get());

        llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
        Builder.SetInsertPoint(BB);

        llvm::Value *Result = Expr->codegen();
        
        // Crear la llamada a print_result
        llvm::Function* printFunc = TheModule->getFunction("print_result");
        if (!printFunc) {
            std::cerr << "Error: print_result function not found\n";
            return;
        }
        Builder.CreateCall(printFunc, Result);
        Builder.CreateRetVoid();

        // Verificar y mostrar el IR
        llvm::verifyFunction(*F);
        TheModule->print(llvm::outs(), nullptr);

        // Añadir el módulo al motor de ejecución
        TheExecutionEngine->addModule(std::move(TheModule));
        TheExecutionEngine->finalizeObject();

        // Ejecutar la función
        TheExecutionEngine->runFunction(F, {});

        // Crear un nuevo módulo para la siguiente operación
        TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);
    } else {
        size_t equalPos = line.find('=');
        if (equalPos == std::string::npos) {
            std::cerr << "Formato de asignación inválido.\n";
            return;
        }

        std::string name = line.substr(0, equalPos);
        std::string value = line.substr(equalPos + 1);

        name = trim(name);
        value = trim(value);

        if (value.empty()) {
            std::cerr << "Valor vacío.\n";
            return;
        }

        try {
            if (value.front() == '"' && value.back() == '"') {
                value = value.substr(1, value.length() - 2);
                NamedValues[name] = Value(value);
                std::cout << "Asignado " << name << " = \"" << value << "\"" << std::endl;
            } else {
                double numVal = std::stod(value);
                NamedValues[name] = Value(numVal);
                std::cout << "Asignado " << name << " = " << numVal << std::endl;
            }
        } catch (const std::invalid_argument&) {
            std::cerr << "Valor inválido. Los strings deben estar entre comillas y los números deben ser válidos.\n";
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