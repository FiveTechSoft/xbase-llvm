#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cstdlib> // Para usar system()
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
static std::map<std::string, double> NamedValues;

// Implementaciones de codegen
llvm::Value *AST::NumberExpr::codegen() {
    return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
}

llvm::Value *AST::VariableExpr::codegen() {
    if (NamedValues.find(Name) == NamedValues.end()) {
        throw std::runtime_error("Unknown variable name: " + Name);
    }
    return llvm::ConstantFP::get(TheContext, llvm::APFloat(NamedValues[Name]));
}

llvm::Value *AST::BinaryExpr::codegen() {
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

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
        throw std::runtime_error("invalid binary operator");
    }
}

// Función para procesar expresiones tipo "x = 5" o "? x + y"
void processLine(const std::string &line) {
    if (line[0] == '?') {
        // Procesar expresión
        std::istringstream iss(line.substr(1));
        std::string lhs, rhs;
        char op;
        iss >> lhs >> op >> rhs;

        if (NamedValues.find(lhs) == NamedValues.end() || NamedValues.find(rhs) == NamedValues.end()) {
            std::cerr << "Variable no definida.\n";
            return;
        }

        // Crear AST para la expresión
        auto LHS = std::make_unique<AST::VariableExpr>(lhs);
        auto RHS = std::make_unique<AST::VariableExpr>(rhs);
        auto Expr = std::make_unique<AST::BinaryExpr>(op, std::move(LHS), std::move(RHS));

        // Generar código para la expresión
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(TheContext), false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "example", TheModule.get());

        llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
        Builder.SetInsertPoint(BB);

        llvm::Value *RetVal = Expr->codegen();
        Builder.CreateRet(RetVal);

        // Verificar el módulo generado
        llvm::verifyFunction(*F);

        // Imprimir el IR generado
        TheModule->print(llvm::outs(), nullptr);
    } else {
        // Procesar asignación "x = 5"
        std::istringstream iss(line);
        std::string varName;
        char eq;
        double val;
        iss >> varName >> eq >> val;

        if (eq == '=') {
            NamedValues[varName] = val;
            std::cout << "Variable " << varName << " asignada con valor " << val << "\n";
        } else {
            std::cerr << "Error de sintaxis.\n";
        }
    }
}

// Función principal del compilador
int main() {
    TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);

    std::string line;
    std::cout << "Escribe expresiones como 'x = 5' o '? x + y'. Escribe 'exit' para salir.\n";
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
