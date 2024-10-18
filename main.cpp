#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include <iostream>
#include <vector>
#include <string>
#include <map>

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
    };

    class VariableExpr : public Expression {
        std::string Name;
    public:
        VariableExpr(const std::string &Name) : Name(Name) {}
        llvm::Value *codegen() override;
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

    class CallExpr : public Expression {
        std::string Callee;
        std::vector<std::unique_ptr<Expression>> Args;
    public:
        CallExpr(const std::string &Callee,
                 std::vector<std::unique_ptr<Expression>> Args)
            : Callee(Callee), Args(std::move(Args)) {}
        llvm::Value *codegen() override;
    };
}

// Contexto global de LLVM
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::Value*> NamedValues;

// Implementaciones de codegen
llvm::Value *AST::NumberExpr::codegen() {
    return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
}

llvm::Value *AST::VariableExpr::codegen() {
    llvm::Value *V = NamedValues[Name];
    if (!V)
        throw std::runtime_error("Unknown variable name");
    return V;
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

llvm::Value *AST::CallExpr::codegen() {
    llvm::Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
        throw std::runtime_error("Unknown function referenced");

    if (CalleeF->arg_size() != Args.size())
        throw std::runtime_error("Incorrect # arguments passed");

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

// Función principal del compilador
int main() {
    TheModule = std::make_unique<llvm::Module>("xBase Compiler", TheContext);

    // Ejemplo de generación de código para una función simple
    llvm::FunctionType *FT = 
        llvm::FunctionType::get(llvm::Type::getDoubleTy(TheContext), false);
    llvm::Function *F = 
        llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "example", TheModule.get());

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", F);
    Builder.SetInsertPoint(BB);

    // Crear una expresión binaria: 3.0 + 4.5
    auto LHS = std::make_unique<AST::NumberExpr>(3.0);
    auto RHS = std::make_unique<AST::NumberExpr>(4.5);
    auto Add = std::make_unique<AST::BinaryExpr>('+', std::move(LHS), std::move(RHS));

    llvm::Value *RetVal = Add->codegen();
    Builder.CreateRet(RetVal);

    // Verificar el módulo generado
    llvm::verifyFunction(*F);

    // Imprimir el IR generado
    TheModule->print(llvm::outs(), nullptr);

    return 0;
}
