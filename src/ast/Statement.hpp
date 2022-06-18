//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#pragma once

#include <string>
#include <vector>

#include <ast/Types.hpp>
#include <ast/Expression.hpp>

class AstExpression;
class AstID;
class AstInt;

// Represents an AST statement
class AstStatement {
public:
    explicit AstStatement() {}
    explicit AstStatement(AstType type) {
        this->type = type;
    }
    
    void setExpression(AstExpression *expr) { this->expr = expr; }
    AstExpression *getExpression() { return expr; }
    bool hasExpression() { return expr != nullptr; }
    
    AstType getType() { return type; }
    virtual void print() {}
private:
    AstExpression *expr = nullptr;
    AstType type = AstType::EmptyAst;
};

// Represents an AST expression statement
// This is basically the same as a statement
class AstExprStatement : public AstStatement {
public:
    explicit AstExprStatement() : AstStatement(AstType::ExprStmt) {}
    void print();
    
    void setDataType(DataType dataType, DataType ptrType = DataType::Void) {
        this->dataType = dataType;
        this->ptrType = ptrType;
    }
    
    DataType getDataType() { return dataType; }
    DataType getPtrType() { return ptrType; }
    
private:
    DataType dataType = DataType::Void;
    DataType ptrType = DataType::Void;
};

// Represents a function call statement
class AstFuncCallStmt : public AstStatement {
public:
    explicit AstFuncCallStmt(std::string name) : AstStatement(AstType::FuncCallStmt) {
        this->name = name;
    }
    
    std::string getName() { return name; }
    void print();
private:
    std::string name = "";
};

// Represents a return statement
class AstReturnStmt : public AstStatement {
public:
    explicit AstReturnStmt() : AstStatement(AstType::Return) {}
    void print();
};

// Represents a variable declaration
class AstVarDec : public AstStatement {
public:
    explicit AstVarDec(std::string name, DataType dataType) : AstStatement(AstType::VarDec) {
        this->name = name;
        this->dataType = dataType;
    }
    
    void setDataType(DataType dataType) { this->dataType = dataType; }
    void setPtrType(DataType dataType) { this->ptrType = dataType; }
    void setPtrSize(AstExpression *size) { this->size = size; }
    
    std::string getName() { return name; }
    DataType getDataType() { return dataType; }
    DataType getPtrType() { return ptrType; }
    AstExpression *getPtrSize() { return size; }
    
    void print();
private:
    std::string name = "";
    AstExpression *size = nullptr;
    DataType dataType = DataType::Void;
    DataType ptrType = DataType::Void;
};

// Represents a structure declaration
class AstStructDec : public AstStatement {
public:
    explicit AstStructDec(std::string varName, std::string structName) : AstStatement(AstType::StructDec) {
        this->varName = varName;
        this->structName = structName;
    }
    
    void setNoInit(bool init) { noInit = init; }
    
    std::string getVarName() { return varName; }
    std::string getStructName() { return structName; }
    bool isNoInit() { return noInit; }
    
    void print();
private:
    std::string varName = "";
    std::string structName = "";
    bool noInit = false;
};

// Represents a statement with a sub-block
class AstBlockStmt : public AstStatement {
public:
    explicit AstBlockStmt(AstType type) : AstStatement(type) {
        block = new AstBlock;
    }
    
    void addStatement(AstStatement *stmt) { block->addStatement(stmt); }
    
    AstBlock *getBlockStmt() { return block; }
    std::vector<AstStatement *> getBlock() { return block->getBlock(); }
    
    virtual void print(int indent = 0) {}
protected:
    AstBlock *block;
};

// Represents a conditional statement
class AstIfStmt : public AstBlockStmt {
public:
    explicit AstIfStmt() : AstBlockStmt(AstType::If) {}
    
    void addBranch(AstStatement *stmt) { branches.push_back(stmt); }
    std::vector<AstStatement *> getBranches() { return branches; }
    
    void print(int indent = 0);
private:
    std::vector<AstStatement *> branches;
};

class AstElifStmt : public AstBlockStmt {
public:
    explicit AstElifStmt() : AstBlockStmt(AstType::Elif) {}
    
    void print(int indent = 0);
};

class AstElseStmt : public AstBlockStmt {
public:
    explicit AstElseStmt() : AstBlockStmt(AstType::Else) {}
    
    void print(int indent = 0);
};

// Represents a while statement
class AstWhileStmt : public AstBlockStmt {
public:
    explicit AstWhileStmt() : AstBlockStmt(AstType::While) {}
    
    void print(int indent = 0);
};

// Represents a break statement for a loop
class AstBreak : public AstStatement {
public:
    explicit AstBreak() : AstStatement(AstType::Break) {}
    void print();
};

// Represents a continue statement for a loop
class AstContinue : public AstStatement {
public:
    explicit AstContinue() : AstStatement(AstType::Continue) {}
    void print();
};

