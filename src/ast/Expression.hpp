//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#pragma once

#include <string>
#include <vector>
#include <stdint.h>

#include <ast/Types.hpp>

// Represents an AST expression
class AstExpression {
public:
    explicit AstExpression() {}
    explicit AstExpression(AstType type) {
        this->type = type;
    }
    
    AstType getType() { return type; }
    virtual void print() {}
protected:
    AstType type = AstType::EmptyAst;
};

// Holds a list of expressions
class AstExprList : public AstExpression {
public:
    AstExprList() : AstExpression(AstType::ExprList) {}
    
    void addExpression(AstExpression *expr) { list.push_back(expr); }
    std::vector<AstExpression *> getList() { return list; }
    
    void print();
private:
    std::vector<AstExpression *> list;
};

// Represents the base of operators
class AstOp : public AstExpression {
public:
    bool isBinaryOp() { return isBinary; }

    virtual void print() {}
protected:
    bool isBinary = true;
};

// Represents the base of a unary expression
class AstUnaryOp : public AstOp {
public:
    AstUnaryOp() {
        isBinary = false;
    }

    void setVal(AstExpression *val) { this->val = val; }
    AstExpression *getVal() { return val; }
    
    virtual void print() {}
protected:
    AstExpression *val;
};

// Represents a negate expression
class AstNegOp : public AstUnaryOp {
public:
    AstNegOp() {
        this->type = AstType::Neg;
    }
    
    void print();
};

// Represents the base of a binary expression
class AstBinaryOp : public AstOp {
public:
    void setLVal(AstExpression *lval) { this->lval = lval; }
    void setRVal(AstExpression *rval) { this->rval = rval; }
    void setPrecedence(int p) { this->precedence = p; }
    
    AstExpression *getLVal() { return lval; }
    AstExpression *getRVal() { return rval; }
    int getPrecedence() { return precedence; }
    
    virtual void print() {}
protected:
    AstExpression *lval, *rval;
    int precedence = 0;
};

// Represents an assignment operation
class AstAssignOp : public AstBinaryOp {
public:
    explicit AstAssignOp() {
        this->type = AstType::Assign;
        this->precedence = 16;
    }
    
    explicit AstAssignOp(AstExpression *lval, AstExpression *rval) {
        this->type = AstType::Assign;
        this->precedence = 16;
        this->lval = lval;
        this->rval = rval;
    }
    
    void print();
};

// Represents an add operation
class AstAddOp : public AstBinaryOp {
public:
    AstAddOp() {
        this->type = AstType::Add;
        this->precedence = 4;
    }
    
    void print();
};

// Represents a subtraction operation
class AstSubOp : public AstBinaryOp {
public:
    AstSubOp() {
        this->type = AstType::Sub;
        this->precedence = 4;
    }
    
    void print();
};

// Represents a multiplication operation
class AstMulOp : public AstBinaryOp {
public:
    AstMulOp() {
        this->type = AstType::Mul;
        this->precedence = 3;
    }
    
    void print();
};

// Represents a division operation
class AstDivOp : public AstBinaryOp {
public:
    AstDivOp() {
        this->type = AstType::Div;
        this->precedence = 3;
    }
    
    void print();
};

// Represents a division operation
class AstAndOp : public AstBinaryOp {
public:
    AstAndOp() {
        this->type = AstType::And;
        this->precedence = 8;
    }
    
    void print();
};

// Represents an or operation
class AstOrOp : public AstBinaryOp {
public:
    AstOrOp() {
        this->type = AstType::Or;
        this->precedence = 10;
    }
    
    void print();
};

// Represents a xor operation
class AstXorOp : public AstBinaryOp {
public:
    AstXorOp() {
        this->type = AstType::Xor;
        this->precedence = 9;
    }
    
    void print();
};

// Represents an equal-to operation
class AstEQOp : public AstBinaryOp {
public:
    AstEQOp() {
        this->type = AstType::EQ;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a not-equal-to operation
class AstNEQOp : public AstBinaryOp {
public:
    AstNEQOp() {
        this->type = AstType::NEQ;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a greater-than operation
class AstGTOp : public AstBinaryOp {
public:
    AstGTOp() {
        this->type = AstType::GT;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a less-than operation
class AstLTOp : public AstBinaryOp {
public:
    AstLTOp() {
        this->type = AstType::LT;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a greater-than-or-equal operation
class AstGTEOp : public AstBinaryOp {
public:
    AstGTEOp() {
        this->type = AstType::GTE;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a less-than-or-equal operation
class AstLTEOp : public AstBinaryOp {
public:
    AstLTEOp() {
        this->type = AstType::LTE;
        this->precedence = 6;
    }
    
    void print();
};

// Represents a logical AND operation
class AstLogicalAndOp : public AstBinaryOp {
public:
    AstLogicalAndOp() {
        this->type = AstType::LogicalAnd;
        this->precedence = 11;
    }
    
    void print();
};

// Represents a logical OR operation
class AstLogicalOrOp : public AstBinaryOp {
public:
    AstLogicalOrOp() {
        this->type = AstType::LogicalOr;
        this->precedence = 12;
    }
    
    void print();
};

// Represents a character literal
class AstChar : public AstExpression {
public:
    explicit AstChar(char val) : AstExpression(AstType::CharL) {
        this->val = val;
    }
    
    char getValue() { return val; }
    void print();
private:
    char val = 0;
};

// Represents a byte literal
class AstI8 : public AstExpression {
public:
    explicit AstI8(uint8_t val) : AstExpression(AstType::I8L) {
        this->val = val;
    }
    
    uint8_t getValue() { return val; }
    void print();
private:
    uint8_t val = 0;
};

// Represents a word literal
class AstI16 : public AstExpression {
public:
    explicit AstI16(uint16_t val) : AstExpression(AstType::I16L) {
        this->val = val;
    }
    
    uint16_t getValue() { return val; }
    void print();
private:
    uint16_t val = 0;
};

// Represents an integer literal
class AstI32 : public AstExpression {
public:
    explicit AstI32(uint64_t val) : AstExpression(AstType::I32L) {
        this->val = val;
    }
    
    void setValue(uint64_t val) { this->val = val; }
    
    uint64_t getValue() { return val; }
    void print();
private:
    uint64_t val = 0;
};

// Represents a QWord literal
class AstI64 : public AstExpression {
public:
    explicit AstI64(uint64_t val) : AstExpression(AstType::I64L) {
        this->val = val;
    }
    
    uint64_t getValue() { return val; }
    void print();
private:
    uint64_t val = 0;
};

// Represents a string literal
class AstString : public AstExpression {
public:
    explicit AstString(std::string val) : AstExpression(AstType::StringL) {
        this->val = val;
    }
    
    std::string getValue() { return val; }
    void print();
private:
    std::string val = "";
};

// Represents a variable reference
class AstID: public AstExpression {
public:
    explicit AstID(std::string val) : AstExpression(AstType::ID) {
        this->val = val;
    }
    
    std::string getValue() { return val; }
    void print();
private:
    std::string val = "";
};

// Represents an array access
class AstArrayAccess : public AstExpression {
public:
    explicit AstArrayAccess(std::string val) : AstExpression(AstType::ArrayAccess) {
        this->val = val;
    }
    
    void setIndex(AstExpression *index) { this->index = index; }
    
    std::string getValue() { return val; }
    AstExpression *getIndex() { return index; }
    void print();
private:
    std::string val = "";
    AstExpression *index;
};

// Represents a structure access
class AstStructAccess : public AstExpression {
public:
    explicit AstStructAccess(std::string var, std::string member) : AstExpression(AstType::StructAccess) {
        this->var = var;
        this->member = member;
    }

    std::string getName() { return var; }
    std::string getMember() { return member; }

    void print();
private:
    std::string var = "";
    std::string member = "";
};

// Represents a function call
class AstFuncCallExpr : public AstExpression {
public:
    explicit AstFuncCallExpr(std::string name) : AstExpression(AstType::FuncCallExpr) {
        this->name = name;
    }
    
    void setArgExpression(AstExpression *expr) { this->expr = expr; }
    AstExpression *getArgExpression() { return expr; }
    std::string getName() { return name; }
    
    void print();
private:
    AstExpression *expr;
    std::string name = "";
};

