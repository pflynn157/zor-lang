//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#include <iostream>
#include <string>
#include <vector>

#include <parser/Parser.hpp>
#include <ast/ast.hpp>

// Builds a variable declaration
// A variable declaration is composed of an Alloca and optionally, an assignment
bool Parser::buildVariableDec(AstBlock *block) {
    Token token = scanner->getNext();
    std::vector<std::string> toDeclare;
    toDeclare.push_back(token.id_val);
    
    if (token.type != Id) {
        syntax->addError(scanner->getLine(), "Expected variable name.");
        return false;
    }
    
    token = scanner->getNext();
    
    while (token.type != Colon) {
        if (token.type == Comma) {
            token = scanner->getNext();
            
            if (token.type != Id) {
                syntax->addError(scanner->getLine(), "Expected variable name.");
                return false;
            }
            
            toDeclare.push_back(token.id_val);
        } else if (token.type != Colon) {
            syntax->addError(scanner->getLine(), "Invalid token in variable declaration.");
            return false;
        }
        
        token = scanner->getNext();
    }
    
    token = scanner->getNext();
    DataType dataType = DataType::Void;
    bool isString = false;
    
    switch (token.type) {
        case Bool: dataType = DataType::Bool; break;
        case Char: dataType = DataType::Char; break;
        case I8: dataType = DataType::I8; break;
        case U8: dataType = DataType::U8; break;
        case I16: dataType = DataType::I16; break;
        case U16: dataType = DataType::U16; break;
        case I32: dataType = DataType::I32; break;
        case U32: dataType = DataType::U32; break;
        case I64: dataType = DataType::I64; break;
        case U64: dataType = DataType::U64; break;
        case Str: dataType = DataType::String; break;
        
        default: {
            syntax->addError(scanner->getLine(), "Invalid data type.");
            return false;
        }
    }
    
    token = scanner->getNext();
    
    // We have an array
    if (token.type == LBracket) {
        AstVarDec *empty = new AstVarDec("", DataType::Ptr);
        AstExpression *arg = buildExpression(DataType::I32, RBracket);
        if (!arg) return false;
        empty->setExpression(arg); 
        
        token = scanner->getNext();
        if (token.type != SemiColon) {
            syntax->addError(scanner->getLine(), "Error: Expected \';\'.");
            return false;
        }
        
        for (std::string name : toDeclare) {
            vars.push_back(name);
            AstVarDec *vd = new AstVarDec(name, DataType::Ptr);
            block->addStatement(vd);
            vd->setExpression(empty->getExpression());
            vd->setPtrType(dataType);
            
            // Create an assignment to a malloc call
            AstExprStatement *va = new AstExprStatement;
            va->setDataType(DataType::Ptr, dataType);
            block->addStatement(va);
            
            AstID *id = new AstID(name);
            AstFuncCallExpr *callMalloc = new AstFuncCallExpr("malloc");
            AstAssignOp *assign = new AstAssignOp(id, callMalloc);
            
            va->setExpression(assign);
            
            // In order to get a proper malloc, we need to multiply the argument by
            // the size of the type. Get the arguments, and do that
            AstExprList *list = new AstExprList;
            callMalloc->setArgExpression(list);
            
            AstI32 *size;
            if (dataType == DataType::I32 | dataType == DataType::U32) size = new AstI32(4);
            else if (dataType == DataType::I64 || dataType == DataType::U64) size = new AstI32(8);
            else if (dataType == DataType::String) size = new AstI32(8);
            else size = new AstI32(1);
            
            AstMulOp *op = new AstMulOp;
            op->setLVal(size);
            op->setRVal(vd->getExpression());
            list->addExpression(op);
            
            // Finally, set the size of the declaration
            vd->setPtrSize(vd->getExpression());
            
            typeMap[name] = std::pair<DataType, DataType>(DataType::Ptr, dataType);
        }
    
    // We're at the end of the declaration
    } else if (token.type == SemiColon) {
        syntax->addError(scanner->getLine(), "Expected init expression.");
        return false;
        
    // Otherwise, we have a regular variable
    } else {
        AstExpression *arg = buildExpression(dataType);
        if (!arg) return false;
    
        for (std::string name : toDeclare) {
            vars.push_back(name);
            AstVarDec *vd = new AstVarDec(name, dataType);
            block->addStatement(vd);
            
            auto typePair = std::pair<DataType, DataType>(dataType, DataType::Void);
            typeMap[name] = typePair;
            
            AstID *id = new AstID(name);
            AstAssignOp *assign = new AstAssignOp(id, arg);
            
            AstExprStatement *va = new AstExprStatement;
            va->setDataType(dataType);
            va->setExpression(assign);
            block->addStatement(va);
        }
    }
    
    return true;
}

// Builds a variable or an array assignment
bool Parser::buildVariableAssign(AstBlock *block, Token idToken) {
    DataType dataType = typeMap[idToken.id_val].first;
    DataType ptrType = typeMap[idToken.id_val].second;
    
    AstExpression *expr = buildExpression((dataType == DataType::Ptr) ? dataType : ptrType);
    if (!expr) return false;
    
    AstExprStatement *stmt = new AstExprStatement;
    stmt->setDataType(dataType, ptrType);
    stmt->setExpression(expr);
    block->addStatement(stmt);
    
    return true;
}

// Builds a constant variable
bool Parser::buildConst(bool isGlobal) {
    Token token = scanner->getNext();
    std::string name = token.id_val;
    
    // Make sure we have a name for our constant
    if (token.type != Id) {
        syntax->addError(scanner->getLine(), "Expected constant name.");
        return false;
    }
    
    // Syntax check
    token = scanner->getNext();
    if (token.type != Colon) {
        syntax->addError(scanner->getLine(), "Expected \':\' in constant expression.");
        return false;
    }
    
    // Get the data type
    token = scanner->getNext();
    DataType dataType = DataType::Void;
    
    switch (token.type) {
        case Bool: dataType = DataType::Bool; break;
        case Char: dataType = DataType::Char; break;
        case I8: dataType = DataType::I8; break;
        case U8: dataType = DataType::U8; break;
        case I16: dataType = DataType::I16; break;
        case U16: dataType = DataType::U16; break;
        case I32: dataType = DataType::I32; break;
        case U32: dataType = DataType::U32; break;
        case I64: dataType = DataType::I64; break;
        case U64: dataType = DataType::U64; break;
        case Str: dataType = DataType::String; break;
        
        default: {
            syntax->addError(scanner->getLine(), "Unknown data type.");
            return false;
        }
    }
    
    // Final syntax check
    token = scanner->getNext();
    if (token.type != Assign) {
        syntax->addError(scanner->getLine(), "Expected \'=\' after const assignment.");
        return false;
    }
    
    // Build the expression. We create a dummy statement for this
    AstExpression *expr = buildExpression(dataType, SemiColon, true);
    if (!expr) return false;
    
    // Put it all together
    if (isGlobal) {
        globalConsts[name] = std::pair<DataType, AstExpression*>(dataType, expr);
    } else {
        localConsts[name] = std::pair<DataType, AstExpression*>(dataType, expr);
    }
    
    return true;
}
