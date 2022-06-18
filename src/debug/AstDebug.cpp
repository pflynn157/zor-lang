//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#include <iostream>

#include <ast/ast.hpp>

std::string printDataType(DataType dataType) {
    switch (dataType) {
        case DataType::Void: return "void";
        case DataType::Bool: return "bool";
        case DataType::Char: return "char";
        case DataType::I8: return "i8";
        case DataType::U8: return "u8";
        case DataType::I16: return "i16";
        case DataType::U16: return "u16";
        case DataType::I32: return "i32";
        case DataType::U32: return "u32";
        case DataType::I64: return "i64";
        case DataType::U64: return "u64";
        case DataType::String: return "string";
        case DataType::Ptr: return "ptr";
        case DataType::Struct: return "struct";
    }
    return "";
}

void AstTree::print() {
    std::cout << "FILE: " << file << std::endl;
    std::cout << std::endl;
    
    for (auto str : structs) str->print();
    
    for (auto stmt : global_statements) {
        stmt->print();
    }
}

void AstExternFunction::print() {
    std::cout << "EXTERN FUNC " << name << "(";
    for (auto var : args) {
        std::cout << printDataType(var.type);
        if (var.subType != DataType::Void)
            std::cout << "*" << printDataType(var.subType);
        std::cout << ", ";
    }
    std::cout << ") ";
    std::cout << " -> " << printDataType(dataType);
    std::cout << std::endl;
}

void AstFunction::print() {
    std::cout << std::endl;
    std::cout << "FUNC " << name << "(";
    for (auto var : args) {
        std::cout << printDataType(var.type);
        if (var.subType != DataType::Void)
            std::cout << "*" << printDataType(var.subType);
        if (var.type == DataType::Struct)
            std::cout << "[" << var.typeName << "]";
        std::cout << ", ";
    }
    std::cout << ") -> ";
    std::cout << printDataType(dataType);
    std::cout << std::endl;
    
    block->print();
}

void AstBlock::print(int indent) {
    for (auto stmt : block) {
        for (int i = 0; i<indent; i++) std::cout << " ";
        switch (stmt->getType()) {
            case AstType::If:
            case AstType::Elif:
            case AstType::Else:
            case AstType::While: {
                static_cast<AstBlockStmt *>(stmt)->print(indent);
            } break;
            
            default: stmt->print();
        }
    }
}

void AstStruct::print() {
    std::cout << "STRUCT " << name << std::endl;
    
    for (auto var : items) {
        std::cout << var.name << " : " << printDataType(var.type);
        if (var.subType != DataType::Void)
            std::cout << "*" << printDataType(var.subType);
        if (var.type == DataType::Struct)
            std::cout << "[" << var.typeName << "]";
        std::cout << " ";
        defaultExpressions[var.name]->print();
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

void AstExprStatement::print() {
    std::cout << "EXPR " << printDataType(dataType);
    if (ptrType != DataType::Void) {
        std::cout << "*" << printDataType(ptrType);
    }
    
    std::cout << " ";
    getExpression()->print();
    std::cout << std::endl;
}

void AstFuncCallStmt::print() {
    std::cout << "FC " << name;
    getExpression()->print();
    std::cout << std::endl;
}

void AstReturnStmt::print() {
    std::cout << "RETURN ";
    if (getExpression()) getExpression()->print();
    std::cout << std::endl;
}

void AstVarDec::print() {
    std::cout << "VAR_DEC " << name << " : " << printDataType(dataType);
    if (ptrType != DataType::Void) {
        std::cout << "*" << printDataType(ptrType);
        std::cout << "[";
        size->print();
        std::cout << "]";
    }
    std::cout << std::endl;
}

void AstStructDec::print() {
    std::cout << "STRUCT " << varName << " : " << structName;
    if (noInit) std::cout << " NOINIT";
    std::cout << std::endl;
}

void AstIfStmt::print(int indent) {
    std::cout << "IF ";
    getExpression()->print();
    std::cout << " THEN" << std::endl;
    block->print(indent + 4);
    
    for (auto br : branches) {
        for (int i = 0; i<indent; i++) std::cout << " ";
        static_cast<AstBlockStmt *>(br)->print(indent);
    }
    
    for (int i = 0; i<indent; i++) std::cout << " ";
    std::cout << "end" << std::endl;
}

void AstElifStmt::print(int indent) {
    std::cout << "ELIF ";
    getExpression()->print();
    std::cout << " THEN" << std::endl;
    block->print(indent + 4);
}

void AstElseStmt::print(int indent) {
    std::cout << "ELSE" << std::endl;
    block->print(indent + 4);
}

void AstWhileStmt::print(int indent) {
    std::cout << "WHILE ";
    getExpression()->print();
    std::cout << " DO" << std::endl;
    
    block->print(indent+4);
}

void AstBreak::print() {
    std::cout << "BREAK" << std::endl;
}

void AstContinue::print() {
    std::cout << "CONTINUE" << std::endl;
}

void AstExprList::print() {
    std::cout << "{";
    for (auto item : list) {
        item->print();
        std::cout << ", ";
    }
    std::cout << "}";
}

void AstNegOp::print() {
    std::cout << "(-";
    val->print();
    std::cout << ")";
}

void AstAssignOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") := (";
    rval->print();
    std::cout << ")";
}

void AstAddOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") + (";
    rval->print();
    std::cout << ")";
}

void AstSubOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") - (";
    rval->print();
    std::cout << ")";
}

void AstMulOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") * (";
    rval->print();
    std::cout << ")";
}

void AstDivOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") / (";
    rval->print();
    std::cout << ")";
}

void AstAndOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") AND (";
    rval->print();
    std::cout << ")";
}

void AstOrOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") OR (";
    rval->print();
    std::cout << ")";
}

void AstXorOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") XOR (";
    rval->print();
    std::cout << ")";
}

void AstEQOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") == (";
    rval->print();
    std::cout << ")";
}

void AstNEQOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") != (";
    rval->print();
    std::cout << ")";
}

void AstGTOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") > (";
    rval->print();
    std::cout << ")";
}

void AstLTOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") < (";
    rval->print();
    std::cout << ")";
}

void AstGTEOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") >= (";
    rval->print();
    std::cout << ")";
}

void AstLTEOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") <= (";
    rval->print();
    std::cout << ")";
}

void AstLogicalAndOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") && (";
    rval->print();
    std::cout << ")";
}

void AstLogicalOrOp::print() {
    std::cout << "(";
    lval->print();
    std::cout << ") || (";
    rval->print();
    std::cout << ")";
}

void AstChar::print() {
    std::cout << "CHAR(" << val << ")";
}

void AstI8::print() {
    std::cout << val;
}

void AstI16::print() {
    std::cout << val;
}

void AstI32::print() {
    std::cout << val;
}

void AstI64::print() {
    std::cout << val;
}

void AstString::print() {
    std::cout << "\"" << val << "\"";
}

void AstID::print() {
    std::cout << val;
}

void AstArrayAccess::print() {
    std::cout << val << "[";
    index->print();
    std::cout << "]";
}

void AstStructAccess::print() {
    std::cout << var << "." << member;
}

void AstFuncCallExpr::print() {
    std::cout << name << "(";
    expr->print();
    std::cout << ")";
}

