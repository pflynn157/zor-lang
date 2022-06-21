#include <iostream>

#include <debug/Unparse.hpp>

//
// Forward declarations
//
void unparse(AstExternFunction *func);
void unparse(AstFunction *func);
void unparse(AstBlock *block, int indent);
void unparse(AstExpression *expr);
void unparse(DataType type, DataType subType = DataType::Void);

//
// The entry point of the unparser
//
void unparse(AstTree *tree) {
    std::cout << "FILE: " << tree->getFile() << std::endl;
    std::cout << std::endl;
    
    for (auto stmt : tree->getGlobalStatements()) {
        if (stmt->getType() == AstType::ExternFunc) unparse(static_cast<AstExternFunction *>(stmt));
        else if (stmt->getType() == AstType::Func) unparse(static_cast<AstFunction *>(stmt));
    }
}

//
// Unparsing for functions
//
void unparse(AstExternFunction *func) {
    std::cout << "extern " << func->getName() << "(";
    
    auto args = func->getArguments();
    for (size_t i = 0; i<args.size(); i++) {
        auto arg = args.at(i);
        std::cout << arg.name << " : ";
        unparse(arg.type, arg.subType);
        
        if (i+1 < args.size()) std::cout << ", ";
    }
    
    std::cout << ")";
    if (func->getDataType() != DataType::Void) {
        std::cout << " -> ";
        unparse(func->getDataType());
    }
    std::cout << ";" << std::endl;
}

void unparse(AstFunction *func) {
    std::cout << std::endl;
    std::cout << "func " << func->getName() << "(";
    
    auto args = func->getArguments();
    for (size_t i = 0; i<args.size(); i++) {
        auto arg = args.at(i);
        std::cout << arg.name << " : ";
        unparse(arg.type, arg.subType);
        
        if (i+1 < args.size()) std::cout << ", ";
    }
    
    std::cout << ")";
    if (func->getDataType() != DataType::Void) {
        std::cout << " -> ";
        if (func->getPtrType() != DataType::Void)
            unparse(func->getPtrType(), func->getDataType());
        else unparse(func->getDataType());
    }
    std::cout << " is" << std::endl;
    unparse(func->getBlock(), 4);
    std::cout << "end" << std::endl;
}

//
// Forward declarations for all the statements
//
void unparse(AstFuncCallStmt *stmt);
void unparse(AstReturnStmt *stmt);
void unparse(AstVarDec *stmt);
void unparse(AstStructDec *stmt);
void unparse(AstBlockStmt *stmt, int indent);
void unparse(AstIfStmt *stmt, int indent);
void unparse(AstElifStmt *stmt, int indent);
void unparse(AstElseStmt *stmt, int indent);
void unparse(AstWhileStmt *stmt, int indent);
void unparse(AstBreak *stmt);
void unparse(AstContinue *stmt);

//
// Unparses a basic block within the AST
//
void unparse(AstBlock *block, int indent) {
    for (auto stmt : block->getBlock()) {
    
    }
}

void unparse(AstFuncCallStmt *stmt) {

}

void unparse(AstReturnStmt *stmt) {

}

void unparse(AstVarDec *stmt) {

}

void unparse(AstStructDec *stmt) {

}

void unparse(AstBlockStmt *stmt, int indent) {

}

void unparse(AstIfStmt *stmt, int indent) {

}

void unparse(AstElifStmt *stmt, int indent) {

}

void unparse(AstElseStmt *stmt, int indent) {

}

void unparse(AstWhileStmt *stmt, int indent) {

}

void unparse(AstBreak *stmt) {

}

void unparse(AstContinue *stmt) {

}

//
// Forward declaration for expression unparsers
//
void unparse(AstExprList *expr);
void unparse(AstNegOp *expr);
void unparse(AstAddOp *expr);
void unparse(AstSubOp *expr);
void unparse(AstMulOp *expr);
void unparse(AstDivOp *expr);
void unparse(AstAndOp *expr);
void unparse(AstOrOp *expr);
void unparse(AstXorOp *expr);
void unparse(AstEQOp *expr);
void unparse(AstNEQOp *expr);
void unparse(AstGTOp *expr);
void unparse(AstLTOp *expr);
void unparse(AstGTEOp *expr);
void unparse(AstLTEOp *expr);
void unparse(AstLogicalAndOp *expr);
void unparse(AstLogicalOrOp *expr);
void unparse(AstChar *expr);
void unparse(AstI8 *expr);
void unparse(AstI16 *expr);
void unparse(AstI32 *expr);
void unparse(AstI64 *expr);
void unparse(AstString *expr);
void unparse(AstID *expr);
void unparse(AstArrayAccess *expr);
void unparse(AstStructAccess *expr);
void unparse(AstFuncCallExpr *expr);

//
// Unparses an expression
//
void unparse(AstExpression *expr) {

}

void unparse(AstExprList *expr) {

}

void unparse(AstNegOp *expr) {

}

void unparse(AstAddOp *expr) {

}

void unparse(AstSubOp *expr) {

}

void unparse(AstMulOp *expr) {

}

void unparse(AstDivOp *expr) {

}

void unparse(AstAndOp *expr) {

}

void unparse(AstOrOp *expr) {

}

void unparse(AstXorOp *expr) {

}

void unparse(AstEQOp *expr) {

}

void unparse(AstNEQOp *expr) {

}

void unparse(AstGTOp *expr) {

}

void unparse(AstLTOp *expr) {

}

void unparse(AstGTEOp *expr) {

}

void unparse(AstLTEOp *expr) {

}

void unparse(AstLogicalAndOp *expr) {

}

void unparse(AstLogicalOrOp *expr) {

}

void unparse(AstChar *expr) {

}

void unparse(AstI8 *expr) {

}

void unparse(AstI16 *expr) {

}

void unparse(AstI32 *expr) {

}

void unparse(AstI64 *expr) {

}

void unparse(AstString *expr) {

}

void unparse(AstID *expr) {

}

void unparse(AstArrayAccess *expr) {

}

void unparse(AstStructAccess *expr) {

}

void unparse(AstFuncCallExpr *expr) {

}

//
// Unparses a data type
//
void unparse(DataType type, DataType subType) {
    switch (type) {
        case DataType::Void: std::cout << "void"; break;
        case DataType::Bool: std::cout << "bool"; break;
        case DataType::Char: std::cout << "char"; break;
        
        case DataType::I8: std::cout << "i8"; break;
        case DataType::U8: std::cout << "u8"; break;
        
        case DataType::I16: std::cout << "i16"; break;
        case DataType::U16: std::cout << "u16"; break;
        
        case DataType::I32: std::cout << "i32"; break;
        case DataType::U32: std::cout << "u32"; break;
        
        case DataType::I64: std::cout << "i64"; break;
        case DataType::U64: std::cout << "u64"; break;
        
        case DataType::String: std::cout << "string"; break;
        case DataType::Struct: std::cout << "struct"; break;
        
        case DataType::Ptr: {
            unparse(subType);
            std::cout << "[]";
        } break;
    }
}

