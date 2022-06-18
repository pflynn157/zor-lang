//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#include <iostream>

#include <parser/Parser.hpp>
#include <ast/ast.hpp>

// Returns the function arguments
bool Parser::getFunctionArgs(std::vector<Var> &args) {
    Token token = scanner->getNext();
    if (token.type == LParen) {
        token = scanner->getNext();
        while (token.type != Eof && token.type != RParen) {
            Token t1 = token;
            Token t2 = scanner->getNext();
            Token t3 = scanner->getNext();
            Var v;
            v.subType = DataType::Void;
            
            if (t1.type != Id) {
                syntax->addError(scanner->getLine(), "Invalid function argument: Expected name.");
                return false;
            }
            
            if (t2.type != Colon) {
                syntax->addError(scanner->getLine(), "Invalid function argument: Expected \':\'.");
                return false;
            }
            
            switch (t3.type) {
                case Bool: v.type = DataType::Bool; break;
                case Char: v.type = DataType::Char; break;
                case I8: v.type = DataType::I8; break;
                case U8: v.type = DataType::U8; break;
                case I16: v.type = DataType::I16; break;
                case U16: v.type = DataType::U16; break;
                case I32: v.type = DataType::I32; break;
                case U32: v.type = DataType::U32; break;
                case I64: v.type = DataType::I64; break;
                case U64: v.type = DataType::U64; break;
                case Str: v.type = DataType::String; break;
                
                case Id: {
                    bool isStruct = false;
                    for (auto s : tree->getStructs()) {
                        if (s->getName() == t3.id_val) {
                            isStruct = true;
                            break;
                        }
                    }
                    
                    if (isStruct) {
                        v.type = DataType::Struct;
                        v.typeName = t3.id_val;
                    }
                } break;
                
                default: {
                    syntax->addError(scanner->getLine(), "Invalid function argument: Unknown type.");
                    token.print();
                    return false;
                }
            }
            
            v.name = t1.id_val;
            vars.push_back(t1.id_val);
            
            token = scanner->getNext();
            if (token.type == Comma) {
                token = scanner->getNext();
            } else if (token.type == LBracket) {
                Token token1 = scanner->getNext();
                Token token2 = scanner->getNext();
                
                if (token1.type != RBracket) {
                    syntax->addError(scanner->getLine(), "Invalid type syntax.");
                    return false;
                }
                
                if (token2.type == Comma) token = scanner->getNext();
                else token = token2;
                
                v.subType = v.type;
                v.type = DataType::Ptr;
            }
            
            args.push_back(v);
            typeMap[v.name] = std::pair<DataType, DataType>(v.type, v.subType);
        }
    } else {
        scanner->rewind(token);
    }
    
    return true;
}

// Builds a function
bool Parser::buildFunction(Token startToken, std::string className) {
    typeMap.clear();
    localConsts.clear();
    vars.clear();
    
    Token token;
    bool isExtern = false;

    // Handle extern function
    if (startToken.type == Extern) {
        isExtern = true;
    }

    // Make sure we have a function name
    token = scanner->getNext();
    std::string funcName = token.id_val;
    
    if (token.type != Id) {
        syntax->addError(scanner->getLine(), "Expected function name.");
        return false;
    }
    
    // Get arguments
    std::vector<Var> args;
    if (!getFunctionArgs(args)) return false;

    // Check to see if there's any return type
    token = scanner->getNext();
    DataType funcType = DataType::Void;
    DataType ptrType = DataType::Void;
    std::string retName = "";
    
    if (token.type == Arrow) {
        token = scanner->getNext();
        switch (token.type) {
            case Bool: funcType = DataType::Bool; break;
            case Char: funcType = DataType::Char; break;
            case I8: funcType = DataType::I8; break;
            case U8: funcType = DataType::U8; break;
            case I16: funcType = DataType::I16; break;
            case U16: funcType = DataType::U16; break;
            case I32: funcType = DataType::I32; break;
            case U32: funcType = DataType::U32; break;
            case I64: funcType = DataType::I64; break;
            case U64: funcType = DataType::U64; break;
            case Str: funcType = DataType::String; break;
            
            case Id: {
                bool isStruct = false;
                for (auto s : tree->getStructs()) {
                    if (s->getName() == token.id_val) {
                        isStruct = true;
                        break;
                    }
                }
                    
                if (isStruct) {
                    funcType = DataType::Struct;
                    retName = token.id_val;
                }
            } break;
            
            default: {}
        }
    
        token = scanner->getNext();
        if (token.type == LBracket) {
            token = scanner->getNext();
            if (token.type != RBracket) {
                syntax->addError(scanner->getLine(), "Invalid function type.");
                return false;
            }
            
            ptrType = funcType;
            funcType = DataType::Ptr;
            
            token = scanner->getNext();
        }
    }
    
    // Do syntax error check
    if (token.type == SemiColon && !isExtern) {
        syntax->addError(scanner->getLine(), "Expected \';\' for extern function.");
        return false;
    } else if (token.type == Is && isExtern) {
        syntax->addError(scanner->getLine(), "Expected \'is\' keyword.");
        return false;
    }

    // Create the function object
    funcs.push_back(funcName);
    
    if (isExtern) {
        AstExternFunction *ex = new AstExternFunction(funcName);
        ex->setArguments(args);
        ex->setDataType(funcType);
        tree->addGlobalStatement(ex);
        return true;
    }
    
    AstFunction *func = new AstFunction(funcName);
    func->setDataType(funcType, ptrType);
    if (funcType == DataType::Struct) func->setDataTypeName(retName);
    func->setArguments(args);
    tree->addGlobalStatement(func);
    
    // Build the body
    int stopLayer = 0;
    if (!buildBlock(func->getBlock())) return false;
    
    // Make sure we end with a return statement
    AstType lastType = func->getBlock()->getBlock().back()->getType();
    if (lastType == AstType::Return) {
        AstStatement *ret = func->getBlock()->getBlock().back();
        if (func->getDataType() == DataType::Void && ret->hasExpression()) {
            syntax->addError(scanner->getLine(), "Cannot return from void function.");
            return false;
        } else if (!ret->hasExpression()) {
            syntax->addError(scanner->getLine(), "Expected return value.");
            return false;
        }
    } else {
        if (func->getDataType() == DataType::Void) {
            func->addStatement(new AstReturnStmt);
        } else {
            syntax->addError(scanner->getLine(), "Expected return statement.");
            return false;
        }
    }
    
    return true;
}

// Builds a function call
bool Parser::buildFunctionCallStmt(AstBlock *block, Token idToken) {
    // Make sure the function exists
    if (!isFunc(idToken.id_val)) {
        syntax->addError(scanner->getLine(), "Unknown function.");
        return false;
    }

    AstFuncCallStmt *fc = new AstFuncCallStmt(idToken.id_val);
    block->addStatement(fc);
    
    AstExpression *args = buildExpression(DataType::Void, SemiColon, false, true);
    if (!args) return false;
    fc->setExpression(args);
    
    return true;
}

// Builds a return statement
bool Parser::buildReturn(AstBlock *block) {
    AstReturnStmt *stmt = new AstReturnStmt;
    block->addStatement(stmt);
    
    AstExpression *arg = buildExpression(DataType::Void);
    if (!arg) return false;
    stmt->setExpression(arg);
    
    return true;
}

