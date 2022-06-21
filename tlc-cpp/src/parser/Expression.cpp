//
// Copyright 2021-2022 Patrick Flynn
// This file is part of the Tiny Lang compiler.
// Tiny Lang is licensed under the BSD-3 license. See the COPYING file for more information.
//
#include <iostream>

#include <parser/Parser.hpp>
#include <ast/ast.hpp>

// Builds a constant expression value
AstExpression *Parser::buildConstExpr(Token token) {
    switch (token.type) {
        case True: return new AstI32(1);
        case False: return new AstI32(0);
        case CharL: return new AstChar(token.i8_val);
        case Int32: return new AstI32(token.i32_val);
        case String: return new AstString(token.id_val);
        
        default: {}
    }
    
    return nullptr;
}

bool Parser::buildOperator(Token token, ExprContext *ctx) {
    switch (token.type) {
        case Assign:
        case Plus: 
        case Minus:
        case Mul:
        case Div:
        case And:
        case Or:
        case Xor:
        case EQ:
        case NEQ:
        case GT:
        case LT:
        case GTE:
        case LTE:
        case Logical_And:
        case Logical_Or: {
            AstBinaryOp *op = new AstBinaryOp;
            AstUnaryOp *op1 = new AstUnaryOp;
            bool useUnary = false;
            switch (token.type) {
                case Assign: op = new AstAssignOp; break;
                case Plus: op = new AstAddOp; break;
                case Mul: op = new AstMulOp; break;
                case Div: op = new AstDivOp; break;
                case And: op = new AstAndOp; break;
                case Or: op = new AstOrOp; break;
                case Xor: op = new AstXorOp; break;
                case EQ: op = new AstEQOp; break;
                case NEQ: op = new AstNEQOp; break;
                case GT: op = new AstGTOp; break;
                case LT: op = new AstLTOp; break;
                case GTE: op = new AstGTEOp; break;
                case LTE: op = new AstLTEOp; break;
                case Logical_And: op = new AstLogicalAndOp; break;
                case Logical_Or: op = new AstLogicalOrOp; break;
                case Minus: {
                    if (ctx->lastWasOp) {
                        op1 = new AstNegOp;
                        useUnary = true;
                    } else {
                        op = new AstSubOp;
                    }
                } break;
            }
            
            if (ctx->opStack.size() > 0 && useUnary == false) {
                AstOp *top = ctx->opStack.top();
                    if (top->isBinaryOp()) {
                    AstBinaryOp *op2 = static_cast<AstBinaryOp *>(top);
                    if (op->getPrecedence() > op2->getPrecedence()) {
                        if (!applyHigherPred(ctx)) return false;
                    }
                }
            }
            
            if (useUnary) ctx->opStack.push(op1);
            else ctx->opStack.push(op);
            ctx->lastWasOp = true;
        } break;
        
        default: {}
    }   
    
    return true;        
}

bool Parser::buildIDExpr(Token token, ExprContext *ctx) {
    ctx->lastWasOp = false;
    int currentLine = scanner->getLine();

    std::string name = token.id_val;
    if (ctx->varType == DataType::Void) {
        ctx->varType = typeMap[name].first;
        if (ctx->varType == DataType::Ptr) ctx->varType = typeMap[name].second;
    }
    
    token = scanner->getNext();
    if (token.type == LBracket) {
        //AstExpression *index = nullptr;
        //buildExpression(nullptr, DataType::I32, RBracket, EmptyToken, &index);
        AstExpression *index = buildExpression(DataType::I32, RBracket);
        
        AstArrayAccess *acc = new AstArrayAccess(name);
        acc->setIndex(index);
        ctx->output.push(acc);
    } else if (token.type == LParen) {
        if (currentLine != scanner->getLine()) {
            syntax->addWarning(scanner->getLine(), "Function call on newline- possible logic error.");
        }
        
        if (!isFunc(name)) {
            syntax->addError(scanner->getLine(), "Unknown function call.");
            return false;
        }
    
        AstFuncCallExpr *fc = new AstFuncCallExpr(name);
        AstExpression *args = buildExpression(ctx->varType, RParen, false, true);
        fc->setArgExpression(args);
        
        ctx->output.push(fc);
    } else if (token.type == Dot) {
        // TODO: Search for structures here

        Token idToken = scanner->getNext();
        if (idToken.type != Id) {
            syntax->addError(scanner->getLine(), "Expected identifier.");
            return false;
        }
        
        AstStructAccess *val = new AstStructAccess(name, idToken.id_val);
        ctx->output.push(val);
    } else {
        int constVal = isConstant(name);
        if (constVal > 0) {
            if (constVal == 1) {
                AstExpression *expr = globalConsts[name].second;
                ctx->output.push(expr);
            } else if (constVal == 2) {
                AstExpression *expr = localConsts[name].second;
                ctx->output.push(expr);
            }
        } else {
            if (isVar(name)) {
                AstID *id = new AstID(name);
                ctx->output.push(id);
            } else {
                syntax->addError(scanner->getLine(), "Unknown variable.");
                return false;
            }
        }
        
        scanner->rewind(token);
    }
    return true;
}

// Applies higher precedence for an operator
bool Parser::applyHigherPred(ExprContext *ctx) {
    if (ctx->output.empty()) {
        syntax->addError(scanner->getLine(), "Invalid expression: No RVAL");
        return false;
    }
    AstExpression *rval = checkExpression(ctx->output.top(), ctx->varType);
    ctx->output.pop();
    
    if (ctx->output.empty()) {
        syntax->addError(scanner->getLine(), "Invalid expression: No LVAL");
        return false;
    }
    AstExpression *lval = checkExpression(ctx->output.top(), ctx->varType);
    ctx->output.pop();
    
    AstBinaryOp *op = static_cast<AstBinaryOp *>(ctx->opStack.top());
    ctx->opStack.pop();
    
    op->setLVal(lval);
    op->setRVal(rval);
    ctx->output.push(op);
    
    return true;
}

// Applies operator associativity
bool Parser::applyAssoc(ExprContext *ctx) {
    AstType lastOp = AstType::EmptyAst;
    while (ctx->opStack.size() > 0) {
        if (ctx->output.empty()) {
            syntax->addError(scanner->getLine(), "Invalid expression: No RVAL");
            return false;
        }
        AstExpression *rval = checkExpression(ctx->output.top(), ctx->varType);
        ctx->output.pop();
        
        if (ctx->output.empty()) {
            syntax->addError(scanner->getLine(), "Invalid expression: No LVAL");
            return false;
        }
        AstExpression *lval = checkExpression(ctx->output.top(), ctx->varType);
        ctx->output.pop();
        
        AstBinaryOp *op = static_cast<AstBinaryOp *>(ctx->opStack.top());
        ctx->opStack.pop();
        
        if (op->getType() == lastOp) {
            AstBinaryOp *op2 = static_cast<AstBinaryOp *>(rval);
            
            rval = op2->getRVal();
            op2->setRVal(op2->getLVal());
            op2->setLVal(lval);
            
            lval = op2;
        }
        
        op->setLVal(lval);
        op->setRVal(rval);
        ctx->output.push(op);
        
        lastOp = op->getType();
    }
    
    return true;
}

// Our new expression builder
AstExpression *Parser::buildExpression(DataType currentType, TokenType stopToken, bool isConst, bool buildList) {
    ExprContext *ctx = new ExprContext;
    ctx->varType = currentType;
    
    AstExprList *list = new AstExprList;
    bool isList = buildList;
    
    Token token = scanner->getNext();
    while (token.type != Eof && token.type != stopToken) {
        switch (token.type) {
            case True:
            case False:
            case CharL:
            case Int32:
            case String: {
                ctx->lastWasOp = false;
                AstExpression *expr = buildConstExpr(token);
                ctx->output.push(expr);
            } break;
            
            case Id: {
                if (!buildIDExpr(token, ctx)) return nullptr;
            } break;
            
            case Assign:
            case Plus: 
            case Minus:
            case Mul:
            case Div:
            case And:
            case Or:
            case Xor:
            case EQ:
            case NEQ:
            case GT:
            case LT:
            case GTE:
            case LTE:
            case Logical_And:
            case Logical_Or: {
                if (!buildOperator(token, ctx)) {
                    return nullptr;
                }
            } break;
            
            case LParen: {
                AstExpression *subExpr = buildExpression(ctx->varType, RParen, false, isList);
                if (!subExpr) {
                    return nullptr;
                }
                ctx->output.push(subExpr);
                ctx->lastWasOp = false;
            } break;
            
            // TODO: We need some syntax checking with this
            case RParen: break;
            
            case Comma: {
                applyAssoc(ctx);
                AstExpression *expr = checkExpression(ctx->output.top(), ctx->varType);
                list->addExpression(expr);
                while (ctx->output.size() > 0) ctx->output.pop();
                while (ctx->opStack.size() > 0) ctx->opStack.pop();
                isList = true;
            } break;
            
            default: {
                syntax->addError(scanner->getLine(), "Invalid token in expression.");
                return nullptr;
            }
        }
        
        if (!ctx->lastWasOp && ctx->opStack.size() > 0) {
            if (ctx->opStack.top()->getType() == AstType::Neg) {
                AstExpression *val = checkExpression(ctx->output.top(), ctx->varType);
                ctx->output.pop();
                
                AstNegOp *op = static_cast<AstNegOp *>(ctx->opStack.top());
                ctx->opStack.pop();
                op->setVal(val);
                ctx->output.push(op);
            }
        }
        
        token = scanner->getNext();
    }
    
    if (token.type == Eof) {
        syntax->addError(scanner->getLine(), "Invalid expression-> missing \';\'.");
        return nullptr;
    }
    
    // Build the expression
    applyAssoc(ctx);
    
    
    if (ctx->output.size() == 0) {
        return list;
    }
    
    // Type check the top
    AstExpression *expr = checkExpression(ctx->output.top(), ctx->varType);
    
    if (isList) {
        list->addExpression(expr);
        return list;
    }
    return expr;
}

