with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Vectors;

with Ast; use Ast;
with Lex; use Lex;

package body Parser is

--
-- Global declarations
--
package AstExprStack is new Ada.Containers.Vectors
    ( Index_Type => Natural,
      Element_Type => AstExpression);

--
-- Forward declarations
--
procedure Parse_Import(file : in out AstFile);
procedure Parse_Struct(file : in out AstFile);
procedure Parse_Function(file : in out AstFile);
procedure Parse_Block(block : in out AstBlock);
function Parse_Const return AstStatement;
function Parse_Expression(end_token : TokenType) return AstExpression;
procedure Parse_Data_Type(data_type : in out DataType);

--
-- The main section of the AST parser
--
function Parse(name : string) return AstFile is
    file : AstFile := Create_Ast_File(name);
    const_stmt : AstStatement;
    t : Token;
begin
    Lex_Init(name);
    
    -- Parse the global scope
    t := Lex_Get_Next;
    while t.token_type /= T_Eof loop
        case t.token_type is
            when T_Import => Parse_Import(file);
            when T_Struct => Parse_Struct(file);
            when T_Func => Parse_Function(file);
            
            when T_Const =>
                const_stmt := Parse_Const;
                Add_Global_Const(file, const_stmt);
            
            when others =>
                Put_Line("Error: Invalid token in global scope.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
    
    -- Return our finished product
    Lex_Close;
    return file;
end Parse;

--
-- The import parser
--
procedure Parse_Import(file : in out AstFile) is
    t : Token;
    path : Unbounded_String;
begin
    t := Lex_Get_Next;
    while t.token_type /= T_SemiColon loop
        case t.token_type is
            when T_Id => Append(path, t.string_value);
            when T_Dot => Append(path, "/");
            
            when others =>
                Put_Line("Error: Invalid token in import.");
                Put_Line(TokenType'Image(t.token_type));
                return;
        end case;
        
        t := Lex_Get_Next;
    end loop;
    
    file.imports.Append(path);
end Parse_Import;

--
-- The structure parser
--
procedure Parse_Struct(file : in out AstFile) is
    t : Token;
    struct_name : Unbounded_String;
    struct : AstStruct;
    
    -- For constructing items
    item_name : Unbounded_String;
    item_type : DataType := Void;
    item_expr : AstExpression;
begin
    -- Start with the structure name
    t := Lex_Get_Next;
    struct_name := t.string_value;
    if t.token_type /= T_Id then
        Put_Line("Error: Expected structure name.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    -- Next token should be is
    t := Lex_Get_Next;
    if t.token_type /= T_Is then
        Put_Line("Error: Expected is.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    -- Create the AST element
    struct := Create_Ast_Struct(To_String(struct_name));
    
    -- Parse the items
    t := Lex_Get_Next;
    while t.token_type /= T_End loop
        item_name := t.string_value;
        if t.token_type /= T_Id then
            Put_Line("Error: Expected structure item name.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        if t.token_type /= T_Colon then
            Put_Line("Error: Expected colon in structure item.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        Parse_Data_Type(item_type);
        
        t := Lex_Get_Next;
        if t.token_type /= T_Assign then
            Put_Line("Error: Structure items require an initial value.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        item_expr := Parse_Expression(T_SemiColon);
        
        -- Add the item
        Add_Struct_Item(struct, item_name, item_type, item_expr);
        
        -- Move on
        t := Lex_Get_Next;
    end loop;
    
    -- Add to the file structure
    Add_Struct(file, struct);
end Parse_Struct;

--
-- The function parser
--
procedure Parse_Function(file : in out AstFile) is
    t : Token;
    func_name : Unbounded_String;
    func : AstFunction;
    data_type : DataType := Void;
    args : AstArgVector.Vector;
begin
    -- Start with the function name
    t := Lex_Get_Next;
    func_name := t.string_value;
    if t.token_type /= T_Id then
        Put_Line("Error: Expected function name.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    t := Lex_Get_Next;
    
    -- Arguments
    if t.token_type = T_LParen then
        t := Lex_Get_Next;
        while t.token_type /= T_RParen loop
            declare
                arg : AstArg;
            begin
                arg.name := t.string_value;
                if t.token_type /= T_Id then
                    Put_Line("Error: Invalid argument: Expected name.");
                    Put_Line(TokenType'Image(t.token_type));
                    return;
                end if;
                
                t := Lex_Get_Next;
                if t.token_type /= T_Colon then
                    Put_Line("Error: Invalid argument: Expected colon.");
                    Put_Line(TokenType'Image(t.token_type));
                    return;
                end if;
                
                Parse_Data_Type(arg.data_type);
                args.Append(arg);
                
                t := Lex_Get_Next;
                if t.token_type = T_Comma then
                    t := Lex_Get_Next;
                end if;
            end;
        end loop;
        
        t := Lex_Get_Next;
    end if;
    
    -- Return type
    if t.token_type = T_Arrow then
        Parse_Data_Type(data_type);
        t := Lex_Get_Next;
    end if;
    
    -- Next token should be "is"
    if t.token_type /= T_Is then
        Put_Line("Error: Expected is.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    -- Construct the AST function and build the block
    func := Create_Ast_Function(To_String(func_name), data_type);
    func.args := args;
    Parse_Block(func.block);
    Add_Function(file, func);
end Parse_Function;

--
-- The statement block parser
--
procedure Parse_Block(block : in out AstBlock) is
    t : Token;
    stmt : AstStatement;
    expr : AstExpression;
    
    -- A helper function for parsing variable declarations
    procedure Parse_Var_Dec is
        name : Unbounded_String;
        data_type : DataType := Void;
        lval : AstExpression;
    begin
        -- Collect variable information and perform basic syntax checks
        t := Lex_Get_Next;
        name := t.string_value;
        if t.token_type /= T_Id then
            Put_Line("Error: Invalid token in variable declaration: Expected name");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        if t.token_type /= T_Colon then
            Put_Line("Error: Expected colon.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        Parse_Data_Type(data_type);
        
        -- Check the next token
        t := Lex_Get_Next;
        
        -- Array declaration
        if t.token_type = T_LBracket then
            expr := Parse_Expression(T_RBracket);
            stmt := Create_Ast_Statement(AST_Array);
            Set_Name(stmt, name);
            Set_Data_Type(stmt, data_type);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
            t := Lex_Get_Next;
            if t.token_type /= T_SemiColon then
                Put_Line("Error: Expected terminator.");
                Put_Line(TokenType'Image(t.token_type));
                return;
            end if;
        
        -- Regular variable assignment
        elsif t.token_type = T_Assign then
            Lex_Unget(t);
        
            -- Now, parse the expression
            expr := Parse_Expression(T_SemiColon);
            
            lval := Create_Ast_Expression(AST_Id);
            lval.string_value := name;
            Create_Binary_Op(expr, lval, expr.rval.all);
            
            -- Create the statement
            stmt := Create_Ast_Statement(AST_Var);
            Set_Name(stmt, name);
            Set_Data_Type(stmt, data_type);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
        -- Syntax error
        else
            Put_Line("Error: Expected '[' or ':='");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
    end Parse_Var_Dec;
    
    -- Parses a structure declaration
    procedure Parse_Struct_Dec is
        var_name, struct_name : Unbounded_String;
        struct_id : AstExpression;
    begin
        -- Syntax checks and gather info
        t := Lex_Get_Next;
        var_name := t.string_value;
        if t.token_type /= T_Id then
            Put_Line("Error: Expected variable name.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        if t.token_type /= T_Colon then
            Put_Line("Error: Expected colon between variable name and structure name.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        struct_name := t.string_value;
        if t.token_type /= T_Id then
            Put_Line("Error: Expected structure name.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        if t.token_type /= T_SemiColon then
            Put_line("Error: Expected terminator.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        -- Construct the AST element
        struct_id := Create_Ast_Expression(AST_Id);
        struct_id.string_value := struct_name;
        
        stmt := Create_Ast_Statement(AST_Struct);
        Set_Name(stmt, var_name);
        Set_Expression(stmt, struct_id);
        Add_Statement(block, stmt);
    end Parse_Struct_Dec;
    
    -- A helper function for parsing ID expressions
    -- TODO: See how we can condense this
    procedure Parse_Id is
        name : Unbounded_String := t.string_value;
        lval, sub_expr : AstExpression;
    begin
        t := Lex_Get_Next;
        if t.token_type = T_LParen then
            stmt := Create_Ast_Statement(AST_Call_Stmt);
            Set_Name(stmt, name);
            expr := Parse_Expression(T_RParen);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
            t := Lex_Get_Next;
            if t.token_type /= T_SemiColon then
                Put_Line("Error: Expected terminator following function call.");
                Put_Line(TokenType'Image(t.token_type));
                return;
            end if;
            
        elsif t.token_type = T_Assign then
            Lex_Unget(t);
            
            expr := Parse_Expression(T_SemiColon);
            
            lval := Create_Ast_Expression(AST_Id);
            lval.string_value := name;
            Create_Binary_Op(expr, lval, expr.rval.all);
            
            -- Create the statement
            stmt := Create_Ast_Statement(AST_Expr_Stmt);
            Set_Name(stmt, name);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
        elsif t.token_type = T_LBracket then
            sub_expr := Parse_Expression(T_RBracket);
            
            expr := Parse_Expression(T_SemiColon);
            
            lval := Create_Ast_Expression(AST_Array_Acc);
            lval.string_value := name;
            Set_Sub_Expr(lval, sub_expr);
            Create_Binary_Op(expr, lval, expr.rval.all);
            
            -- Create the statement
            stmt := Create_Ast_Statement(AST_Expr_Stmt);
            Set_Name(stmt, name);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
        elsif t.token_type = T_Dot then
            -- Create the lval
            lval := Create_Ast_Expression(AST_Struct_Acc);
            lval.string_value := name;
            
            t := Lex_Get_Next;
            if t.token_type /= T_Id then
                Put_Line("Error: Invalid token following structure reference.");
                Put_Line(TokenType'Image(t.token_type));
            end if;
            
            sub_expr := Create_Ast_Expression(AST_Id);
            sub_expr.string_value := t.string_value;
            Set_Sub_Expr(lval, sub_expr);
            
            -- Create the structure
            expr := Parse_Expression(T_SemiColon);
            Create_Binary_Op(expr, lval, expr.rval.all);
            
            -- Create the statement
            stmt := Create_Ast_Statement(AST_Expr_Stmt);
            Set_Name(stmt, name);
            Set_Expression(stmt, expr);
            Add_Statement(block, stmt);
            
        else
            Put_Line("Error: Invalid token following ID.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
    end Parse_Id;
    
    -- A helper function to handle sub-blocks
    procedure Parse_Sub_Block(is_branch : boolean := false) is
        block2 : AstBlock;
        block_obj : AstBlockObj;
    begin
        Parse_Block(block2);
        block_obj := new AstBlock'(statements => block2.statements, branches => block2.branches);
        stmt.block := block_obj;
        
        if is_branch then
            Add_Branch(block, stmt);
        else
            Add_Statement(block, stmt);
        end if;
    end Parse_Sub_Block;
    
    -- Main Parse body
begin
    t := Lex_Get_Next;
    while t.token_type /= T_End and t.token_type /= T_Eof loop
        case t.token_type is
            when T_Var => Parse_Var_Dec;
            when T_Struct => Parse_Struct_Dec;
            when T_Id => Parse_Id;
            
            when T_Const =>
                stmt := Parse_Const;
                Add_Statement(block, stmt);
            
            when T_While =>
                stmt := Create_Ast_Statement(AST_While);
                expr := Parse_Expression(T_Do);
                Set_Expression(stmt, expr);
                Parse_Sub_Block;
                
            when T_If =>
                stmt := Create_Ast_Statement(AST_If);
                expr := Parse_Expression(T_Then);
                Set_Expression(stmt, expr);
                Parse_Sub_Block;
            
            when T_Elif =>
                stmt := Create_Ast_Statement(AST_Elif);
                expr := Parse_Expression(T_Then);
                Set_Expression(stmt, expr);
                Parse_Sub_Block(true);
                return;
            
            when T_Else =>
                stmt := Create_Ast_Statement(AST_Else);
                Parse_Sub_Block(true);
                return;
        
            when T_Return =>
                stmt := Create_Ast_Statement(AST_Return);
                expr := Parse_Expression(T_SemiColon);
                if expr.ast_type /= AST_None then
                    Set_Expression(stmt, expr);
                end if;
                Add_Statement(block, stmt);
                
            when T_Break | T_Continue =>
                if t.token_type = T_Continue then stmt := Create_Ast_Statement(AST_Continue);
                else stmt := Create_Ast_Statement(AST_Break);
                end if;
                Add_Statement(block, stmt);
                t := Lex_Get_Next;
                if t.token_type /= T_SemiColon then
                    Put_Line("Error: Expected terminator.");
                    Put_Line(TokenType'Image(t.token_type));
                end if;
            
            when others =>
                Put_Line("Error: Invalid token in statement.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
end Parse_Block;

--
-- Parses a constant declaration
--
function Parse_Const return AstStatement is
    t : Token;
    stmt : AstStatement := Create_Ast_Statement(AST_Const);
    name : Unbounded_String;
    data_type : DataType := Void;
    expr, lval : AstExpression;
begin
    -- The name
    t := Lex_Get_Next;
    name := t.string_value;
    if t.token_type /= T_Id then
        Put_Line("Error: Expected constant name.");
        Put_Line(TokenType'Image(t.token_type));
        return stmt;
    end if;
    
    -- Make sure we have a colon
    t := Lex_Get_Next;
    if t.token_type /= T_Colon then
        Put_Line("Error: Expected colon in constant declaration.");
        Put_Line(TokenType'Image(t.token_type));
        return stmt;
    end if;
    
    -- The data type
    Parse_Data_Type(data_type);
    
    -- Parse the assignment
    expr := Parse_Expression(T_SemiColon);
    
    lval := Create_Ast_Expression(AST_Id);
    lval.string_value := name;
    Create_Binary_Op(expr, lval, expr.rval.all);
    
    -- Create the statement
    Set_Name(stmt, name);
    Set_Data_Type(stmt, data_type);
    Set_Expression(stmt, expr);

    return stmt;
end Parse_Const;

--
-- The expression parser
--
function Parse_Expression(end_token : TokenType) return AstExpression is
    t : Token;
    expr, sub_expr : AstExpression;
    stack, op_stack : AstExprStack.Vector;
    
    -- For lists
    list_expr : AstExpression := Create_Ast_Expression(AST_Expr_List);
    is_list : boolean := false;
    
    procedure Process_Stack is
        lval, rval, op : AstExpression;
    begin
        while op_stack.Length > 0 loop
            op := op_stack.Last_Element;
            op_stack.Delete_Last;
            
            rval := stack.Last_Element;
            stack.Delete_Last;
            
            if op.ast_type = AST_Assign then
                lval := Create_Ast_Expression(AST_None);
            else
                lval := stack.Last_Element;
                stack.Delete_Last;
            end if;
            
            Create_Binary_Op(op, lval, rval);
            stack.Append(op);
        end loop;
    end Process_Stack;
begin
    t := Lex_Get_Next;
    while t.token_type /= end_token and t.token_type /= T_Eof loop
        case t.token_type is
            when T_Int =>
                expr := Create_Ast_Expression(AST_Int);
                expr.int_value := t.int_value;
                stack.Append(expr);
                
            when T_StringL =>
                expr := Create_Ast_Expression(AST_String);
                expr.string_value := t.string_value;
                stack.Append(expr);
                
            when T_CharL =>
                expr := Create_Ast_Expression(AST_Char);
                expr.char_value := t.char_value;
                stack.Append(expr);
                
            when T_True => stack.Append(Create_Ast_Expression(AST_True));
            when T_False => stack.append(Create_Ast_Expression(AST_False));
                
            -- TODO: Should probably go in a separate function
            when T_Id =>
                declare
                    name : Unbounded_String := t.string_value;
                begin
                    t := Lex_Get_Next;
                    if t.token_type = T_LParen then
                        expr := Create_Ast_Expression(AST_Call_Expr);
                        expr.string_value := name;
                        
                        sub_expr := Parse_Expression(T_RParen);
                        Set_Sub_Expr(expr, sub_expr);
                        
                        stack.Append(expr);
                        
                    elsif t.token_type = T_LBracket then
                        expr := Create_Ast_Expression(AST_Array_Acc);
                        expr.string_value := name;
                        
                        sub_expr := Parse_Expression(T_RBracket);
                        Set_Sub_Expr(expr, sub_expr);
                        
                        stack.Append(expr);
                       
                    elsif t.token_type = T_Dot then
                        expr := Create_Ast_Expression(AST_Struct_Acc);
                        expr.string_value := name;
                        
                        t := Lex_Get_Next;
                        if t.token_type /= T_Id then
                            Put_Line("Error: Invalid token following structure reference.");
                            Put_Line(TokenType'Image(t.token_type));
                        end if;
                        
                        sub_expr := Create_Ast_Expression(AST_Id);
                        sub_expr.string_value := t.string_value;
                        Set_Sub_Expr(expr, sub_expr);
                        stack.Append(expr);
                     
                    else
                        Lex_Unget(t);
                        
                        expr := Create_Ast_Expression(AST_Id);
                        expr.string_value := name;
                        stack.Append(expr);
                    end if;
                end;
                
            -- Operators
            when T_Assign => op_stack.Append(Create_Ast_Expression(AST_Assign));
            when T_Add => op_stack.Append(Create_Ast_Expression(AST_Add));
            when T_Sub => op_stack.Append(Create_Ast_Expression(AST_Sub));
            when T_Mul => op_stack.Append(Create_Ast_Expression(AST_Mul));
            when T_Div => op_stack.Append(Create_Ast_Expression(AST_Div));
            when T_Mod => op_stack.Append(Create_Ast_Expression(AST_Mod));
            when T_And => op_stack.Append(Create_Ast_Expression(AST_And));
            when T_Or => op_stack.Append(Create_Ast_Expression(AST_Or));
            when T_Xor => op_stack.Append(Create_Ast_Expression(AST_Xor));
            
            when T_Eq => op_stack.Append(Create_Ast_Expression(AST_Eq));
            when T_Ne => op_stack.Append(Create_Ast_Expression(AST_Ne));
            when T_Gt => op_stack.Append(Create_Ast_Expression(AST_Gt));
            when T_Ge => op_stack.Append(Create_Ast_Expression(AST_Ge));
            when T_Lt => op_stack.Append(Create_Ast_Expression(AST_Lt));
            when T_Le => op_stack.Append(Create_Ast_Expression(AST_Le));
            
            when T_Lg_And => op_stack.Append(Create_Ast_Expression(AST_Lg_And));
            when T_Lg_Or => op_stack.Append(Create_Ast_Expression(AST_Lg_Or));
            
            -- Comma -> denotes a list
            when T_Comma =>
                is_list := true;
                Process_Stack;
                --list_expr.list(list_expr.list_size) := stack.Last_Element;
                Add_List_Item(list_expr, stack.Last_Element);
                stack.Delete_Last;
            
            -- Unknown
            when others =>
                Put_Line("Error: Invalid token in expression.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
    
    Process_Stack;
    
    if is_list then
        --list_expr.list(list_expr.list_size) := stack.Last_Element;
        Add_List_Item(list_expr, stack.Last_Element);
        return list_expr;
    end if;

    if stack.Length = 0 then
        return Create_Ast_Expression(AST_None);
    end if;
    
    return stack.Last_Element;
end Parse_Expression;

--
-- Parses a data type token from the token stream
--
procedure Parse_Data_Type(data_type : in out DataType) is
    t : Token := Lex_Get_Next;
begin
    case t.token_type is
        when T_I8 => data_type := I8;
        when T_U8 => data_type := U8;
        
        when T_I16 => data_type := I16;
        when T_U16 => data_type := U16;
        
        when T_I32 => data_type := I32;
        when T_U32 => data_type := U32;
        
        when T_I64 => data_type := I64;
        when T_U64 => data_type := U64;
        
        when T_Char => data_type := Char;
        when T_String => data_type := Str;
        when T_Bool => data_type := Bool;
        
        when others =>
            Put_Line("Error: Invalid data type.");
            Put_Line(TokenType'Image(t.token_type));
            data_type := Void;
    end case;
end Parse_Data_Type;

end Parser; -- End body of Parser

