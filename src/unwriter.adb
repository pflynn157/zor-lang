--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;

package body Unwriter is

--
-- Forward declarations
--
procedure unwrite_block(block : AstBlock; indent : integer := 4);
procedure unwrite_statement(stmt : AstStatement; indent : integer);
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true);
procedure unwrite_data_type(data_type : DataType);

--
-- The entry point of the unwriter
--
procedure unwrite(file : AstFile) is
    arg_index : Count_Type := 0;
begin
    -- Imports
    if file.imports.Length > 0 then
        for import of file.imports loop
            Put("import ");
            for c of To_String(import) loop
                if c = '/' then
                    Put(".");
                else
                    Put(c);
                end if;
            end loop;
            Put_Line(";");
        end loop;
        New_Line;
    end if;

    -- Global constants if they exist
    if file.consts.Length > 0 then
        for const of file.consts loop
            unwrite_statement(const, 0);
        end loop;
    end if;
    
    -- Print structures
    for struct of file.structs loop
        Put_Line("struct " & To_String(struct.name) & " is");
        for arg of struct.args loop
            for i in 0 .. 4 loop Put(" "); end loop;
            Put(To_String(arg.name) & " : ");
            unwrite_data_type(arg.data_type);
            Put(" := ");
            unwrite_expression(arg.expr);
            Put_Line(";");
        end loop;
        Put_Line("end");
    end loop;
    
    -- Now write functions
    for func of file.funcs loop
        Put("func " & To_String(func.name));
        if func.args.Length > 0 then
            Put("(");
            for arg of func.args loop
                Put(To_String(arg.name) & " : ");
                unwrite_data_type(arg.data_type);
                if arg_index + 1 < (func.args.Length) then
                    Put(", ");
                end if;
                arg_index := arg_index + 1;
            end loop;
            Put(")");
        end if;
        if func.data_type /= Void then
            Put(" -> ");
            unwrite_data_type(func.data_type);
        end if;
        Put_Line(" is");
        unwrite_block(func.block);
        Put_Line("end");
        
        arg_index := 0;
    end loop;
end unwrite;

--
-- Uwrites a statement block
--
procedure unwrite_block(block : AstBlock; indent : integer := 4) is
begin
    for stmt of block.statements loop
        unwrite_statement(stmt, indent);
    end loop;
end unwrite_block;

--
-- Unwrites a statement
--
procedure unwrite_statement(stmt : AstStatement; indent : integer) is
    procedure Do_Indent is
    begin
        if indent > 0 then
            for i in 0 .. indent loop Put(" "); end loop;
        end if;
    end Do_Indent;
begin
    Do_Indent;

    case stmt.ast_type is
        when AST_Var =>
            Put("var " & To_String(stmt.name) & " : ");
            unwrite_data_type(stmt.data_type);
            unwrite_expression(stmt.expr, false);
            Put_Line(";");
            
        when AST_Const =>
            Put("const " & To_String(stmt.name) & " : ");
            unwrite_data_type(stmt.data_type);
            unwrite_expression(stmt.expr, false);
            Put_Line(";");
            
        when AST_Struct =>
            Put("struct " & To_String(stmt.name) & " : ");
            unwrite_expression(stmt.expr);
            Put_Line(";");
            
        when AST_Array =>
            Put("var " & To_String(stmt.name) & " : ");
            unwrite_data_type(stmt.data_type);
            Put("[");
            unwrite_expression(stmt.expr);
            Put_Line("];");
            
        when AST_Call_Stmt =>
            Put(To_String(stmt.name) & "(");
            unwrite_expression(stmt.expr);
            Put_Line(");");
        
        when AST_Return =>
            Put("return");
            if Has_Expression(stmt) then
                Put(" ");
                unwrite_expression(stmt.expr);
            end if;
            Put_Line(";");
            
        when AST_Expr_Stmt =>
            unwrite_expression(stmt.expr);
            Put_Line(";");
            
        when AST_While =>
            Put("while ");
            unwrite_expression(stmt.expr);
            Put_Line(" do");
            unwrite_block(stmt.block.all, indent + 4);
            Do_Indent; Put_Line("end");
            
        when AST_If | AST_Elif =>
            if stmt.ast_type = AST_Elif then Put("elif ");
            else Put("if ");
            end if;
            unwrite_expression(stmt.expr);
            Put_Line(" then");
            unwrite_block(stmt.block.all, indent + 4);
            for br of stmt.block.all.branches loop
                unwrite_statement(br, indent);
            end loop;
            if stmt.ast_type = AST_If then
                Do_Indent; Put_Line("end");
            end if;
            
        when AST_Else =>
            Put_Line("else");
            unwrite_block(stmt.block.all, indent + 4);
            
        when AST_Break => Put_Line("break;");
        when AST_Continue => Put_Line("continue;");
        
        when others => null;
    end case;
end unwrite_statement;

--
-- Unwrites an expression
--
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true) is
begin
    case expr.ast_type is
        -- Literals and identifiers
        when AST_Int => Put(expr.int_value, 0);
        when AST_String => Put('"' & To_String(expr.string_value) & '"');
        when AST_Char => Put("'" & expr.char_value & "'");
        when AST_Id => Put(To_String(expr.string_value));
        
        when AST_Array_Acc =>
            Put(To_String(expr.string_value) & "[");
            unwrite_expression(expr.sub_expr.all);
            Put("]");
            
        when AST_Struct_Acc =>
            Put(To_String(expr.string_value) & ".");
            unwrite_expression(expr.sub_expr.all);
        
        when AST_True => Put("true");
        when AST_False => Put("false");
        
        -- Operators
        when AST_Assign |
             AST_Add | AST_Sub | AST_Mul | AST_Div | AST_Mod |
             AST_And | AST_Or | AST_Xor |
             AST_Eq | AST_Ne | AST_Gt | AST_Ge | AST_Lt | AST_Le |
             AST_Lg_And | AST_Lg_Or =>
            if print_lval then
                unwrite_expression(expr.lval.all);
            end if;
            case expr.ast_type is
                when AST_Assign => Put(" := ");
                when AST_Add => Put(" + ");
                when AST_Sub => Put(" - ");
                when AST_Mul => Put(" * ");
                when AST_Div => Put(" / ");
                when AST_Mod => Put(" % ");
                when AST_And => Put(" & ");
                when AST_Or => Put(" | ");
                when AST_Xor => Put(" ^ ");
                
                when AST_Eq => Put(" = ");
                when AST_Ne => Put(" != ");
                when AST_Gt => Put(" > ");
                when AST_Ge => Put(" >= ");
                when AST_Lt => Put(" < ");
                when AST_Le => Put(" <= ");
                
                when AST_Lg_And => Put(" && ");
                when AST_Lg_Or => Put(" || ");
                
                when others => null;
            end case;
            unwrite_expression(expr.rval.all);
            
        -- Expression list
        when AST_Expr_List =>
            for i in 0 .. (expr.list_size - 1) loop
                unwrite_expression(expr.list(i).all);
                if i < (expr.list_size - 1) then
                    Put(", ");
                end if;
            end loop;
            
        when AST_Call_Expr =>
            Put(To_String(expr.string_value) & "(");
            unwrite_expression(expr.sub_expr.all);
            Put(")");
        
        -- Other
        when others => null;
    end case;
end unwrite_expression;

--
-- Unwrites a data type
--
procedure unwrite_data_type(data_type : DataType) is
begin
    case data_type is
        when I8 => Put("i8");
        when U8 => Put("u8");
        
        when I16 => Put("i16");
        when U16 => Put("u16");
        
        when I32 => Put("i32");
        when U32 => Put("u32");
        
        when I64 => Put("i64");
        when U64 => Put("u64");
        
        when Char => Put("char");
        when Str => Put("string");
        when Bool => Put("bool");
        
        when others => Put("void");
    end case;
end unwrite_data_type;

end Unwriter; -- End package

