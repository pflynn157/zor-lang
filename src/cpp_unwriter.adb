--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;

package body Cpp_Unwriter is

--
-- Forward declarations
--
procedure unwrite_block(block : AstBlock; indent : integer := 4);
procedure unwrite_statement(stmt : AstStatement; indent : integer);
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true; is_println : boolean := false);
procedure unwrite_data_type(data_type : DataType);

-- Control variables
use_std_types : boolean := false;

--
-- The entry point of the unwriter
--
procedure unwrite(file : AstFile; use_stdint_types : boolean) is
    arg_index : Count_Type := 0;
begin
    use_std_types := use_stdint_types;
    
    -- Default needed imports
    put_line("#include <iostream>");
    put_line("#include <string>");
    if use_std_types then
        put_line("#include <cstdint>");
    end if;
    new_line;

    -- Imports
    if file.imports.Length > 0 then
        put_line("Warning: Imports currently ignored in C++ unparser.");
    end if;

    -- Global constants if they exist
    if file.consts.Length > 0 then
        for const of file.consts loop
            unwrite_statement(const, 0);
        end loop;
    end if;
    
    -- Print structures
    for struct of file.structs loop
        put_line("struct " & to_string(struct.name) & " {");
        for arg of struct.args loop
            for i in 0 .. 4 loop put(" "); end loop;
            unwrite_data_type(arg.data_type);
            put(" " & to_string(arg.name) & " = ");
            unwrite_expression(arg.expr);
            put_line(";");
        end loop;
        put_line("};");
        new_line;
    end loop;
    
    -- Now write functions
    for func of file.funcs loop
        unwrite_data_type(func.data_type);
        put(" " & To_String(func.name) & "(");
        
        if func.args.Length > 0 then
            for arg of func.args loop
                unwrite_data_type(arg.data_type);
                Put(" " & To_String(arg.name));
                if arg_index + 1 < (func.args.Length) then
                    Put(", ");
                end if;
                arg_index := arg_index + 1;
            end loop;
        end if;
        put_line(")");
        
        unwrite_block(func.block);
        new_line;
        
        arg_index := 0;
    end loop;
end unwrite;

--
-- Uwrites a statement block
--
procedure unwrite_block(block : AstBlock; indent : integer := 4) is
    procedure Do_Indent is
    begin
        if indent >= 4 and (indent - 4) > 0 then
            for i in 0 .. (indent - 4) loop put(" "); end loop;
        end if;
    end Do_Indent;
begin
    do_indent;
    put_line("{");
    
    for stmt of block.statements loop
        unwrite_statement(stmt, indent);
    end loop;
    
    do_indent;
    put_line("}");
end unwrite_block;

--
-- Unwrites a statement
--
-- TODO: We need a way to cache pointers and delete
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
            unwrite_data_type(stmt.data_type);
            Put(" " & To_String(stmt.name));
            unwrite_expression(stmt.expr, false);
            Put_Line(";");
            
        when AST_Const =>
            Put("const ");
            unwrite_data_type(stmt.data_type);
            Put(" " & To_String(stmt.name));
            unwrite_expression(stmt.expr, false);
            Put_Line(";");
        
        when AST_Struct =>
            unwrite_expression(stmt.expr);
            put(" " & To_String(stmt.name));
            put_line(";");
            
        when AST_Array =>
            unwrite_data_type(stmt.data_type);
            Put(" *" & To_String(stmt.name) & " = new ");
            unwrite_data_type(stmt.data_type);
            Put("[");
            unwrite_expression(stmt.expr);
            Put_Line("];");
        
        when AST_Call_Stmt =>
            if stmt.name = "println" then
                put("std::cout << ");
                unwrite_expression(stmt.expr, true, true);
                put_line(" << std::endl;");
            else
                put(to_string(stmt.name) & "(");
                unwrite_expression(stmt.expr);
                put_line(");");
            end if;
        
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
            Put("while (");
            unwrite_expression(stmt.expr);
            Put_Line(")");
            unwrite_block(stmt.block.all, indent + 4);
          
        
        when AST_If | AST_Elif =>
            if stmt.ast_type = AST_Elif then Put("else "); end if;
            put("if (");
            unwrite_expression(stmt.expr);
            put_line(")");
            unwrite_block(stmt.block.all, indent + 4);
            for br of stmt.block.all.branches loop
                unwrite_statement(br, indent);
            end loop;
        
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
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true; is_println : boolean := false) is
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
                when AST_Assign => Put(" = ");
                when AST_Add => Put(" + ");
                when AST_Sub => Put(" - ");
                when AST_Mul => Put(" * ");
                when AST_Div => Put(" / ");
                when AST_Mod => Put(" % ");
                when AST_And => Put(" & ");
                when AST_Or => Put(" | ");
                when AST_Xor => Put(" ^ ");
                
                when AST_Eq => Put(" == ");
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
                    if is_println then put(" << ");
                    else put(", ");
                    end if;
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
    if use_std_types then
        case data_type is
            when I8 => Put("int8_t");
            when U8 => Put("uint8_t");
            
            when I16 => Put("int16_t");
            when U16 => Put("uint16_t");
            
            when I32 => Put("int32_t");
            when U32 => Put("uint32_t");
            
            when I64 => Put("int64_t");
            when U64 => Put("uint64_t");
            
            when Char => Put("char");
            when Str => Put("std::string");
            when Bool => Put("bool");
            
            when others => Put("void");
        end case;
    else
        case data_type is
            when I8 => Put("signed char");
            when U8 => Put("unsigned char");
            
            when I16 => Put("short");
            when U16 => Put("unsigned short");
            
            when I32 => Put("int");
            when U32 => Put("unsigned int");
            
            when I64 => Put("long int");
            when U64 => Put("unsigned long int");
            
            when Char => Put("char");
            when Str => Put("std::string");
            when Bool => Put("bool");
            
            when others => Put("void");
        end case;
    end if;
end unwrite_data_type;

end Cpp_Unwriter; -- End package

