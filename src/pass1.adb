with ada.text_io;           use ada.text_io;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.containers;        use ada.containers;
with ada.containers.indefinite_ordered_maps;
with ada.containers.vectors;

package body pass1 is

--
-- Forward declarations
--
procedure step1(func : AstFunction);

--
-- Entry point to pass 1
--
procedure run_pass1(file : in out AstFile) is
begin
    put_line("Running pass1 verifications...");
    for func of file.funcs loop
        step1(func);
    end loop;
    put_line("Done");
    new_line;
end run_pass1;

--
-- Scans a function for any missing variable declarations
--
procedure step1(func : AstFunction) is
    -- Define the list
    package Name_List is new Ada.Containers.Vectors
        ( Index_Type => Natural, Element_Type => Unbounded_String);
    use Name_List;

    -- Contains the list of identifiers
    -- This is only added when we have a variable declaration
    table : Name_List.Vector;
    
    -- Scans an expression
    function scan_expression(expr : AstExpression) return boolean is
    begin
        case get_type(expr) is
            -- Expressions
            when AST_Expr_List => null;
            when AST_Call_Expr => null;
            
            -- Operators
            when AST_Assign
                | AST_Add | AST_Sub | AST_Mul | AST_Div | AST_Mod
                | AST_And | AST_Or | AST_Xor
                | AST_Eq | AST_Ne
                | AST_Gt | AST_Ge
                | AST_Lt | AST_Le
                | AST_Lg_And | AST_Lg_Or =>
                    if not scan_expression(expr.lval.all) then
                        return false;
                    elsif not scan_expression(expr.rval.all) then
                        return false;
                    end if;
            
            -- Literals
            when AST_Id => null;
            when AST_Array_Acc => null;
            when AST_Struct_Acc => null;
            
            when others => null;
        end case;
        
        return true;
    end scan_expression;
begin
    for statement of func.block.statements loop
        case statement.ast_type is
            when AST_Var =>
                if table.contains(statement.name) then
                    put_line("Fatal: Redeclaration of variable " & to_string(statement.name));
                    return;
                else
                    table.append(statement.name);
                end if;
            
            when others =>
                if has_expression(statement) then
                    if not scan_expression(get_expression(statement)) then
                        return;
                    end if;
                end if;
        end case;
    end loop;
end step1;

end pass1;  -- End package pass1

