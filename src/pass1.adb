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
-- Defintes the errors
--
type error_type is (
    P1_None,
    P1_Redec,
    P1_Unknown_Dec
);
error : error_type := P1_None;

--
-- Performs the error evaluation
--
function evaluate_error1(testing : boolean := false) return boolean is
begin
    if error = P1_None then
        return true;
    end if;
    
    if testing then
        case error is
            when P1_Redec => put_line("P1_REDEC");
            when P1_Unknown_Dec => put_line("P1_UNKNOWN_DEC");
            when others => null;
        end case;
    else
        case error is
            when P1_Redec => put_line("Fatal: Variable redeclaration.");
            when P1_Unknown_Dec => put_line("Fatal: Unknown variable declaration.");
            when others => null;
        end case;
    end if;
    
    return false;
end evaluate_error1;

--
-- Entry point to pass 1
--
procedure run_pass1(file : in out AstFile) is
begin
    for func of file.funcs loop
        step1(func);
    end loop;
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
            when AST_Id =>
                if not table.contains(get_name(expr)) then
                    error := P1_Unknown_Dec;
                end if;
                
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
                    error := P1_Redec;
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

