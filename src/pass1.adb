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
    P1_Unknown_Dec,
    P1_Unknown_Struct_Dec,
    P1_Unknown_Array_Dec
);
type error_array is array (0 .. 100) of error_type;
error_list : error_array;
error_list_index : integer := 0;

type name_array is array (0 .. 100) of Unbounded_String;
name_list : name_array;
name_list_index : integer := 0;

procedure add_error(error : error_type; var_name : Unbounded_String) is
begin
    error_list(error_list_index) := error;
    error_list_index := error_list_index + 1;
    
    name_list(name_list_index) := var_name;
    name_list_index := name_list_index + 1;
end add_error;

--
-- Performs the error evaluation
--
function evaluate_error1(testing : boolean := false) return boolean is
    error : error_type := P1_None;
begin
    if error_list_index = 0 then
        return true;
    end if;
    
    for i in 0 .. (error_list_index - 1) loop
        error := error_list(i);
        if testing then
            case error is
                when P1_Redec => put_line("P1_REDEC;");
                when P1_Unknown_Dec => put_line("P1_UNKNOWN_DEC;");
                when P1_Unknown_Struct_Dec => put_line("P1_UNKNOWN_STRUCT_DEC;");
                when P1_Unknown_Array_Dec => put_line("P1_UNKNOWN_ARRAY_DEC;");
                when others => put_line("NONE;");
            end case;
        else
            case error is
                when P1_Redec =>
                    put_line("Fatal: Variable redeclaration: " & to_string(name_list(i)));
                    
                when P1_Unknown_Dec =>
                    put_line("Fatal: Unknown variable declaration: "  & to_string(name_list(i)));
                    
                when P1_Unknown_Struct_Dec =>
                    put_line("Fatal: Unknown structure declaration: " & to_string(name_list(i)));
                    
                when P1_Unknown_Array_Dec =>
                    put_line("Fatal: Unknown array declaration: " & to_string(name_list(i)));
                    
                when others => put_line("Unknown error from pass 1: " & error_type'Image(error));
            end case;
        end if;
    end loop;
    
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
    procedure scan_expression(expr : AstExpression) is
    begin
        case get_type(expr) is
            -- Expressions
            when AST_Expr_List =>
                for i in 0 .. get_list_size(expr) loop
                    scan_expression(get_list_item(expr, i));
                end loop;
            
            when AST_Call_Expr => scan_expression(get_sub_expression(expr));
            
            -- Operators
            when AST_Assign
                | AST_Add | AST_Sub | AST_Mul | AST_Div | AST_Mod
                | AST_And | AST_Or | AST_Xor
                | AST_Eq | AST_Ne
                | AST_Gt | AST_Ge
                | AST_Lt | AST_Le
                | AST_Lg_And | AST_Lg_Or =>
                    scan_expression(expr.lval.all);
                    scan_expression(expr.rval.all);
            
            -- Literals
            when AST_Id =>
                if not table.contains(get_name(expr)) then
                    add_error(P1_Unknown_Dec, get_name(expr));
                end if;
                
            when AST_Array_Acc =>
                if not table.contains(get_name(expr)) then
                    add_error(P1_Unknown_Array_Dec, get_name(expr));
                end if;
                scan_expression(get_sub_expression(expr));
                
            when AST_Struct_Acc =>
                if not table.contains(get_name(expr)) then
                    add_error(P1_Unknown_Struct_Dec, get_name(expr));
                end if;
                -- TODO: We need controls for the child (member)
            
            when others => null;
        end case;
    end scan_expression;
begin
    -- Load any arguments
    for i in 0 .. get_arg_size(func) loop
        table.append(get_name(get_arg(func, i)));
    end loop;

    -- Scan the statements
    for statement of func.block.statements loop
        case statement.ast_type is
            when AST_Var | AST_Array | AST_Struct =>
                if table.contains(statement.name) then
                    add_error(P1_Redec, get_name(statement));
                else
                    table.append(statement.name);
                end if;
            
            when others =>
                if has_expression(statement) then
                    scan_expression(get_expression(statement));
                end if;
        end case;
    end loop;
end step1;

end pass1;  -- End package pass1

