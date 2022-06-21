with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package AST is

    --
    -- AST types
    --
    type AstType is (
        AST_None,
        
        -- Statements
        AST_Import,
        AST_Var,
        AST_Struct,
        AST_Array,
        AST_Const,
        AST_Return,
        AST_Call_Stmt,
        AST_While,
        AST_If, AST_Elif, AST_Else,
        AST_Expr_Stmt,
        AST_Break, AST_Continue,
        
        -- Expressions
        AST_Expr_List,
        AST_Call_Expr,
        
        -- Operators
        AST_Assign,
        AST_Add,
        AST_Sub,
        AST_Mul,
        AST_Div,
        AST_Mod,
        AST_And,
        AST_Or,
        AST_Xor,
        
        AST_Eq, AST_Ne,
        AST_Gt, AST_Ge,
        AST_Lt, AST_Le,
        AST_Lg_And, AST_Lg_Or,
        
        -- Literals
        AST_Id,
        AST_Array_Acc,
        AST_Struct_Acc,
        AST_Int,
        AST_String,
        AST_Char,
        AST_True,
        AST_False
    );
    
    type DataType is (
        Void,
        I8, U8,
        I16, U16,
        I32, U32,
        I64, U64,
        Char, Str,
        Bool
    );

    --
    -- Expression representation
    --
    type AstExpression;
    type AstExprObj is access AstExpression;
    type AstExprList is array (0 .. 20) of AstExprObj;
    type AstExpression is record
        ast_type : AstType := AST_None;
        lval, rval : AstExprObj;
        sub_expr : AstExprObj;
        
        int_value : integer := 0;
        string_value : Unbounded_String;
        char_value : character := ' ';
        
        -- Only for lists
        list : AstExprList;
        list_size : integer := 0;
    end record;

    --
    -- Statement representation
    --
    type AstBlock;
    type AstBlockObj is access AstBlock;
    
    type AstStatement is record
        ast_type : AstType := AST_None;
        expr : AstExpression;
        data_type : DataType := Void;
        name : Unbounded_String;
        block : AstBlockObj;
    end record;
    
    package AstStmtVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstStatement);
          
    --
    -- Block representation
    --
    type AstBlock is record
        statements : AstStmtVector.Vector;
        
        -- For conditionals
        branches : AstStmtVector.Vector;
    end record;
    
    --
    -- Argument representation
    --
    type AstArg is record
        name : Unbounded_String;
        data_type : DataType := Void;
        expr : AstExpression;
    end record;
    
    package AstArgVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstArg);

    --
    -- Function representation
    --
    type AstFunction is record
        name : Unbounded_String;
        data_type : DataType := Void;
        block : AstBlock;
        args : AstArgVector.Vector;
    end record;

    package AstFuncVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstFunction);
          
    --
    -- Structure representation
    --
    type AstStruct is record
        name : Unbounded_String;
        args : AstArgVector.Vector;
    end record;
    
    package AstStructVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstStruct);
          
    --
    -- String list
    --
    package StringList is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => Unbounded_String);

    --
    -- The file representation
    --
    type AstFile is record
        name : Unbounded_String;
        imports : StringList.Vector;
        consts : AstStmtVector.Vector;      -- Only meant to hold constants
        structs : AstStructVector.Vector;
        funcs : AstFuncVector.Vector;
    end record;
    
    --
    -- Functions
    --
    function Has_Expression(stmt : AstStatement) return boolean;
    procedure Create_Binary_Op(op : in out AstExpression; lval, rval : AstExpression);
    procedure Set_Sub_Expr(op : in out AstExpression; expr : AstExpression);
    procedure Add_List_Item(op : in out AstExpression; item : AstExpression);
    procedure Set_Expression(stmt : in out AstStatement; expr : AstExpression);
    procedure Set_Name(stmt : in out AstStatement; name : Unbounded_String);
    procedure Set_Data_Type(stmt : in out AstStatement; data_type : DataType);
    procedure Add_Branch(block : in out AstBlock; stmt : AstStatement);
    procedure Add_Statement(block : in out AstBlock; stmt : AstStatement);
    procedure Add_Statement(func : in out AstFunction; stmt : AstStatement);
    procedure Add_Function(Self: in out AstFile; ast_func : AstFunction);
    procedure Add_Struct(file : in out AstFile; struct : AstStruct);
    procedure Add_Struct_Item(struct : in out AstStruct; name : Unbounded_String; data_type : DataType; expr : AstExpression);
    procedure Add_Global_Const(file : in out AstFile; const : AstStatement);
    procedure Print_Ast(file : AstFile);
    
    -- Helper functions
    function Create_Ast_File(name : string) return AstFile;
    function Create_Ast_Struct(name : string) return AstStruct;
    function Create_Ast_Function(name : string; data_type : DataType := Void) return AstFunction;
    function Create_Ast_Statement(ast_type : AstType) return AstStatement;
    function Create_Ast_Expression(ast_type : AstType) return AstExpression;

end AST; -- End AST definition

