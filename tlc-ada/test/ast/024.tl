FILE: test/parser/024.tl

STRUCT S1 is
     x : I32 := 10;
     y : I32 := 20;
end

FUNC main -> VOID is
   AST_STRUCT s VOID S1
   AST_EXPR_STMT s VOID (STRUCT(s.x) := 1)
   AST_VAR x I32 (x := STRUCT(s.y))
   AST_VAR y I32 (y := (STRUCT(s.y) + 20))
   AST_RETURN  VOID 0
end

