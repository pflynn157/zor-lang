FILE: test/parser/023.tl

STRUCT S1 is
     x : I32 := 10;
     y : I32 := 20;
end

FUNC main -> VOID is
   AST_STRUCT s VOID S1
   AST_RETURN  VOID 0
end

