FILE: test/parser/027.tl

AST_CONST num U64 (num := 200)


FUNC main -> VOID is
   AST_CONST x I32 (x := 20)
   AST_CONST s STR (s := "Hello!")
   AST_RETURN  VOID 0
end

