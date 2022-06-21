FILE: test/parser/007.tl

FUNC main -> VOID is
   AST_VAR x I32 (x := 20)
   AST_VAR y1 I32 (y1 := (x + 30))
   AST_VAR y2 I32 (y2 := (x - 40))
   AST_VAR y3 I32 (y3 := (x * 50))
   AST_VAR y4 I32 (y4 := (x / 60))
   AST_VAR y5 I32 (y5 := (x % 70))
   AST_VAR y6 I32 (y6 := (x & 80))
   AST_VAR y7 I32 (y7 := (x | 90))
   AST_VAR y8 I32 (y8 := (x ^ 100))
end

