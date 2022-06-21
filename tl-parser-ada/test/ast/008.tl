FILE: test/parser/008.tl

FUNC main -> VOID is
   AST_VAR x I32 (x := 20)
   AST_VAR y1 BOOL (y1 := (x = 30))
   AST_VAR y2 BOOL (y2 := (x > 40))
   AST_VAR y3 BOOL (y3 := (x < 50))
   AST_VAR y4 BOOL (y4 := (x != 60))
   AST_VAR y5 BOOL (y5 := (x >= 70))
   AST_VAR y6 BOOL (y6 := (x <= 80))
end

