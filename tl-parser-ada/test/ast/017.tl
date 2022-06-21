FILE: test/parser/017.tl

FUNC main -> VOID is
   AST_VAR x1 I32 (x1 := n[2])
   AST_VAR y1 I32 (y1 := n[i])
   AST_VAR z1 I32 (z1 := n[(i * 2)])
   AST_VAR x2 I32 (x2 := (1 + (n[2] * 3)))
   AST_VAR y2 I32 (y2 := (2 + (n[i] * 4)))
   AST_VAR z2 I32 (z2 := (5 + (n[(i * 2)] * 6)))
end

