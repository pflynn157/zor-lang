FILE: test/parser/016.tl

FUNC main -> VOID is
   AST_VAR x I32 (x := (get_x(AST_NONE) * 2))
   AST_VAR y I32 (y := (get_x(1) * (2 + 3)))
   AST_VAR z I32 (z := (get_x({1, 2, 3}) * (2 + 4)))
   AST_CALL_STMT print VOID 
end

