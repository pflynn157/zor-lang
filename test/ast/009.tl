FILE: test/parser/009.tl

FUNC main -> VOID is
   AST_CALL_STMT print VOID 
   AST_CALL_STMT print VOID 1
   AST_CALL_STMT print VOID {1, 2}
   AST_CALL_STMT print VOID (x + 10)
   AST_CALL_STMT print VOID {(x + 20), (x + 30)}
end

