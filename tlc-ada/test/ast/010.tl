FILE: test/parser/010.tl

FUNC main -> VOID is
   AST_WHILE  VOID (x < 10)
       AST_CALL_STMT println VOID "Hello!"
       AST_WHILE  VOID (y < 20)
           AST_CALL_STMT println VOID "YO"
   AST_CALL_STMT println VOID "Done!"
end

