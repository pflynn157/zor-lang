FILE: test/parser/011.tl

FUNC main -> VOID is
   AST_IF  VOID (x < 10)
       AST_CALL_STMT println VOID "Hello!"
   AST_ELIF  VOID (x < 20)
       AST_CALL_STMT println VOID "Hello 2"
       AST_IF  VOID (y < 30)
           AST_CALL_STMT println VOID "HELLO"
       AST_ELSE  VOID 
           AST_CALL_STMT println VOID "HI"
   AST_ELIF  VOID (x < 30)
       AST_CALL_STMT println VOID "Hello 3"
   AST_ELSE  VOID 
       AST_CALL_STMT println VOID "Hello 4"
   AST_CALL_STMT println VOID "Done!"
end

