FILE: test/parser/003.tl

FUNC sayHello -> VOID is
   AST_CALL_STMT println VOID "Hello!"
end

FUNC main -> VOID is
   AST_CALL_STMT sayHello VOID 
end

