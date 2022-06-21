FILE: test/parser/025.tl


FUNC main -> VOID is
   AST_IF  VOID (x > (20 && (y > 30)))
       AST_CALL_STMT println VOID "YO"
   AST_IF  VOID (x > (40 || (y > 50)))
       AST_CALL_STMT println VOID "HI"
   AST_RETURN  VOID 0
end

