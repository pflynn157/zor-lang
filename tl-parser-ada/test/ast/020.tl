FILE: test/parser/020.tl

FUNC add(x : I32, y : U32) -> I32 is
   AST_RETURN  VOID (x + y)
end

FUNC add2(x : I32, y : U32) -> VOID is
   AST_CALL_STMT println VOID (x + y)
end

