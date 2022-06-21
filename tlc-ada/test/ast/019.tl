FILE: test/parser/019.tl

FUNC f1 -> VOID is
   AST_RETURN  VOID 
end

FUNC f2 -> I32 is
   AST_RETURN  VOID 3
end

FUNC f3 -> I64 is
   AST_RETURN  VOID (4 + x)
end

