FILE: test/parser/018.tl

FUNC f1 -> VOID is
   AST_RETURN  VOID 
end

FUNC f2 -> VOID is
   AST_RETURN  VOID 3
end

FUNC f3 -> VOID is
   AST_RETURN  VOID (4 + x)
end

