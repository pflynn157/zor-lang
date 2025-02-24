--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ast; use Ast;

package Cpp_Unwriter is
    procedure unwrite(file : AstFile; use_stdint_types : boolean);
end Cpp_Unwriter;

