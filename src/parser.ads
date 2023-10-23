--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ast; use Ast;

package Parser is

    function Parse(name : string) return AstFile;

end Parser;
