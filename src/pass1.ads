--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with ast; use ast;

--
-- Pass 1 performs the basic verification checks.
-- Which are:
-- 1) Variable existence before being used
-- 2) Type checking and assignment
-- 3) Function existence when called
--
--

package pass1 is
    function evaluate_error1(testing : boolean := false) return boolean;
    procedure run_pass1(file : in out AstFile);
end pass1;

