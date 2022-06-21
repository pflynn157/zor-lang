with ast; use ast;

--
-- Pass 1 performs the basic verification checks.
-- Which are:
-- 1) Variable existence before being used
-- 2) Function existence when called
--
--

package pass1 is
    procedure run_pass1(file : AstFile);
end pass1;

