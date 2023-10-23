--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers;         use Ada.Containers;
with Ada.Containers.Vectors;

package body Lex is

--
-- Global package variables
--
F : File_Type;

package TokenStack is new Ada.Containers.Vectors
    ( Index_Type => Natural,
      Element_Type => Token);
      
token_stack : TokenStack.Vector;

--
-- Init's the lexical analyzer
--
procedure Lex_Init(Path : string) is
begin
    Open(F, In_File, Path);
end Lex_Init;

procedure Lex_Close is
begin
    Close(F);
end Lex_Close;

--
-- Pushes a token to the stack for future retrieval
--
procedure Lex_Unget(t : Token) is
begin
    token_stack.Append(t);
end Lex_Unget;

--
-- The token getter function
--
function Lex_Get_Next return Token is
    t : Token := (T_None, To_Unbounded_String(""), 0, others => <>);
    buffer : Unbounded_String := To_Unbounded_String("");
    c : character := ' ';
    
    -- A helper function to see if we have a symbol
    function Is_Symbol return boolean is
    begin
        case c is
            when '(' | ')' => return true;
            when '[' | ']' => return true;
            when ';' | ':' | ',' | '.' | '=' => return true;
            when '+' | '-' | '*' | '/' | '%' => return true;
            when '&' | '^' | '|' => return true;
            when '!' | '>' | '<' => return true;
            
            when others => return false;
        end case;
    end Is_Symbol;
    
    -- A helper function to see if we have an integer literal
    function Is_Int return Boolean is
        Dummy : Integer;
    begin
        Dummy := Integer'Value(To_String(buffer));
        return True;
    exception
        when others => return False;
    end Is_Int;
    
    -- A helper function to get the symbol based on the character
    procedure Get_Symbol is
        c2 : character;
        eol : boolean;
    begin
        case c is
            when '(' => t.token_type := T_LParen;
            when ')' => t.token_type := T_RParen;
            when '[' => t.token_type := T_LBracket;
            when ']' => t.token_type := T_RBracket;
            when ';' => t.token_type := T_SemiColon;
            when ',' => t.token_type := T_Comma;
            when '.' => t.token_type := T_Dot;
            when '+' => t.token_type := T_Add;
            when '*' => t.token_type := T_Mul;
            when '/' => t.token_type := T_Div;
            when '%' => t.token_type := T_Mod;
            when '^' => t.token_type := T_Xor;
            when '=' => t.token_type := T_Eq;
            
            when '-' =>
                Look_Ahead(F, c2, eol);
                if c2 = '>' then
                    Get_Immediate(F, c);
                    t.token_type := T_Arrow;
                else
                    t.token_type := T_Sub;
                end if;
            
            when ':' =>
                Look_Ahead(F, c2, eol);
                if c2 = '=' then
                    Get_Immediate(F, c);
                    t.token_type := T_Assign;
                else
                    t.token_type := T_Colon;
                end if;
                
            when '!' =>
                Look_Ahead(F, c2, eol);
                if c2 = '=' then
                    Get_Immediate(F, c);
                    t.token_type := T_Ne;
                else
                    t.token_type := T_None;
                end if;
                
            when '>' =>
                Look_Ahead(F, c2, eol);
                if c2 = '=' then
                    Get_Immediate(F, c);
                    t.token_type := T_Ge;
                else
                    t.token_type := T_Gt;
                end if;
            
            when '<' =>
                Look_Ahead(F, c2, eol);
                if c2 = '=' then
                    Get_Immediate(F, c);
                    t.token_type := T_Le;
                else
                    t.token_type := T_Lt;
                end if;
                
            when '&' =>
                Look_Ahead(F, c2, eol);
                if c2 = '&' then
                    Get_Immediate(F, c);
                    t.token_type := T_Lg_And;
                else
                    t.token_type := T_And;
                end if;
            
            when '|' =>
                Look_Ahead(F, c2, eol);
                if c2 = '|' then
                    Get_Immediate(F, c);
                    t.token_type := T_Lg_Or;
                else
                    t.token_type := T_Or;
                end if;
            
            when others => null;
        end case;
    end Get_Symbol;
    
    -- A helper function to get the keyword based on the buffer
    procedure Get_Keyword is
    begin
        if buffer = "import" then t.token_type := T_Import;
        elsif buffer = "func" then t.token_type := T_Func;
        elsif buffer = "is" then t.token_type := T_Is;
        elsif buffer = "end" then t.token_type := T_End;
        elsif buffer = "return" then t.token_type := T_Return;
        elsif buffer = "var" then t.token_type := T_Var;
        elsif buffer = "const" then t.token_type := T_Const;
        elsif buffer = "while" then t.token_type := T_While;
        elsif buffer = "do" then t.token_type := T_Do;
        elsif buffer = "then" then t.token_type := T_Then;
        elsif buffer = "if" then t.token_type := T_If;
        elsif buffer = "elif" then t.token_type := T_Elif;
        elsif buffer = "else" then t.token_type := T_Else;
        elsif buffer = "break" then t.token_type := T_Break;
        elsif buffer = "continue" then t.token_type := T_Continue;
        elsif buffer = "struct" then t.token_type := T_Struct;
        elsif buffer = "i8" then t.token_type := T_I8;
        elsif buffer = "u8" then t.token_type := T_U8;
        elsif buffer = "i16" then t.token_type := T_I16;
        elsif buffer = "u16" then t.token_type := T_U16;
        elsif buffer = "i32" then t.token_type := T_I32;
        elsif buffer = "u32" then t.token_type := T_U32;
        elsif buffer = "i64" then t.token_type := T_I64;
        elsif buffer = "u64" then t.token_type := T_U64;
        elsif buffer = "char" then t.token_type := T_Char;
        elsif buffer = "string" then t.token_type := T_String;
        elsif buffer = "bool" then t.token_type := T_Bool;
        elsif buffer = "true" then t.token_type := T_True;
        elsif buffer = "false" then t.token_type := T_False;
        else t.token_type := T_None;
        end if;
    end Get_Keyword;
    
    -- A helper function to proccess the buffer
    procedure Process_Buffer is
    begin
        Get_Keyword;
        
        if t.token_type = T_None then
            if Is_Int then
                t.token_type := T_Int;
                t.int_value := Integer'Value(To_String(buffer));
            else
                t.token_type := T_Id;
                t.string_value := buffer;
            end if;
        end if;
        
        buffer := To_Unbounded_String("");
    end Process_Buffer;
begin
    if token_stack.Length > 0 then
        t := token_stack.Last_Element;
        token_stack.Delete_Last;
        return t;
    end if;

    -- TODO: See if that "Proccess_Buffer" can be removed
    if End_Of_File(F) then
        if Length(buffer) > 0 then
            Process_Buffer;
        else
            t.token_type := T_Eof;
        end if;
        
        return t;
    end if;
    
    while not End_Of_File(F) loop
        Get_Immediate(F, c);
        
        -- If we have a string, parse it
        if c = '"' then
            buffer := To_Unbounded_String("");
            Get_Immediate(F, c);
            while c /= '"' loop
                Append(buffer, c);
                Get_Immediate(F, c);
            end loop;
            
            t.token_type := T_StringL;
            t.string_value := buffer;
            buffer := To_Unbounded_String("");
            return t;
        end if;
        
        -- Check for characters
        if c = ''' then
            Get_Immediate(F, c);
            t.token_type := T_CharL;
            t.char_value := c;
            Get_Immediate(F, c);
            return t;
        end if;
        
        if c = ' ' or c = LF or Is_Symbol then
            if Is_Symbol then
                Get_Symbol;
                if Length(buffer) = 0 then
                    return t;
                else
                    token_stack.Append(t);
                end if;
            end if;
            
            if Length(buffer) > 0 then
                Process_Buffer;
                return t;
            end if;
        else
            Append(buffer, c);
        end if;
    end loop;
    
    Process_Buffer;
    return t;
end Lex_Get_Next;

end Lex; -- End Lex package body

