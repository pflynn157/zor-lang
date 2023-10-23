--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ast; use ast;
with parser; use parser;
with lex; use lex;
with unwriter;
with cpp_unwriter;

with pass1;

procedure Main is

    -- Command line control variables
    output_lex, output_ast, testing : boolean := false;
    input_file : Unbounded_String;
    
    -- Pass controls
    run_pass1 : boolean := true;
    
    procedure Lex_Test is
        t : Token;
    begin
        Lex_Init(To_String(input_file));
        t := Lex_Get_Next;
        while t.token_type /= T_Eof loop
            Put_Line(TokenType'Image(t.token_type));
            t := Lex_Get_Next;
        end loop;
        Lex_Close;
    end Lex_Test;
    
    -- For the unwriter
    type Unwrite_Lang is (
        UL_TinyLang,
        UL_Cpp
    );
    lang : Unwrite_Lang := UL_TinyLang;
    
    -- The ast file
    ast_file : AstFile;
begin
    if Argument_Count >= 1 then
        for i in 1 .. Argument_Count loop
            if Argument(i) = "--lex" then
                output_lex := true;
            elsif Argument(i) = "--ast" then
                output_ast := true;
            elsif Argument(i) = "--output:cpp" then
                lang := UL_Cpp;
            elsif argument(i) = "--testing" then
                testing := true;
            elsif argument(i) = "--skip-pass1" then
                run_pass1 := false;
            else
                input_file := To_Unbounded_String(Argument(i));
            end if;
        end loop;
    else
        put_line("Error: No input file!");
        -- TODO: Exit
    end if;
    
    if not output_lex then
        ast_file := Parse(To_String(input_file));
        
        if run_pass1 then
            pass1.run_pass1(ast_file);
            if not pass1.evaluate_error1(testing) then
                return;
            end if;
        end if;
    end if;

    -- Print as dictated
    if output_lex then
        Put_Line("========");
        Lex_Test;
        Put_Line("========");
    elsif output_ast then
        Print_Ast(ast_file);
    else
        case lang is
            when UL_TinyLang => unwriter.unwrite(ast_file);
            when UL_Cpp => cpp_unwriter.unwrite(ast_file, false);
        end case;
    end if;
end Main;

