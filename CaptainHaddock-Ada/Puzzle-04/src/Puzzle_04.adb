-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------

With Passport;
Use Passport;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;


procedure Puzzle_04 is

    myPassport : Passport_page;

--  DAT_File_Name : String(1..2**15); -- 32_768
    Database : File_Type;
    Missing_FileName : exception;


    
    -- ==============================================================

begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;


    New_Line;

    put_line(" =" & Integer'Image(111));

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        put_line("usage: "& Command_Name & " Tree-Map_file_name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        put_line(Standard_Error,"file '"&Argument(1)&"' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"Error when Reading the file !");
        set_Exit_Status(Failure);
        raise;
end Puzzle_04;
