-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA
-- ------------------------------------------------

with Passport;
use Passport;

with Ada.Command_Line;
use Ada.Command_Line;

with Ada.Storage_IO;
use Ada.Storage_IO;

with Ada.Text_IO;
-- use Ada.Text_IO;

-- with Ada.Text_IO.Text_Streams;

with Ada.Strings.Bounded;
use Ada.Strings.Bounded;

procedure Puzzle_04 is
-- pragma Restrictions (No_Obsolescent_Features);

    package IO renames Ada.Text_IO;
    package IOs renames Ada.Text_IO.Text_Streams;
    package TXT renames Ada.Text_IO;
    package TXTs renames Ada.Text_IO.Text_Streams;

    myPassport : Passport_page;

    Password_Database : TXT.File_Type;
    Missing_FileName : exception;

    -- ==============================================================
 --   InStreamAccess : IOs.Stream_Access;
--    Console        : TXTs.Stream_Access;

    package Record_String is new Generic_Bounded_Length (Max_Record_Length);
    Password_Record : Record_String.Bounded_String;
    use Record_String;

    package Record_String_IO is new Ada.Storage_IO(String);
    
    Record_Count : Natural := 0;

    Buffer            : String (1 .. Max_Record_Length);
    Buffer_Last_index : Natural := 0;

    TAG_Separator : character := ' ';
   

begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    TXT.Open
       (File => Password_Database, Name => Argument (1), Mode => TXT.In_File);

    -- InStreamAccess := IOs.Stream (File => Password_Database);

    GO_TROUGH_THE_FILE :
    while not IO.End_Of_File (Password_Database) loop

        Record_Count    := Record_Count + 1;
        Password_Record := Null_Bounded_String;

        Buffer_Last_index := 0;

        GET_A_LOGICAL_RECORD :
        while not IO.End_Of_File (Password_Database) loop
            -- get all record segments in one logical record

            TXT.Get_Line
               (File => Password_Database,
                Item => Buffer,
                Last => Buffer_Last_index);
            exit when Buffer_Last_index = 0; -- end of logical record

            -- append passport info gathered into the logical record
            Password_Record :=
               Password_Record & (Buffer (1 .. Buffer_Last_index) & " ");

        end loop GET_A_LOGICAL_RECORD;


        DISPATCH_FOR_EACH_TAG_OF_THE_RECORD :
        while not False loop --FIXME
            GET_A_TAG:
            declare
                  Some_Password_Info :  Passport_Element'Class :=
                    Passport_Element'Class'Input (S);
            begin
                case Some_Password_Info'Tag is
                    when 
                Character'Read (InStreamAccess, TAG_Separator);
            end GET_A_TAG;
        end loop DISPATCH_FOR_EACH_TAG_OF_THE_RECORD;


        TXT.Put
           (TXT.Standard_Error,
            "Read record n°" & Natural'image (Record_Count) & "  ");
        TXT.Put_Line (TXT.Standard_Error, To_String (Password_Record));
--            Display (Console,myRecord);

    end loop GO_TROUGH_THE_FILE;

    TXT.Put_Line ("Nb of records =" & Integer'Image (Record_Count));
    TXT.Close (Password_Database);

    Set_Exit_Status (Success);

exception
    when Missing_FileName =>
        TXT.Put_Line ("usage: " & Command_Name & " Tree-Map_file_name");
        Set_Exit_Status (Failure);

    when TXT.Status_Error =>
        TXT.Put_Line
           (TXT.Standard_Error, "file '" & Argument (1) & "' not found!");
        Set_Exit_Status (Failure);
        raise;

    when others =>
        TXT.Put_Line (TXT.Standard_Error, "Error when Reading the file !");
        Set_Exit_Status (Failure);
        raise;
end Puzzle_04;
