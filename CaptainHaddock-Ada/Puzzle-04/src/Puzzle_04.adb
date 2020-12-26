-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA
-- ------------------------------------------------

with Passport; use Passport;

with Passport_Tags; use Passport_Tags;

with Ada.Command_Line; use Ada.Command_Line;

with Ada.Text_IO;
-- use Ada.Text_IO;

with Ada.Strings;
with Ada.Strings.Bounded; use Ada.Strings;

procedure Puzzle_04 is
-- pragma Restrictions (No_Obsolescent_Features);

    package TXT renames Ada.Text_IO;

    
    Passport_Stream : TXT.File_Type;
    Missing_FileName : exception;

    -- ==============================================================
    --   InStreamAccess : IOs.Stream_Access;
--    Console        : TXTs.Stream_Access;


    -- Counting for reports
    Record_Count, Passport_Count, Valid_Passport_Count : Natural := 0;

    Buffer            : String (1 .. Max_Record_Length);
    Buffer_Last_index : Natural := 0;

    TAG_Separator : Character := ' ';
    Tag_Index     : Natural;

    use Bounded_Record_String;
    Some_Passport_Str : Passport_String;

    Element : access Passport_Element'Class;

    Some_Passport : Passport_Ref;


begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    TXT.Open
       (File => Passport_Stream, Name => Argument (1), Mode => TXT.In_File);

    -- InStreamAccess := IOs.Stream (File => Passport_Stream);

    GO_TROUGH_THE_FILE :
    while not TXT.End_Of_File (Passport_Stream) loop

        Record_Count    := Record_Count + 1;
        Some_Passport_Str := Bounded_Record_String.Null_Bounded_String;

        Buffer_Last_index := 0;
        GET_A_LOGICAL_RECORD :
        while not TXT.End_Of_File (Passport_Stream) loop
            -- get all record segments in one logical record

            TXT.Get_Line
               (File => Passport_Stream,
                Item => Buffer,
                Last => Buffer_Last_index);
            exit when Buffer_Last_index = 0; -- end of logical record

            -- append passport info gathered into the logical record
            Some_Passport_Str :=
               Some_Passport_Str & (Buffer (1 .. Buffer_Last_index) & " ");

        end loop GET_A_LOGICAL_RECORD;

        Some_Passport := new Passport_Record;

        FOR_EACH_TAG_OF_THE_RECORD :
        for Tag in Passport_Info_list'Range loop
            declare

            begin
                Tag_Index :=
                   Index
                      (Source  => Some_Passport_Str,
                       Pattern =>
                          Tag_Value (Tag), -- Passport_Info_list'Image(Tag)
                       From => 1);

                -- found one !
                Some_Passport.Is_Present (Tag) := True;

                -- select Object Class
                case Tag is
                    when BYR =>
                        Element := new Birth_Year;
                    when IYR =>
                        Element := new Issue_Year;
                    when EYR =>
                        Element := new Expiration_Year;
                    when HGT =>
                        Element := new Height;
                    when HCL =>
                        Element := new Hair_Color;
                    when ECL =>
                        Element := new Eye_Color;
                    when PID =>
                        Element := new Passport_ID;
                    when CID =>
                        Element := new Country_ID;
                end case;

                -- Dispatch reading and validating
                Input
                   (This         => Element.All,
                    From         => Some_Passport_Str,
                    At_Tag_Index => Tag_Index,
                    Add_To       => Some_Passport.All);

                -- procedure dispatch(This : Passport_Element'Class; From :
                -- Passport_String; At_Tag_Index : Natural; Add_To : in out
                -- Passport_Record);

                -- Passport_Element'Class'Input(This_Passport, Passport_Record,
                -- Tag_Index);

                -- Some_Passport_Info : Passport_Element'Class :=
                -- Passport_Element'Class'Input (S); Character'Read
                -- (InStreamAccess, TAG_Separator);

            exception
                when Index_Error =>
                    Some_Passport.is_Present (Tag) := False;

            end;

        end loop FOR_EACH_TAG_OF_THE_RECORD;

        TXT.Put
           (TXT.Standard_Error,
            "Read record n°" & Natural'image (Record_Count) & "  ");
        TXT.Put_Line (TXT.Standard_Error, To_String (Some_Passport_Str));
--            Display (Console,myRecord);

        -- Counting for reporting
        if Required_Infos_are_present (Some_Passport.All) then
            Passport_Count := Valid_Passport_Count + 1;
        end if;

        if Is_Valid (Some_Passport.All) then
            Valid_Passport_Count := Valid_Passport_Count + 1;
        end if;

    end loop GO_TROUGH_THE_FILE;

    TXT.Close (Passport_Stream);

    -- Reporting results
    TXT.Put_Line ("Nb of records         =" & Integer'Image (Record_Count));
    TXT.Put_Line ("Nb of passports       =" & Integer'Image (Passport_Count));
    TXT.Put_Line
       ("Nb of valid passports =" & Integer'Image (Valid_Passport_Count));

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
        TXT.Put_Line (TXT.Standard_Error, "Error when reading the file !");
        Set_Exit_Status (Failure);
        raise;
end Puzzle_04;
