-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Creation date : 2020-12-04
-- ------------------------------------------------
-- Copyright: to the Author
-- ------------------------------------------------
-- License  : CC-BY-SA
-- ------------------------------------------------


-- with Ada.Streams.Stream_IO;
-- use Ada.Streams.Stream_IO;

-- with Ada.Tags;
-- use Ada.Tags;

-- with Ada.Text_IO;
-- use Ada.Text_IO;

-- with Ada.Assertions;
-- use Ada.Assertions;

-- with Ada.Characters.Handling;
-- use Ada.Characters.Handling;

package body Passport is


    -- -------------------------------------------------
    -- Passport_Element
    -- -------------------------------------------------

    function Input
       (Stream : access Root_Stream_Type'Class) return Passport_Element'Class
    is
        -- Element : Passport_Element;

        TAG : String (1 .. Tag_Length);


    begin
        --  On the stream we have <tag_name>, we want to get "tag_name" Read
        --  tag

        for I in TAG'range loop
            Character'Read (Stream, TAG (I));
        end loop;

        Ada.Text_IO.Put_Line ("TAG=" & TAG);

        declare
            External_Tag : constant String := TAG;
            Element : Passport_Element;
            -- Element : Passport_Element'Class := Dispatching
            --        (Ada.Tags.Internal_Tag (External_Tag), Stream);
        begin
            return Element;
        end;
    end Input;

    procedure Output
       (Stream :    access Root_Stream_Type'Class;
        Item   : in Passport_Element'Class)
    is
    begin
        null;
    end Output;

    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Passport_Element)
    is
    begin
        null;
    end Read;

    procedure Write
       (Stream : access Root_Stream_Type'Class; Item : in Passport_Element)
    is
    begin
        null;
    end Write;

    procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Passport_Element)
    is
    begin
        null;
    end Display;

    -- -------------------------------------------------
    -- Birth_Year
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Birth_Year)
    is

    begin
        Year_String'Read(Stream, Item.BYR);

    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Birth_Year)
    is
    begin
        Year_String'Write(Stream, Item.BYR);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Birth_Year)
    is
    begin
        null;
    end Display;

    function is_Valid_Birth_Year_Value
       (Year_Str : String) return Boolean
    is
      Year_Num : Integer;  
    begin
        Year_Num := Integer'Value(Year_Str);
        return (Year_Num in Birth_Year_Value'range); -- test membership
    end is_Valid_Birth_Year_Value;

    -- -------------------------------------------------
    -- Issue_Year
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Issue_Year)
    is
    begin
        Year_String'Read(Stream, Item.IYR);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Issue_Year)
    is
    begin
        Year_String'Write(Stream, Item.IYR);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Issue_Year)
    is
    begin
        null;
    end Display;

    function is_Valid_Issue_Year_Value
       (Year_Str : String) return Boolean
    is
      Year_Num : Integer;  
    begin
        Year_Num := Integer'Value(Year_Str);
        return (Year_Num in Issue_Year_Value'range); -- test membership
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Year_Str & "''");
            return False;
    end is_Valid_Issue_Year_Value;

    -- -------------------------------------------------
    -- Expiration_Year
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Expiration_Year)
    is
    begin
        Year_String'Read(Stream, Item.EYR);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Expiration_Year)
    is
    begin
        Year_String'Write(Stream, Item.EYR);
    end write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Expiration_Year)
    is
    begin
        null;
    end Display;

    function is_Valid_Expiration_Year_Value
       (Year_Str : String) return Boolean
    is
      Year_Num : Integer;  
    begin
        Year_Num := Integer'Value(Year_Str);
        return (Year_Num in Expiration_Year_Value'range); -- test membership
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Year_Str & "''");
            return False;
    end is_Valid_Expiration_Year_Value;

    -- -------------------------------------------------
    -- Height
    -- -------------------------------------------------
    procedure Read (Stream : access Root_Stream_Type'Class; Item : out Height)
    is
    begin
        Height_String'Read(Stream, Item.HGT);
    end Read;

    procedure write (Stream : access Root_Stream_Type'Class; Item : in Height)
    is
    begin
        Height_String'Write(Stream, Item.HGT);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Height)
    is
    begin
        null;
    end Display;

    function is_Valid_Height_Value (Some_Height : Height_String) return Boolean
    is
      Num : Integer;  
      Unit : String(1..2);

    begin
        Num := Integer'Value(Some_Height);
        Unit := Some_Height(Some_Height'length-1 .. Some_Height'length);

        if Unit = "CM" then
            return (Num in Height_CM_Value'range); -- test membership
        end if;
        if Unit = "IN" then
            return (Num in Height_Inches_Value'range); -- test membership
        end if;
        return False;
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Some_Height & "''");
            return False;
    end is_Valid_Height_Value;

    -- -------------------------------------------------
    -- Hair_Color
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Hair_Color)
    is
    begin
        Color_RGB'Read(Stream, Item.HCL);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Hair_Color)
    is
    begin
        Color_RGB'Write(Stream, Item.HCL);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Hair_Color)
    is
    begin
        null;
    end Display;

    function is_Valid_Color_RGB_Value (Color : Color_RGB) return Boolean
    is
    begin
        if Color'Length /= 6 then 
            return false; 
        end if;

        for x in Color'Range loop 
            if not Is_Hexadecimal_Digit(Color(x)) then
                return False;
            end if;
        end loop;
        return True;
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Color & "''");
            return False;
    end is_Valid_Color_RGB_Value;
    -- -------------------------------------------------
    -- Eye_Color
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Eye_Color)
    is
    begin
        Color_of_eye'Read(Stream, Item.ECL);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Eye_Color)
    is
    begin
        Color_of_eye'Write(Stream, Item.ECL);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Eye_Color)
    is
    begin
        null;
    end Display;

    function is_Valid_Eye_Color_Value
       (Some_Color : Color_of_eye) return Boolean
    is
    begin
        for c in Color_of_eye_list'Range loop
            if To_Upper(Some_Color) = Color_of_eye_list'Image(c) then
                return True;
            end if;
        end loop;
        return False;
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Some_Color & "''");
            return False;
    end is_Valid_Eye_Color_Value;

    -- -------------------------------------------------
    -- Passport_ID
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Passport_ID)
    is
    begin
        Passport_ID_String'Read(Stream, Item.PID);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Passport_ID)
    is
    begin
        Passport_ID_String'Write(Stream, Item.PID);
    end write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Passport_ID)
    is
    begin
        null;
    end Display;

    function is_Valid_Passport_ID_Value
       (Some_ID : Passport_ID_String) return Boolean
    is
    begin
        if Some_ID'Length /= 9 then 
            return false; 
        end if;

        for i in Some_ID'Range loop
            if not Is_Digit(Some_ID(i)) then
                return False;
            end if;
        end loop;
        return True;
    exception
        when others =>
            Put_line ("Exception occured on convertion of ''" & Some_ID & "''");
            return False;
    end is_Valid_Passport_ID_Value;

    -- -------------------------------------------------
    -- Country_ID
    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Country_ID)
    is
    begin
        Country_Code'Read(Stream, Item.CID);
    end Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Country_ID)
    is
    begin
        Country_Code'Write(Stream, Item.CID);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Country_ID)
    is
    begin
        null;
    end Display;


end Passport.Stream;
