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

with Ada.Tags;
use Ada.Tags;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Assertions;
use Ada.Assertions;

package body Passport is

    -- -------------------------------------------------
    -- Record Structure
    -- -------------------------------------------------
    Record_End : constant String := "\n\n";

    -- -------------------------------------------------
    -- Passport_Element
    -- -------------------------------------------------
    function Input
       (Stream : access Root_Stream_Type'Class) return Passport_Element'Class
    is
        Element : Passport_Element;

        TAG : String (1 .. Tag_length);


    begin
        --  On the stream we have <tag_name>, we want to get "tag_name" Read
        --  tag

        for I in TAG'range loop
            Character'Read (Stream, TAG (I));
        end loop;

        Ada.Text_IO.Put_Line ("TAG=" & TAG);

        declare
            External_Tag : constant String := TAG;
            Passport_Element : constant Passport_Element'Class := Dispatching_Input
                  (Ada.Tags.Internal_Tag (External_Tag), Stream);
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
       (Birth_Year : Year_String) return Boolean
    is
    begin
        return False;
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
       (Issue_Year : Year_String) return Boolean
    is
    begin
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
       (Expiration_Year : Year_String) return Boolean
    is
    begin
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
    begin
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

    function is_Valid_Color_RGB_Value (Some_Color : Color_RGB) return Boolean
    is
    begin
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
        Country_Code'Read(Stream, Item.CID);
    end write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Country_ID)
    is
    begin
        null;
    end Display;

    -- -------------------------------------------------
    function get_Passport (File_Name : String) return Passport_page is
        Some_Passport : Passport_page;
    begin
        return Some_Passport;
    end get_Passport;

    -- -------------------------------------------------
    -- Password
    -- -------------------------------------------------

    function Required_Info_are_present (This : Passport_page) return Boolean is
        Status_OK : Info_Status := (others => True);
    begin
        return This.is_Present = Status_OK;
    end Required_Info_are_present;

    function is_Valid (This : Passport_page) return Boolean is
        Status_OK : Info_Status := (others => True);
    begin
        return This.is_Valid = Status_OK;
    end is_Valid;

end Passport;
