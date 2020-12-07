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

with Ada.Streams;
use Ada.Streams;

with Ada.Text_IO.Text_Streams;
use Ada.Text_IO.Text_Streams;

with Ada.Strings.Bounded;

with Ada.Text_IO;
use Ada.Text_IO;

package Passport is

    -- -------------------------------------------------
    package Info is new Ada.Strings.Bounded.Generic_Bounded_Length (25);
    use Info;

    -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
    -- iyr (Issue Year) - four digits; at least 2010 and at most 2020. eyr
    -- (Expiration Year) - four digits; at least 2020 and at most 2030. hgt
    -- (Height) - a number followed by either cm or in: If cm, the number must
    -- be at least 150 and at most 193. If in, the number must be at least
    -- 59 and at most 76. hcl (Hair Color) - a # followed by exactly six
    -- characters 0-9 or a-f. ecl (Eye Color) - exactly one of: amb blu brn gry
    -- grn hzl oth. pid (Passport ID) - a nine-digit number, including leading
    -- zeroes. cid (Country ID) - ignored, missing or not.

    -- -------------------------------------------------
    type Year_String is new String (1 .. 4);
    subtype Year is Positive;

    -- -------------------------------------------------
    Tag_length : constant Positive := 4;

-- -------------------------------------------------
    Tag_NULL : constant String (1 .. Tag_length) := "    ";

    type Passport_Element is
        tagged null record;
    for Passport_Element'External_Tag use Tag_NULL;

    function Input
       (Stream : access Root_Stream_Type'Class) return Passport_Element'Class;
    for Passport_Element'Class'Input use Input;

    procedure Output
       (Stream :    access Root_Stream_Type'Class;
        Item   : in Passport_Element'Class);
    for Passport_Element'Class'Output use Output;

    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Passport_Element);
    for Passport_Element'Read use Read;

    procedure Write
       (Stream : access Root_Stream_Type'Class; Item : in Passport_Element);
    for Passport_Element'Write use Write;

    procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Passport_Element);

    -- -------------------------------------------------
    Tag_BYR : constant String (1 .. Tag_length) := "byr:";
    subtype Birth_Year_Value is Positive range 1_920 .. 2_002;
    function is_Valid_Birth_Year_Value
       (Birth_Year : Year_String) return Boolean;
    type Birth_Year is
        new Passport_Element with record
            BYR : Year_String;
        end record;
    for Birth_Year'External_Tag use Tag_BYR;

    -- -------------------------------------------------
    Tag_IYR : constant String (1 .. Tag_length) := "iyr:";
    subtype Issue_Year_Value is Positive range 2_010 .. 2_020;
    function is_Valid_Issue_Year_Value
       (Issue_Year : Year_String) return Boolean;

    type Issue_Year is
        new Passport_Element with record
            IYR : Year;
        end record;
    for Issue_Year'External_Tag use Tag_IYR;

    -- -------------------------------------------------
    Tag_EYR : constant String (1 .. Tag_length) := "eyr:";
    subtype Expiration_Year_Value is Positive range 2_020 .. 2_030;
    function is_Valid_Expiration_Year_Value
       (Expiration_Year : Year_String) return Boolean;

    type Expiration_Year is
        new Passport_Element with record
            EYR : Year;
        end record;
    for Expiration_Year'External_Tag use Tag_EYR;

    -- -------------------------------------------------
    Tag_HGT : constant String (1 .. Tag_length) := "hgt:";
    type Height_String is new String (1 .. 5);
    type Height_Unit is
       (CM,
        INCHES);  -- sorry, Ada does not accept keyword 'IN' as list component identifier :-(
    subtype Height_CM_Value is Positive range 150 .. 193;
    subtype Height_Inches_Value is Positive range 59 .. 76;
    type Height_universal is
        record
            unit  : Height_Unit;
            value : Positive;
        end record;
    function is_Valid_Height_Value
       (Some_Height : Height_String) return Boolean;

    type Height is
        new Passport_Element with record
            HGT : Height_universal;
        end record;
    for Height'External_Tag use Tag_HGT;

    -- -------------------------------------------------
    Tag_HCL : constant String (1 .. Tag_length) := "hcl:";
    type Color_RGB is new String (1 .. 6);
    function is_Valid_Color_RGB_Value (Some_Color : Color_RGB) return Boolean;

    type Hair_Color is
        new Passport_Element with record
            HCL : Color_RGB;
        end record;
    for Hair_Color'External_Tag use Tag_HCL;

    -- -------------------------------------------------
    Tag_ECL : constant String (1 .. Tag_length) := "ecl:";
    type Color_of_eye is new String (1 .. 3);
    type Color_of_eye_list is (AMB, BLU, BRN, GRY, GRN, HZL, OTH);
    function is_Valid_Eye_Color_Value
       (Some_Color : Color_of_eye) return Boolean;

    type Eye_Color is
        new Passport_Element with record
            ECL : Color_of_eye;
        end record;
    for Eye_Color'External_Tag use Tag_ECL;

    -- -------------------------------------------------
    Tag_PID : constant String (1 .. Tag_length) := "pid:";
    type Passport_ID_String is new String (1 .. 9);
    function is_Valid_Passport_ID_Value
       (Some_ID : Passport_ID_String) return Boolean;

    type Passport_ID is
        new Passport_Element with record
            PID : Passport_ID_String;
        end record;
    for Passport_ID'External_Tag use Tag_PID;

    -- -------------------------------------------------
    Tag_CID : constant String (1 .. Tag_length) := "cid:";
    type Country_Code is new String (1 .. 3);
    subtype Country_Number is Positive;

    type Country_ID is -- optional in password record
        new Passport_Element with record
            CID : Country_Code;
        end record;
    for Country_ID'External_Tag use Tag_CID;

    -- -------------------------------------------------
    type Password_Info_list is (BYR, IYR, EYR, HGT, HCL, ECL, PID, CID) with
        Size => Tag_length * 8;

    Max_Record_Length : constant Natural :=
       (Password_Info_list'Pos (Password_Info_list'Last) + 1) *
       (Tag_length + 1 + 1) +
       (3 * Year_String'Length +
        Height_String'Length +
        Color_RGB'Length +
        Color_of_eye'Length +
        Passport_ID_String'Length +
        Country_Code'Length) -
       1;

    subtype Password_Info_required is
       Password_Info_list range BYR .. PID;  -- CID est optional
    type Info_Status is array (Password_Info_required) of Boolean;

    type Passport_page (With_CID : Boolean := True) is
        record
            is_Present : Info_Status;
            is_Valid   : Info_Status;
            BYR        : Birth_Year_Value;
            IYR        : Issue_Year_Value;
            EYR        : Expiration_Year_Value;
            HGT        : Height_universal;
            HCL        : Color_RGB;
            ECL        : Color_of_eye_list;
            PID        : Passport_ID_String;
            case With_CID is
                when True =>
                    cid : Country_Number;
                when False =>
                    null;
            end case;
        end record;

    function Required_Info_are_present (This : Passport_page) return Boolean;
    function is_Valid (This : Passport_page) return Boolean;

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Birth_Year);
    for Birth_Year'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Birth_Year);
    for Birth_Year'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Birth_Year);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Issue_Year);
    for Issue_Year'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Issue_Year);
    for Issue_Year'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Issue_Year);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Expiration_Year);
    for Expiration_Year'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Expiration_Year);
    for Expiration_Year'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Expiration_Year);

    -- -------------------------------------------------
    procedure Read (Stream : access Root_Stream_Type'Class; Item : out Height);
    for Height'Read use Read;

    procedure write (Stream : access Root_Stream_Type'Class; Item : in Height);
    for Height'Write use write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Height);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Passport_ID);
    for Passport_ID'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Passport_ID);
    for Passport_ID'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Passport_ID);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Hair_Color);
    for Hair_Color'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Hair_Color);
    for Hair_Color'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Hair_Color);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Eye_Color);
    for Eye_Color'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Eye_Color);
    for Eye_Color'Write use write;

    overriding procedure Display
       (Console : Ada.Text_IO.Text_Streams.Stream_Access; Item : in Eye_Color);

    -- -------------------------------------------------
    procedure Read
       (Stream : access Root_Stream_Type'Class; Item : out Country_ID);
    for Country_ID'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Country_ID);
    for Country_ID'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Country_ID);

    -- -------------------------------------------------
    function get_Passport (File_Name : String) return Passport_page;

end Passport;
