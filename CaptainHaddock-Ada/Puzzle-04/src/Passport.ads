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

with Passport_Tags;
use Passport_Tags;

with Ada.Strings.Bounded;

-- with Ada.Text_IO;
-- use Ada.Text_IO;

package Passport is

    -- -------------------------------------------------
    -- Passport informations are stored sucessively : 
    -- * 'Passport_String' : identical to the file's content
    -- * 'Passport_String' : structured record as per String
    -- * 'Passport_Record' : structured record as per Value
    -- -------------------------------------------------
    -- * byr (Birth Year) - four digits; at least 1920 and at most 2002.
    -- * iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    -- * eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    -- * hgt (Height) - a number followed by either cm or in: 
    -- ** If cm, the number must be at least 150 and at most 193. 
    -- ** If in, the number must be at least 59 and at most 76. 
    -- * hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    -- * ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth. 
    -- * pid (Passport ID) - a nine-digit number, including leading zeroes.
    -- * cid (Country ID) - ignored, missing or not.
    -- -------------------------------------------------

    subtype Year_String is String (1 .. 4);
    subtype Year is Positive;

    -- -------------------------------------------------
    

    type Passport_Element is
        tagged null record;
    for Passport_Element'External_Tag use Tag_NULL;



    -- -------------------------------------------------
    subtype Birth_Year_Value is Positive range 1_920 .. 2_002;
    function is_Valid_Birth_Year_Value
       (Year_Str : String) return Boolean;
    type Birth_Year is
        new Passport_Element with record
            BYR : Year_String;
        end record;
    for Birth_Year'External_Tag use Tag_BYR;

    -- -------------------------------------------------
    subtype Issue_Year_Value is Positive range 2_010 .. 2_020;
    function is_Valid_Issue_Year_Value
       (Year_Str : String) return Boolean;

    type Issue_Year is
        new Passport_Element with record
            IYR : Year_String;
        end record;
    for Issue_Year'External_Tag use Tag_IYR;

    -- -------------------------------------------------
    subtype Expiration_Year_Value is Positive range 2_020 .. 2_030;
    function is_Valid_Expiration_Year_Value
       (Year_Str : String) return Boolean;

    type Expiration_Year is
        new Passport_Element with record
            EYR : Year_String;
        end record;
    for Expiration_Year'External_Tag use Tag_EYR;

    -- -------------------------------------------------
    subtype Height_String is String (1 .. 5);
    type Height_Unit is
       (CM,
        INCHES);  -- sorry, Ada does not accept keyword 'IN' as list component identifier :-(
    subtype Height_CM_Value is Positive range 150 .. 193;
    subtype Height_Inches_Value is Positive range 59 .. 76;
    type Height_universal is
        record
            value : Positive;
            unit  : Height_Unit;
        end record;
    function is_Valid_Height_Value
       (Some_Height : Height_String) return Boolean;

    type Height is
        new Passport_Element with record
            HGT : Height_String;
        end record;
    for Height'External_Tag use Tag_HGT;

    -- -------------------------------------------------
    subtype Color_RGB is String (1 .. 6);
    function is_Valid_Color_RGB_Value (Color : Color_RGB) return Boolean;

    type Hair_Color is
        new Passport_Element with record
            HCL : Color_RGB;
        end record;
    for Hair_Color'External_Tag use Tag_HCL;

    -- -------------------------------------------------
    subtype Color_of_eye is String (1 .. 3);
    type Color_of_eye_list is (AMB, BLU, BRN, GRY, GRN, HZL, OTH);
    function is_Valid_Eye_Color_Value
       (Some_Color : Color_of_eye) return Boolean;

    type Eye_Color is
        new Passport_Element with record
            ECL : Color_of_eye;
        end record;
    for Eye_Color'External_Tag use Tag_ECL;

    -- -------------------------------------------------
    subtype Passport_ID_String is String (1 .. 9);
    function is_Valid_Passport_ID_Value
       (Some_ID : Passport_ID_String) return Boolean;

    type Passport_ID is
        new Passport_Element with record
            PID : Passport_ID_String;
        end record;
    for Passport_ID'External_Tag use Tag_PID;

    -- -------------------------------------------------
    subtype Country_Code is String (1 .. 3);
    subtype Country_Number is Positive;

    type Country_ID is -- optional in Passport record
        new Passport_Element with record
            CID : Country_Code;
        end record;
    for Country_ID'External_Tag use Tag_CID;

    -- -------------------------------------------------

    subtype Passport_Info_required is
       Passport_Info_list range BYR .. PID;  -- CID est optional

    type Info_Status is array (Passport_Info_list) of Boolean;

    Max_Record_Length : constant Natural :=
       (Passport_Info_list'Pos (Passport_Info_list'Last) + 1) *
       (Tag_length + 1 + 1) -- Tag_length + ' ' or possibly a '\r\n' for each info
       + (3 * Year_String'Length
       + Height_String'Length
       + Color_RGB'Length
       + Color_of_eye'Length
       + Passport_ID_String'Length
       + Country_Code'Length)
       -1;

    package Bounded_Record_String is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Record_Length);
    subtype Passport_String is Bounded_Record_String.Bounded_String; -- renames

    type Passport_Record (With_CID : Boolean := True) is
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
    type Passport_Ref is access Passport_Record;

    function Required_Infos_are_present (This : Passport_Record) return Boolean;
    function is_Valid (This : Passport_Record) return Boolean;

    procedure Input(This : Passport_Element'Class; From : Passport_String; At_Tag_Index : Natural; Add_To : in out Passport_Record);

        
end Passport;
