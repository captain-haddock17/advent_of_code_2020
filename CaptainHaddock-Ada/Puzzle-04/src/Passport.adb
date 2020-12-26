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



-- with Ada.Tags;
-- use Ada.Tags;

with Ada.Text_IO;
use Ada.Text_IO;

-- with Ada.Assertions;
-- use Ada.Assertions;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

package body Passport is

    -- -------------------------------------------------
    -- Passport global info
    -- -------------------------------------------------
    function Required_Infos_are_present (This : Passport_Record) return Boolean is
        Status_OK : Info_Status := (others => True);
    begin
        return This.is_Present = Status_OK;
    end Required_Infos_are_present;


    function is_Valid (This : Passport_Record) return Boolean is
        Status_OK : Info_Status := (others => True);
    begin
        return This.is_Valid = Status_OK;
    end is_Valid;


    -- -------------------------------------------------
    -- Passport_Element
    -- -------------------------------------------------
    procedure Input(This : Passport_Element'Class; From : Passport_String; At_Tag_Index : Natural; Add_To : in out Passport_Record) is
    begin
--FIXME        Read(Passport_String, This );
    null;
    end;


    -- -------------------------------------------------
    -- Birth_Year
    -- -------------------------------------------------
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
    -- no need to test, as it's an optionnal field


end Passport;
