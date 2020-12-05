-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-04
-- ------------------------------------------------
-- License : CC-BY-SA 
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
type passowrd_Info_list is (
      byr,
      iyr,
      eyr,
      hgt,
      hcl,
      ecl,
      pid,
      cid);

-- -------------------------------------------------
      Tag_NULL : constant String := "    ";

      Tag_byr : constant String := "byr:";
      Tag_iyr : constant String := "iyr:";
      Tag_eyr : constant String := "eyr:";
      Tag_hgt : constant String := "hgt:";
      Tag_hcl : constant String := "hcl:";
      Tag_ecl : constant String := "ecl:";
      Tag_pid : constant String := "pid:";
      Tag_cid : constant String := "cid:";

-- -------------------------------------------------
      package Info is new Ada.Strings.Bounded.Generic_Bounded_Length(25);
      use Info;


      -- -------------------------------------------------
      type Passport_Element is tagged null record;
      for Passport_Element'External_Tag use Tag_NULL;

      function Input
            (Stream : access Root_Stream_Type'Class) 
            return Passport_Element'Class;
      for Passport_Element'Class'Input use Input;

      procedure Output
            (Stream : access Root_Stream_Type'Class; Item : in  Passport_Element'Class);
      for Passport_Element'Class'Output use Output;

      procedure Read
            (Stream : access Root_Stream_Type'Class; Item : out  Passport_Element);
      for Passport_Element'Read use Read;

      procedure Write
            (Stream : access Root_Stream_Type'Class; Item : in  Passport_Element);
      for  Passport_Element'Write use Write;

      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access;
            Item : in  Passport_Element);

      -- -------------------------------------------------
      type Birth_Year is new Passport_Element with record
            byr : Bounded_String;
      end record;
      for Birth_Year'External_Tag use Tag_byr;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Birth_Year);
      for Birth_Year'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Birth_Year);
      for Birth_Year'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Birth_Year);

      -- -------------------------------------------------
      type Issue_Year is new Passport_Element with record
            iyr : Bounded_String;
      end record;
      for Issue_Year'External_Tag use Tag_iyr;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Issue_Year);
      for Issue_Year'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Issue_Year);
      for Issue_Year'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Issue_Year);

      -- -------------------------------------------------
      type Expiration_Year is new Passport_Element with record
            eyr : Bounded_String;
      end record;
      for Expiration_Year'External_Tag use Tag_eyr;
      
      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Expiration_Year);
      for Expiration_Year'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Expiration_Year);
      for Expiration_Year'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Expiration_Year);

      -- -------------------------------------------------
      type Height is new Passport_Element with record
            hgt : Bounded_String;
      end record;
      for Height'External_Tag use Tag_hgt;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Height);
      for Height'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Height);
      for Height'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Height);

      -- -------------------------------------------------
      type Passport_ID is new Passport_Element with record
            pid : Bounded_String;
      end record;
      for Passport_ID'External_Tag use Tag_pid;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Passport_ID);
      for Passport_ID'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Passport_ID);
      for Passport_ID'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Passport_ID);

      -- -------------------------------------------------
      type Hair_Color is new Passport_Element with record
            hcl : Bounded_String;
      end record;
      for Hair_Color'External_Tag use Tag_hcl;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Hair_Color);
      for Hair_Color'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Hair_Color);
      for Hair_Color'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Hair_Color);

      -- -------------------------------------------------
      type Eye_Color is new Passport_Element with record
            ecl : Bounded_String;
      end record;
      for Eye_Color'External_Tag use Tag_ecl;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Eye_Color);
      for Eye_Color'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Eye_Color);
      for Eye_Color'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Eye_Color);

      -- -------------------------------------------------
      type Country_ID is new Passport_Element with record
            cid : Bounded_String;
      end record;
      for Country_ID'External_Tag use Tag_cid;

      procedure Read
            (Stream : access Root_Stream_Type'Class;
            Item : out Country_ID);
      for Country_ID'Read use Read;

      procedure write
            (Stream : access Root_Stream_Type'Class;
            Item : in Country_ID);
      for Country_ID'Write use write;

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Country_ID);

      -- -------------------------------------------------

-- -------------------------------------------------
type Passport_page is record
      byr : Birth_Year;
      iyr : Issue_Year;
      eyr : Expiration_Year ;
      hgt : Height;
      hcl : Hair_Color;
      ecl : Eye_Color;
      pid : Passport_ID;
      cid : Country_ID;
end record;

-- -------------------------------------------------
   function get_Passport(File_Name : String) return Passport_page;

end Passport;
