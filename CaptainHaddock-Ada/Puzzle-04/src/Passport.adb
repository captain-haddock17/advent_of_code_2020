-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------

with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Assertions;
use Ada.Assertions;

package body Passport is

      -- -------------------------------------------------
      -- Passport_Element
      -- -------------------------------------------------
      function Input
            (Stream : access Root_Stream_Type'Class) 
            return Passport_Element'Class is
	begin
		null;

and Input;

      procedure Output
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read; Item : in  Passport_Element'Class);

      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read; Item : out  Passport_Element);
      

      procedure Write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write; Item : in  Passport_Element);
      

      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access;
            Item : in  Passport_Element);

      -- -------------------------------------------------
      -- Birth_Year
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Birth_Year);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Birth_Year);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Birth_Year);

      -- -------------------------------------------------
      -- Issue_Year
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Issue_Year);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Issue_Year);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Issue_Year);

      -- -------------------------------------------------
      -- Expiration_Year
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Expiration_Year);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Expiration_Year);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Expiration_Year);

      -- -------------------------------------------------
      -- Height
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Height);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Height);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Height);

      -- -------------------------------------------------
      -- Passport_ID
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Passport_ID);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Passport_ID);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Passport_ID);

      -- -------------------------------------------------
      -- Hair_Color
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Hair_Color);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Hair_Color);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Hair_Color);

      -- -------------------------------------------------
      -- Eye_Color
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Eye_Color);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Eye_Color);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Eye_Color);

      -- -------------------------------------------------
      -- Country_ID
      -- -------------------------------------------------
      procedure Read
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Read;
            Item : out Country_ID);
      

      procedure write
            (Stream : access Root_Stream_Type'Class is
	begin
		null;

and Write;
            Item : in Country_ID);
      

      overriding 
      procedure Display
            (Console : Ada.Text_IO.Text_Streams.Stream_Access; 
            Item    : in Country_ID);

      -- -------------------------------------------------


   -- -------------------------------------------------
   function get_Passport(File_Name : String) return Passport_page;


end Passport;
