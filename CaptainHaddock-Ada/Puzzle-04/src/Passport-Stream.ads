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

-- with Passport_Tags;
-- use Passport_Tags;

with Ada.Streams;
use Ada.Streams;

-- with Ada.Text_IO.Text_Streams;
-- use Ada.Text_IO.Text_Streams;

-- with Ada.Strings.Bounded;

-- with Ada.Text_IO;
-- use Ada.Text_IO;

package Passport.Stream is


    -- -------------------------------------------------
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
       (Stream : access Root_Stream_Type'Class; Item : out Country_ID);
    for Country_ID'Read use Read;

    procedure write
       (Stream : access Root_Stream_Type'Class; Item : in Country_ID);
    for Country_ID'Write use write;

    overriding procedure Display
       (Console :    Ada.Text_IO.Text_Streams.Stream_Access;
        Item    : in Country_ID);

    -- -------------------------------------------------
        
end Passport.Stream;
