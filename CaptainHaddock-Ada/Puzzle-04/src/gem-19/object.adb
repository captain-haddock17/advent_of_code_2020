
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

package body Object is

   use Ada.Text_IO;

   ------------------------------------------- XML parsing helper functions

   procedure Skip_Tag
     (S      : access Ada.Streams.Root_Stream_Type'Class;
      Ending : in     Character := '>');
   --  Skip the next tag on stream S, returns when Ending is found

   function Get_Value
     (S : access Ada.Streams.Root_Stream_Type'Class) return String;
   --  Returns the current value read on stream S

   --------------
   -- Skip_Tag --
   --------------

   procedure Skip_Tag
     (S      : access Ada.Streams.Root_Stream_Type'Class;
      Ending : in     Character := '>')
   is
      C : Character;
   begin
      loop
         Character'Read (S, C);
         exit when C = Ending;
      end loop;
   end Skip_Tag;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (S : access Ada.Streams.Root_Stream_Type'Class) return String
   is
      Buffer : String (1 .. 100);
      K      : Positive := Buffer'First;
   begin
      loop
         Character'Read (S, Buffer (K));
         exit when Buffer (K) = '<';
         K := K + 1;
      end loop;
      return Buffer (1 .. K - 1);
   end Get_Value;

   ------------------------------------------- Point'Class

   ------------------
   -- Class_Output --
   ------------------

   procedure Class_Output
     (S : access Ada.Streams.Root_Stream_Type'Class; O : in Point'Class) is
   begin
      --  Write the opening tag <tag_name>
      Character'Write (S, '<');
      String'Write (S, Ada.Tags.External_Tag (O'Tag));
      String'Write (S, '>' & ASCII.LF);

      --  Write the object, dispatching call to Point/Pixel'Write
      Point'Output (S, O);

      --  Write the closing tag </tag_name>
      String'Write (S, "</");
      String'Write (S, Ada.Tags.External_Tag (O'Tag));
      String'Write (S, '>' & ASCII.LF);
   end Class_Output;

   -----------------
   -- Class_Input --
   -----------------

   function Class_Input
     (S : access Ada.Streams.Root_Stream_Type'Class) return Point'Class
   is
      function Dispatching_Input is
         new Ada.Tags.Generic_Dispatching_Constructor
           (T           => Point,
            Parameters  => Ada.Streams.Root_Stream_Type'Class,
            Constructor => Point'Input);
      Input     : String (1 .. 20);
      Input_Len : Natural := 0;
   begin
      --  On the stream we have <tag_name>, we want to get "tag_name"
      --  Read first character, must be '<'
      Character'Read (S, Input (1));
      if Input (1) /= '<' then
         raise Ada.Tags.Tag_Error with "Starting with " & Input (1);
      end if;

      --  Read tag
      Input_Len := 0;
      for I in Input'range loop
         Character'Read (S, Input (I));
         Input_Len := I;
         exit when Input (I) = '>';
      end loop;

      --  Check ending tag
      if Input (Input_Len) /= '>'
        or else Input_Len <= 1
      then -- Empty tag
         raise Ada.Tags.Tag_Error with "empty tag";
      else
         Input_Len := Input_Len - 1;
      end if;

      declare
         External_Tag : constant String := Input (1 .. Input_Len);
         O            : constant Point'Class := Dispatching_Input
                          (Ada.Tags.Internal_Tag (External_Tag), S);
         --  Dispatches to appropriate Point/Pixel'Input depending on
         --  the tag name.
      begin
         --  Skip closing object tag
         Skip_Tag (S, ASCII.LF);
         return O;
      end;
   end Class_Input;

   ------------------------------------------- Point

   -------------
   -- Display --
   -------------

   procedure Display (O : in Point) is
   begin
      Put_Line ("*** A point");
      Point'Output (Text_Streams.Stream (Current_Output), O);
   end Display;

   ----------
   -- Read --
   ----------

   procedure Read (S : access Root_Stream_Type'Class; O : out Point) is
   begin
      Skip_Tag (S); O.X := Float'Value (Get_Value (S)); Skip_Tag (S, ASCII.LF);
      Skip_Tag (S); O.Y := Float'Value (Get_Value (S)); Skip_Tag (S, ASCII.LF);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class; O : in Point) is
   begin
      String'Write (S, "   <x>"  & Float'Image (O.X) & "</x>" & ASCII.LF);
      String'Write (S, "   <y>"  & Float'Image (O.Y) & "</y>" & ASCII.LF);
   end Write;

   ------------------------------------------- Pixel

   -------------
   -- Display --
   -------------

   overriding procedure Display (O : in Pixel) is
   begin
      Put_Line ("*** A pixel");
      Pixel'Output (Text_Streams.Stream (Current_Output), O);
   end Display;

   ----------
   -- Read --
   ----------

   procedure Read (S : access Root_Stream_Type'Class; O : out Pixel) is
   begin
      Read (S, Point (O));
      Skip_Tag (S);
      O.Color := Color_Name'Value (Get_Value (S));
      Skip_Tag (S, ASCII.LF);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (S : access Root_Stream_Type'Class; O : in Pixel) is
   begin
      Write (S, Point (O));
      String'write
        (S, "   <color>"
           & Color_Name'Image (O.Color) & "</color>" & ASCII.LF);
   end Write;

end Object;
