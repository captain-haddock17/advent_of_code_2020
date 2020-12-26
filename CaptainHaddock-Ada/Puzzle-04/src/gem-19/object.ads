
with Ada.Streams;

package Object is

   use Ada.Streams;

   --  Point  --

   type Point is tagged record
      X, Y : Float;
   end record;

   procedure Class_Output
     (S : access Ada.Streams.Root_Stream_Type'Class; O : in Point'Class);
   for Point'Class'Output use Class_Output;

   function Class_Input
     (S : access Ada.Streams.Root_Stream_Type'Class) return Point'Class;
   for Point'Class'Input use Class_Input;

   for Point'External_Tag use "point";

   procedure Write
     (S : access Root_Stream_Type'Class; O : in Point);
   for Point'Write use Write;

   procedure Read (S : access Root_Stream_Type'Class; O : out Point);
   for Point'Read use Read;

   procedure Display (O : in Point);

   --  Pixel  --

   type Color_Name is (Red, Green, Blue);

   type Pixel is new Point with record
      Color : Color_Name;
   end record;

   for Pixel'External_Tag use "pixel";

   procedure Write
     (S : access Root_Stream_Type'Class; O : in Pixel);
   for Pixel'Write use Write;

   procedure Read (S : access Root_Stream_Type'Class; O : out Pixel);
   for Pixel'Read use Read;

   overriding procedure Display (O : in Pixel);

end Object;
