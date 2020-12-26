
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with Object;

procedure Main is

   use Ada;
   use Ada.Text_IO;
   use Object;

   File : Text_IO.File_Type;

begin
   --  Write some objects

   declare
      P  : Point := (6.8, 0.1);
      CP : Pixel := (1.2, 8.4, Red);
   begin
      Create (File, Out_File, "data");
      Point'Class'Output (Text_Streams.Stream (File), CP);
      Point'Class'Output (Text_Streams.Stream (File), P);
      Close (File);
   end;

   --  Read them back

   Open (File, In_File, "data");

   declare
      P1 : constant Point'Class :=
             Point'Class'Input (Text_Streams.Stream (File));
      P2 : constant Point'Class :=
             Point'Class'Input (Text_Streams.Stream (File));
   begin
      P1.Display; New_Line;
      P2.Display; New_Line;
   end;

   Close (File);
end Main;
