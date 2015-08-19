with Ada.Text_IO;

package body Using_An_Enumeration is
   use Ada.Text_IO;

   procedure Run (Mode : in Choices := Interactive) is
   begin
      Put_Line ("Mode = " & Choices'Image (Mode));
   end Run;

   procedure Run (Configuration_File : in     String;
                  Mode               : in     Choices) is
   begin
      Put_Line ("Configuration_File = """ & Configuration_File & """");
      Put_Line ("Mode               = " & Choices'Image (Mode));
   end Run;
end Using_An_Enumeration;
