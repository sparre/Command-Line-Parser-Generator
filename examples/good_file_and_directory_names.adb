with Ada.Text_IO;

package body Good_File_And_Directory_Names is
   procedure List (File      : in     File_Name) is
      use Ada.Text_IO;
   begin
      Put_Line ("List (File => """ & String (File) & """);");
   end List;

   procedure List (Directory : in     Directory_Name) is
      use Ada.Text_IO;
   begin
      Put_Line ("List (Directory => """ & String (Directory) & """);");
   end List;
end Good_File_And_Directory_Names;
