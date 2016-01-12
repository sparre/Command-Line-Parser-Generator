with Ada.Command_Line;

package body Web_Site_Generator is
   procedure Fail (Fail : in     Flag) is
      pragma Unreferenced (Fail);
   begin
      raise Constraint_Error with "Processing failed (on request).";
   end Fail;

   procedure Put_Help (File : in     Ada.Text_IO.File_Type) is
      use Ada.Command_Line, Ada.Text_IO;
   begin
      Put_Line (File, "Usage:");
      New_Line (File);
      Put_Line (File, "Run:");
      Put_Line (File, "   " & Command_Name);
      New_Line (File);
      Put_Line (File, "Show_Help:");
      Put_Line (File, "   " & Command_Name);
      Put_Line (File, "     --help");
      New_Line (File);
   end Put_Help;

   procedure Run is
      use Ada.Text_IO;
   begin
      Put_Line ("Run;");
   end Run;
end Web_Site_Generator;
