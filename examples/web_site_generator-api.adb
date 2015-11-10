with Ada.Text_IO;

package body Web_Site_Generator.API is
   procedure Generate_All_Pages
     (Sources   : in     Directory_Name := ".";
      Target    : in     Directory_Name := "/var/www";
      Documents : in     String         := "documents";
      Debug     : in     Boolean        := False) is
      use Ada.Text_IO;
   begin
      Put_Line ("Generate_All_Pages");
      Put_Line ("  (Sources   => """ & String (Sources) & """,");
      Put_Line ("   Target    => """ & String (Target) & """,");
      Put_Line ("   Documents => """ & Documents & """,");
      Put_Line ("   Debug     => " & Boolean'Image (Debug) & ");");
   end Generate_All_Pages;
end Web_Site_Generator.API;
