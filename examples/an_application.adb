with Ada.Text_IO;

package body An_Application is
   use Ada.Text_IO;

   procedure Show_Help (Help : Boolean) is
   begin
      if Help then
         Put_Line ("Usage:");
         Put_Line ("   an_application-run [ --help ]");
      end if;
   end Show_Help;

   procedure Run_Interactive is
   begin
      Put_Line ("Please press <enter>.");
      Skip_Line;
      Put_Line ("Done.");
   end Run_Interactive;
end An_Application;
