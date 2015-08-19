with Ada.Text_IO;

package body Good_Multiple_Optional_Parameters is
   procedure Run (Help : in     Boolean := False;
                  Name : in     String := "Jacob";
                  Year : in     Ada.Calendar.Year_Number := 2015) is
      use Ada.Calendar, Ada.Text_IO;
   begin
      Put_Line ("   Run (Help => " & Boolean'Image (Help) & ",");
      Put_Line ("        Name => """ & Name & """,");
      Put_Line ("        Year =>" & Year_Number'Image (Year) & ");");
   end Run;
end Good_Multiple_Optional_Parameters;
