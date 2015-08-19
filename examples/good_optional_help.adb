with Ada.Text_IO;

package body Good_Optional_Help is
   procedure Run (Help : in Boolean := False) is
   begin
      if Help then
         Ada.Text_IO.Put_Line ("<help>");
      else
         Ada.Text_IO.Put_Line ("<run>");
      end if;
   end Run;
end Good_Optional_Help;
