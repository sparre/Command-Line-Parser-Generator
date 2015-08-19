with Ada.Text_IO;

package body Good_Single_Character is
   procedure Show (Char : in     Character) is
   begin
      Ada.Text_IO.Put_Line ("   Show (Char => '" & Char & "');");
   end Show;
end Good_Single_Character;
