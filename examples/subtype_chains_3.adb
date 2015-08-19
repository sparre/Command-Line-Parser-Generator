with Ada.Text_IO;

package body Subtype_Chains_3 is
   procedure Run (Even : in Two) is
   begin
      Ada.Text_IO.Put_Line (Two'Image (Even));
   end Run;
end Subtype_Chains_3;
