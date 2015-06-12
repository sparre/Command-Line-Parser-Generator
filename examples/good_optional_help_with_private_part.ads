with Ada.Text_IO;

package Good_Optional_Help_With_Private_Part is
   procedure Run (Help : in Boolean := False);
private
   procedure Debug (To : Ada.Text_IO.File_Type);
end Good_Optional_Help_With_Private_Part;
