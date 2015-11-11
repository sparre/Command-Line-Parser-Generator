private
with Ada.Text_IO;

package Web_Site_Generator is
   type Flag is (Present);

   procedure Run (Run : in     Flag);
   procedure Fail (Fail : in     Flag);
private
   procedure Put_Help (File : in     Ada.Text_IO.File_Type);
end Web_Site_Generator;
