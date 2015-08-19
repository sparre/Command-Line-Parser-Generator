package body Good_Optional_Help_With_Private_Part is
   procedure Debug (To : Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line (File => To,
                            Item => "<debug>");
   end Debug;

   procedure Reset (File : in out Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Reset (File => File);
   end Reset;

   procedure Run (Help : in Boolean := False) is
   begin
      if Help then
         Ada.Text_IO.Put_Line ("<help>");
      else
         Ada.Text_IO.Put_Line ("<run>");
      end if;
   end Run;
end Good_Optional_Help_With_Private_Part;
