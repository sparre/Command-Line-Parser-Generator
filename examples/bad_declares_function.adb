with Ada.Text_IO;

package body Bad_Declares_Function is
   procedure Batch_Mode (Help : in     Boolean := False;
                         A, B : in     Natural) is
      pragma Unreferenced (A, B);
   begin
      if Help then
         Ada.Text_IO.Put_Line ("<help>");
      else
         Ada.Text_IO.Put_Line ("<batch mode>");
      end if;
   end Batch_Mode;

   function Help_Text return String is
   begin
      return "<help text>";
   end Help_Text;

   procedure Run (Help : in Boolean := False) is
   begin
      if Help then
         Ada.Text_IO.Put_Line ("<help>");
      else
         Ada.Text_IO.Put_Line ("<run>");
      end if;
   end Run;
end Bad_Declares_Function;
