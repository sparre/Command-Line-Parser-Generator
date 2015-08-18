with Ada.Calendar;

with Bad_Declares_Function;

package Good_Multiple_Optional_Parameters is
   procedure Run (Help : in     Boolean := False;
                  Name : in     String := "Jacob";
                  Year : in     Ada.Calendar.Year_Number := 2015);

   procedure Batch_Mode (Help : in     Boolean := False;
                         A, B : in     Natural)
     renames Bad_Declares_Function.Batch_Mode;
end Good_Multiple_Optional_Parameters;
