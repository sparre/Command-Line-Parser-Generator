with Ada.Command_Line,
     Ada.Text_IO;

with An_Application.Command_Line_Parser.Argument,
     An_Application.Command_Line_Parser.Argument_List,
     An_Application.Command_Line_Parser.Profiles;

package body An_Application.Command_Line_Parser is
   Is_Initialised : Boolean := False;
   Arguments      : Argument_List.Instance;

   Profile_Found : Boolean := False;
   Profile_Index : Profiles.Index;

   procedure Call_Matching_Profile is
   begin
      if Profile_Found then
         Profiles.Call (Profile   => Profile_Index,
                        Arguments => Arguments);
      else
         raise Program_Error
           with "Call_Matching_Profile called without a valid profile.";
      end if;
   end Call_Matching_Profile;

   function Count_Matching_Call_Profiles return Natural is
      Counter : Natural := 0;
   begin
      for Index in Profiles.Index loop
         if Profiles.Match (Index, Arguments) then
            Counter := Counter + 1;

            Profile_Index := Index;
            Profile_Found := True;
         end if;
      end loop;

      return Counter;
   end Count_Matching_Call_Profiles;

   procedure Initialise is
      use Ada;
   begin
      for Index in 1 .. Command_Line.Argument_Count loop
         Arguments.Insert (Command_Line_Parser.Argument.Value
                             (Command_Line.Argument (Index)));
      end loop;

      Is_Initialised := True;
   end Initialise;

   function Initialised return Boolean is
   begin
      return Is_Initialised;
   end Initialised;

   package body Errors is
      procedure More_Than_One_Matching_Call_Profile is
         use Ada.Command_Line, Ada.Text_IO;
      begin
         Put_Line (File => Standard_Error,
                   Item => "Ambigous command line arguments.");
         Set_Exit_Status (Failure);
      end More_Than_One_Matching_Call_Profile;

      procedure No_Matching_Call_Profile is
         use Ada.Command_Line, Ada.Text_IO;
      begin
         Put_Line (File => Standard_Error,
                   Item => "Missing or too many command line arguments.");
         Set_Exit_Status (Failure);
      end No_Matching_Call_Profile;
   end Errors;
end An_Application.Command_Line_Parser;
