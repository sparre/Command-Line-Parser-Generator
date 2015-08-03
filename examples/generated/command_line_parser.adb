with Ada.Command_Line;

with Command_Line_Parser.Argument,
     Command_Line_Parser.Argument_List;

package body Command_Line_Parser is
   Is_Initialised : Boolean := False;
   Arguments      : Argument_List.Instance;

   function Initialised return Boolean is
   begin
      return Is_Initialised;
   end Initialised;

   procedure Initialise is
      use Ada;
   begin
      for Index in 1 .. Command_Line.Argument_Count loop
         Arguments.Insert
           (Command_Line_Parser.Argument.Value (Command_Line.Argument (1)));
      end loop;

      Is_Initialised := True;
   end Initialise;

   function Count_Matching_Call_Profiles return Natural is
   begin
      return raise Program_Error
        with "Count_Matching_Call_Profiles not implemented yet";
   end Count_Matching_Call_Profiles;

   procedure Call_Matching_Profile is
   begin
      raise Program_Error
        with "Call_Matching_Profile not implemented yet";
   end Call_Matching_Profile;

   package body Errors is
      procedure No_Matching_Call_Profile is
      begin
         raise Program_Error
           with "Errors.No_Matching_Call_Profile not implemented yet";
      end No_Matching_Call_Profile;

      procedure More_Than_One_Matching_Call_Profile is
      begin
         raise Program_Error
           with "Errors.More_Than_One_Matching_Call_Profile " &
                "not implemented yet";
      end More_Than_One_Matching_Call_Profile;
   end Errors;
end Command_Line_Parser;
