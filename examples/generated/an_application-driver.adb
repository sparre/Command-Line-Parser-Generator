with An_Application.Command_Line_Parser;

procedure An_Application.Driver is
   use Command_Line_Parser;
begin
   Initialise;

   case Count_Matching_Call_Profiles is
      when 0 =>
         Errors.No_Matching_Call_Profile;
      when 1 =>
         Call_Matching_Profile;
      when others =>
         Errors.More_Than_One_Matching_Call_Profile;
   end case;
end An_Application.Driver;
