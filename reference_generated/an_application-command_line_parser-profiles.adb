with An_Application.Command_Line_Parser.Key_List;

package body An_Application.Command_Line_Parser.Profiles is
   procedure Call (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance) is
      use all type Key_List.Instance;
   begin
      pragma Style_Checks ("-M0");

      case Profile is
         when 1 =>
            if Arguments = (+"Help") then
               An_Application.Show_Help
                 (Help => Standard.Boolean'Value (Arguments.Element ("Help")));
            else
               raise Program_Error
                 with "Profiles.Call called with invalid arguments.";
            end if;
         when 2 =>
            if Arguments.Is_Empty then
               An_Application.Run_Interactive;
            else
               raise Program_Error
                 with "Profiles.Call called with invalid arguments.";
            end if;
      end case;

      pragma Style_Checks ("-M79");
   end Call;

   function Match (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance) return Boolean is
      Buffer : Argument_List.Instance := Arguments;
   begin
      case Profile is
         when 1 =>
            if Buffer.Contains (Key => "Help") then
               declare
                  Dummy : Standard.Boolean;
               begin
                  Dummy := Standard.Boolean'Value (Buffer.Element (Key => "Help"));
               exception
                  when Constraint_Error =>
                     return False;
               end;

               Buffer.Delete (Key => "Help");
            else
               return False;
            end if;
         when 2 =>
            null;
      end case;

      return Buffer.Is_Empty;
   end Match;
end An_Application.Command_Line_Parser.Profiles;
