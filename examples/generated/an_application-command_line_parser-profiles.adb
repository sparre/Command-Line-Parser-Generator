with Ada.Containers;

package body An_Application.Command_Line_Parser.Profiles is
   procedure Call (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance) is
   begin
      pragma Style_Checks ("-M0");

      case Profile is
         when 1 =>
            An_Application.Show_Help (Help => Standard.Boolean'Value (Arguments.Element ("Help")));
         when 2 =>
            An_Application.Run_Interactive;
      end case;

      pragma Style_Checks ("-M79");
   end Call;

   function Match (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance) return Boolean is
      use type Ada.Containers.Count_Type;

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

      return Buffer.Length = 0;
   end Match;
end An_Application.Command_Line_Parser.Profiles;
