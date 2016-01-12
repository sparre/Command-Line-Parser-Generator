--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

package body Command_Line_Parser_Generator.Argument_Zsh_Pattern_Map is
   procedure Append (Container : in out Instance;
                     Key       : in     Source_Text;
                     Pattern   : in     Zsh_Argument_Pattern.Instance) is
   begin
      if Container.Contains (Key) then
         Container.Replace (Key      => Key,
                            New_Item => Pattern or Container.Element (Key));
      else
         Container.Insert (Key      => Key,
                           New_Item => Pattern);
      end if;
   end Append;
end Command_Line_Parser_Generator.Argument_Zsh_Pattern_Map;
