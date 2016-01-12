--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

package body Command_Line_Parser_Generator.Identifier_Matrix is
   procedure Append (Container : in out Instance;
                     Key       : in     Source_Text;
                     Value     : in     Source_Text) is
   begin
      if Container.Contains (Key) then
         Container.Reference (Key).Append (Value);
      else
         Container.Insert (Key      => Key,
                           New_Item => Identifier_Set.To_Set (Value));
      end if;
   end Append;
end Command_Line_Parser_Generator.Identifier_Matrix;
