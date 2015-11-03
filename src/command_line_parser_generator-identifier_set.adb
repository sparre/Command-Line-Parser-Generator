package body Command_Line_Parser_Generator.Identifier_Set is
   procedure Append (Container : in out Instance;
                     Value     : in     Source_Text) is
   begin
      if not Container.Contains (Value) then
         Container.Insert (Value);
      end if;
   end Append;
end Command_Line_Parser_Generator.Identifier_Set;
