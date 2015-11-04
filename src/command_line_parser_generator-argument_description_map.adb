package body Command_Line_Parser_Generator.Argument_Description_Map is
   function Image (Container : in     Instance;
                   Key       : in     Source_Text) return Wide_String is
      use type Source_Text;
      Result : Source_Text;
   begin
      for Description of Container.Constant_Reference (Key) loop
         if Description = "" then
            null;
         else
            if Result = "" then
               Result := Description;
            else
               Result := Result & "/" & Description;
            end if;
         end if;
      end loop;

      if Result = "" then
         return "<none>";
      else
         return +Result;
      end if;
   end Image;
end Command_Line_Parser_Generator.Argument_Description_Map;
