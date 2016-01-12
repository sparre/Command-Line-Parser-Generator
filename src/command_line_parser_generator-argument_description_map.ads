--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Command_Line_Parser_Generator.Identifier_Matrix;

package Command_Line_Parser_Generator.Argument_Description_Map is
   type Instance is new Identifier_Matrix.Instance with null record;

   function Image (Container : in     Instance;
                   Key       : in     Source_Text) return Wide_String;
end Command_Line_Parser_Generator.Argument_Description_Map;
