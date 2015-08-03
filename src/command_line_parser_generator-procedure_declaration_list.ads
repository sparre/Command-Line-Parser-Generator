with Ada.Containers.Vectors;

with Command_Line_Parser_Generator.Procedure_Declaration;

package Command_Line_Parser_Generator.Procedure_Declaration_List is
   package Vectors is new Ada.Containers.Vectors
                            (Element_Type => Procedure_Declaration.Instance,
                             Index_Type   => Positive,
                             "="          => Procedure_Declaration."=");

   type Instance is new Vectors.Vector with null record;

   function Image (Item : in Instance) return Wide_String;
end Command_Line_Parser_Generator.Procedure_Declaration_List;
