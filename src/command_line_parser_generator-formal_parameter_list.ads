with Ada.Containers.Vectors;

with Command_Line_Parser_Generator.Formal_Parameter;

package Command_Line_Parser_Generator.Formal_Parameter_List is
   package Vectors is
      new Ada.Containers.Vectors (Element_Type => Formal_Parameter.Instance,
                                  Index_Type   => Positive,
                                  "="          => Formal_Parameter."=");

   type Instance is new Vectors.Vector with null record;

   function Image (Item : in Instance) return Wide_String;
end Command_Line_Parser_Generator.Formal_Parameter_List;
