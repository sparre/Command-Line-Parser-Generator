private
with Ada.Wide_Text_IO;

package Command_Line_Parser_Generator.Templates is
   procedure Set    (Target_Directory : in String);
   procedure Create (Target_Directory : in String);

   procedure Runner (Package_Name : in     Wide_String);
   procedure Parser (Package_Name : in     Wide_String);
private
   procedure Create_Specification (Name : in     Wide_String;
                                   File : in out Ada.Wide_Text_IO.File_Type);
   procedure Create_Body (Name : in     Wide_String;
                          File : in out Ada.Wide_Text_IO.File_Type);
end Command_Line_Parser_Generator.Templates;
