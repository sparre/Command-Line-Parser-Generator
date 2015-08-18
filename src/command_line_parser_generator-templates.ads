with Command_Line_Parser_Generator.Procedure_Declaration_List;

private
with Ada.Wide_Text_IO;

package Command_Line_Parser_Generator.Templates is
   procedure Set    (Target_Directory : in String);
   procedure Create (Target_Directory : in String);

   procedure Runner        (Package_Name : in     Wide_String);
   procedure Parser        (Package_Name : in     Wide_String);
   procedure Argument_Type (Package_Name : in     Wide_String);
   procedure Argument_List (Package_Name : in     Wide_String);
   procedure Profiles
     (Package_Name : in     Wide_String;
      Procedures   : in     Procedure_Declaration_List.Instance)
     with Pre => not Procedures.Is_Empty;
private
   procedure Create_Specification (Name : in     Wide_String;
                                   File : in out Ada.Wide_Text_IO.File_Type);
   procedure Create_Body (Name : in     Wide_String;
                          File : in out Ada.Wide_Text_IO.File_Type);
end Command_Line_Parser_Generator.Templates;
