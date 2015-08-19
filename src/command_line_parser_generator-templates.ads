with Command_Line_Parser_Generator.Procedure_Declaration_List;

private
with Ada.Wide_Text_IO;

private
with Command_Line_Parser_Generator.Procedure_Declaration;

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

   subtype Simple_Procedure is Procedure_Declaration.Instance
     with Dynamic_Predicate
            => Simple_Procedure.Number_Of_Optional_Parameters = 0;

   procedure Call (Target      : in     Ada.Wide_Text_IO.File_Type;
                   Profile     : in     Simple_Procedure;
                   Indentation : in     Natural);
end Command_Line_Parser_Generator.Templates;
