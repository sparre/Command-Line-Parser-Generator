with Ada.Wide_Text_IO;

with Asis.Declarations,
     Asis.Elements,
     Asis.Expressions;

with Thick_Queries;

package body Command_Line_Parser_Generator.Utilities is
   function Defining_Name (Item : Asis.Declaration) return Wide_String is
      Name_List : constant Asis.Defining_Name_List :=
                    Asis.Declarations.Names (Item);
   begin
      return
        Asis.Declarations.Defining_Name_Image (Name_List (Name_List'First));
   end Defining_Name;

   function Full_Defining_Name (Item : Asis.Declaration) return Wide_String is
      Full_Type : Asis.Element;

      use Asis.Expressions;
   begin
      Full_Type := Corresponding_Name_Declaration (Thick_Queries.Simple_Name
                                                     (Item));

      return Source_Name (Item) & "." & Defining_Name (Full_Type);
   end Full_Defining_Name;

   procedure Generate_Reader (For_Type : in     Asis.Declaration) is
      Package_Name : constant Wide_String := Source_Name (For_Type);
      Type_Name    : constant Wide_String := Full_Defining_Name (For_Type);

      use Ada.Wide_Text_IO;
   begin
      New_Line;
      Put_Line ("----------------------");
      Put_Line ("with Ada.Command_Line,");
      Put_Line ("     Ada.Text_IO;");
      Put_Line ("with " & Package_Name & ";");
      Put_Line ("procedure Reader is");
      Put_Line ("   use Ada.Command_Line, Ada.Text_IO;");
      Put_Line ("   O : constant " & Type_Name & " := " & Type_Name &
                  "'Value (Argument (1));");
      Put_Line ("begin");
      Put_Line ("   Put_Line (""" & Type_Name & ": "" & " & Type_Name &
                  "'Image (O));");
      Put_Line ("end Reader;");
      Put_Line ("----------------------");
   end Generate_Reader;

   function Source_Name (Item : Asis.Declaration) return Wide_String is
      Full_Type      : Asis.Element;
      CU_Of_Type     : Asis.Compilation_Unit;
      CU_Declaration : Asis.Declaration;

      use Asis.Elements, Asis.Expressions;
   begin
      Full_Type      := Corresponding_Name_Declaration
                          (Thick_Queries.Simple_Name (Item));
      CU_Of_Type     := Enclosing_Compilation_Unit (Full_Type);
      CU_Declaration := Unit_Declaration (CU_Of_Type);

      return Defining_Name (CU_Declaration);
   end Source_Name;
end Command_Line_Parser_Generator.Utilities;
