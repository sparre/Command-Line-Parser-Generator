with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Wide_Text_IO;

with Asis,
     Asis.Ada_Environments,
     Asis.Compilation_Units,
     Asis.Declarations,
     Asis.Elements,
     Asis.Implementation,
     Asis.Text;

with Setup;

procedure ASIS_Experiment is
   Context             : Asis.Context;
   Compilation_Unit    : Asis.Compilation_Unit;
   Package_Declaration : Asis.Declaration;
begin
   Setup (Context => Context);

   Check_For_Package :
   declare
      use Ada.Characters.Handling, Ada.Command_Line, Ada.Wide_Text_IO;

      use Asis.Compilation_Units;
      use all type Asis.Unit_Kinds;
   begin
      Compilation_Unit := Library_Unit_Declaration
                            (Name        => To_Wide_String (Argument (1)),
                             The_Context => Context);

      case Unit_Kind (Compilation_Unit) is
         when A_Package =>
            Put_Line (Item => """" & To_Wide_String (Argument (1)) &
                        """ is a package. :-)");
         when An_Unknown_Unit =>
            Put_Line (File => Standard_Error,
                      Item => "No semantic information available for """ &
                              To_Wide_String (Argument (1)) & """.");
            Set_Exit_Status (Failure);
            return;
         when others =>
            Put_Line (File => Standard_Error,
                      Item => """" & To_Wide_String (Argument (1)) &
                              """ is not a package specification.");
            Set_Exit_Status (Failure);
            return;
      end case;

      Put_Line (Item => Debug_Image (Compilation_Unit));
   end Check_For_Package;

   Show_Package_Declaration :
   declare
      use Ada.Wide_Text_IO;
      use Asis.Elements, Asis.Text;
   begin
      Package_Declaration := Unit_Declaration (Compilation_Unit);

      Put_Line (Item => Debug_Image (Package_Declaration));
      New_Line;
      Put_Line (Item => "Package declaration source text:");
      New_Line;
      Put_Line (Item => Element_Image (Package_Declaration));
      New_Line;
   end Show_Package_Declaration;

   Visible_Declaration_Elements :
   declare
      use Ada.Wide_Text_IO;
      use Asis.Declarations, Asis.Elements, Asis.Text;
      Declarations : constant Asis.Declarative_Item_List :=
                       Visible_Part_Declarative_Items (Package_Declaration);
   begin
      Put_Line (Item => "Visible declarations:");

      for Declaration of Declarations loop
         Put_Line (Item => Debug_Image (Declaration));
         New_Line;
         Put_Line (Item => "Source text:");
         New_Line;
         Put_Line (Item => Element_Image (Declaration));
         New_Line;
      end loop;
   end Visible_Declaration_Elements;

   Asis.Ada_Environments.Close (The_Context => Context);
   Asis.Ada_Environments.Dissociate (The_Context => Context);
   Asis.Implementation.Finalize (Parameters => "");
end ASIS_Experiment;
