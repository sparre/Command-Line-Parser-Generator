with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Wide_Text_IO;

with Asis,
     Asis.Ada_Environments,
     Asis.Compilation_Units,
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

   Package_Declaration := Asis.Elements.Unit_Declaration (Compilation_Unit);

   Ada.Wide_Text_IO.Put_Line
     (Item => Asis.Elements.Debug_Image (Package_Declaration));
   Ada.Wide_Text_IO.Put_Line
     (Item => "Package source text:");
   Ada.Wide_Text_IO.New_Line;
   Ada.Wide_Text_IO.Put_Line
     (Item => Asis.Text.Element_Image (Package_Declaration));
   Ada.Wide_Text_IO.New_Line;

   Asis.Ada_Environments.Close (The_Context => Context);
   Asis.Ada_Environments.Dissociate (The_Context => Context);
   Asis.Implementation.Finalize (Parameters => "");
end ASIS_Experiment;
