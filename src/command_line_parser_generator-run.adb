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

with Command_Line_Parser_Generator.Formal_Parameter,
     Command_Line_Parser_Generator.Procedure_Declaration,
     Command_Line_Parser_Generator.Procedure_Declaration_List,
     Command_Line_Parser_Generator.Setup,
     Command_Line_Parser_Generator.Templates,
     Command_Line_Parser_Generator.Utilities;

procedure Command_Line_Parser_Generator.Run is
   Context             : Asis.Context;
   Compilation_Unit    : Asis.Compilation_Unit;
   Package_Declaration : Asis.Declaration;

   Package_Name : Source_Text;
   Profiles     : Procedure_Declaration_List.Instance;
   A_Procedure  : Procedure_Declaration.Instance;
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
            null; --  Good.  We continue.
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
   end Check_For_Package;

   Show_Package_Declaration :
   declare
      use Asis.Elements;
   begin
      Package_Declaration := Unit_Declaration (Compilation_Unit);

      Package_Name := +Utilities.Defining_Name (Package_Declaration);
   end Show_Package_Declaration;

   Visible_Declaration_Elements :
   declare
      use Ada.Command_Line, Ada.Wide_Text_IO;
      use Asis.Declarations, Asis.Elements, Asis.Text;
      use all type Asis.Declaration_Kinds;
      Declarations : constant Asis.Declarative_Item_List :=
                       Visible_Part_Declarative_Items (Package_Declaration);
   begin
      for Declaration of Declarations loop
         if Declaration_Kind (Declaration) = A_Procedure_Declaration then
            A_Procedure :=
              (Name              => +Utilities.Defining_Name (Declaration),
               Formal_Parameters => <>);

            Show_Parameters :
            declare
               use all type Asis.Mode_Kinds;

               Parameters : constant Asis.Parameter_Specification_List :=
                              Parameter_Profile (Declaration);

               A_Formal_Parameter : Formal_Parameter.Instance :=
                 (Name              => +"<formal parameter>",
                  Image_Function    => +"'Image",
                  Value_Function    => +"'Value",
                  Default_Value     => +"<default value>",
                  Type_Name         => +"<type name>");
            begin
               for Parameter of Parameters loop
                  for Name of Names (Parameter) loop
                     A_Formal_Parameter.Name := +Defining_Name_Image (Name);

                     case Mode_Kind (Parameter) is
                        when A_Default_In_Mode | An_In_Mode =>
                           null; --  Fine, we continue.
                        when An_In_Out_Mode | An_Out_Mode =>
                           Put_Line (File => Standard_Error,
                                     Item => "Out parameters not allowed.");
                           Set_Exit_Status (Failure);
                           return;
                        when Not_A_Mode =>
                           Put_Line (File => Standard_Error,
                                     Item => "ASIS error.");
                           Set_Exit_Status (Failure);
                           return;
                     end case;

                     if Has_Aliased (Parameter) then
                        Put_Line (File => Standard_Error,
                                  Item => "Aliased parameters not allowed.");
                        Set_Exit_Status (Failure);
                        return;
                     end if;

                     Full_Type_Name :
                     declare
                        use Utilities;

                        Type_Of_Parameter : constant Asis.Declaration :=
                          Object_Declaration_View (Parameter);
                     begin
                        A_Formal_Parameter.Type_Name :=
                          +Full_Defining_Name (Type_Of_Parameter);
                     end Full_Type_Name;

                     Check_For_Default_Value :
                     declare
                        use all type Asis.Element_Kinds;

                        Value : constant Asis.Expression :=
                                  Initialization_Expression (Parameter);
                     begin
                        case Element_Kind (Value) is
                           when Not_An_Element =>
                              null; --  No default value.

                              A_Formal_Parameter.Default_Value := +"";
                           when An_Expression =>
                              A_Formal_Parameter.Default_Value :=
                                +Trim (Element_Image (Value));
                           when others =>
                              Put_Line (File => Standard_Error,
                                        Item => Debug_Image (Value));
                              Put_Line (File => Standard_Error,
                                        Item => "ASIS error.");
                              Set_Exit_Status (Failure);
                              return;
                        end case;
                     end Check_For_Default_Value;

                     Image_And_Value_Functions :
                     declare
                        use type Source_Text;
                     begin
                        A_Formal_Parameter.Image_Function :=
                          A_Formal_Parameter.Type_Name & "'Image";
                        A_Formal_Parameter.Value_Function :=
                          A_Formal_Parameter.Type_Name & "'Value";
                     end Image_And_Value_Functions;

                     A_Procedure.Formal_Parameters.Append
                       (A_Formal_Parameter);
                  end loop;
               end loop;
            end Show_Parameters;

            Profiles.Append (A_Procedure);
         elsif Declaration_Kind (Declaration) = A_Function_Declaration then
            Put_Line
              (File => Standard_Error,
               Item => "Functions are not allowed in the visible part of " &
                       "the package specification.");
            Put_Line
              (File => Standard_Error,
               Item => """" & Element_Image (Declaration) & """ is an error.");
            Set_Exit_Status (Failure);
            return;
         else
            Put_Line (File => Standard_Error,
                      Item => "Warning: """ & Element_Image (Declaration) &
                              """ is ignored.");
         end if;
      end loop;

      Put_Line ("package " & (+Package_Name) & " is");
      New_Line;
      Put_Line (Profiles.Image);
      Put_Line ("end " & (+Package_Name) & ";");

      Templates.Create (Target_Directory => "generated");
      Templates.Runner (Package_Name => +Package_Name);
   exception
      when others =>
         Put_Line (Standard_Error,
                   Asis.Implementation.Diagnosis);
         raise;
   end Visible_Declaration_Elements;

   Asis.Ada_Environments.Close (The_Context => Context);
   Asis.Ada_Environments.Dissociate (The_Context => Context);
   Asis.Implementation.Finalize (Parameters => "");
end Command_Line_Parser_Generator.Run;
