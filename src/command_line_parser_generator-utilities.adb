with Ada.Wide_Text_IO;

with Asis.Declarations,
     Asis.Definitions,
     Asis.Elements,
     Asis.Expressions,
     Asis.Text;

with Thick_Queries;

package body Command_Line_Parser_Generator.Utilities is
   function First_Subtype_Definition (Item   : Asis.Declaration;
                                      Prefix : Wide_String := "* ")
                                     return Asis.Element;

   function Defining_Name (Item : Asis.Declaration) return Wide_String is
      Name_List : constant Asis.Defining_Name_List :=
                    Asis.Declarations.Names (Item);
   begin
      return
        Asis.Declarations.Defining_Name_Image (Name_List (Name_List'First));
   end Defining_Name;

   function Enumeration_Values (Type_Declaration : in Asis.Declaration)
                               return Identifier_Set.Instance is
      use Ada.Wide_Text_IO;
      use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions,
          Asis.Text;
      use all type Asis.Declaration_Kinds, Asis.Type_Kinds;

      Name        : constant Asis.Element :=
                      Name_Definition (Type_Declaration);
      Declaration : constant Asis.Element :=
                      Enclosing_Element (Name);
      Definition  : constant Asis.Element :=
                      Type_Declaration_View (Declaration);
      Literals    : constant Asis.Declaration_List :=
                      Enumeration_Literal_Declarations (Definition);
   begin
      if Full_Defining_Name (Type_Declaration) = "Standard.Boolean" then
         return Result : Identifier_Set.Instance do
            Result.Append (+"True");
            Result.Append (+"False");
         end return;
      elsif Declaration_Kind (Declaration) = An_Ordinary_Type_Declaration
            and then
            Type_Kind (Definition) = An_Enumeration_Type_Definition
      then
         return Result : Identifier_Set.Instance do
            for Literal of Literals loop
               Result.Append (+Trim (Element_Image (Literal)));
            end loop;
         end return;
      else
         raise Constraint_Error
           with "Enumeration_Values called with something, which isn't a " &
                "type declaration.";
      end if;
   exception
      when others =>
         Put_Line (File => Standard_Error,
                   Item => "Exception raised in Enumeration_Values:");

         Put_Line (File => Standard_Error,
                   Item => "   Type of formal parameter: '" &
                     Element_Image (Type_Declaration) & "'");

         Put_Line (File => Standard_Error,
                   Item => "   Type name: '" &
                     Element_Image (Name) & "'");

         Put_Line (File => Standard_Error,
                   Item => "   Type declaration: '" &
                     Element_Image (Declaration) & "'");
         Put_Line (File => Standard_Error,
                   Item => Debug_Image (Declaration));
         New_Line (File => Standard_Error);

         Put_Line (File => Standard_Error,
                   Item => "   Type declaration view: '" &
                     Element_Image (Definition) &
                     "'");
         Put_Line (File => Standard_Error,
                   Item => Debug_Image (Definition));
         New_Line (File => Standard_Error);

         raise;
   end Enumeration_Values;

   function First_Subtype_Definition (Item   : Asis.Declaration;
                                      Prefix : Wide_String := "* ")
                                     return Asis.Element
   is
   begin
      declare
         use Asis.Declarations, Asis.Definitions, Asis.Elements,
             Asis.Expressions,
             Asis.Text;

         Name        : constant Asis.Element := Name_Definition (Item);
         Declaration : constant Asis.Element := Enclosing_Element (Name);
         Kind        : constant Asis.Declaration_Kinds := Declaration_Kind
                                                            (Declaration);
         Definition  : constant Asis.Element := Type_Declaration_View
                                                  (Declaration);
      begin
         case Kind is
            when Asis.An_Ordinary_Type_Declaration =>
               case Type_Kind (Definition) is
                  when Asis.A_Derived_Type_Definition =>
                     return First_Subtype_Definition
                              (Subtype_Mark (Parent_Subtype_Indication
                                               (Definition)),
                               "*" & Prefix);
                  when others =>
                     return Definition;
               end case;
            when Asis.A_Subtype_Declaration =>
               return First_Subtype_Definition (Subtype_Mark (Definition),
                                                "*" & Prefix);
            when others =>
               return
                 raise Program_Error
                   with "First_Subtype_Definition does not handle " &
                        Asis.Declaration_Kinds'Image (Kind) & " yet.";
         end case;
      exception
         when others =>
            declare
               use Ada.Wide_Text_IO;
            begin
               Put_Line (File => Standard_Error,
                         Item => Prefix & "   Type name: '" &
                                 Element_Image (Name) & "'");

               Put_Line (File => Standard_Error,
                         Item => Prefix & "   Type declaration: '" &
                                 Element_Image (Declaration) & "'");
               Put_Line (File => Standard_Error,
                         Item => Prefix & Debug_Image (Declaration));
               Put_Line (File => Standard_Error,
                         Item => Prefix);

               Put_Line (File => Standard_Error,
                         Item => Prefix & "   Type declaration view: '" &
                                 Element_Image (Definition) & "'");
               Put_Line (File => Standard_Error,
                         Item => Prefix & Debug_Image (Definition));
               Put_Line (File => Standard_Error,
                         Item => Prefix);
               New_Line (File => Standard_Error);
            end;

            raise;
      end;
   exception
      when others =>
         declare
            use Ada.Wide_Text_IO;
            use Asis.Elements;
         begin
            Put_Line (File => Standard_Error,
                      Item => Prefix & Debug_Image (Item));
            Put_Line (File => Standard_Error,
                      Item => Prefix);
            Put_Line (File => Standard_Error,
                      Item => Prefix & "First_Subtype_Definition terminated " &
                              "by an exception.");
         end;

         raise;
   end First_Subtype_Definition;

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

   function Is_Enumeration (Type_Declaration : in Asis.Declaration)
                           return Boolean is
      use Ada.Wide_Text_IO;
      use Asis.Declarations, Asis.Definitions, Asis.Elements, Asis.Expressions,
          Asis.Text;

      Definition  : Asis.Element;
   begin
      if Full_Defining_Name (Type_Declaration) = "Standard.Character" then
         return False;
      else
         Definition := First_Subtype_Definition (Type_Declaration);

         case Type_Kind (Definition) is
            when Asis.An_Enumeration_Type_Definition =>
               return True;
            when Asis.Not_A_Type_Definition =>
               raise Program_Error
                 with "Is_Enumeration attempted to get the type kind of " &
                      "something which isn't a type.";
            when Asis.An_Unconstrained_Array_Definition =>
               return False;
            when Asis.A_Derived_Type_Definition =>
               raise Program_Error
                 with "First_Subtype_Definition should not be a derived " &
                      "type definition.";
            when Asis.A_Signed_Integer_Type_Definition =>
               return False;
            when others =>
               raise Program_Error
                 with "Is_Enumeration (type) not fully implemented yet.";
         end case;
      end if;
   end Is_Enumeration;

   function Is_Integer_Compatible (The_Subtypes : in Asis.Element_List)
                                  return Boolean is
   begin
      if The_Subtypes'Length /= 1 then
         raise Constraint_Error
           with "Is_Integer_Compatible expects exactly one subtype in the " &
                "list.";
      end if;

      return Is_Integer_Compatible (The_Subtypes (The_Subtypes'First));
   end Is_Integer_Compatible;

   function Is_Integer_Compatible (The_Subtype : in Asis.Element)
                                  return Boolean is
   begin
      if Full_Defining_Name (The_Subtype) = "Standard.Integer" then
         return True;
      elsif Full_Defining_Name (The_Subtype) = "Standard.Natural" then
         return True;
      elsif Full_Defining_Name (The_Subtype) = "Standard.Positive" then
         return True;
      else
         return raise Program_Error
           with "Is_Integer compatible not implemented yet.";
      end if;
   exception
      when others =>
         declare
            use Ada.Wide_Text_IO;
            use Asis.Text;
         begin
            Put_Line (File => Standard_Error,
                      Item => "Is_Integer_Compatible terminated by an " &
                              "exception.");
         end;

         raise;
   end Is_Integer_Compatible;

   function Is_String_Compatible (Type_Declaration : in Asis.Declaration)
                                 return Boolean is
   begin
      if Full_Defining_Name (Type_Declaration) = "Standard.Character" then
         return False;
      elsif Full_Defining_Name (Type_Declaration) = "Standard.String" then
         return True;
      end if;

      declare
         use Asis.Definitions, Asis.Elements;

         Definition : constant Asis.Element := First_Subtype_Definition
                                                 (Type_Declaration);
      begin
         case Type_Kind (Definition) is
            when Asis.An_Enumeration_Type_Definition =>
               return False;
            when Asis.Not_A_Type_Definition =>
               raise Program_Error
                 with "Is_String_Compatible attempted to get the type kind " &
                      "of something which isn't a type.";
            when Asis.An_Unconstrained_Array_Definition =>
               return
                 Is_Integer_Compatible (Index_Subtype_Definitions (Definition))
                 and
                 Static_Match_With_Character (Array_Component_Definition
                                                (Definition));
            when Asis.A_Derived_Type_Definition =>
               raise Program_Error
                 with "First_Subtype_Definition should not be a derived " &
                      "type definition.";
            when Asis.A_Signed_Integer_Type_Definition =>
               return False;
            when others =>
               raise Program_Error
                 with "Is_String_Compatible not fully implemented yet.";
         end case;
      end;
   end Is_String_Compatible;

   function Name_Definition (Item : Asis.Declaration) return Asis.Element is
      use Asis, Asis.Elements, Asis.Expressions;
   begin
      case Expression_Kind (Item) is
         when An_Identifier =>
            return Corresponding_Name_Definition (Item);
         when A_Selected_Component =>
            return Name_Definition (Selector (Item));
         when others =>
            raise Constraint_Error
              with "Name_Definition: Unknown kind of name.";
      end case;
   end Name_Definition;

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

   function Static_Match_With_Character
     (The_Subtype : in Asis.Declaration) return Boolean is
   begin
      declare
         use Asis.Definitions;

         View : Asis.Definition renames Component_Definition_View
                                          (The_Subtype);
         Mark : Asis.Expression renames Subtype_Mark (View);
      begin
         if Full_Defining_Name (Mark) = "Standard.Character" then
            return True;
         end if;
      end;

      return raise Program_Error
               with "Static_Match_With_Character not fully implemented yet.";
   exception
      when others =>
         declare
            use Ada.Wide_Text_IO;
            use Asis.Definitions, Asis.Elements;
         begin
            Put_Line (File => Standard_Error,
                      Item => Debug_Image (The_Subtype));
            New_Line (File => Standard_Error);
            Put_Line (File => Standard_Error,
                      Item => Debug_Image (Component_Definition_View
                                             (The_Subtype)));
            New_Line (File => Standard_Error);
            Put_Line (File => Standard_Error,
                      Item => Debug_Image (Subtype_Mark
                                             (Component_Definition_View
                                                (The_Subtype))));
            New_Line (File => Standard_Error);
            Put_Line (File => Standard_Error,
                      Item => "Static_Match_With_Character terminate by an " &
                              "exception");
         end;

         raise;
   end Static_Match_With_Character;
end Command_Line_Parser_Generator.Utilities;
