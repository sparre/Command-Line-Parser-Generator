--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Asis.Declarations;

with Command_Line_Parser_Generator.Utilities;

package body Command_Line_Parser_Generator.Help is
   function Is_Put_Help_Procedure (Item : in Procedure_Specification)
                                  return Boolean;
   function Is_Show_Help_Procedure (Item : in Procedure_Specification)
                                   return Boolean;

   function Generate_Help_Texts (Item : in Package_Specification)
                                return Boolean
   is
      Has_Help_Texts : Boolean := False;

      use Asis.Declarations;
   begin
      for Declaration of Private_Part_Declarative_Items (Item) loop
         case Declaration_Kind (Declaration) is
            when Asis.A_Procedure_Declaration |
                 Asis.A_Procedure_Renaming_Declaration =>
               Has_Help_Texts := Is_Put_Help_Procedure (Declaration);
            when others =>
               null; --  Not relevant here.
         end case;

         exit when Has_Help_Texts;
      end loop;

      return not Has_Help_Texts;
   end Generate_Help_Texts;

   function Generate_Show_Help (Item : in Package_Specification)
                               return Boolean
   is
      Has_Show_Help : Boolean := False;

      use Asis.Declarations;
   begin
      for Declaration of Visible_Part_Declarative_Items (Item) loop
         case Declaration_Kind (Declaration) is
            when Asis.A_Procedure_Declaration |
                 Asis.A_Procedure_Renaming_Declaration =>
               Has_Show_Help := Is_Show_Help_Procedure (Declaration);
            when others =>
               null; --  Not relevant here.
         end case;

         exit when Has_Show_Help;
      end loop;

      return not Has_Show_Help;
   end Generate_Show_Help;

   function Is_Put_Help_Procedure (Item : in Procedure_Specification)
                                  return Boolean
   is
      use Asis.Declarations;
      use Utilities;
   begin
      if Utilities.Defining_Name (Item) /= "Put_Help" then
         return False;
      end if;

      if Parameter_Profile (Item)'Length /= 1 then
         return False;
      end if;

      for Parameter of Parameter_Profile (Item) loop
         if Names (Parameter)'Length /= 1 then
            return False;
         end if;

         if Utilities.Defining_Name (Parameter) /= "File" then
            return False;
         end if;

         if Full_Defining_Name (Object_Declaration_View (Parameter))
              /= "Ada.Text_IO.File_Type"
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Put_Help_Procedure;

   function Is_Show_Help_Procedure (Item : in Procedure_Specification)
                                  return Boolean
   is
      use Asis.Declarations;
      use Utilities;
   begin
      if Utilities.Defining_Name (Item) /= "Show_Help" then
         return False;
      end if;

      if Parameter_Profile (Item)'Length /= 1 then
         return False;
      end if;

      for Parameter of Parameter_Profile (Item) loop
         if Names (Parameter)'Length /= 1 then
            return False;
         end if;

         if Utilities.Defining_Name (Parameter) /= "Help" then
            return False;
         end if;

         if Full_Defining_Name (Object_Declaration_View (Parameter))
              /= "Standard.Boolean"
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Show_Help_Procedure;
end Command_Line_Parser_Generator.Help;
