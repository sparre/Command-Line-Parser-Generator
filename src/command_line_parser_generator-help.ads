--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with
  Asis,
  Asis.Elements;

package Command_Line_Parser_Generator.Help is
   use Asis, Asis.Elements;

   subtype Package_Specification is Asis.Declaration
     with Dynamic_Predicate => Declaration_Kind (Package_Specification)
                                 = A_Package_Declaration;

   subtype Procedure_Specification is Asis.Declaration
     with Dynamic_Predicate => Declaration_Kind (Procedure_Specification)
                                 in A_Procedure_Declaration         |
                                    A_Procedure_Renaming_Declaration;

   function Generate_Show_Help (Item : in Package_Specification)
                               return Boolean;
   function Generate_Help_Texts (Item : in Package_Specification)
                                return Boolean;
end Command_Line_Parser_Generator.Help;
