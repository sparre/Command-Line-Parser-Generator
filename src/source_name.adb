with Asis.Elements,
     Asis.Expressions;

with Thick_Queries;

with Defining_Name;

function Source_Name (Item : Asis.Declaration) return Wide_String is
   Full_Type      : Asis.Element;
   CU_Of_Type     : Asis.Compilation_Unit;
   CU_Declaration : Asis.Declaration;

   use Asis.Elements, Asis.Expressions;
begin
   Full_Type      := Corresponding_Name_Declaration (Thick_Queries.Simple_Name
                                                       (Item));
   CU_Of_Type     := Enclosing_Compilation_Unit (Full_Type);
   CU_Declaration := Unit_Declaration (CU_Of_Type);

   return Defining_Name (CU_Declaration);
end Source_Name;
