with Asis.Expressions;

with Thick_Queries;

with Defining_Name,
     Source_Name;

function Full_Defining_Name (Item : Asis.Declaration) return Wide_String is
   Full_Type : Asis.Element;

   use Asis.Expressions;
begin
   Full_Type := Corresponding_Name_Declaration (Thick_Queries.Simple_Name
                                                  (Item));

   return Source_Name (Item) & "." & Defining_Name (Full_Type);
end Full_Defining_Name;
