with Asis.Declarations;

function Defining_Name (Item : Asis.Declaration) return Wide_String is
   Name_List : constant Asis.Defining_Name_List :=
     Asis.Declarations.Names (Item);
begin
   return
     Asis.Declarations.Defining_Name_Image (Name_List (Name_List'First));
end Defining_Name;
