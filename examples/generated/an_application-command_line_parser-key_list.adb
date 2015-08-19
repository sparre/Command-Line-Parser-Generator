package body An_Application.Command_Line_Parser.Key_List is
   function "+" (Left  : in     Instance;
                 Right : in     String) return Instance is
   begin
      return Result : Instance := Left do
         Result.Insert (Right);
      end return;
   end "+";

   function "+" (Right : in     String) return Instance is
   begin
      return Result : Instance do
         Result.Insert (Right);
      end return;
   end "+";

   function "=" (Left  : in     Argument_List.Instance;
                 Right : in     Instance) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      for Key of Right loop
         if not Left.Contains (Key) then
            return False;
         end if;
      end loop;

      return Left.Length = Right.Length;
   end "=";
end An_Application.Command_Line_Parser.Key_List;
