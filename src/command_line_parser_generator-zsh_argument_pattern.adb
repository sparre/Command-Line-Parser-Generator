--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

package body Command_Line_Parser_Generator.Zsh_Argument_Pattern is
   function "or" (Left, Right : Instance) return Instance is
      use all type Identifier_Set.Instance;
   begin
      case Left.Kind is
         when Files =>
            case Right.Kind is
               when Files | Directories =>
                  return Create (Kind => Files);
               when Enumeration | Anything | Flag =>
                  return Create (Kind => Anything);
            end case;
         when Directories =>
            case Right.Kind is
               when Files =>
                  return Create (Kind => Files);
               when Directories =>
                  return Create (Kind => Directories);
               when Enumeration | Anything | Flag =>
                  return Create (Kind => Anything);
            end case;
         when Enumeration =>
            case Right.Kind is
               when Files | Directories | Anything =>
                  return Create (Kind => Anything);
               when Enumeration =>
                  return
                    Create_Enumeration (Values => Left.Values or Right.Values);
               when Flag =>
                  return Create_Enumeration
                           (Values => Left.Values or To_Set (+"True"));
            end case;
         when Anything =>
            return Create (Kind => Anything);
         when Flag =>
            case Right.Kind is
               when Files | Directories | Anything =>
                  return Create (Kind => Anything);
               when Flag =>
                  return Create (Kind => Flag);
               when Enumeration =>
                  return Create_Enumeration
                           (Values => Right.Values or To_set (+"True"));
            end case;
      end case;
   end "or";

   function Create (Kind : in Simple_Kinds) return Instance is
   begin
      return Instance'(Kind   => Kind,
                       Values => <>);
   end Create;

   function Create_Enumeration (Values : in Identifier_Set.Instance)
                               return Instance is
   begin
      return Instance'(Kind   => Enumeration,
                       Values => Values);
   end Create_Enumeration;

   function Image (Item : in Instance) return Wide_String is
   begin
      case Item.Kind is
         when Files =>
            return "_files";
         when Directories =>
            return "_directories";
         when Flag =>
            raise Constraint_Error
              with "Flags have no arguments.";
         when Anything =>
            return "";
         when Enumeration =>
            declare
               use all type Source_Text;
               Buffer : Source_Text := +"(";
            begin
               for Identifier of Item.Values loop
                  Append (Buffer, " ");
                  Append (Buffer, Identifier);
               end loop;
               return (+Buffer) & " )";
            end;
      end case;
   end Image;

   function Kind (Item : in Instance) return Kinds is
   begin
      return Item.Kind;
   end Kind;

   function Values (Item : in Instance) return Identifier_Set.Instance is
   begin
      if Item.Kind = Enumeration then
         return Item.Values;
      else
         raise Constraint_Error;
      end if;
   end Values;
end Command_Line_Parser_Generator.Zsh_Argument_Pattern;
