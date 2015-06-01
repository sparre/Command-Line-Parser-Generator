with OpenToken.Token.Enumerated,
     OpenToken.Token.Enumerated.Identifier,
     OpenToken.Token.Enumerated.List,
     OpenToken.Token.Enumerated.Nonterminal;

private
with Ada.Strings.Unbounded;

generic
   with package Token       is new OpenToken.Token.Enumerated (<>);
   with package Token_List  is new Token.List;
   with package Nonterminal is new Token.Nonterminal (Token_List);
   with package Identifier  is new Token.Identifier;
   Dotted_Identifier_ID : Token.Token_ID;
   Identifier_ID        : Token.Token_ID;
   Dot_ID               : Token.Token_ID;
package Dotted_Identifier_Token is
   type Instance is new Nonterminal.Instance with private;
   subtype Class is Instance'Class;
   type Reference is access all Class;

   function Get (ID     : in Token.Token_ID;
                 Value  : in String := "") return Class;

   function Value (Item : in Instance) return String;

   overriding
   procedure Synthesize_By_Copying (New_Token :    out Instance;
                                    Source    : in     Token.Instance'Class;
                                    To_ID     : in     Token.Token_ID);

   Join_Dotted_Identifiers : constant Nonterminal.Synthesize;
   --  Valid for a token list consisting of two identifiers/dotted
   --  identifiers separated by a dot.
   Undotted_Identifier     : constant Nonterminal.Synthesize;
   --  Valid for a token list consisting of a single identifier.
private
   type Instance is new Nonterminal.Instance with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Synthesize_Join (New_Token :    out Nonterminal.Class;
                              Source    : in     Token_List.Instance'Class;
                              To_ID     : in     Token.Token_ID);
   procedure Synthesize_Copy (New_Token :    out Nonterminal.Class;
                              Source    : in     Token_List.Instance'Class;
                              To_ID     : in     Token.Token_ID);

   Join_Dotted_Identifiers : constant Nonterminal.Synthesize :=
                               Synthesize_Join'Access;
   Undotted_Identifier     : constant Nonterminal.Synthesize :=
                               Synthesize_Copy'Access;

end Dotted_Identifier_Token;
