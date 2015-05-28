package body Dotted_Identifier_Token is
   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+" (Item : in OpenToken.Buffers.Bounded_String)
                return Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in Ada.Strings.Unbounded.Unbounded_String)
                return String
     renames Ada.Strings.Unbounded.To_String;

   function "+" (Item : in OpenToken.Buffers.Bounded_String)
                return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return +OpenToken.Buffers.To_String (Item);
   end "+";

   function Get (ID     : in Token.Token_ID;
                 Value  : in String := "") return Class is
   begin
      return Class (Instance'(Nonterminal.Instance (Nonterminal.Get (ID))
                                with Value => +Value));
   end Get;

   overriding
   procedure Synthesize_By_Copying (New_Token :    out Instance;
                                    Source    : in     Token.Instance'Class;
                                    To_ID     : in     Token.Token_ID) is
   begin
      if Source in Identifier.Instance'Class then
         New_Token :=
           (Nonterminal.Instance (Nonterminal.Get (To_ID))
              with Value => +Identifier.Instance (Source).Identifier);
      elsif Source in Class then
         New_Token :=
           (Nonterminal.Instance (Nonterminal.Get (To_ID))
              with Instance (Source).Value);
      else
         raise Nonterminal.Invalid_Synth_Argument
           with "Token " & Token.Token_ID'Image (To_ID) & " cannot be " &
                "synthesized solely from a " &
                Token.Token_ID'Image (Token.ID (Source)) & ".";
      end if;
   end Synthesize_By_Copying;

   procedure Synthesize_Join (New_Token :    out Nonterminal.Class;
                              Source    : in     Token_List.Instance'Class;
                              To_ID     : in     Token.Token_ID) is
      use type Token.Token_ID;
      Left  : constant Token_List.List_Iterator :=
                                          Token_List.Initial_Iterator (Source);
      Dot   : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
   begin
      Token_List.Next_Token (Dot);

      Token_List.Next_Token (Right);
      Token_List.Next_Token (Right);

      if Token_List.ID (Dot) /= Dot_ID then
         raise Constraint_Error with "Expected a dot as the middle token.";
      end if;

      New_Token :=
        Class (Instance'
                 (Token.Instance (Token.Get (To_ID))
                    with +(Value (Class (Token_List.Token_Handle (Left).all)) &
                           "." &
                           Value (Class (Token_List.Token_Handle (Right).all)))
                    ));
   end Synthesize_Join;

   function Value (Item : in Instance) return String is
   begin
      return +Item.Value;
   end Value;
end Dotted_Identifier_Token;
