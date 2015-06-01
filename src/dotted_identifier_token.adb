with Ada.Exceptions,
     Ada.Tags,
     Ada.Text_IO;

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
      use type Token.Token_ID;
   begin
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier.Synthesize_By_Copying (" &
           "Source    => " & Ada.Tags.Expanded_Name (Source'Tag) & " [ID = " &
                             Token.Token_Image (Source.ID) & "], " &
           "To_ID     => " & Token.Token_ID'Image (To_ID) & "):");

      if Source in Class then
         New_Token :=
           (Nonterminal.Instance (Nonterminal.Get (To_ID))
              with Value => Instance (Source).Value
           );
      elsif Source in Identifier.Instance'Class then
         New_Token :=
           (Nonterminal.Instance (Nonterminal.Get (To_ID))
              with Value => +Identifier.Instance (Source).Identifier
           );
      elsif Source.ID = Identifier_ID then
         New_Token :=
           (Nonterminal.Instance (Nonterminal.Get (To_ID))
              with Value => +Identifier.Instance (Source).Identifier
           );
      else
         raise Nonterminal.Invalid_Synth_Argument
           with "Token " & Token.Token_ID'Image (To_ID) & " cannot be " &
                "synthesized solely from a " &
                Token.Token_ID'Image (Token.ID (Source)) & ".";
      end if;

      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier: """ & (+Class (New_Token).Value) & """");
   end Synthesize_By_Copying;

   procedure Synthesize_Copy (New_Token :    out Nonterminal.Class;
                              Source    : in     Token_List.Instance'Class;
                              To_ID     : in     Token.Token_ID) is
      use type Token.Token_ID;

      List : constant Token_List.List_Iterator :=
               Token_List.Initial_Iterator (Source);
      Head : constant Token.Handle :=
               Token_List.Token_Handle (List);
   begin
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier.Synthesize_Copy (To_ID => " &
           Token.Token_ID'Image (To_ID) & "):");

      if Head.ID = Identifier_ID then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "   Processing a single identifier...");
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "      Image: " & Head.Image);
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "   Source tag: " & Ada.Tags.Expanded_Name (Head'Tag));
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "      Value:" & (+Class (Head.all).Value));

         New_Token :=
           Class (Instance'
                    (Token.Instance (Token.Get (To_ID))
                       with Value => +Head.Image)
                 );
      elsif Head.all in Class then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "   Processing a dotted identifier...");

         New_Token :=
           Class (Instance'
                    (Token.Instance (Token.Get (To_ID))
                       with Value => +(Value (Class (Head.all)))
                    )
                 );
      else
         raise Nonterminal.Invalid_Synth_Argument
           with "Token " & Token.Token_ID'Image (To_ID) & " cannot be " &
                "synthesized solely from a " &
                Token.Token_ID'Image (Token.ID (Head.all)) & ".";
      end if;

      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier: """ & (+Class (New_Token).Value) & """");
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Unexpected exception in Dotted_Identifier_Token." &
              "Synthesize_Copy: " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
         raise;
   end Synthesize_Copy;

   procedure Synthesize_Join (New_Token :    out Nonterminal.Class;
                              Source    : in     Token_List.Instance'Class;
                              To_ID     : in     Token.Token_ID) is
      use type OpenToken.Buffers.Bounded_String;
      use type Token.Token_ID;

      Left  : constant Token_List.List_Iterator :=
                                          Token_List.Initial_Iterator (Source);
      Dot   : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
      Right : Token_List.List_Iterator := Token_List.Initial_Iterator (Source);
   begin
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier.Synthesize_Join:");

      Token_List.Next_Token (Dot);

      Token_List.Next_Token (Right);
      Token_List.Next_Token (Right);

      if not (Token_List.ID (Left) = Identifier_ID or
              Token_List.ID (Left) = Dotted_Identifier_ID)
      then
         raise Constraint_Error
           with "Expected a (dotted) identifier as the left token.";
      end if;

      if Token_List.ID (Dot) /= Dot_ID then
         raise Constraint_Error with "Expected a dot as the middle token.";
      end if;

      if not (Token_List.ID (Right) = Identifier_ID or
              Token_List.ID (Right) = Dotted_Identifier_ID)
      then
         raise Constraint_Error
           with "Expected a (dotted) identifier as the right token.";
      end if;

      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "   New_Token tag: " &
                   Ada.Tags.Expanded_Name (New_Token'Tag));

      New_Token :=
        Class (Instance'
                 (Token.Instance (Token.Get (To_ID))
                    with Value => +(Token_List.Token_Handle (Left).Image &
                                      "." &
                                      Token_List.Token_Handle (Right).Image)
                 )
              );

      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Dotted identifier: """ & (+Class (New_Token).Value) & """");
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "   Left token tag: " &
              Ada.Tags.Expanded_Name (Token_List.Token_Handle (Left)'Tag));
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "   Right token tag: " &
              Ada.Tags.Expanded_Name (Token_List.Token_Handle (Right)'Tag));
         raise;
   end Synthesize_Join;

   function Value (Item : in Instance) return String is
   begin
      return +Item.Value;
   end Value;
end Dotted_Identifier_Token;
