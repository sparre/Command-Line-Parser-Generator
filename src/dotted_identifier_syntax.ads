with Ada.Strings.Maps.Constants,
     Ada.Text_IO;

with OpenToken.Production,
     OpenToken.Production.List,
     OpenToken.Production.Parser,
     OpenToken.Production.Parser.LALR,
     OpenToken.Production.Parser.LALR.Generator,
     OpenToken.Production.Parser.LALR.Parser,
     OpenToken.Production.Parser.LALR.Parser_Lists,
     OpenToken.Recognizer.Character_Set,
     OpenToken.Recognizer.End_Of_File,
     OpenToken.Recognizer.Identifier,
     OpenToken.Recognizer.Line_Comment,
     OpenToken.Recognizer.Separator,
     OpenToken.Text_Feeder.Text_IO,
     OpenToken.Token.Enumerated,
     OpenToken.Token.Enumerated.Analyzer,
     OpenToken.Token.Enumerated.Identifier,
     OpenToken.Token.Enumerated.List,
     OpenToken.Token.Enumerated.Nonterminal;

with Dotted_Identifier_Token;

package Dotted_Identifier_Syntax is
   type Token_IDs is
     (
      Comment_T,
      Whitespace_T,
      Dot_T,
      Identifier_T,
      --
      End_Of_File_T,
      --
      Compilation_Unit_T,
      Dotted_Identifier_T);

   package Master_Token is
     new OpenToken.Token.Enumerated (Token_ID       => Token_IDs,
                                     First_Terminal => Dot_T,
                                     Last_Terminal  => End_Of_File_T,
                                     Token_Image    => Token_IDs'Image);
   package Tokenizer is
     new Master_Token.Analyzer;
   package Token_List is
     new Master_Token.List;
   package Nonterminal is
     new Master_Token.Nonterminal (Token_List => Token_List);

   package Identifiers is
     new Master_Token.Identifier;
   package Dotted_Identifiers is
     new Dotted_Identifier_Token (Token                => Master_Token,
                                  Token_List           => Token_List,
                                  Nonterminal          => Nonterminal,
                                  Identifier           => Identifiers,
                                  Dotted_Identifier_ID => Dotted_Identifier_T,
                                  Identifier_ID        => Identifier_T,
                                  Dot_ID               => Dot_T);

   package Production is
     new OpenToken.Production (Token       => Master_Token,
                               Token_List  => Token_List,
                               Nonterminal => Nonterminal);
   package Production_List is
     new Production.List;
   package Parser is
     new Production.Parser (Tokenizer => Tokenizer);
   package LALRs is
     new Parser.LALR (First_State_Index => 1);
   package LALR_Generators is
     new LALRs.Generator (Token_Image_Width => Token_IDs'Width,
                          Production_List   => Production_List);
   package Parser_Lists is
     new LALRs.Parser_Lists (First_Parser_Label => 1);
   package LALR_Parsers is
     new LALRs.Parser (First_Parser_Label => 1,
                       Parser_Lists       => Parser_Lists);

   pragma Style_Checks ("M180"); --  nice table format

   Syntax : constant Tokenizer.Syntax :=
     (Dot_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
      Identifier_T          => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars     => Ada.Strings.Maps.Constants.Letter_Set,
            Body_Chars      => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
      Comment_T             => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--")),
      Whitespace_T          => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      End_Of_File_T         => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Dot        : constant Master_Token.Class := Master_Token.Get (Dot_T);
   EOF        : constant Master_Token.Class := Master_Token.Get (End_Of_File_T);
   Identifier : constant Identifiers.Instance'Class := Identifiers.Get (Identifier_T);

   Compilation_Unit  : constant Nonterminal.Class := Nonterminal.Get (Compilation_Unit_T);
   Dotted_Identifier : constant Dotted_Identifiers.Class := Dotted_Identifiers.Get (Dotted_Identifier_T);

   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   Grammar : constant Production_List.Instance :=
     Compilation_Unit  <= Dotted_Identifier & EOF and
     Dotted_Identifier <= Dotted_Identifier & Dot & Identifier + Dotted_Identifiers.Join_Dotted_Identifiers and
     Dotted_Identifier <= Identifier;

   pragma Style_Checks ("M79"); --  Standard line length

   Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Standard_Input);

   Analyzer : constant Tokenizer.Handle :=
                Tokenizer.Initialize (Syntax,
                                      Feeder'Access);
end Dotted_Identifier_Syntax;
