with Ada.Strings.Maps.Constants;

with OpenToken.Production,
     OpenToken.Production.List,
     OpenToken.Production.Parser,
     OpenToken.Production.Parser.LALR,
     OpenToken.Production.Parser.LALR.Generator,
     OpenToken.Production.Parser.LALR.Parser,
     OpenToken.Production.Parser.LALR.Parser_Lists,
     OpenToken.Recognizer.Based_Integer_Ada_Style,
     OpenToken.Recognizer.Based_Real_Ada_Style,
     OpenToken.Recognizer.Character_Set,
     OpenToken.Recognizer.End_Of_File,
     OpenToken.Recognizer.Graphic_Character,
     OpenToken.Recognizer.Identifier,
     OpenToken.Recognizer.Integer,
     OpenToken.Recognizer.Keyword,
     OpenToken.Recognizer.Line_Comment,
     OpenToken.Recognizer.Nothing,
     OpenToken.Recognizer.Real,
     OpenToken.Recognizer.Separator,
     OpenToken.Recognizer.String,
     OpenToken.Token.Enumerated,
     OpenToken.Token.Enumerated.Analyzer,
     OpenToken.Token.Enumerated.List,
     OpenToken.Token.Enumerated.Nonterminal;

package body Simple_Package_Syntax is
   package Master_Token is
     new OpenToken.Token.Enumerated (Token_ID       => Token_IDs,
                                     First_Terminal => Abort_T,
                                     Last_Terminal  => End_Of_File_T,
                                     Token_Image    => Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;

   pragma Style_Checks ("M120"); --  nice table format

   Syntax : constant Tokenizer.Syntax :=
     (Abort_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abort")),
      Abs_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abs")),
      Abstract_T            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("abstract")),
      Accept_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("accept")),
      Access_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("access")),
      Aliased_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("aliased")),
      All_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("all")),
      And_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("and")),
      Array_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("array")),
      At_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("at")),
      Begin_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("begin")),
      Body_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("body")),
      Case_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("case")),
      Constant_T            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("constant")),
      Declare_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("declare")),
      Delay_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("delay")),
      Delta_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("delta")),
      Digits_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("digits")),
      Do_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("do")),
      Else_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("else")),
      Elsif_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("elsif")),
      End_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("end")),
      Entry_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("entry")),
      Exception_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("exception")),
      Exit_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("exit")),
      For_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("for")),
      Function_T            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("function")),
      Generic_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("generic")),
      Goto_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("goto")),
      If_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("if")),
      In_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("in")),
      Interface_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("interface")),
      Is_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("is")),
      Limited_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("limited")),
      Loop_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("loop")),
      Mod_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("mod")),
      New_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("new")),
      Not_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("not")),
      Null_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("null")),
      Of_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("of")),
      Or_T                  => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("or")),
      Others_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("others")),
      Out_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("out")),
      Overriding_T          => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("overriding")),
      Package_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("package")),
      Pragma_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("pragma")),
      Private_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("private")),
      Procedure_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("procedure")),
      Protected_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("protected")),
      Raise_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("raise")),
      Range_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("range")),
      Record_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("record")),
      Rem_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("rem")),
      Renames_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("renames")),
      Requeue_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("requeue")),
      Return_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("return")),
      Reverse_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("reverse")),
      Select_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("select")),
      Separate_T            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("separate")),
      Some_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("some")),
      Subtype_T             => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("subtype")),
      Synchronized_T        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("synchronized")),
      Tagged_T              => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("tagged")),
      Task_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("task")),
      Terminate_T           => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("terminate")),
      Then_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("then")),
      Type_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("type")),
      Until_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("until")),
      Use_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("use")),
      When_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("when")),
      While_T               => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("while")),
      With_T                => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("with")),
      Xor_T                 => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("xor")),
      Colon_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":")),
      Comma_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      Dot_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (".")),
      Semicolon_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";")),
      Tick_T                => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("'")),
      Left_Parenthesis_T    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Right_Parenthesis_T   => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      Concatenate_T         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("&")),
      Alternative_T         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("|")),
      Equal_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=")),
      Not_Equal_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("/=")),
      Greater_Than_T        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">")),
      Less_Than_T           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<")),
      Greater_Equal_T       => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">=")),
      Less_Equal_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<=")),
      Plus_T                => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+")),
      Minus_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("-")),
      Times_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("*")),
      Divide_T              => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("/")),
      Arrow_T               => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("=>")),
      Assignment_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (":=")),
      Double_Dot_T          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("..")),
      Exponentiate_T        => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("**")),
      Left_Label_Bracket_T  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<<")),
      Right_Label_Bracket_T => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (">>")),
      Box_T                 => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("<>")),
      Integer_T             => Tokenizer.Get (OpenToken.Recognizer.Integer.Get (Allow_Signs => False)),
      Based_Integer_T       => Tokenizer.Get (OpenToken.Recognizer.Based_Integer_Ada_Style.Get),
      Real_T                => Tokenizer.Get (OpenToken.Recognizer.Real.Get (Allow_Signs => False)),
      Based_Real_T          => Tokenizer.Get (OpenToken.Recognizer.Based_Real_Ada_Style.Get),
      Character_T           => Tokenizer.Get (OpenToken.Recognizer.Graphic_Character.Get),
      String_T              => Tokenizer.Get (OpenToken.Recognizer.String.Get),
      Identifier_T          => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars     => Ada.Strings.Maps.Constants.Letter_Set,
            Body_Chars      => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
      Comment_T             => Tokenizer.Get (OpenToken.Recognizer.Line_Comment.Get ("--")),
      Whitespace_T          => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                          (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Bad_Token_T           => Tokenizer.Get (OpenToken.Recognizer.Nothing.Get),
      End_of_File_T         => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   pragma Style_Checks ("M79"); --  regular line length

   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax);

   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is
     new OpenToken.Production (Token       => Master_Token,
                               Token_List  => Token_List,
                               Nonterminal => Nonterminal);
   package Production_List is new Production.List;
   package OpenToken_Parser is new Production.Parser (Tokenizer);
   package LALRs is new OpenToken_Parser.LALR (First_State_Index => 1);
   package LALR_Generators is
     new LALRs.Generator (Token_Image_Width => Token_IDs'Width,
                          Production_List   => Production_List);
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label => 1);
   package LALR_Parsers is
     new LALRs.Parser (First_Parser_Label => 1,
                       Parser_Lists       => Parser_Lists);

   pragma Unreferenced (Analyzer,
                        LALR_Generators,
                        LALR_Parsers);

   pragma Style_Checks ("M150"); --  Readable table

   --  Grammar : constant Production_List.Instance :=
   --    Compilation_Unit             <= With_Clauses & Package_Specification & EOF and
   --    Compilation_Unit             <= Package_Specification & EOF and
   --    With_Clauses                 <= With_Clause and
   --    With_Clauses                 <= With_Clause & With_Clauses and
   --    With_Clause                  <= With_T & Package_Name_List & Semicolon_T and
   --    Package_Name_List            <= Package_Name and
   --    Package_Name_List            <= Package_Name & Comma_T & Package_Name_List and
   --    Package_Name                 <= Dotted_Identifier and
   --    Dotted_Identifier            <= Identifier_T and
   --    Dotted_Identifier            <= Identifier_T & Dot_T & Dotted_Identifier and
   --    Package_Specification        <= Package_T & Package_Name & Is_T & Procedure_Specification_List & End_T & Package_Name & Semicolon_T and
   --    Procedure_Specification_List <= Procedure_Specification and
   --    Procedure_Specification_List <= Procedure_Specification & Procedure_Specification_List and
   --    Procedure_Specification      <= Procedure_T & Identifier_T & Semicolon_T and
   --    Procedure_Specification      <= Procedure_T & Identifier_T & Left_Parenthesis_T & Parameter_List & Right_Parenthesis_T & Semicolon_T and
   --    Parameter_List               <= Formal_Parameter_Declaration and
   --    Parameter_List               <= Formal_Parameter_Declaration & Semicolon_T & Parameter_List and
   --    Formal_Parameter_Declaration <= Parameter_Name_List & Colon_T & In_T & Type_Name & Assignment_T & Default_Value and
   --    Formal_Parameter_Declaration <= Parameter_Name_List & Colon_T & In_T & Type_Name                                and
   --    Formal_Parameter_Declaration <= Parameter_Name_List & Colon_T &        Type_Name & Assignment_T & Default_Value and
   --    Formal_Parameter_Declaration <= Parameter_Name_List & Colon_T &        Type_Name                                and
   --    Parameter_Name_List          <= Identifier_T and
   --    Parameter_Name_List          <= Identifier_T & Comma_T & Parameter_Name_List and
   --    Type_Name                    <= Dotted_Identifier and
   --    Default_Value                <= String_Constant and
   --    Default_Value                <= Character_Constant and
   --    Default_Value                <= Numeric_Constant and
   --    Default_Value                <= Enumeration_Constant and
   --    Enumeration_Constant         <= Dotted_Identifier;

   pragma Style_Checks ("M79"); --  Standard line length

   procedure Something is null;
end Simple_Package_Syntax;
