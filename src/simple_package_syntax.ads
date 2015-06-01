with Ada.Strings.Maps.Constants,
     Ada.Text_IO;

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
     OpenToken.Text_Feeder.Text_IO,
     OpenToken.Token.Enumerated,
     OpenToken.Token.Enumerated.Analyzer,
     OpenToken.Token.Enumerated.Identifier,
     OpenToken.Token.Enumerated.List,
     OpenToken.Token.Enumerated.Nonterminal;

with Dotted_Identifier_Token;

package Simple_Package_Syntax is
   type Token_IDs is
     (
      Comment_T,
      Whitespace_T,
      --  Reserved words ARM 2.9 (2)
      Abort_T, Abs_T, Abstract_T, Accept_T, Access_T, Aliased_T, All_T, And_T,
      Array_T, At_T,
      Begin_T, Body_T,
      Case_T, Constant_T,
      Declare_T, Delay_T, Delta_T, Digits_T, Do_T,
      Else_T, Elsif_T, End_T, Entry_T, Exception_T, Exit_T,
      For_T, Function_T,
      Generic_T, Goto_T,
      If_T, In_T, Interface_T, Is_T,
      Limited_T, Loop_T,
      Mod_T,
      New_T, Not_T, Null_T,
      Of_T, Or_T, Others_T, Out_T, Overriding_T,
      Package_T, Pragma_T, Private_T, Procedure_T, Protected_T,
      Raise_T, Range_T, Record_T, Rem_T, Renames_T, Requeue_T, Return_T,
      Reverse_T,
      Select_T, Separate_T, Some_T, Subtype_T, Synchronized_T,
      Tagged_T, Task_T, Terminate_T, Then_T, Type_T,
      Until_T, Use_T,
      When_T, While_T, With_T,
      Xor_T,
      --  Delimiters ARM 2.2 (9)
      --  & ' ( ) * + , - . / : ; < = > |
      --  Compound delimiters ARM 2.2 (11)
      --  => .. ** := /= >= <= << >> <>
      Colon_T, Comma_T, Dot_T, Semicolon_T, Tick_T,         -- : , . ; '
      Left_Parenthesis_T, Right_Parenthesis_T,              -- ( )
      Concatenate_T,                                        -- &
      Alternative_T,                                        -- |
      Equal_T, Not_Equal_T, Greater_Than_T, Less_Than_T,    -- = /= > <
      Greater_Equal_T, Less_Equal_T,                        -- >= <=
      Plus_T, Minus_T, Times_T, Divide_T,                   -- + - * /
      Arrow_T, Assignment_T, Double_Dot_T, Exponentiate_T,  -- => := .. **
      Left_Label_Bracket_T, Right_Label_Bracket_T, Box_T,   -- << >> <>

      --  Literals ARM 2.4 .. 2.6
      Integer_T,               -- 1, 1E+10
      Based_Integer_T,         -- 13#C#, 13#C#E+10
      Real_T,                  -- -3.141, 1.0E+10
      Based_Real_T,            -- 13#C.B#, 13#C.B#E+5
      Character_T, String_T,
      --  Other tokens
      Identifier_T,
      --  Syntax error
      Bad_Token_T,
      --
      End_Of_File_T,

      --  non-terminals
      Compilation_Unit_T,
      With_Clauses_T,
      With_Clause_T,
      Package_Name_List_T,
      Package_Name_T,
      Dotted_Identifier_T,
      Package_Specification_T,
      Procedure_Specification_List_T,
      Procedure_Specification_T,
      Parameter_List_T,
      Formal_Parameter_Declaration_T,
      Parameter_Name_List_T,
      Type_Name_T,
      Default_Value_T,
      Enumeration_Constant_T,
      Numeric_Constant_T);

   package Master_Token is
     new OpenToken.Token.Enumerated (Token_ID       => Token_IDs,
                                     First_Terminal => Abort_T,
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
      End_Of_File_T         => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Assignment             : constant Master_Token.Class := Master_Token.Get (Assignment_T);
   Based_Integer_Constant : constant Master_Token.Class := Master_Token.Get (Based_Integer_T);
   Based_Real_Constant    : constant Master_Token.Class := Master_Token.Get (Based_Real_T);
   Character_Constant     : constant Master_Token.Class := Master_Token.Get (Character_T);
   Colon                  : constant Master_Token.Class := Master_Token.Get (Colon_T);
   Comma                  : constant Master_Token.Class := Master_Token.Get (Comma_T);
   Dot                    : constant Master_Token.Class := Master_Token.Get (Dot_T);
   EOF                    : constant Master_Token.Class := Master_Token.Get (End_Of_File_T);
   End_Keyword            : constant Master_Token.Class := Master_Token.Get (End_T);
   Identifier             : constant Identifiers.Instance'Class := Identifiers.Get (Identifier_T);
   In_Keyword             : constant Master_Token.Class := Master_Token.Get (In_T);
   Integer_Constant       : constant Master_Token.Class := Master_Token.Get (Integer_T);
   Is_Keyword             : constant Master_Token.Class := Master_Token.Get (Is_T);
   Left_Parenthesis       : constant Master_Token.Class := Master_Token.Get (Left_Parenthesis_T);
   Package_Keyword        : constant Master_Token.Class := Master_Token.Get (Package_T);
   Procedure_Keyword      : constant Master_Token.Class := Master_Token.Get (Procedure_T);
   Real_Constant          : constant Master_Token.Class := Master_Token.Get (Real_T);
   Right_Parenthesis      : constant Master_Token.Class := Master_Token.Get (Right_Parenthesis_T);
   Semicolon              : constant Master_Token.Class := Master_Token.Get (Semicolon_T);
   String_Constant        : constant Master_Token.Class := Master_Token.Get (String_T);
   With_Keyword           : constant Master_Token.Class := Master_Token.Get (With_T);

   Bad_Token                    : constant Nonterminal.Class := Nonterminal.Get (Bad_Token_T);
   Compilation_Unit             : constant Nonterminal.Class := Nonterminal.Get (Compilation_Unit_T);
   Default_Value                : constant Nonterminal.Class := Nonterminal.Get (Default_Value_T);
   Dotted_Identifier            : constant Dotted_Identifiers.Class := Dotted_Identifiers.Get (Dotted_Identifier_T);
   Enumeration_Constant         : constant Nonterminal.Class := Nonterminal.Get (Enumeration_Constant_T);
   Formal_Parameter_Declaration : constant Nonterminal.Class := Nonterminal.Get (Formal_Parameter_Declaration_T);
   Numeric_Constant             : constant Nonterminal.Class := Nonterminal.Get (Numeric_Constant_T);
   Package_Name                 : constant Nonterminal.Class := Nonterminal.Get (Package_Name_T);
   Package_Name_List            : constant Nonterminal.Class := Nonterminal.Get (Package_Name_List_T);
   Package_Specification        : constant Nonterminal.Class := Nonterminal.Get (Package_Specification_T);
   Parameter_List               : constant Nonterminal.Class := Nonterminal.Get (Parameter_List_T);
   Parameter_Name_List          : constant Nonterminal.Class := Nonterminal.Get (Parameter_Name_List_T);
   Procedure_Specification      : constant Nonterminal.Class := Nonterminal.Get (Procedure_Specification_T);
   Procedure_Specification_List : constant Nonterminal.Class := Nonterminal.Get (Procedure_Specification_List_T);
   Type_Name                    : constant Nonterminal.Class := Nonterminal.Get (Type_Name_T);
   With_Clause                  : constant Nonterminal.Class := Nonterminal.Get (With_Clause_T);
   With_Clauses                 : constant Nonterminal.Class := Nonterminal.Get (With_Clauses_T);

   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   Grammar : constant Production_List.Instance :=
     Compilation_Unit             <=                Package_Specification & EOF and
     Compilation_Unit             <= With_Clauses & Package_Specification & EOF and
     With_Clauses                 <= With_Clause                and
     With_Clauses                 <= With_Clause & With_Clauses and
     With_Clause                  <= With_Keyword & Package_Name_List & Semicolon and
     Package_Name_List            <= Package_Name                             and
     Package_Name_List            <= Package_Name & Comma & Package_Name_List and
     Package_Name                 <= Dotted_Identifier and
     Dotted_Identifier            <= Dotted_Identifier & Dot & Identifier + Dotted_Identifiers.Join_Dotted_Identifiers and
     Dotted_Identifier            <= Identifier and
     Package_Specification        <= Package_Keyword & Package_Name & Is_Keyword &
     Procedure_Specification_List &
     End_Keyword & Package_Name & Semicolon + Nonterminal.Synthesize_Self and
     Procedure_Specification_List <= Procedure_Specification                                + Nonterminal.Synthesize_Self and
     Procedure_Specification_List <= Procedure_Specification & Procedure_Specification_List + Nonterminal.Synthesize_Self and
     Procedure_Specification      <= Procedure_Keyword & Identifier &                                                         Semicolon + Nonterminal.Synthesize_Self and
     Procedure_Specification      <= Procedure_Keyword & Identifier & Left_Parenthesis & Parameter_List & Right_Parenthesis & Semicolon + Nonterminal.Synthesize_Self and
     Parameter_List               <= Formal_Parameter_Declaration                              + Nonterminal.Synthesize_Self and
     Parameter_List               <= Formal_Parameter_Declaration & Semicolon & Parameter_List + Nonterminal.Synthesize_Self and
     Formal_Parameter_Declaration <= Parameter_Name_List & Colon & In_Keyword & Type_Name & Assignment & Default_Value and
     Formal_Parameter_Declaration <= Parameter_Name_List & Colon & In_Keyword & Type_Name                              and
     Formal_Parameter_Declaration <= Parameter_Name_List & Colon &              Type_Name & Assignment & Default_Value and
     Formal_Parameter_Declaration <= Parameter_Name_List & Colon &              Type_Name                              and
     Parameter_Name_List          <= Identifier                               + Nonterminal.Synthesize_Self and
     Parameter_Name_List          <= Identifier & Comma & Parameter_Name_List + Nonterminal.Synthesize_Self and
     Type_Name                    <= Dotted_Identifier and
     Default_Value                <= String_Constant and
     Default_Value                <= Character_Constant and
     Default_Value                <= Numeric_Constant and
     Default_Value                <= Enumeration_Constant and
     Enumeration_Constant         <= Dotted_Identifier and
     Numeric_Constant             <= Integer_Constant and
     Numeric_Constant             <= Based_Integer_Constant and
     Numeric_Constant             <= Real_Constant and
     Numeric_Constant             <= Based_Real_Constant;

   pragma Style_Checks ("M79"); --  Standard line length

   Feeder : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Ada.Text_IO.Standard_Input);

   Analyzer : constant Tokenizer.Handle :=
                Tokenizer.Initialize (Syntax,
                                      Feeder'Access);
end Simple_Package_Syntax;
