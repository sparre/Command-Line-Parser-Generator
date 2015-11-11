with Ada.Characters.Conversions,
     Ada.Characters.Handling,
     Ada.Containers,
     Ada.Directories,
     Ada.Strings.Unbounded;

with Command_Line_Parser_Generator.Argument_Description_Map,
     Command_Line_Parser_Generator.Argument_Zsh_Pattern_Map,
     Command_Line_Parser_Generator.Formal_Parameter,
     Command_Line_Parser_Generator.Formal_Parameter_List,
     Command_Line_Parser_Generator.Identifier_Matrix,
     Command_Line_Parser_Generator.Identifier_Set,
     Command_Line_Parser_Generator.Utilities,
     Command_Line_Parser_Generator.Zsh_Argument_Pattern;

package body Command_Line_Parser_Generator.Templates is
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String) return Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (Item : in Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Target_Directory : Unbounded_String := +"/tmp/";

   function To_File_Name (Item : in Wide_String) return String;

   procedure Argument_List (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser.Argument_List",
                            File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with Ada.Containers.Indefinite_Hashed_Maps,");
      Put_Line (File => Target, Item => "     Ada.Strings.Fixed.Equal_Case_Insensitive,");
      Put_Line (File => Target, Item => "     Ada.Strings.Fixed.Hash_Case_Insensitive;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser.Argument;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "private");
      Put_Line (File => Target, Item => "package " & Package_Name & ".Command_Line_Parser.Argument_List is");
      Put_Line (File => Target, Item => "   package Maps is");
      Put_Line (File => Target, Item => "      new Ada.Containers.Indefinite_Hashed_Maps");
      Put_Line (File => Target, Item => "            (Element_Type    => String,");
      Put_Line (File => Target, Item => "             Key_Type        => String,");
      Put_Line (File => Target, Item => "             Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,");
      Put_Line (File => Target, Item => "             Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive);");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   type Instance is new Maps.Map with null record;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Image (Container : in     Instance) return String;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Insert (Container : in out Instance;");
      Put_Line (File => Target, Item => "                     New_Item  : in     Argument.Instance);");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Argument_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser.Argument_List",
                   File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with Ada.Strings.Unbounded;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "package body " & Package_Name & ".Command_Line_Parser.Argument_List is");
      Put_Line (File => Target, Item => "   function Image (Container : in     Instance) return String is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Unbounded;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      Buffer : Unbounded_String;");
      Put_Line (File => Target, Item => "      Index  : Maps.Cursor := Container.First;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      use type Maps.Cursor;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      while Index /= Maps.No_Element loop");
      Put_Line (File => Target, Item => "         Append");
      Put_Line (File => Target, Item => "           (Source   => Buffer,");
      Put_Line (File => Target, Item => "            New_Item => "" "" & Argument.Compose");
      Put_Line (File => Target, Item => "                                (Key   => Maps.Key (Index),");
      Put_Line (File => Target, Item => "                                 Value => Maps.Element (Index)).Image);");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "         Maps.Next (Index);");
      Put_Line (File => Target, Item => "      end loop;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      return To_String (Buffer);");
      Put_Line (File => Target, Item => "   end Image;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Insert (Container : in out Instance;");
      Put_Line (File => Target, Item => "                     New_Item  : in     Argument.Instance) is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Unbounded;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      Container.Insert (Key      => To_String (New_Item.Key),");
      Put_Line (File => Target, Item => "                        New_Item => To_String (New_Item.Value));");
      Put_Line (File => Target, Item => "   end Insert;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Argument_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Argument_List;

   procedure Argument_Type (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser.Argument",
                            File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with Ada.Containers,");
      Put_Line (File => Target, Item => "     Ada.Strings.Unbounded;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "private");
      Put_Line (File => Target, Item => "package " & Package_Name & ".Command_Line_Parser.Argument is");
      Put_Line (File => Target, Item => "   type Instance is tagged");
      Put_Line (File => Target, Item => "      record");
      Put_Line (File => Target, Item => "         Key   : Ada.Strings.Unbounded.Unbounded_String;");
      Put_Line (File => Target, Item => "         Value : Ada.Strings.Unbounded.Unbounded_String;");
      Put_Line (File => Target, Item => "      end record;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   overriding");
      Put_Line (File => Target, Item => "   function ""="" (Left, Right : in Instance) return Boolean;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Compose (Key   : in String;");
      Put_Line (File => Target, Item => "                     Value : in String) return Instance;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Equal_Keys (Left, Right : in Instance) return Boolean;");
      Put_Line (File => Target, Item => "   function Key_Hash (Item : in Instance) return Ada.Containers.Hash_Type;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Image (Item : in Instance) return String;");
      Put_Line (File => Target, Item => "   function Value (Item : in String) return Instance;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Argument;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser.Argument",
                   File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with Ada.Strings.Fixed,");
      Put_Line (File => Target, Item => "     Ada.Strings.Unbounded.Equal_Case_Insensitive,");
      Put_Line (File => Target, Item => "     Ada.Strings.Unbounded.Hash_Case_Insensitive;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "package body " & Package_Name & ".Command_Line_Parser.Argument is");
      Put_Line (File => Target, Item => "   function ""+"" (Item : in String)");
      Put_Line (File => Target, Item => "                return Ada.Strings.Unbounded.Unbounded_String");
      Put_Line (File => Target, Item => "     renames Ada.Strings.Unbounded.To_Unbounded_String;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   overriding");
      Put_Line (File => Target, Item => "   function ""="" (Left, Right : in Instance) return Boolean is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Unbounded;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Equal_Keys (Left, Right) and Left.Value = Right.Value;");
      Put_Line (File => Target, Item => "   end ""="";");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Compose (Key   : in String;");
      Put_Line (File => Target, Item => "                     Value : in String) return Instance is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return (Key   => +Key,");
      Put_Line (File => Target, Item => "              Value => +Value);");
      Put_Line (File => Target, Item => "   end Compose;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Equal_Keys (Left, Right : in Instance) return Boolean is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Unbounded;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Equal_Case_Insensitive (Left.Key, Right.Key);");
      Put_Line (File => Target, Item => "   end Equal_Keys;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Image (Item : in Instance) return String is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Unbounded;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return ""--"" & To_String (Item.Key) & ""="" & To_String (Item.Value);");
      Put_Line (File => Target, Item => "   end Image;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Key_Hash (Item : in Instance) return Ada.Containers.Hash_Type is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Ada.Strings.Unbounded.Hash_Case_Insensitive (Item.Key);");
      Put_Line (File => Target, Item => "   end Key_Hash;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Value (Item : in String) return Instance is");
      Put_Line (File => Target, Item => "      use Ada.Strings.Fixed;");
      Put_Line (File => Target, Item => "      Key_Value_Separator : Natural;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      if Head (Item, 2) = ""--"" then");
      Put_Line (File => Target, Item => "         Key_Value_Separator := Index (Item, ""="");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "         if Key_Value_Separator = 0 then");
      Put_Line (File => Target, Item => "            return (Key   => +Item (Item'First + 2 .. Item'Last),");
      Put_Line (File => Target, Item => "                    Value => +""True"");");
      Put_Line (File => Target, Item => "         else");
      Put_Line (File => Target, Item => "            return (Key   => +Item (Item'First + 2 .. Key_Value_Separator - 1),");
      Put_Line (File => Target, Item => "                    Value => +Item (Key_Value_Separator + 1 .. Item'Last));");
      Put_Line (File => Target, Item => "         end if;");
      Put_Line (File => Target, Item => "      else");
      Put_Line (File => Target, Item => "         raise Constraint_Error");
      Put_Line (File => Target, Item => "           with ""Incorrect argument format.  Expected: --<key>=<value>"";");
      Put_Line (File => Target, Item => "      end if;");
      Put_Line (File => Target, Item => "   end Value;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Argument;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Argument_Type;

   procedure Call (Target  : in     Ada.Wide_Text_IO.File_Type;
                   Profile : in     Procedure_Declaration.Instance;
                   Prefix  : in     Wide_String;
                   First   : in     Boolean) is
   begin
      if Profile in Simple_Procedure then
         Simple_Call (Target  => Target,
                      Profile => Profile,
                      Prefix  => Prefix,
                      First   => First);
      else
         declare
            Available       : Procedure_Declaration.Instance := Profile;
            Unavailable     : Procedure_Declaration.Instance := Profile;
            Parameter_Index : Positive;
            --  As Profile isn't a Simple_Procedure, at least one
            --  formal parameter has a default value. => We don't have
            --  to worry that Parameter_Index will not be set in the loop.
         begin
            for Index in Profile.Formal_Parameters.First_Index .. Profile.Formal_Parameters.Last_Index loop
               if Profile.Formal_Parameters.Element (Index).Has_Default_Value then
                  Parameter_Index := Index;
                  exit;
               end if;
            end loop;

            Available.Formal_Parameters.Reference (Parameter_Index).Default_Value := +"";
            Unavailable.Formal_Parameters.Delete (Parameter_Index);

            Call (Target  => Target,
                  Profile => Available,
                  Prefix  => Prefix,
                  First   => First);
            Call (Target  => Target,
                  Profile => Unavailable,
                  Prefix  => Prefix,
                  First   => False);
         end;
      end if;
   end Call;

   procedure Create (Target_Directory : in String) is
      use Ada.Directories;
   begin
      Create_Path (Target_Directory);
      Templates.Target_Directory := +Full_Name (Target_Directory);
   end Create;

   procedure Create_Body (Name : in     Wide_String;
                          File : in out Ada.Wide_Text_IO.File_Type) is
      use Ada.Wide_Text_IO;
   begin
      Create (File => File,
              Name => +Target_Directory & "/" & To_File_Name (Name) & ".adb",
              Mode => Out_File);
   end Create_Body;

   procedure Create_Specification (Name : in     Wide_String;
                                   File : in out Ada.Wide_Text_IO.File_Type) is
      use Ada.Wide_Text_IO;
   begin
      Create (File => File,
              Name => +Target_Directory & "/" & To_File_Name (Name) & ".ads",
              Mode => Out_File);
   end Create_Specification;

   procedure Key_List (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser.Key_List",
                            File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with Ada.Containers.Indefinite_Hashed_Sets,");
      Put_Line (File => Target, Item => "     Ada.Strings.Fixed.Equal_Case_Insensitive,");
      Put_Line (File => Target, Item => "     Ada.Strings.Fixed.Hash_Case_Insensitive;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser.Argument_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "private");
      Put_Line (File => Target, Item => "package " & Package_Name & ".Command_Line_Parser.Key_List is");
      Put_Line (File => Target, Item => "   package Sets is");
      Put_Line (File => Target, Item => "      new Ada.Containers.Indefinite_Hashed_Sets");
      Put_Line (File => Target, Item => "            (Element_Type        => String,");
      Put_Line (File => Target, Item => "             Hash                => Ada.Strings.Fixed.Hash_Case_Insensitive,");
      Put_Line (File => Target, Item => "             Equivalent_Elements => Ada.Strings.Fixed.Equal_Case_Insensitive);");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   type Instance is new Sets.Set with null record;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function ""+"" (Left  : in     Instance;");
      Put_Line (File => Target, Item => "                 Right : in     String) return Instance;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function ""+"" (Right : in     String) return Instance;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function ""="" (Left  : in     Argument_List.Instance;");
      Put_Line (File => Target, Item => "                 Right : in     Instance) return Boolean;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Key_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser.Key_List",
                   File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "package body " & Package_Name & ".Command_Line_Parser.Key_List is");
      Put_Line (File => Target, Item => "   function ""+"" (Left  : in     Instance;");
      Put_Line (File => Target, Item => "                 Right : in     String) return Instance is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Result : Instance := Left do");
      Put_Line (File => Target, Item => "         Result.Insert (Right);");
      Put_Line (File => Target, Item => "      end return;");
      Put_Line (File => Target, Item => "   end ""+"";");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function ""+"" (Right : in     String) return Instance is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Result : Instance do");
      Put_Line (File => Target, Item => "         Result.Insert (Right);");
      Put_Line (File => Target, Item => "      end return;");
      Put_Line (File => Target, Item => "   end ""+"";");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function ""="" (Left  : in     Argument_List.Instance;");
      Put_Line (File => Target, Item => "                 Right : in     Instance) return Boolean is");
      Put_Line (File => Target, Item => "      use type Ada.Containers.Count_Type;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      for Key of Right loop");
      Put_Line (File => Target, Item => "         if not Left.Contains (Key) then");
      Put_Line (File => Target, Item => "            return False;");
      Put_Line (File => Target, Item => "         end if;");
      Put_Line (File => Target, Item => "      end loop;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      return Left.Length = Right.Length;");
      Put_Line (File => Target, Item => "   end ""="";");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Key_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Key_List;

   procedure Parser (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser",
                            File => Target);
      Put_Line (File => Target, Item => "package " & Package_Name & ".Command_Line_Parser is");
      Put_Line (File => Target, Item => "   function Initialised return Boolean;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   pragma Warnings (Off);");
      Put_Line (File => Target, Item => "   --  GNAT 4.9.2 claims Initialised may be called before body is seen.");
      Put_Line (File => Target, Item => "   procedure Initialise");
      Put_Line (File => Target, Item => "     with Pre  => not Initialised,");
      Put_Line (File => Target, Item => "          Post => Initialised;");
      Put_Line (File => Target, Item => "   pragma Warnings (On);");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Count_Matching_Call_Profiles return Natural");
      Put_Line (File => Target, Item => "     with Pre => Initialised;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Call_Matching_Profile");
      Put_Line (File => Target, Item => "     with Pre => Initialised;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   package Errors is");
      Put_Line (File => Target, Item => "      procedure No_Matching_Call_Profile");
      Put_Line (File => Target, Item => "        with Pre => Count_Matching_Call_Profiles = 0;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      procedure More_Than_One_Matching_Call_Profile");
      Put_Line (File => Target, Item => "        with Pre => Count_Matching_Call_Profiles > 1;");
      Put_Line (File => Target, Item => "   end Errors;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser;");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser",
                   File => Target);
      Put_Line (File => Target, Item => "with Ada.Command_Line,");
      Put_Line (File => Target, Item => "     Ada.Text_IO;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser.Argument,");
      Put_Line (File => Target, Item => "     " & Package_Name & ".Command_Line_Parser.Argument_List,");
      Put_Line (File => Target, Item => "     " & Package_Name & ".Command_Line_Parser.Profiles;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "package body " & Package_Name & ".Command_Line_Parser is");
      Put_Line (File => Target, Item => "   Is_Initialised : Boolean := False;");
      Put_Line (File => Target, Item => "   Arguments      : Argument_List.Instance;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   Profile_Found : Boolean := False;");
      Put_Line (File => Target, Item => "   Profile_Index : Profiles.Index;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Call_Matching_Profile is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      if Profile_Found then");
      Put_Line (File => Target, Item => "         Profiles.Call (Profile   => Profile_Index,");
      Put_Line (File => Target, Item => "                        Arguments => Arguments);");
      Put_Line (File => Target, Item => "      else");
      Put_Line (File => Target, Item => "         raise Program_Error");
      Put_Line (File => Target, Item => "           with ""Call_Matching_Profile called without a valid profile."";");
      Put_Line (File => Target, Item => "      end if;");
      Put_Line (File => Target, Item => "   end Call_Matching_Profile;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Count_Matching_Call_Profiles return Natural is");
      Put_Line (File => Target, Item => "      Counter : Natural := 0;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      for Index in Profiles.Index loop");
      Put_Line (File => Target, Item => "         if Profiles.Match (Index, Arguments) then");
      Put_Line (File => Target, Item => "            Counter := Counter + 1;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "            Profile_Index := Index;");
      Put_Line (File => Target, Item => "            Profile_Found := True;");
      Put_Line (File => Target, Item => "         end if;");
      Put_Line (File => Target, Item => "      end loop;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      return Counter;");
      Put_Line (File => Target, Item => "   end Count_Matching_Call_Profiles;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Initialise is");
      Put_Line (File => Target, Item => "      use Ada;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      for Index in 1 .. Command_Line.Argument_Count loop");
      Put_Line (File => Target, Item => "         exit when Command_Line.Argument (Index) = ""--"";");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "         Arguments.Insert (Command_Line_Parser.Argument.Value");
      Put_Line (File => Target, Item => "                             (Command_Line.Argument (Index)));");
      Put_Line (File => Target, Item => "      end loop;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      Is_Initialised := True;");
      Put_Line (File => Target, Item => "   end Initialise;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Initialised return Boolean is");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      return Is_Initialised;");
      Put_Line (File => Target, Item => "   end Initialised;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   package body Errors is");
      Put_Line (File => Target, Item => "      procedure More_Than_One_Matching_Call_Profile is");
      Put_Line (File => Target, Item => "         use Ada.Command_Line, Ada.Text_IO;");
      Put_Line (File => Target, Item => "      begin");
      Put_Line (File => Target, Item => "         Put_Line (File => Standard_Error,");
      Put_Line (File => Target, Item => "                   Item => ""Ambigous command line arguments."");");
      Put_Line (File => Target, Item => "         Set_Exit_Status (Failure);");
      Put_Line (File => Target, Item => "      end More_Than_One_Matching_Call_Profile;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "      procedure No_Matching_Call_Profile is");
      Put_Line (File => Target, Item => "         use Ada.Command_Line, Ada.Text_IO;");
      Put_Line (File => Target, Item => "      begin");
      Put_Line (File => Target, Item => "         Put_Line (File => Standard_Error,");
      Put_Line (File => Target, Item => "                   Item => ""Missing or too many command line arguments."");");
      Put_Line (File => Target, Item => "         Set_Exit_Status (Failure);");
      Put_Line (File => Target, Item => "      end No_Matching_Call_Profile;");
      Put_Line (File => Target, Item => "   end Errors;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser;");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Parser;

   procedure Profiles
     (Package_Name       : in     Wide_String;
      Procedures         : in     Procedure_Declaration_List.Instance;
      External_Show_Help : in     Boolean)
   is
      use Ada.Wide_Text_IO;
      use type Ada.Containers.Count_Type;

      List   : Procedure_Declaration_List.Instance := Procedures;
      Target : File_Type;
   begin
      if External_Show_Help then
         List.Append (Show_Help);
      end if;

      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser.Profiles",
                            File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser.Argument_List;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "private");
      Put_Line (File => Target, Item => "package " & Package_Name & ".Command_Line_Parser.Profiles is");
      Put_Line (File => Target, Item => "   type Index is range 1 .." & Ada.Containers.Count_Type'Wide_Image (List.Length) & ";");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Match (Profile   : in     Index;");
      Put_Line (File => Target, Item => "                   Arguments : in     Argument_List.Instance) return Boolean;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   procedure Call (Profile   : in     Index;");
      Put_Line (File => Target, Item => "                   Arguments : in     Argument_List.Instance);");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Profiles;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser.Profiles",
                   File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M0"");");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser.Key_List;");

      if External_Show_Help then
         Put_Line (File => Target, Item => "with " & Package_Name & ".Show_Help;");
      end if;

      New_Line (File => Target);
      Put_Line (File => Target, Item => "package body " & Package_Name & ".Command_Line_Parser.Profiles is");
      Put_Line (File => Target, Item => "   procedure Call (Profile   : in     Index;");
      Put_Line (File => Target, Item => "                   Arguments : in     Argument_List.Instance) is");
      Put_Line (File => Target, Item => "      use all type Key_List.Instance;");
      Put_Line (File => Target, Item => "   begin");
      Put_Line (File => Target, Item => "      case Profile is");
      for Index in List.First_Index .. List.Last_Index loop
         Put_Line (File => Target, Item => "         when" & Positive'Wide_Image (Index) & " =>");

         declare
            Profile : Procedure_Declaration.Instance renames List.Element (Index);
         begin
            Call
              (Target      => Target,
               Profile     => (Name              => +(Package_Name & "." & (+Profile.Name)),
                               Formal_Parameters => Profile.Formal_Parameters),
               Prefix      => "            ",
               First       => True);
         end;

         Put_Line (File => Target, Item => "            else");
         Put_Line (File => Target, Item => "               raise Program_Error");
         Put_Line (File => Target, Item => "                 with ""Profiles.Call called with invalid arguments."";");
         Put_Line (File => Target, Item => "            end if;");
      end loop;
      Put_Line (File => Target, Item => "      end case;");
      Put_Line (File => Target, Item => "   end Call;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   function Match (Profile   : in     Index;");
      Put_Line (File => Target, Item => "                   Arguments : in     Argument_List.Instance) return Boolean is");

      if List.Length = 1 and then List.Element (1).Formal_Parameters.Is_Empty then
         Put_Line (File => Target, Item => "      pragma Unreferenced (Profile);");
         Put_Line (File => Target, Item => "   begin");
         Put_Line (File => Target, Item => "      return Arguments.Is_Empty;");
      else
         Put_Line (File => Target, Item => "      Buffer : Argument_List.Instance := Arguments;");
         Put_Line (File => Target, Item => "   begin");
         Put_Line (File => Target, Item => "      case Profile is");
         for Index in List.First_Index .. List.Last_Index loop
            Put_Line (File => Target, Item => "         when" & Positive'Wide_Image (Index) & " =>");
            declare
               Profile        : Procedure_Declaration.Instance renames List.Element (Index);
               Has_Statements : Boolean := False;
            begin
               for Parameter of Profile.Formal_Parameters loop
                  if Has_Statements then
                     New_Line (File => Target);
                  end if;

                  Put_Line (File => Target, Item => "            if Buffer.Contains (Key => """ & (+Parameter.Name) & """) then");

                  if +Parameter.Type_Name = "Standard.String" then
                     null;
                  elsif +Parameter.Type_Name = "Standard.Character" then
                     Put_Line (File => Target, Item => "               if Buffer.Element (Key => """ & (+Parameter.Name) & """)'Length /= 1 then");
                     Put_Line (File => Target, Item => "                  return False;");
                     Put_Line (File => Target, Item => "               end if;");
                     New_Line (File => Target);
                  else
                     Put_Line (File => Target, Item => "               begin");
                     Put_Line (File => Target, Item => "                  declare");
                     Put_Line (File => Target, Item => "                     Dummy : constant " & (+Parameter.Type_Name));
                     Put_Line (File => Target, Item => "                       := " & (+Parameter.Value_Function) & " (Buffer.Element (Key => """ & (+Parameter.Name) & """));");
                     Put_Line (File => Target, Item => "                  begin");
                     Put_Line (File => Target, Item => "                     null;");
                     Put_Line (File => Target, Item => "                  end;");
                     Put_Line (File => Target, Item => "               exception");
                     Put_Line (File => Target, Item => "                  when Constraint_Error =>");
                     Put_Line (File => Target, Item => "                     return False;");
                     Put_Line (File => Target, Item => "               end;");
                     New_Line (File => Target);
                  end if;

                  Put_Line (File => Target, Item => "               Buffer.Delete (Key => """ & (+Parameter.Name) & """);");

                  if not Parameter.Has_Default_Value then
                     Put_Line (File => Target, Item => "            else");
                     Put_Line (File => Target, Item => "               return False;");
                  end if;

                  Put_Line (File => Target, Item => "            end if;");

                  Has_Statements := True;
               end loop;

               if not Has_Statements then
                  Put_Line (File => Target, Item => "            null;");
               end if;
            end;
         end loop;
         Put_Line (File => Target, Item => "      end case;");
         New_Line (File => Target);
         Put_Line (File => Target, Item => "      return Buffer.Is_Empty;");
      end if;
      Put_Line (File => Target, Item => "   end Match;");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Command_Line_Parser.Profiles;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "pragma Style_Checks (""-M79"");");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Profiles;

   procedure Put_Help
     (Package_Name       : in     Wide_String;
      Procedures         : in     Procedure_Declaration_List.Instance;
      External_Show_Help : in     Boolean)
   is
      use Ada.Wide_Text_IO;

      List   : Procedure_Declaration_List.Instance := Procedures;
      Target : File_Type;
   begin
      if External_Show_Help then
         List.Append (Show_Help);
      end if;

      pragma Style_Checks ("-M160");

      Create_Specification (Name => Package_Name & ".Put_Help",
                            File => Target);
      Put_Line (File => Target, Item => "with Ada.Command_Line;");
      Put_Line (File => Target, Item => "with Ada.Text_IO;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Put_Help (File : in     Ada.Text_IO.File_Type);");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Put_Help",
                   File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Put_Help (File : in     Ada.Text_IO.File_Type) is");
      Put_Line (File => Target, Item => "   use Ada.Command_Line, Ada.Text_IO;");
      Put_Line (File => Target, Item => "begin");
      Put_Line (File => Target, Item => "   Put_Line (File, ""Help for " & Package_Name & ":"");");
      Put_Line (File => Target, Item => "   New_Line (File);");

      for Profile of List loop
         Put_Line (File => Target, Item => "   Put_Line (File, ""   " & (+Profile.Name) & ":"");");
         Put_Line (File => Target, Item => "   New_Line (File);");

         if Profile.Formal_Parameters.Is_Empty then
            Put_Line (File => Target, Item => "   Put_Line (File, ""      "" & Command_Name);");
         else
            Put_Line (File => Target, Item => "   Put_Line (File, ""      "" & Command_Name & "" [arguments]"");");
            Put_Line (File => Target, Item => "   New_Line (File);");
         end if;

         for Parameter of Profile.Formal_Parameters loop
            Put      (File => Target, Item => "   Put_Line (File, ""         --" & (+Parameter.Name) & "=<" & (+Parameter.Type_Name) & ">");

            if +Parameter.Default_Value = "" then
               Put_Line (File => Target, Item => " - required"");");
            else
               Put_Line (File => Target, Item => " - optional, default = " & Utilities.To_Ada_Source (Parameter.Default_Value) & """);");
            end if;
         end loop;

         Put_Line (File => Target, Item => "   New_Line (File);");
      end loop;

      Put_Line (File => Target, Item => "end " & Package_Name & ".Put_Help;");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Put_Help;

   procedure Runner (Package_Name      : in     Wide_String;
                     External_Put_Help : in     Boolean) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Driver",
                            File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Driver;");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Driver",
                   File => Target);
      Put_Line (File => Target, Item => "with Ada.Command_Line;");
      Put_Line (File => Target, Item => "with Ada.Text_IO;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name & ".Command_Line_Parser;");

      if External_Put_Help then
         Put_Line (File => Target, Item => "with " & Package_Name & ".Put_Help;");
      end if;

      New_Line (File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Driver is");
      Put_Line (File => Target, Item => "   use Command_Line_Parser;");
      Put_Line (File => Target, Item => "begin");
      Put_Line (File => Target, Item => "   Initialise;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "   case Count_Matching_Call_Profiles is");
      Put_Line (File => Target, Item => "      when 0 =>");
      Put_Line (File => Target, Item => "         Errors.No_Matching_Call_Profile;");
      Put_Line (File => Target, Item => "      when 1 =>");
      Put_Line (File => Target, Item => "         Call_Matching_Profile;");
      Put_Line (File => Target, Item => "      when others =>");
      Put_Line (File => Target, Item => "         Errors.More_Than_One_Matching_Call_Profile;");
      Put_Line (File => Target, Item => "   end case;");
      Put_Line (File => Target, Item => "exception");
      Put_Line (File => Target, Item => "   when others =>");
      Put_Line (File => Target, Item => "      " & Package_Name & ".Put_Help (File => Ada.Text_IO.Standard_Error);");
      Put_Line (File => Target, Item => "      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Driver;");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Runner;

   procedure Set (Target_Directory : in String) is
      use Ada.Directories;
   begin
      Templates.Target_Directory := +Full_Name (Target_Directory);
   end Set;

   procedure Show_Help (Package_Name      : in     Wide_String;
                        External_Put_Help : in     Boolean)
   is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Show_Help",
                            File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Show_Help (Help : in     Boolean := False);");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Show_Help",
                   File => Target);
      Put_Line (File => Target, Item => "with Ada.Text_IO;");
      New_Line (File => Target);

      if External_Put_Help then
         Put_Line (File => Target, Item => "with " & Package_Name & ".Put_Help;");
         New_Line (File => Target);
      end if;

      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Show_Help (Help : in     Boolean := False) is");
      Put_Line (File => Target, Item => "begin");
      Put_Line (File => Target, Item => "   " & Package_Name &".Put_Help (File => Ada.Text_IO.Standard_Output);");
      Put_Line (File => Target, Item => "end " & Package_Name & ".Show_Help;");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Show_Help;

   function Show_Help return Procedure_Declaration.Instance
   is
      use all type Formal_Parameter_List.Instance;
      use Zsh_Argument_Pattern;
   begin
      return (Name              => +"Show_Help",
              Formal_Parameters =>
                To_Vector ((Name           => +"Help",
                            Image_Function => +"Standard.Boolean'Image",
                            Value_Function => +"Standard.Boolean'Value",
                            Default_Value  => +"False",
                            Type_Name      => +"Standard.Boolean",
                            Zsh_Pattern    => Create (Flag)),
                           1));
   end Show_Help;

   procedure Simple_Call (Target  : in     Ada.Wide_Text_IO.File_Type;
                          Profile : in     Simple_Procedure;
                          Prefix  : in     Wide_String;
                          First   : in     Boolean) is
      use Ada.Wide_Text_IO;
   begin
      if First then
         Put      (File => Target, Item => Prefix & "if ");
      else
         Put      (File => Target, Item => Prefix & "elsif ");
      end if;

      if Profile.Formal_Parameters.Is_Empty then
         Put_Line (File => Target, Item => "Arguments.Is_Empty then");

         Put_Line (File => Target, Item => Prefix & "   " & (+Profile.Name) & ";");
      else
         Put      (File => Target, Item => "Arguments = (");
         for Index in Profile.Formal_Parameters.First_Index .. Profile.Formal_Parameters.Last_Index loop
            declare
               Is_First  : constant Boolean := Index = Profile.Formal_Parameters.First_Index;
               Parameter : Formal_Parameter.Instance renames Profile.Formal_Parameters.Element (Index);
            begin
               if Is_First then
                  Put      (File => Target, Item => "+""" & (+Parameter.Name) & """");
               else
                  Put      (File => Target, Item => " + """ & (+Parameter.Name) & """");
               end if;
            end;
         end loop;
         Put_Line (File => Target, Item => ") then");

         Put_Line (File => Target, Item => Prefix & "   " & (+Profile.Name));

         for Index in Profile.Formal_Parameters.First_Index .. Profile.Formal_Parameters.Last_Index loop
            declare
               Is_First  : constant Boolean := Index = Profile.Formal_Parameters.First_Index;
               Is_Last   : constant Boolean := Index = Profile.Formal_Parameters.Last_Index;
               Parameter : Formal_Parameter.Instance renames Profile.Formal_Parameters.Element (Index);
            begin
               if Is_First then
                  Put      (File => Target, Item => Prefix & "     (");
               else
                  Put      (File => Target, Item => Prefix & "      ");
               end if;

               Put      (File => Target, Item => (+Parameter.Name) & " => ");

               if +Parameter.Type_Name = "Standard.String" then
                  Put      (File => Target, Item => "Arguments.Element (""" & (+Parameter.Name) & """)");
               elsif +Parameter.Type_Name = "Standard.Character" then
                  Put      (File => Target, Item => "Arguments.Element (""" & (+Parameter.Name) & """) (1)");
               else
                  Put      (File => Target, Item => (+Parameter.Value_Function) & " (Arguments.Element (""" & (+Parameter.Name) & """))");
               end if;

               if Is_Last then
                  Put_Line (File => Target, Item => ");");
               else
                  Put_Line (File => Target, Item => ",");
               end if;
            end;
         end loop;
      end if;
   end Simple_Call;

   function To_File_Name (Item : in Wide_String) return String is
      use Ada.Characters, Ada.Characters.Handling;

      Result : String := To_Lower (Conversions.To_String (Item));
   begin
      for C of Result loop
         if C = '.' then
            C := '-';
         end if;
      end loop;

      return Result;
   end To_File_Name;

   procedure Zsh_Command_Completion
     (Package_Name : in     Wide_String;
      Procedures   : in     Procedure_Declaration_List.Instance) is

      use Ada.Characters.Conversions, Ada.Wide_Text_IO;
      use type Source_Text;

      Coexisting_Arguments : Identifier_Matrix.Instance;
      All_Arguments        : Identifier_Set.Instance;
      Patterns             : Argument_Zsh_Pattern_Map.Instance;
      Descriptions         : Argument_Description_Map.Instance;
      Type_Descriptions    : Argument_Description_Map.Instance;
      Target               : File_Type;
   begin
      for Profile of Procedures loop
         for Parameter of Profile.Formal_Parameters loop
            All_Arguments.Append (Value => Parameter.Name);
            Patterns.Append (Key     => Parameter.Name,
                             Pattern => Parameter.Zsh_Pattern);
            Descriptions.Append (Key   => Parameter.Name,
                                 Value => Profile.Name & " -> " & Parameter.Name);
            Type_Descriptions.Append (Key   => Parameter.Name,
                                      Value => Parameter.Type_Name);

            for Coparameter of Profile.Formal_Parameters loop
               Coexisting_Arguments.Append (Key   => Parameter.Name,
                                            Value => Coparameter.Name);
            end loop;
         end loop;
      end loop;

      pragma Style_Checks ("-M160");

      Create (File => Target,
              Name => (+Target_Directory) & "/_" & To_File_Name (Package_Name) & "-driver",
              Mode => Out_File);

      Put_Line (File => Target, Item => "#compdef " & To_Wide_String (To_File_Name (Package_Name)) & "-driver");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "local arguments");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "arguments=(");

      for Argument of All_Arguments loop
         declare
            use Zsh_Argument_Pattern;
            use type Identifier_Set.Instance;

            Excluded_Arguments : constant Identifier_Set.Instance :=
              All_Arguments - Coexisting_Arguments.Element (Argument);
         begin
            Put      (File => Target, Item => "  '");

            if not Excluded_Arguments.Is_Empty then
               Put (File => Target, Item => "(");
               for Excluded of Excluded_Arguments loop
                  Put (File => Target, Item => "--" & (+Excluded) & " ");
               end loop;
               Put (File => Target, Item => ")");
            end if;

            if Patterns.Element (Argument).Kind = Flag then
               Put      (File => Target, Item => "--" & (+Argument));
               Put      (File => Target, Item => "[" & Descriptions.Image (Argument) & "]");
            else
               Put      (File => Target, Item => "--" & (+Argument));
               Put      (File => Target, Item => "=[" & Descriptions.Image (Argument) & "]:");
               Put      (File => Target, Item => Type_Descriptions.Image (Argument) & ":");
               Put      (File => Target, Item => Patterns.Element (Argument).Image);
            end if;

            Put_Line (File => Target, Item => "'");
         end;
      end loop;

      Put_Line (File => Target, Item => ")");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "_arguments -s $arguments");

      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Zsh_Command_Completion;
end Command_Line_Parser_Generator.Templates;
