with Ada.Characters.Conversions,
     Ada.Characters.Handling,
     Ada.Directories,
     Ada.Strings.Unbounded;

package body Command_Line_Parser_Generator.Templates is
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String) return Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (Item : in Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Target_Directory : Unbounded_String := +"/tmp/";

   function To_File_Name (Item : in Wide_String) return String;

   procedure Argument_Type (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Command_Line_Parser.Argument",
                            File => Target);
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
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Command_Line_Parser.Argument",
                   File => Target);
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
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Argument_Type;

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

   procedure Runner (Package_Name : in     Wide_String) is
      use Ada.Wide_Text_IO;

      Target : File_Type;
   begin
      pragma Style_Checks ("-M120");

      Create_Specification (Name => Package_Name & ".Run",
                            File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Run;");
      Close (File => Target);

      Create_Body (Name => Package_Name & ".Run",
                   File => Target);
      Put_Line (File => Target, Item => "with " & Package_Name &".Command_Line_Parser;");
      New_Line (File => Target);
      Put_Line (File => Target, Item => "procedure " & Package_Name & ".Run is");
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
      Put_Line (File => Target, Item => "end " & Package_Name & ".Run;");
      Close (File => Target);

      pragma Style_Checks ("-M79");
   end Runner;

   procedure Set (Target_Directory : in String) is
      use Ada.Directories;
   begin
      Templates.Target_Directory := +Full_Name (Target_Directory);
   end Set;

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
end Command_Line_Parser_Generator.Templates;
