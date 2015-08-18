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
