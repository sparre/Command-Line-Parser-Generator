--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Ada.Characters.Conversions,
     Ada.Environment_Variables,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

with Asis.Ada_Environments,
     Asis.Implementation;

procedure Command_Line_Parser_Generator.Setup (Context :    out Asis.Context)
is
   function Source_Directories return Wide_String;
   function Source_Directories return Wide_String is
      use Ada.Characters.Conversions,
           Ada.Environment_Variables,
           Ada.Strings.Fixed,
           Ada.Strings.Unbounded;

      Ada_Include_Path : constant String := "ADA_INCLUDE_PATH";

      List   : String renames Value (Name    => Ada_Include_Path,
                                     Default => ".");
      Next   : Positive := List'First;
      Cut    : Natural;
      Buffer : Unbounded_String;
   begin
      while Next in List'Range loop
         Cut := Index (Source  => List (Next .. List'Last),
                       Pattern => ":");

         if Cut = 0 then
            Cut := List'Last + 1;
         end if;

         if Cut > Next then
            Append (Source   => Buffer,
                    New_Item => " -I" & List (Next .. Cut - 1));
         end if;

         Next := Cut + 1;
      end loop;

      return To_Wide_String (To_String (Buffer));
   end Source_Directories;
begin
   Asis.Implementation.Initialize ("");
   Asis.Ada_Environments.Associate
     (The_Context => Context,
      Name        => "CLPG",
      Parameters  => "-CA -FM" & Source_Directories);
   Asis.Ada_Environments.Open (The_Context => Context);
end Command_Line_Parser_Generator.Setup;
