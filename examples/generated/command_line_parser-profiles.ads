with Command_Line_Parser.Argument_List;

private
package Command_Line_Parser.Profiles is
   type Index is range 1 .. 2;

   function Match (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance) return Boolean;

   procedure Call (Profile   : in     Index;
                   Arguments : in     Argument_List.Instance);
end Command_Line_Parser.Profiles;
