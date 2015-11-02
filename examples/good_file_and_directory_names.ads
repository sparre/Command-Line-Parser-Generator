package Good_File_And_Directory_Names is
   type File_Name      is new String;
   type Directory_Name is new String;

   procedure List (File      : in     File_Name);
   procedure List (Directory : in     Directory_Name);
end Good_File_And_Directory_Names;
