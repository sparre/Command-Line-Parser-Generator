package Command_Line_Parser is
   function Initialised return Boolean;

   procedure Initialise
     with Pre  => not Initialised,
          Post => Initialised;

   function Count_Matching_Call_Profiles return Natural
     with Pre => Initialised;

   procedure Call_Matching_Profile
     with Pre => Initialised;

   package Errors is
      procedure No_Matching_Call_Profile
        with Pre => Count_Matching_Call_Profiles = 0;

      procedure More_Than_One_Matching_Call_Profile
        with Pre => Count_Matching_Call_Profiles > 1;
   end Errors;
end Command_Line_Parser;
