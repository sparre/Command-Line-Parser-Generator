with Good_Derived_Types_2;

package Good_Derived_Types_3 is
   use Good_Derived_Types_2;

   type C is new B;

   procedure Use_It (Name : in  C);
end Good_Derived_Types_3;
