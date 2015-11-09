with Good_Derived_Types_1;

package Good_Derived_Types_2 is
   use Good_Derived_Types_1;

   type B is new A;

   procedure Run (Name : B);
end Good_Derived_Types_2;
