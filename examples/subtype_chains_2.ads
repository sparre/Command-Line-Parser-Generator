with Subtype_Chains_1;

package Subtype_Chains_2 is
   subtype Two is Subtype_Chains_1.One
     with Dynamic_Predicate => Two mod 2 = 0;

   procedure Something_To_Call
     renames Subtype_Chains_1.Something_To_Call;
end Subtype_Chains_2;
