with Subtype_Chains_1;

package Subtype_Chains_2 is
   subtype Two is Subtype_Chains_1.One
     with Dynamic_Predicate => Two mod 2 = 0;
end Subtype_Chains_2;
