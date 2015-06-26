package Subtype_Chains_1 is
   subtype One is Integer
     with Dynamic_Predicate => One /= 2;
end Subtype_Chains_1;
