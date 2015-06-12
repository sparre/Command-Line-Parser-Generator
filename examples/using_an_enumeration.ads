package Using_An_Enumeration is
   type Choices is (Interactive, Batch, Service);

   procedure Run (Mode : in Choices := Interactive);
end Using_An_Enumeration;
