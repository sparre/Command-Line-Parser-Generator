package Using_An_Enumeration is
   type Choices is (Interactive, Batch, Service);

   procedure Run (Mode : in Choices := Interactive);

   procedure Run (Configuration_File : in     String;
                  Mode               : in     Choices);
end Using_An_Enumeration;
