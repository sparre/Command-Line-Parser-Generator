package Web_Site_Generator.API is
   subtype Directory_Name is String;

   procedure Generate_All_Pages
     (Sources   : in     Directory_Name := ".";
      Target    : in     Directory_Name := "/var/www";
      Documents : in     String         := "documents";
      Debug     : in     Boolean        := False);
end Web_Site_Generator.API;
