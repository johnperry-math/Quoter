with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Latin_1;

with Quote_Structure;

procedure Main is

   subtype Quote is Quote_Structure.Quote; -- convenience
   subtype Quote_Vector is Quote_Structure.Quote_Vector; -- convenience
   use all type Quote_Structure.Quote;
   use all type Quote_Structure.Quote_Vector;

   package TIO renames Ada.Text_IO; -- convenience
   All_Quotes: Quote_Vector; -- stores all available quotes

begin

   -- verify that we have a command line argument for the filename
   if Ada.Command_Line.Argument_Count < 1 then
      TIO.Put_Line("Requires a filename, sorry");
      return;
   end if;

   if Ada.Command_Line.Argument(1) = "--help" then
      -- give help
      Tio.Put_Line("usage: <command> <quote_file>");
      Tio.Put_Line("reads all the quotes from <quote_file>, which should be");
      Tio.Put_Line("in json format, selects one at random, and prints it to");
      Tio.Put_Line("standard output.");
      return;
   end if;

   Read_Quotes(Ada.Command_Line.Argument(1), All_Quotes);

   -- output a random quote according to some format
   declare Selection: Quote := Random_Quote(All_Quotes);
   begin
      TIO.Put_Line("-- ");
      Tio.Put_Line(Body_Of(Selection));
      if Has_Speaker(Selection) then
         Tio.Put(Ada.Characters.Latin_1.HT);
         Tio.Put_Line("as spoken by " & Speaker_Of(Selection));
      end if;
      if Has_Source(Selection) then
         Tio.Put(Ada.Characters.Latin_1.HT);
         if Has_Author(Selection) then
            Tio.Put("from " & Source_Of(Selection));
            TIO.Put_Line(", by " & Author_Of(Selection));
         else
            Tio.Put_Line("from " & Source_Of(Selection));
         end if;
      elsif Has_Author(Selection) then
         Tio.Put(Ada.Characters.Latin_1.HT);
         TIO.Put_Line(Author_Of(Selection));
      end if;
   end;

end Main;
