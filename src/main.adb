with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Latin_1;

with Quote_Structure;

procedure Main is

   subtype Quote is Quote_Structure.Quote;
   subtype Quote_Vector is Quote_Structure.Quote_Vector;
   use all type Quote_Structure.Quote;
   use all type Quote_Structure.Quote_Vector;

   package TIO renames Ada.Text_IO;
   All_Quotes: Quote_Vector;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      TIO.Put_Line("Requires a filename, sorry");
      return;
   end if;
   Read_Quotes(Ada.Command_Line.Argument(1), All_Quotes);

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
