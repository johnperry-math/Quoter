with Ada.Text_IO;
with Ada.Command_Line;

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
      TIO.Put_Line(Author_Of(Selection) & ":");
      Tio.Put_Line(Body_Of(Selection));
   end;

end Main;
