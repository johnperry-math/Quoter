with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;

package body Quote_Structure is

   function To_String(Source: Unbounded_String) return String
                      renames Ada.Strings.Unbounded.To_String;
   
   function body_of(Q: Quote) return String is
      (To_String(Q.Quotation));
   
   function Author_Of(Q: Quote) return String is
      (To_String(Q.Author));
   
   function Source_Of(Q: Quote) return String is
      (To_String(Q.Source_Text));

   package TIO renames Ada.Text_IO;
   
   
   procedure Read_Quotes(From: String; Into: in out Quote_Vector) is
      File: TIO.File_Type;
      function To_Unbounded_String(Source: String) return Unbounded_String
                                   renames Ada.Strings.Unbounded.To_Unbounded_String;
   begin
      Tio.Open(File, TIO.In_File, From);
      while not TIO.End_Of_File(File) loop
         declare
            New_Quote: Quote;
            Text_Line: String := Tio.Get_Line(File);
            Author_Line: String := Tio.Get_Line(File);
         begin
            New_Quote.Quotation := To_Unbounded_String(Text_Line);
            New_Quote.Author := To_Unbounded_String(Author_Line);
            Into.Append(New_Quote);
         end;
      end loop;
      Tio.Close(File);
   end Read_Quotes;
   
   function Random_Quote(From: Quote_Vector) return Quote is
      package Randomizer is new Ada.Numerics.Discrete_Random
         (Result_Subtype => Natural);
      Number_Of_Quotes: Natural := Natural(From.Length);
      Result: Quote;
      Generator: Randomizer.Generator;
   begin
      Randomizer.Reset(Generator);
      Result := From(Randomizer.Random(Generator) mod Number_Of_Quotes);
      return Result;
   end Random_Quote;

end Quote_Structure;
