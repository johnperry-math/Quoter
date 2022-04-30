with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;

package body Quote_Structure is

   function To_String(Source: Unbounded_String) return String
                      renames Ada.Strings.Unbounded.To_String;
   
   function body_of(Q: Quote) return String is
      (To_String(Q.Quotation));
   
   function Author_Of(Q: Quote) return String is
      (To_String(Q.Author));
   
   function Source_Of(Q: Quote) return String is
      (To_String(Q.Source_Text));
   
   function Speaker_Of(Q: Quote) return String is
      (To_String(Q.Speaker));
   
   function Has_Author(Q: Quote) return Boolean is (Q.Has_Author);
   
   function Has_Source(Q: Quote) return Boolean is (Q.Has_Source);
   
   function Has_Speaker(Q: Quote) return Boolean is (Q.Has_Speaker);

   package TIO renames Ada.Text_IO;
   
   procedure Read_Quotes(From: String; Into: in out Quote_Vector) is
      File: TIO.File_Type;
      function To_Unbounded_String(Source: String) return Unbounded_String
                                   renames Ada.Strings.Unbounded.To_Unbounded_String;
      All_Quotes: Json_Array;
      All_Lines : Unbounded_String;
      Counter   : Positive;
      Parsing_Result: Read_Result;
   begin
      Tio.Open(File, TIO.In_File, From);
      while not TIO.End_Of_File(File) loop
         declare Line: String := Tio.Get_Line(File);
         begin
            Append(All_Lines, Line);
         end;
      end loop;
      Tio.Close(File);
      Parsing_Result := Read(All_Lines);
      if Parsing_Result.Success then
         All_Quotes := Get(Parsing_Result.Value);
         Counter := Array_First(All_Quotes);
         while Array_Has_Element(All_Quotes, Counter) loop
            declare
               Json_Quote: Json_Value := Array_Element(All_Quotes, Counter);
               New_Quote : Quote;
            begin
               New_Quote.Quotation := Get(Json_Quote, "quote");
               if Has_Field(Json_Quote, "author") then
                  New_Quote.Has_Author := True;
                  New_Quote.Author := Get(Json_Quote, "author");
               end if;
               if Has_Field(Json_Quote, "speaker") then
                  New_Quote.Has_Speaker := True;
                  New_Quote.Speaker := Get(Json_Quote, "speaker");
               end if;
               if Has_Field(Json_Quote, "text") then
                  New_Quote.Has_Source := True;
                  New_Quote.Source_Text := Get(Json_Quote, "text");
               end if;
               Into.Append(New_Quote);
            end;
            Counter := Array_Next(All_Quotes, Counter);
         end loop;
      end if;
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
