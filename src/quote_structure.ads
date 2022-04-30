with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GnatColl.Json; use GnatColl.Json;

package Quote_Structure is
   
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   use all type Unbounded_String;
   
   type Quote is private;
   type Quote_Vector is private;
   
   function Body_Of(Q: Quote) return String;
   function Author_Of(Q: Quote) return String;
   function Source_Of(Q: Quote) return String;
   function Speaker_Of(Q: Quote) return String;
   
   function Has_Author(Q: Quote) return Boolean;
   function Has_Source(Q: Quote) return Boolean;
   function Has_Speaker(Q: Quote) return Boolean;
  
   procedure Read_Quotes(From: String; Into: in out Quote_Vector);
   function Random_Quote(From: Quote_Vector) return Quote;
   
private
   
   type Quote is record
      Quotation   : UTF8_Unbounded_String;
      Has_Author  : Boolean := False;
      Author      : UTF8_Unbounded_String;
      Has_Speaker : Boolean := False;
      Speaker     : UTF8_Unbounded_String;
      Has_Source  : Boolean := False;
      Source_Text : UTF8_Unbounded_String;
   end record;

   package Quote_Vector_Package is new Ada.Containers.Vectors
      (
       Index_Type   => Natural,
       Element_Type => Quote
      );
   type Quote_Vector is new Quote_Vector_Package.Vector
   with record null; end record;
   
end Quote_Structure;
