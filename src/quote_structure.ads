with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Quote_Structure is
   
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   use all type Unbounded_String;
   
   type Quote is private;
   type Quote_Vector is private;
   
   function body_of(Q: Quote) return String;
   function Author_Of(Q: Quote) return String;
   function Source_Of(Q: Quote) return String;
   
   procedure Read_Quotes(From: String; Into: in out Quote_Vector);
   function Random_Quote(From: Quote_Vector) return Quote;
   
private
   
   type Quote is record
      Quotation   : Unbounded_String;
      Author      : Unbounded_String;
      Speaker     : Unbounded_String;
      Source_Text : Unbounded_String;
   end record;

   package Quote_Vector_Package is new Ada.Containers.Vectors
      (
       Index_Type   => Natural,
       Element_Type => Quote
      );
   type Quote_Vector is new Quote_Vector_Package.Vector
   with record null; end record;
   
end Quote_Structure;
