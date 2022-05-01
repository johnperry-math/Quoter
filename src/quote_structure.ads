with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GnatColl.Json; use GnatColl.Json;

package Quote_Structure is
   
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   --  a convenience
   use all type Unbounded_String;
   
   type Quote is private;
   --  the main type, contains at least a body, and possibly
   -- author, source, and speaker
   type Quote_Vector is private;
   --  a vector of quotes
   
   function Body_Of(Q: Quote) return String; -- returns quote body
   function Author_Of(Q: Quote) return String; -- returns quote author
   function Source_Of(Q: Quote) return String; -- returns quote source
   function Speaker_Of(Q: Quote) return String;
   -- returns quote speaker (as in a work of fiction)
   
   function Has_Author(Q: Quote) return Boolean; -- true if author is known
   function Has_Source(Q: Quote) return Boolean; -- true if source is known
   function Has_Speaker(Q: Quote) return Boolean; -- true if speaker is known
  
   procedure Read_Quotes(From: String; Into: in out Quote_Vector);
   -- read quotes `From` a path name `Into` a vector
   function Random_Quote(From: Quote_Vector) return Quote;
   -- return a random quote `From` the ones known
   
private
   
   -- TODO wrap the optional fields in an optional type
   
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
   -- instantiate a package for vectors of quotes
      (
       Index_Type   => Natural,
       Element_Type => Quote
      );
   type Quote_Vector is new Quote_Vector_Package.Vector
   with record null; end record;
   -- instantiate a type for vectors of quotes
   
end Quote_Structure;
