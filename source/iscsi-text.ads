--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Storage_Elements;

with A0B.Types;

package iSCSI.Text with Pure is

   type UTF8_String is array (Positive range <>) of A0B.Types.Unsigned_8;

   type Segment is private;

   function "=" (Left : Segment; Right : UTF8_String) return Boolean;

   function Text (Self : Segment) return UTF8_String;

   function Is_Null (Self : Segment) return Boolean;

   type Parser is limited private;

   procedure Initialize
     (Self    : in out Parser;
      Address : System.Address;
      Length  : System.Storage_Elements.Storage_Count);

   function Forward (Self : in out Parser) return Boolean;

   function Key (Self : Parser) return UTF8_String;

   function Key (Self : Parser) return Segment;

   function Value (Self : Parser) return UTF8_String;

   function Value (Self : Parser) return Segment;

private

   type Segment is record
      Address  : System.Address := System.Null_Address;
      Offset   : System.Storage_Elements.Storage_Count;
      Length   : System.Storage_Elements.Storage_Count;
   end record;

   type Parser is limited record
      Address   : System.Address := System.Null_Address;
      Length    : System.Storage_Elements.Storage_Count;
      Current   : System.Storage_Elements.Storage_Count;
      Key_First : System.Storage_Elements.Storage_Count;
      Delimiter : System.Storage_Elements.Storage_Count;
   end record;

end iSCSI.Text;
