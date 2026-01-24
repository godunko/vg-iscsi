--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

package body iSCSI.Text with Pure is

   use type System.Storage_Elements.Storage_Element;
   use type System.Storage_Elements.Storage_Offset;

   ---------
   -- "=" --
   ---------

   function "=" (Left : Segment; Right : UTF8_String) return Boolean is
   begin
      return Text (Left) = Right;
   end "=";

   -------------
   -- Forward --
   -------------

   function Forward (Self : in out Parser) return Boolean is
      Storage : constant
        System.Storage_Elements.Storage_Array (0 .. Self.Length - 1)
          with Import, Address => Self.Address;

   begin
      Self.Key_First := Self.Current;

      if Self.Current >= Self.Length then
         return False;
      end if;

      loop
         if Storage (Self.Current) = Character'Pos ('=') then
            Self.Delimiter := Self.Current;

            exit;
         end if;

         Self.Current := @ + 1;
      end loop;

      loop
         if Storage (Self.Current) = 0 then
            Self.Current := @ + 1;

            exit;
         end if;

         Self.Current := @ + 1;
      end loop;

      return True;
   end Forward;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Parser;
      Address : System.Address;
      Length  : System.Storage_Elements.Storage_Count) is
   begin
      Self.Address := Address;
      Self.Length  := Length;
      Self.Current := 0;
   end Initialize;

   ---------
   -- Key --
   ---------

   function Key (Self : Parser) return Segment is
   begin
      return
        (Address => Self.Address,
         Offset  => Self.Key_First,
         Length  => Self.Delimiter - Self.Key_First);
   end Key;

   ---------
   -- Key --
   ---------

   function Key (Self : Parser) return UTF8_String is
      Storage : constant
        System.Storage_Elements.Storage_Array (0 .. Self.Length - 1)
          with Import, Address => Self.Address;

   begin
      declare
         Text : constant
           UTF8_String (1 .. Natural (Self.Delimiter - Self.Key_First))
             with Import, Address => Storage (Self.Key_First)'Address;

      begin
         return Text;
      end;
   end Key;

   ----------
   -- Text --
   ----------

   function Text (Self : Segment) return UTF8_String is
      Text : constant
        UTF8_String (1 .. Natural (Self.Length))
          with Import, Address => Self.Address + Self.Offset;

   begin
      return Text;
   end Text;

   -----------
   -- Value --
   -----------

   function Value (Self : Parser) return Segment is
   begin
      return
        (Address => Self.Address,
         Offset  => Self.Delimiter + 1,
         Length  => Self.Current - Self.Delimiter - 1 - 1);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Self : Parser) return UTF8_String is
      Storage : constant
        System.Storage_Elements.Storage_Array (0 .. Self.Length - 1)
          with Import, Address => Self.Address;

   begin
      declare
         Text : constant
           UTF8_String (1 .. Natural (Self.Current - Self.Delimiter - 1 - 1))
             with Import, Address => Storage (Self.Delimiter + 1)'Address;

      begin
         return Text;
      end;
   end Value;

end iSCSI.Text;
