--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;
with System.Storage_Elements;

with A0B.Types;

with iSCSI.PDUs;
with iSCSI.Text;
with iSCSI.Types;

procedure Target.Driver is

   use type Ada.Streams.Stream_Element_Offset;
   use type A0B.Types.Unsigned_8;

   function Adjusted_Size
     (Value : Ada.Streams.Stream_Element_Offset)
      return Ada.Streams.Stream_Element_Offset;

   function To_String (Item : iSCSI.Text.UTF8_String) return String;

   -------------------
   -- Adjusted_Size --
   -------------------

   function Adjusted_Size
     (Value : Ada.Streams.Stream_Element_Offset)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return ((Value + 4 - 1) / 4) * 4;
   end Adjusted_Size;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : iSCSI.Text.UTF8_String) return String is
      Result : constant String (1 .. Item'Length)
        with Import, Address => Item'Address;

   begin
      return Result;
   end To_String;

   Listen_Address : constant GNAT.Sockets.Sock_Addr_Type :=
     (Family => GNAT.Sockets.Family_Inet,
      Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
      Port   => 3260);
      --  Port   => 16#BC_0C#);
   Client_Address : GNAT.Sockets.Sock_Addr_Type;

   Listen_Socket : GNAT.Sockets.Socket_Type;
   Accept_Socket : GNAT.Sockets.Socket_Type;

   Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47);
   Last           : Ada.Streams.Stream_Element_Offset;
   Basic_Header   : iSCSI.PDUs.Basic_Header_Segment
     with Import, Address => Header_Storage'Address;

   Data_Storage   : Ada.Streams.Stream_Element_Array (0 .. 65_535);
   Data_Last      : Ada.Streams.Stream_Element_Offset;

begin
   GNAT.Sockets.Create_Socket (Listen_Socket);

   GNAT.Sockets.Bind_Socket (Listen_Socket, Listen_Address);
   GNAT.Sockets.Listen_Socket (Listen_Socket, 1);

   GNAT.Sockets.Accept_Socket
     (Server   => Listen_Socket,
      Socket   => Accept_Socket,
      Address  => Client_Address);

   GNAT.Sockets.Receive_Socket
     (Socket => Accept_Socket,
      Item   => Header_Storage,
      Last   => Last);

   Put_Line (iSCSI.Types.Opcode_Type'Image (Basic_Header.Opcode));
   Put_Line (A0B.Types.Unsigned_8'Image (Basic_Header.TotalAHSLength));
   Put_Line (A0B.Types.Unsigned_24'Image (Basic_Header.DataSegmentLength));

   if Basic_Header.TotalAHSLength /= 0 then
      raise Program_Error;
   end if;

   GNAT.Sockets.Receive_Socket
     (Socket => Accept_Socket,
      Item   => Data_Storage,
      Last   => Data_Last);

   if (Data_Last + 1)
     /= Adjusted_Size
       (Ada.Streams.Stream_Element_Offset (Basic_Header.DataSegmentLength))
   then
      raise Program_Error;
   end if;

   declare
      Parser : iSCSI.Text.Parser;

   begin
      iSCSI.Text.Initialize
        (Parser,
         Data_Storage'Address,
         System.Storage_Elements.Storage_Offset
           (Basic_Header.DataSegmentLength));

      while iSCSI.Text.Forward (Parser) loop
         Ada.Text_IO.Put_Line (To_String (iSCSI.Text.Key (Parser)));
         Ada.Text_IO.Put_Line (To_String (iSCSI.Text.Value (Parser)));
      end loop;
   end;
   --  case Basic_Header.Opcode is
   --     when iSCSI.Types.Login_Request =>
   --        raise Program_Error;
   --
   --     when other =>
   --        raise Program_Error;
   --  end case;
end Target.Driver;
