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
with iSCSI.Target.Login;
with iSCSI.Text;
with iSCSI.Types;

procedure Target.Driver is

   use type Ada.Streams.Stream_Element_Offset;
   use type A0B.Types.Unsigned_8;
   use type iSCSI.Types.Opcode_Type;

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

   Response_Storage : Ada.Streams.Stream_Element_Array (0 .. 65_535);
   Response_Length  : A0B.Types.Unsigned_32;

   ---------------------------
   -- Process_Login_Request --
   ---------------------------

   procedure Process_Login_Request is
      Header : iSCSI.PDUs.Login_Request_Header
        with Import, Address => Header_Storage'Address;
      Parser : iSCSI.Text.Parser;

   begin
      Put_Line ("  Immediate          : " & Header.Immediate'Image);
      Put_Line ("  OpCode             :" & Header.Opcode'Image);
      Put_Line ("  Transit            : " & Header.Transit'Image);
      Put_Line ("  Continue           : " & Header.Continue'Image);
      Put_Line ("  CSG                :" & Header.CSG'Image);
      Put_Line ("  NSG                :" & Header.NSG'Image);
      Put_Line ("  Version-max        :" & Header.Version_Max'Image);
      Put_Line ("  Version-min        :" & Header.Version_Min'Image);
      Put_Line ("  TotalAHSLength     :" & Header.TotalAHSLength'Image);
      Put_Line ("  DataSegmentLength  :" & Header.DataSegmentLength'Image);
      Put_Line ("  ISID               :" & Header.ISID'Image);
      Put_Line ("  TSIH               :" & Header.TSIH'Image);
      Put_Line ("  Initiator Task Tag :" & Header.Initiator_Task_Tag'Image);
      Put_Line ("  CID                :" & Header.CID'Image);
      Put_Line ("  CmdSN              :" & Header.CmdSN'Image);
      Put_Line ("  ExpStatSN          :" & Header.ExpStatSN'Image);

      iSCSI.Text.Initialize
        (Parser,
         Data_Storage'Address,
         System.Storage_Elements.Storage_Offset
           (Basic_Header.DataSegmentLength));

      while iSCSI.Text.Forward (Parser) loop
         Ada.Text_IO.Put (''');
         Ada.Text_IO.Put
           (To_String (iSCSI.Text.Text (iSCSI.Text.Key (Parser))));
         Ada.Text_IO.Put ("' => '");
         Ada.Text_IO.Put
           (To_String (iSCSI.Text.Text (iSCSI.Text.Value (Parser))));
         Ada.Text_IO.Put_Line ("'");
      end loop;

      iSCSI.Target.Login.Process
        (Header_Address        => Header_Storage'Address,
         Request_Data_Address  => Data_Storage'Address,
         Response_Data_Address => Response_Storage'Address,
         Response_Data_Length  => Response_Length);

      declare
         Request_Header : iSCSI.PDUs.Login_Request_Header
           with Import, Address => Header_Storage'Address;

         Header       : iSCSI.PDUs.Login_Response_Header :=
           (Transit            => True,
            Continue           => False,
            CSG                => iSCSI.Types.LoginOperationalNegotiation,
            NSG                => iSCSI.Types.FullFeaturePhase,
            Version_Max        => 1,
            Version_Active     => 0,
            TotalAHSLength     => 0,
            DataSegmentLength  => A0B.Types.Unsigned_24 (Response_Length),
            ISID               => Request_Header.ISID,
            TSIH               => 16#1234#,
            Initiator_Task_Tag => Request_Header.Initiator_Task_Tag,
            StatSN             => 0,
            ExpCmdSN           => 0,
            MaxCmdSN           => 0,
            Status_Class       => 0,
            Status_Detail      => 0,
            others             => <>);
         Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
           with Import, Address => Header'Address;

         Data           : Ada.Streams.Stream_Element_Array
           (0 .. Adjusted_Size (Ada.Streams.Stream_Element_Offset (Response_Length)))
           with Import, Address => Response_Storage'Address;

      begin
         GNAT.Sockets.Send_Socket
           (Socket => Accept_Socket,
            Item   => Header_Storage,
            Last   => Last);
         GNAT.Sockets.Send_Socket
           (Socket => Accept_Socket,
            Item   => Data,
            Last   => Last);
      end;
   end Process_Login_Request;

begin
   GNAT.Sockets.Create_Socket (Listen_Socket);

   GNAT.Sockets.Bind_Socket (Listen_Socket, Listen_Address);
   GNAT.Sockets.Listen_Socket (Listen_Socket, 1);

   GNAT.Sockets.Accept_Socket
     (Server   => Listen_Socket,
      Socket   => Accept_Socket,
      Address  => Client_Address);

   loop
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

      if Basic_Header.Opcode = iSCSI.Types.Login_Request then
         Process_Login_Request;
            null;

      elsif Basic_Header.Opcode = iSCSI.Types.Text_Request then
         raise Program_Error;

      else
         raise Program_Error;
      end if;
   end loop;
end Target.Driver;
