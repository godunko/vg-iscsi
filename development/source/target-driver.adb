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

with A0B.Types.Arrays;
with A0B.Types.Big_Endian;

with iSCSI.PDUs;
with iSCSI.Target.Login;
with iSCSI.Text;
with iSCSI.Types;
with SCSI.SAM5;
with SCSI.SPC5.Sense;

with Target.Handler;

procedure Target.Driver is

   use type Ada.Streams.Stream_Element_Offset;
   use type A0B.Types.Unsigned_8;
   use type A0B.Types.Unsigned_24;
   use type A0B.Types.Unsigned_32;
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

   --  Command_DataSN           : A0B.Types.Unsigned_32 := 0;
   Session_CmdSN     : A0B.Types.Unsigned_32 := 0;
   Session_ExpCmdSN  : A0B.Types.Unsigned_32 := 0;
   Session_MaxCmdSN  : A0B.Types.Unsigned_32 := 0;
   Connection_StatSN : A0B.Types.Unsigned_32 := 0;

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

      if not Header.Immediate then
         Session_ExpCmdSN := @ + 1;
         Session_MaxCmdSN := @ + 1;
      end if;

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
            StatSN             => Connection_StatSN,
            ExpCmdSN           => Session_ExpCmdSN,
            MaxCmdSN           => Session_MaxCmdSN,
            Status_Class       => 0,
            Status_Detail      => 0,
            others             => <>);
         Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
           with Import, Address => Header'Address;

         Data           : Ada.Streams.Stream_Element_Array
           (0 .. Adjusted_Size (Ada.Streams.Stream_Element_Offset (Response_Length)) - 1)
           with Import, Address => Response_Storage'Address;

      begin
         Connection_StatSN := @ + 1;

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

   --------------------------
   -- Process_SCSI_Command --
   --------------------------

   procedure Process_SCSI_Command is
      Request_Header : iSCSI.PDUs.SCSI_Command_Header
        with Import, Address => Header_Storage'Address;

      Command_Initiator_Task_Tag            : A0B.Types.Unsigned_32;
      Command_Expected_Data_Transfer_Length : A0B.Types.Unsigned_32;

   begin
      Put_Line ("iSCSI Immediate: " & Request_Header.Immediate'Image);
      Put_Line ("iSCSI Final: " & Request_Header.Final'Image);
      Put_Line ("iSCSI Read: " & Request_Header.Read'Image);
      Put_Line ("iSCSI Write: " & Request_Header.Write'Image);
      Put_Line ("iSCSI Attr: " & Request_Header.Attr'Image);
      Put_Line ("iSCSI LUN: " & Request_Header.Logical_Unit_Number'Image);
      Put_Line ("iSCSI Initiator Task Tag" & Request_Header.Initiator_Task_Tag'Image);
      Put_Line ("iSCSI Expected Data Transfer Length" & Request_Header.Expected_Data_Transfer_Length'Image);
      Put_Line ("iSCSI CmdSN" & Request_Header.CmdSN'Image);
      Put_Line ("iSCSI ExpStatSN" & Request_Header.ExpStatSN'Image);
      Put_Line ("iSCSI CDB " & Request_Header.SCSI_Command_Descriptor_Block'Image);

      Command_Initiator_Task_Tag := Request_Header.Initiator_Task_Tag;
      Command_Expected_Data_Transfer_Length :=
        Request_Header.Expected_Data_Transfer_Length;

      Target.Handler.Process_Command
        (A0B.Types.Arrays.Unsigned_8_Array
           (Request_Header.SCSI_Command_Descriptor_Block));

      if Target.Handler.Has_Data_In then
         Target.Handler.Data_In (Response_Storage'Address, Response_Length);

         declare
            Header       : iSCSI.PDUs.SCSI_Data_In_Header :=
              (Opcode              => <>,
               Final               => True,
               Acknowledge         => False,
               Residual_Overflow   => False,
               Residual_Underflow  => False,
               Status_Flag         => False,  --  Send in SCSI Response
               Status              => <>,     --  Send in SCSI Response
               TotalAHSLength      => 0,
               DataSegmentLength   => A0B.Types.Unsigned_24 (Response_Length),
               Logical_Unit_Number => 0,
               Initiator_Task_Tag  => Command_Initiator_Task_Tag,
               Target_Transfer_Tag => 0,
               StatSN              => Connection_StatSN,
               ExpCmdSN            => Session_ExpCmdSN,
               MaxCmdSN            => Session_MaxCmdSN,
               DataSN              => 0,  --  DataSN,
               Buffer_Offset       => 0,
               Residual_Count      => 0,  --  Send in SCSI Response
               others              => <>);
            Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
              with Import, Address => Header'Address;

            Data           : Ada.Streams.Stream_Element_Array
              (0 .. Adjusted_Size (Ada.Streams.Stream_Element_Offset (Response_Length)) - 1)
              with Import, Address => Response_Storage'Address;

         begin
            --  DataSN := @ + 1;

            Put_Line ("Send Data-In ...");
            GNAT.Sockets.Send_Socket
              (Socket => Accept_Socket,
               Item   => Header_Storage,
               Last   => Last);
            Put_Line (Last'Image);
            GNAT.Sockets.Send_Socket
              (Socket => Accept_Socket,
               Item   => Data,
               Last   => Last);
            Put_Line (Last'Image);
            Put_Line ("  ... done.");
         end;

      else
         Response_Length := 0;
      end if;

      if not Request_Header.Immediate then
         Session_ExpCmdSN := @ + 1;
         Session_MaxCmdSN := @ + 1;
         Put_Line ("   ... ExpCmdSN/MaxCmdSN incremented");
      end if;

      declare
         use type SCSI.SAM5.STATUS;

         Header       : iSCSI.PDUs.SCSI_Response_Header :=
           (Opcode                                => <>,
            Bidirectional_Read_Residual_Overflow  => False,
            Bidirectional_Read_Residual_Underflow => False,
            Residual_Overflow                     => False,
            Residual_Underflow                    =>
              Command_Expected_Data_Transfer_Length > Response_Length,
            Response                              => 0,
            Status                                => Target.Handler.Status,
            TotalAHSLength                        => 0,
            DataSegmentLength                     => 0,
            Initiator_Task_Tag                    => Command_Initiator_Task_Tag,
            SNACK_Tag                             => 0,
            StatSN                                => Connection_StatSN,
            ExpCmdSN                              => Session_ExpCmdSN,
            MaxCmdSN                              => Session_MaxCmdSN,
            ExpDataSN                             => 0,
            Bidirectional_Read_Residual_Count     => 0,
            Residual_Count                        =>
              Command_Expected_Data_Transfer_Length - Response_Length,
            others                                => <>);
         Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
           with Import, Address => Header'Address;

         Data           : Ada.Streams.Stream_Element_Array
          (0 .. Adjusted_Size (18 + 2) - 1)
             with Import, Address => Response_Storage'Address;
         SenseLength    : A0B.Types.Big_Endian.Unsigned_16
           with Import, Address => Data (0)'Address;
         SenseData      : SCSI.SPC5.Sense.Fixed_Format
           with Import, Address => Data (2)'Address;

      begin
         Connection_StatSN := @ + 1;

         if Header.Status /= SCSI.SAM5.GOOD then
            SenseLength := (Value => 18);

            SenseData :=
              (VALID                           => False,
               RESPONSE_CODE                   => <>,
               FILEMARK                        => False,
               EOM                             => False,
               ILI                             => False,
               SDAT_OVFL                       => False,
               SENSE_KEY                       => Target.Handler.Sense.SENSE_KEY,
               INFORMATION                     => (Value => 0),
               ADDITIONAL_SENSE_LENGTH         => <>,
               COMMAND_SPECIFIC_INFORMATION    => (Value => 0),
               ADDITIONAL_SENSE_CODE           =>
                 Target.Handler.Sense.ADDITIONAL_SENSE_CODE,
               ADDITIONAL_SENSE_CODE_QUALIFIER =>
                 Target.Handler.Sense.ADDITIONAL_SENSE_CODE_QUALIFIER,
               FIELD_REPLACEABLE_UNIT_CODE     => 0,
               others                          => <>);

            Header.DataSegmentLength := Data'Length;
         end if;

         Put_Line ("Send SCSI Response ...");
         GNAT.Sockets.Send_Socket
           (Socket => Accept_Socket,
            Item   => Header_Storage,
            Last   => Last);
         Put_Line (Last'Image);

         if Header.Status /= SCSI.SAM5.GOOD then
            GNAT.Sockets.Send_Socket
              (Socket => Accept_Socket,
               Item   => Data,
               Last   => Last);
            Put_Line (Last'Image);
         end if;

         Put_Line ("  ... done.");
      end;
   end Process_SCSI_Command;

begin
   GNAT.Sockets.Create_Socket (Listen_Socket);
   GNAT.Sockets.Set_Socket_Option
     (Listen_Socket,
      GNAT.Sockets.Socket_Level,
      (GNAT.Sockets.Reuse_Address, True));

   GNAT.Sockets.Bind_Socket (Listen_Socket, Listen_Address);
   GNAT.Sockets.Listen_Socket (Listen_Socket, 1);

   GNAT.Sockets.Accept_Socket
     (Server   => Listen_Socket,
      Socket   => Accept_Socket,
      Address  => Client_Address);
   GNAT.Sockets.Close_Socket (Listen_Socket);

   loop
      GNAT.Sockets.Receive_Socket
        (Socket => Accept_Socket,
         Item   => Header_Storage,
         Last   => Last);

      if Last /= Header_Storage'Last then
         raise Program_Error;
      end if;

      Put_Line ("---------------------------------------------------------");
      Put_Line
        ("iSCSI OpCode " & iSCSI.Types.Opcode_Type'Image (Basic_Header.Opcode));
      Put_Line
        ("iSCSI AHSLen "
         & A0B.Types.Unsigned_8'Image (Basic_Header.TotalAHSLength));
      Put_Line
        ("iSCSI DSLen  "
         & A0B.Types.Unsigned_24'Image (Basic_Header.DataSegmentLength));

      if Basic_Header.TotalAHSLength /= 0 then
         raise Program_Error;
      end if;

      if Basic_Header.DataSegmentLength /= 0 then
         GNAT.Sockets.Receive_Socket
           (Socket => Accept_Socket,
            Item   => Data_Storage,
            Last   => Data_Last);

         if (Data_Last + 1)
           /= Adjusted_Size
               (Ada.Streams.Stream_Element_Offset
                 (Basic_Header.DataSegmentLength))
         then
            raise Program_Error;
         end if;
      end if;

      if Basic_Header.Opcode = iSCSI.Types.SCSI_Command then
         Process_SCSI_Command;

      elsif Basic_Header.Opcode = iSCSI.Types.Login_Request then
         Process_Login_Request;

      elsif Basic_Header.Opcode = iSCSI.Types.Text_Request then
         raise Program_Error;

      else
         raise Program_Error;
      end if;
   end loop;
end Target.Driver;
