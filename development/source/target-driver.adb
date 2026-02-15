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

   procedure Receive_PDU;

   procedure Send_Data_In;

   procedure Send_Response;

   procedure Process_Login_Request;

   procedure Process_SCSI_Command;

   procedure Dispatch_PDU;

   -------------------
   -- Adjusted_Size --
   -------------------

   function Adjusted_Size
     (Value : Ada.Streams.Stream_Element_Offset)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return ((Value + 4 - 1) / 4) * 4;
   end Adjusted_Size;

   Listen_Address : constant GNAT.Sockets.Sock_Addr_Type :=
     (Family => GNAT.Sockets.Family_Inet,
      Addr   => GNAT.Sockets.Inet_Addr ("127.0.0.1"),
      Port   => 3260);
      --  Port   => 16#BC_0C#);
   Client_Address : GNAT.Sockets.Sock_Addr_Type;

   Listen_Socket : GNAT.Sockets.Socket_Type;
   Accept_Socket : GNAT.Sockets.Socket_Type;

   Request_Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47);
   Last                   : Ada.Streams.Stream_Element_Offset;

   Data_Storage   : Ada.Streams.Stream_Element_Array (0 .. 256*1024 -1); -- 65_535);
   Data_Last      : Ada.Streams.Stream_Element_Offset;

   Response_Storage : Ada.Streams.Stream_Element_Array (0 .. 256*1024 -1); --  65_535);

   Session_CmdSN     : A0B.Types.Unsigned_32 := 0;
   Session_ExpCmdSN  : A0B.Types.Unsigned_32 := 0;
   Session_MaxCmdSN  : A0B.Types.Unsigned_32 := 0;
   Connection_StatSN : A0B.Types.Unsigned_32 := 0;

   type State_Kind is (Receive_PDU, Data_In, Response);

   State : State_Kind := Receive_PDU;

   type PDU is record
      Header_Storage : System.Address;
      Data_Storage   : System.Address;
      Data_Length    : A0B.Types.Unsigned_32;
   end record;

   type Command is record
      Immediate                           : Boolean;
      Write                               : Boolean;
      Read                                : Boolean;
      Initiator_Task_Tag                  : A0B.Types.Unsigned_32;
      Write_Expected_Data_Transfer_Length : A0B.Types.Unsigned_32;
      Read_Expected_Data_Transfer_Length  : A0B.Types.Unsigned_32;
      Write_Data_Transfer_Length          : A0B.Types.Unsigned_32;
      Read_Data_Transfer_Length           : A0B.Types.Unsigned_32;
      DataSN                              : A0B.Types.Unsigned_32;
   end record;

   Current_PDU     : PDU;
   Current_Command : Command;

   ------------------
   -- Dispatch_PDU --
   ------------------

   procedure Dispatch_PDU is
      Header : iSCSI.PDUs.Basic_Header_Segment
        with Import, Address => Current_PDU.Header_Storage;

   begin
      if Header.Opcode = iSCSI.Types.SCSI_Command then
         Process_SCSI_Command;

      elsif Header.Opcode = iSCSI.Types.Login_Request then
         Process_Login_Request;

      elsif Header.Opcode = iSCSI.Types.Text_Request then
         raise Program_Error;

      else
         raise Program_Error;
      end if;
   end Dispatch_PDU;

   ---------------------------
   -- Process_Login_Request --
   ---------------------------

   procedure Process_Login_Request is
      Header          : iSCSI.PDUs.Login_Request_Header
        with Import, Address => Current_PDU.Header_Storage;
      Parser          : iSCSI.Text.Parser;
      Response_Length : A0B.Types.Unsigned_32;

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
           (Header.DataSegmentLength));

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
        (Header_Address        => Current_PDU.Header_Storage,
         Request_Data_Address  => Data_Storage'Address,
         Response_Data_Address => Response_Storage'Address,
         Response_Data_Length  => Response_Length);

      if not Header.Immediate then
         Session_ExpCmdSN := @ + 1;
         Session_MaxCmdSN := @ + 1;
      end if;

      declare
         Request_Header : iSCSI.PDUs.Login_Request_Header
           with Import, Address => Current_PDU.Header_Storage;

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
      use type SCSI.SAM5.STATUS;

      Request_Header : iSCSI.PDUs.SCSI_Command_Header
        with Import, Address => Current_PDU.Header_Storage;

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

      Current_Command.Immediate          := Request_Header.Immediate;
      Current_Command.Initiator_Task_Tag := Request_Header.Initiator_Task_Tag;

      Current_Command.Write                      := Request_Header.Write;
      Current_Command.Read                       := Request_Header.Read;
      Current_Command.Write_Data_Transfer_Length := 0;
      Current_Command.Read_Data_Transfer_Length  := 0;
      Current_Command.DataSN                     := 0;

      if Request_Header.Write then
         Current_Command.Write_Expected_Data_Transfer_Length :=
           Request_Header.Expected_Data_Transfer_Length;

         if Request_Header.Read then
            --  XXX Process Bidirectional Read Expected Data Transfer Length AHS
            raise Program_Error;
         end if;

      elsif Request_Header.Read then
         Current_Command.Write_Expected_Data_Transfer_Length := 0;
         Current_Command.Read_Expected_Data_Transfer_Length :=
           Request_Header.Expected_Data_Transfer_Length;
      else
         Current_Command.Write_Expected_Data_Transfer_Length := 0;
         Current_Command.Read_Expected_Data_Transfer_Length := 0;
      end if;

      Target.Handler.Process_Command
        (A0B.Types.Arrays.Unsigned_8_Array
           (Request_Header.SCSI_Command_Descriptor_Block));

      if Target.Handler.Status /= SCSI.SAM5.GOOD then
         State := Response;

      elsif Current_Command.Write then
         raise Program_Error;

      elsif Current_Command.Read then
         State := Data_In;

      else
         State := Response;
      end if;
   end Process_SCSI_Command;

   -----------------
   -- Receive_PDU --
   -----------------

   procedure Receive_PDU is
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Accept_Socket,
         Item   => Request_Header_Storage,
         Last   => Last);

      if Last /= Request_Header_Storage'Last then
         raise Program_Error;
      end if;

      Current_PDU.Header_Storage := Request_Header_Storage'Address;

      declare
         Header : iSCSI.PDUs.Basic_Header_Segment
           with Import, Address => Current_PDU.Header_Storage;

      begin
         Put_Line ("---------------------------------------------------------");
         Put_Line
           ("iSCSI OpCode " & iSCSI.Types.Opcode_Type'Image (Header.Opcode));
         Put_Line
           ("iSCSI AHSLen "
            & A0B.Types.Unsigned_8'Image (Header.TotalAHSLength));
         Put_Line
           ("iSCSI DSLen  "
            & A0B.Types.Unsigned_24'Image (Header.DataSegmentLength));

         if not Header.Final then
            --  XXX Not supported

            raise Program_Error;
         end if;

         if Header.TotalAHSLength /= 0 then
            raise Program_Error;
         end if;

         if Header.DataSegmentLength /= 0 then
            GNAT.Sockets.Receive_Socket
              (Socket => Accept_Socket,
               Item   => Data_Storage,
               Last   => Data_Last);

            if (Data_Last + 1)
                 /= Adjusted_Size
                   (Ada.Streams.Stream_Element_Offset
                      (Header.DataSegmentLength))
            then
               raise Program_Error;
            end if;

            Current_PDU.Data_Storage := Data_Storage'Address;
            Current_PDU.Data_Length  :=
              A0B.Types.Unsigned_32 (Header.DataSegmentLength);

         else
            Current_PDU.Data_Storage := System.Null_Address;
            Current_PDU.Data_Length  := 0;
         end if;
      end;
   end Receive_PDU;

   ------------------
   -- Send_Data_In --
   ------------------

   procedure Send_Data_In is
      Data_Length : A0B.Types.Unsigned_32;

   begin
      Target.Handler.Data_In (Response_Storage'Address, Data_Length);

      Current_Command.Read_Data_Transfer_Length := @ + Data_Length;

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
            DataSegmentLength   => A0B.Types.Unsigned_24 (Data_Length),
            Logical_Unit_Number => 0,
            Initiator_Task_Tag  => Current_Command.Initiator_Task_Tag,
            Target_Transfer_Tag => 0,
            StatSN              => Connection_StatSN,
            ExpCmdSN            => Session_ExpCmdSN,
            MaxCmdSN            => Session_MaxCmdSN,
            DataSN              => Current_Command.DataSN,
            Buffer_Offset       => 0,
            Residual_Count      => 0,  --  Send in SCSI Response
            others              => <>);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
           with Import, Address => Header'Address;
         pragma Warnings (On, "overlay changes scalar storage order");

         Data           : Ada.Streams.Stream_Element_Array
           (0 .. Adjusted_Size (Ada.Streams.Stream_Element_Offset (Data_Length)) - 1)
           with Import, Address => Response_Storage'Address;

      begin
         Current_Command.DataSN := @ + 1;

         Put_Line ("Send Data-In ..." & Data_Length'Image);
         Put_Line (Header'Image);
         Put_Line (Data'Image);
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

      State := Response;
   end Send_Data_In;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response is
   begin
      if not Current_Command.Immediate then
         Session_ExpCmdSN := @ + 1;
         Session_MaxCmdSN := @ + 1;
         Put_Line ("   ... ExpCmdSN/MaxCmdSN incremented");
      end if;

      declare
         use type SCSI.SAM5.STATUS;

         function Bidirectional_Read_Residual_Overflow return Boolean;

         function Bidirectional_Read_Residual_Underflow return Boolean;

         function Residual_Overflow return Boolean;

         function Residual_Underflow return Boolean;

         function Bidirectional_Read_Residual_Count
           return A0B.Types.Unsigned_32;

         function Residual_Count return A0B.Types.Unsigned_32;

         ---------------------------------------
         -- Bidirectional_Read_Residual_Count --
         ---------------------------------------

         function Bidirectional_Read_Residual_Count
           return A0B.Types.Unsigned_32 is
         begin
            if Current_Command.Write and Current_Command.Read then
               raise Program_Error;

            else
               return 0;
            end if;
         end Bidirectional_Read_Residual_Count;

         ------------------------------------------
         -- Bidirectional_Read_Residual_Overflow --
         ------------------------------------------

         function Bidirectional_Read_Residual_Overflow return Boolean is
         begin
            if Current_Command.Write and Current_Command.Read then
               raise Program_Error;

            else
               return False;
            end if;
         end Bidirectional_Read_Residual_Overflow;

         -------------------------------------------
         -- Bidirectional_Read_Residual_Underflow --
         -------------------------------------------

         function Bidirectional_Read_Residual_Underflow return Boolean is
         begin
            if Current_Command.Write and Current_Command.Read then
               raise Program_Error;

            else
               return False;
            end if;
         end Bidirectional_Read_Residual_Underflow;

         --------------------
         -- Residual_Count --
         --------------------

         function Residual_Count return A0B.Types.Unsigned_32 is
         begin
            if Current_Command.Write then
               raise Program_Error;

            elsif Current_Command.Read then
               if Current_Command.Read_Data_Transfer_Length
                 = Current_Command.Read_Expected_Data_Transfer_Length
               then
                  return 0;

               elsif Current_Command.Read_Data_Transfer_Length
                 < Current_Command.Read_Expected_Data_Transfer_Length
               then
                  return
                    Current_Command.Read_Expected_Data_Transfer_Length
                      - Current_Command.Read_Data_Transfer_Length;

               else
                  return
                    Current_Command.Read_Data_Transfer_Length
                      - Current_Command.Read_Expected_Data_Transfer_Length;
               end if;

            else
               return 0;
            end if;
         end Residual_Count;

         -----------------------
         -- Residual_Overflow --
         -----------------------

         function Residual_Overflow return Boolean is
         begin
            if Current_Command.Write then
               raise Program_Error;

            elsif Current_Command.Read then
               return
                 Current_Command.Read_Data_Transfer_Length
                   > Current_Command.Read_Expected_Data_Transfer_Length;

            else
               return False;
            end if;
         end Residual_Overflow;

         ------------------------
         -- Residual_Underflow --
         ------------------------

         function Residual_Underflow return Boolean is
         begin
            if Current_Command.Write then
               raise Program_Error;

            elsif Current_Command.Read then
               return
                 Current_Command.Read_Data_Transfer_Length
                   < Current_Command.Read_Expected_Data_Transfer_Length;

            else
               return False;
            end if;
         end Residual_Underflow;

         Header       : iSCSI.PDUs.SCSI_Response_Header :=
           (Opcode                                => <>,
            Bidirectional_Read_Residual_Overflow  =>
              Bidirectional_Read_Residual_Overflow,
            Bidirectional_Read_Residual_Underflow =>
              Bidirectional_Read_Residual_Underflow,
            Residual_Overflow                     => Residual_Overflow,
            Residual_Underflow                    => Residual_Underflow,
            Response                              => 0,
            Status                                => Target.Handler.Status,
            TotalAHSLength                        => 0,
            DataSegmentLength                     => 0,
            Initiator_Task_Tag                    =>
              Current_Command.Initiator_Task_Tag,
            SNACK_Tag                             => 0,
            StatSN                                => Connection_StatSN,
            ExpCmdSN                              => Session_ExpCmdSN,
            MaxCmdSN                              => Session_MaxCmdSN,
            ExpDataSN                             => 0,
            Bidirectional_Read_Residual_Count     =>
              Bidirectional_Read_Residual_Count,
            Residual_Count                        => Residual_Count,
            others                                => <>);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Header_Storage : Ada.Streams.Stream_Element_Array (0 .. 47)
           with Import, Address => Header'Address;
         pragma Warnings (On, "overlay changes scalar storage order");

         Data           : Ada.Streams.Stream_Element_Array
          (0 .. Adjusted_Size (18 + 2) - 1)
             with Import, Address => Response_Storage'Address;
         pragma Warnings (Off, "overlay changes scalar storage order");
         SenseLength    : A0B.Types.Big_Endian.Unsigned_16
           with Import, Address => Data (0)'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
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
         Put_Line (Header'Image);
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

      State := Receive_PDU;
   end Send_Response;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : iSCSI.Text.UTF8_String) return String is
      Result : constant String (1 .. Item'Length)
        with Import, Address => Item'Address;

   begin
      return Result;
   end To_String;

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
      case State is
         when Receive_PDU =>
            Receive_PDU;
            Dispatch_PDU;

         when Data_In =>
            Send_Data_In;

         when Response =>
            Send_Response;
      end case;
   end loop;
end Target.Driver;
