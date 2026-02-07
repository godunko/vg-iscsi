--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  PDUs defined by [RFC7143]

with System;

with A0B.Types.Arrays;

with SCSI.SAM5;

with iSCSI.Types;

package iSCSI.PDUs with Pure is

   Byte_Size                   : constant := 8;
   Basic_Header_Segment_Length : constant := 48;

   type Basic_Header_Segment is record
      Immediate          : Boolean;
      Opcode             : iSCSI.Types.Opcode_Type;
      Final              : Boolean;
      TotalAHSLength     : A0B.Types.Unsigned_8;
      DataSegmentLength  : A0B.Types.Unsigned_24;
      Initiator_Task_Tag : A0B.Types.Unsigned_32;
   end record
     with Size                 => Basic_Header_Segment_Length * Byte_Size,
          Bit_Order            => System.High_Order_First,
          Scalar_Storage_Order => System.High_Order_First;

   for Basic_Header_Segment use record
      Immediate          at 0 range 1 .. 1;
      Opcode             at 0 range 2 .. 7;
      Final              at 1 range 0 .. 0;
      TotalAHSLength     at 4 range 0 .. 7;
      DataSegmentLength  at 5 range 0 .. 23;
      Initiator_Task_Tag at 16 range 0 .. 31;
   end record;

   --------------------------------------
   --  Login Request / Login Response  --
   --------------------------------------

   type Login_Request_Header is record
      Immediate          : Boolean                 := True;
      Opcode             : iSCSI.Types.Opcode_Type :=
        iSCSI.Types.Login_Request;
      Transit            : Boolean;
      Continue           : Boolean;
      CSG                : iSCSI.Types.Stage;
      NSG                : iSCSI.Types.Stage;
      Version_Max        : A0B.Types.Unsigned_8;
      Version_Min        : A0B.Types.Unsigned_8;
      TotalAHSLength     : A0B.Types.Unsigned_8;
      DataSegmentLength  : A0B.Types.Unsigned_24;
      ISID               : A0B.Types.Unsigned_48;
      TSIH               : A0B.Types.Unsigned_16;
      Initiator_Task_Tag : A0B.Types.Unsigned_32;
      CID                : A0B.Types.Unsigned_16;
      CmdSN              : A0B.Types.Unsigned_32;
      ExpStatSN          : A0B.Types.Unsigned_32;
   end record
     with Size                 => Basic_Header_Segment_Length * Byte_Size,
          Bit_Order            => System.High_Order_First,
          Scalar_Storage_Order => System.High_Order_First;

   for Login_Request_Header use record
      Immediate          at 0 range 1 .. 1;
      Opcode             at 0 range 2 .. 7;
      Transit            at 1 range 0 .. 0;
      Continue           at 1 range 1 .. 1;
      CSG                at 1 range 4 .. 5;
      NSG                at 1 range 6 .. 7;
      Version_Max        at 2 range 0 .. 7;
      Version_Min        at 3 range 0 .. 7;
      TotalAHSLength     at 4 range 0 .. 7;
      DataSegmentLength  at 5 range 0 .. 23;
      ISID               at 8 range 0 .. 47;
      TSIH               at 14 range 0 .. 15;
      Initiator_Task_Tag at 16 range 0 .. 31;
      CID                at 20 range 0 .. 15;
      CmdSN              at 24 range 0 .. 31;
      ExpStatSN          at 28 range 0 .. 31;
   end record;

   type Login_Response_Header is record
      Reserved_0_0_1     : A0B.Types.Reserved_2    := A0B.Types.Zero;
      Opcode             : iSCSI.Types.Opcode_Type :=
        iSCSI.Types.Login_Response;
      Transit            : Boolean;
      Continue           : Boolean;
      Reserved_1_2_3     : A0B.Types.Reserved_2    := A0B.Types.Zero;
      CSG                : iSCSI.Types.Stage;
      NSG                : iSCSI.Types.Stage;
      Version_Max        : A0B.Types.Unsigned_8;
      Version_Active     : A0B.Types.Unsigned_8;
      TotalAHSLength     : A0B.Types.Unsigned_8;
      DataSegmentLength  : A0B.Types.Unsigned_24;
      ISID               : A0B.Types.Unsigned_48;
      TSIH               : A0B.Types.Unsigned_16;
      Initiator_Task_Tag : A0B.Types.Unsigned_32;
      Reserved_20        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_21        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_22        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_23        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      StatSN             : A0B.Types.Unsigned_32;
      ExpCmdSN           : A0B.Types.Unsigned_32;
      MaxCmdSN           : A0B.Types.Unsigned_32;
      Status_Class       : A0B.Types.Unsigned_8;
      Status_Detail      : A0B.Types.Unsigned_8;
      Reserved_38        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_39        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_40        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_41        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_42        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_43        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_44        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_45        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_46        : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_47        : A0B.Types.Reserved_8    := A0B.Types.Zero;
   end record
     with Size                 => Basic_Header_Segment_Length * Byte_Size,
          Bit_Order            => System.High_Order_First,
          Scalar_Storage_Order => System.High_Order_First;

   for Login_Response_Header use record
      Reserved_0_0_1     at 0 range 0 .. 1;
      Opcode             at 0 range 2 .. 7;
      Transit            at 1 range 0 .. 0;
      Continue           at 1 range 1 .. 1;
      Reserved_1_2_3     at 1 range 2 .. 3;
      CSG                at 1 range 4 .. 5;
      NSG                at 1 range 6 .. 7;
      Version_Max        at 2 range 0 .. 7;
      Version_Active     at 3 range 0 .. 7;
      TotalAHSLength     at 4 range 0 .. 7;
      DataSegmentLength  at 5 range 0 .. 23;
      ISID               at 8 range 0 .. 47;
      TSIH               at 14 range 0 .. 15;
      Initiator_Task_Tag at 16 range 0 .. 31;
      Reserved_20        at 20 range 0 .. 7;
      Reserved_21        at 21 range 0 .. 7;
      Reserved_22        at 22 range 0 .. 7;
      Reserved_23        at 23 range 0 .. 7;
      StatSN             at 24 range 0 .. 31;
      ExpCmdSN           at 28 range 0 .. 31;
      MaxCmdSN           at 32 range 0 .. 31;
      Status_Class       at 36 range 0 .. 7;
      Status_Detail      at 37 range 0 .. 7;
      Reserved_38        at 38 range 0 .. 7;
      Reserved_39        at 39 range 0 .. 7;
      Reserved_40        at 40 range 0 .. 7;
      Reserved_41        at 41 range 0 .. 7;
      Reserved_42        at 42 range 0 .. 7;
      Reserved_43        at 43 range 0 .. 7;
      Reserved_44        at 44 range 0 .. 7;
      Reserved_45        at 45 range 0 .. 7;
      Reserved_46        at 46 range 0 .. 7;
      Reserved_47        at 47 range 0 .. 7;
   end record;

   --------------------
   --  SCSI Command  --
   --------------------

   type SCSI_Command_Descriptor_Block is
     new A0B.Types.Arrays.Unsigned_8_Array (0 .. 15);

   type SCSI_Command_Header is record
      Reserved_0_0_0                : A0B.Types.Reserved_1    := A0B.Types.Zero;
      Immediate                     : Boolean;
      Opcode                        : iSCSI.Types.Opcode_Type :=
        iSCSI.Types.SCSI_Command;
      Final                         : Boolean;
      Read                          : Boolean;
      Write                         : Boolean;
      Reserved_1_3_4                : A0B.Types.Reserved_2    := A0B.Types.Zero;
      Attr                          : A0B.Types.Unsigned_3;
      Reserved_2                    : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Reserved_3                    : A0B.Types.Reserved_8    := A0B.Types.Zero;
      TotalAHSLength                : A0B.Types.Unsigned_8;
      DataSegmentLength             : A0B.Types.Unsigned_24;
      Logical_Unit_Number           : A0B.Types.Unsigned_64;
      Initiator_Task_Tag            : A0B.Types.Unsigned_32;
      Expected_Data_Transfer_Length : A0B.Types.Unsigned_32;
      CmdSN                         : A0B.Types.Unsigned_32;
      ExpStatSN                     : A0B.Types.Unsigned_32;
      SCSI_Command_Descriptor_Block : iSCSI.PDUs.SCSI_Command_Descriptor_Block;
   end record
     with Size                 => Basic_Header_Segment_Length * Byte_Size,
          Bit_Order            => System.High_Order_First,
          Scalar_Storage_Order => System.High_Order_First;

   for SCSI_Command_Header use record
      Reserved_0_0_0                at 0 range 0 .. 0;
      Immediate                     at 0 range 1 .. 1;
      Opcode                        at 0 range 2 .. 7;
      Final                         at 1 range 0 .. 0;
      Read                          at 1 range 1 .. 1;
      Write                         at 1 range 2 .. 2;
      Reserved_1_3_4                at 1 range 3 .. 4;
      Attr                          at 1 range 5 .. 7;
      Reserved_2                    at 2 range 0 .. 7;
      Reserved_3                    at 3 range 0 .. 7;
      TotalAHSLength                at 4 range 0 .. 7;
      DataSegmentLength             at 5 range 0 .. 23;
      Logical_Unit_Number           at 8 range 0 .. 63;
      Initiator_Task_Tag            at 16 range 0 .. 31;
      Expected_Data_Transfer_Length at 20 range 0 .. 31;
      CmdSN                         at 24 range 0 .. 31;
      ExpStatSN                     at 28 range 0 .. 31;
      SCSI_Command_Descriptor_Block at 32 range 0 .. 127;
   end record;

   --------------------
   --  SCSI Data-In  --
   --------------------

   type SCSI_Data_In_Header is record
      Reserved_0_0_0      : A0B.Types.Reserved_1    := A0B.Types.Zero;
      Reserved_0_1_1      : A0B.Types.Reserved_1    := A0B.Types.Zero;
      Opcode              : iSCSI.Types.Opcode_Type := iSCSI.Types.SCSI_Data_In;
      Final               : Boolean;
      Acknowledge         : Boolean;
      Residual_Overflow   : Boolean;
      Residual_Underflow  : Boolean;
      Status_Flag         : Boolean;
      Reserved_1_2_4      : A0B.Types.Reserved_3    := A0B.Types.Zero;
      Reserved_2          : A0B.Types.Reserved_8    := A0B.Types.Zero;
      Status              : SCSI.SAM5.STATUS;
      TotalAHSLength      : A0B.Types.Unsigned_8;
      DataSegmentLength   : A0B.Types.Unsigned_24;
      Logical_Unit_Number : A0B.Types.Unsigned_64;
      Initiator_Task_Tag  : A0B.Types.Unsigned_32;
      Target_Transfer_Tag : A0B.Types.Unsigned_32;
      StatSN              : A0B.Types.Unsigned_32;
      ExpCmdSN            : A0B.Types.Unsigned_32;
      MaxCmdSN            : A0B.Types.Unsigned_32;
      DataSN              : A0B.Types.Unsigned_32;
      Buffer_Offset       : A0B.Types.Unsigned_32;
      Residual_Count      : A0B.Types.Unsigned_32;
   end record
     with Size                 => Basic_Header_Segment_Length * Byte_Size,
          Bit_Order            => System.High_Order_First,
          Scalar_Storage_Order => System.High_Order_First;

   for SCSI_Data_In_Header use record
      Reserved_0_0_0      at 0 range 0 .. 0;
      Reserved_0_1_1      at 0 range 1 .. 1;
      Opcode              at 0 range 2 .. 7;
      Final               at 1 range 0 .. 0;
      Acknowledge         at 1 range 1 .. 1;
      Reserved_1_2_4      at 1 range 2 .. 4;
      Residual_Overflow   at 1 range 5 .. 5;
      Residual_Underflow  at 1 range 6 .. 6;
      Status_Flag         at 1 range 7 .. 7;
      Reserved_2          at 2 range 0 .. 7;
      Status              at 3 range 0 .. 7;
      TotalAHSLength      at 4 range 0 .. 7;
      DataSegmentLength   at 5 range 0 .. 23;
      Logical_Unit_Number at 8 range 0 .. 63;
      Initiator_Task_Tag  at 16 range 0 .. 31;
      Target_Transfer_Tag at 20 range 0 .. 31;
      StatSN              at 24 range 0 .. 31;
      ExpCmdSN            at 28 range 0 .. 31;
      MaxCmdSN            at 32 range 0 .. 31;
      DataSN              at 36 range 0 .. 31;
      Buffer_Offset       at 40 range 0 .. 31;
      Residual_Count      at 44 range 0 .. 31;
   end record;

end iSCSI.PDUs;
