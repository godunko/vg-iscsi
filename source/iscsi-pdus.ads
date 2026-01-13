--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  PDUs defined by [RFC7143]

with System;

with A0B.Types;

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

   type Login_Request_Header is record
      Immediate          : Boolean                 := True;
      Opcode             : iSCSI.Types.Opcode_Type :=
        iSCSI.Types.Login_Request;
      Transit            : Boolean;
      Continue           : Boolean;
      CSG                : A0B.Types.Unsigned_2;
      NSG                : A0B.Types.Unsigned_2;
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

end iSCSI.PDUs;
