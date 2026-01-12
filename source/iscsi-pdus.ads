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

end iSCSI.PDUs;
