--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with A0B.Types;

with SCSI.Buffers;

package iSCSI.Target.Connections with Pure is

   type Connection is tagged limited record
      StatSN          : A0B.Types.Unsigned_32;

      Header_Buffer   : SCSI.Buffers.Data_Buffer;
      Data_In_Buffer  : SCSI.Buffers.Data_Buffer;
      Data_Out_Buffer : SCSI.Buffers.Data_Buffer;
   end record;

end iSCSI.Target.Connections;
