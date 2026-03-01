--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with A0B.Types.Arrays;

with iSCSI.PDUs;

package iSCSI.Target.Commands with Pure is

   type Abstract_Command is tagged limited record
      Initiator_Task_Tag                  : A0B.Types.Unsigned_32;
      Logical_Unit_Number                 : A0B.Types.Unsigned_64;
      Immediate                           : Boolean;
      Write                               : Boolean;
      Read                                : Boolean;
      Write_Expected_Data_Transfer_Length : A0B.Types.Unsigned_32;
      Read_Expected_Data_Transfer_Length  : A0B.Types.Unsigned_32;
      Write_Data_Transfer_Length          : A0B.Types.Unsigned_32;
      Read_Data_Transfer_Length           : A0B.Types.Unsigned_32;
      DataSN                              : A0B.Types.Unsigned_32;
      R2TSN                               : A0B.Types.Unsigned_32;
      CDB_Storage                         :
        A0B.Types.Arrays.Unsigned_8_Array (0 .. 31);
      --  Reserve space for CDB up to 32 bytes length. It is enough for all
      --  commands defined in SPC-5/SBC-4.
      CDB_Length                          : A0B.Types.Unsigned_32;
   end record;

   procedure Initialize
     (Self                : out Abstract_Command;
      SCSI_Command_Header : iSCSI.PDUs.SCSI_Command_Header);
   --  Initialize object by information from the SCSI Command PDU header.

end iSCSI.Target.Commands;
