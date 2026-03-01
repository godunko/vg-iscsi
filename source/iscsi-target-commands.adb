--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body iSCSI.Target.Commands is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                : out Abstract_Command;
      SCSI_Command_Header : iSCSI.PDUs.SCSI_Command_Header) is
   begin
      --  XXX Not processed fields:
      --    - Final
      --    - Attr
      --    - CmdSN
      --    - ExpStatSN

      Self.Logical_Unit_Number        :=
        SCSI_Command_Header.Logical_Unit_Number;
      Self.Immediate                  := SCSI_Command_Header.Immediate;
      Self.Initiator_Task_Tag         := SCSI_Command_Header.Initiator_Task_Tag;
      Self.Write                      := SCSI_Command_Header.Write;
      Self.Read                       := SCSI_Command_Header.Read;
      Self.Write_Data_Transfer_Length := 0;
      Self.Read_Data_Transfer_Length  := 0;
      Self.DataSN                     := 0;
      Self.R2TSN                      := 0;

      if SCSI_Command_Header.Write then
         Self.Write_Expected_Data_Transfer_Length :=
           SCSI_Command_Header.Expected_Data_Transfer_Length;

         if SCSI_Command_Header.Read then
            --  XXX Process Bidirectional Read Expected Data Transfer Length AHS
            raise Program_Error;

         else
            Self.Read_Expected_Data_Transfer_Length := 0;
         end if;

      elsif SCSI_Command_Header.Read then
         Self.Write_Expected_Data_Transfer_Length := 0;
         Self.Read_Expected_Data_Transfer_Length  :=
           SCSI_Command_Header.Expected_Data_Transfer_Length;

      else
         Self.Write_Expected_Data_Transfer_Length := 0;
         Self.Read_Expected_Data_Transfer_Length  := 0;
      end if;

      Self.CDB_Storage (0 .. 15) :=
        A0B.Types.Arrays.Unsigned_8_Array
          (SCSI_Command_Header.SCSI_Command_Descriptor_Block);
      Self.CDB_Length            := 16;
   end Initialize;

end iSCSI.Target.Commands;
