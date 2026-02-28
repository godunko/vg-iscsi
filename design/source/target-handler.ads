--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

with A0B.Callbacks;
with A0B.Types.Arrays;

with SCSI.SAM5;

package Target.Handler is

   procedure Execute_Command
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      On_Finished : A0B.Callbacks.Callback);
   --  Data_Out_Storage  : System.Address;
   --  Data_Out_Capacity : A0B.Types.Unsigned_32;
   --  Data_In_Storage   : System.Address;
   --  Data_In_Capacity  : A0B.Types.Unsigned_32;

   procedure Data_In
     (Storage_Address : System.Address;
      Data_Length     : out A0B.Types.Unsigned_32);

   procedure Data_Out
     (Buffer_Offset   : A0B.Types.Unsigned_32;
      Storage_Address : System.Address;
      Data_Length     : A0B.Types.Unsigned_32);

   function Has_Data_In return Boolean;

   function Status return SCSI.SAM5.STATUS;

   function Sense return SCSI.SAM5.Sense_Data;

   function Write_Data_Length return A0B.Types.Unsigned_64;
   --  Expected amount of data to be data-out by the command.

   function Read_Data_Length return A0B.Types.Unsigned_64;
   --  Expected maximum amount of data to be data-in by the command. Actual
   --  transferred amount might be less even when command is executed
   --  successfully.

private

   type Length_Check_Rule is (Default, USB_MSC_BOOT, iSCSI);
   --  Rules of command descriptor block length check
   --
   --  @enum Default      Default according to SPC/SBC
   --  @enum USB_MSC_BOOT
   --    Command descriptor block of some commands can be 12 bytes length
   --  @enum iSCSI
   --    All command descriptor block has at least 16 bytes length

   Length_Check : Length_Check_Rule := iSCSI;

end Target.Handler;
