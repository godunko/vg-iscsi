--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

with A0B.Types.Arrays;

package Target.Handler is

   procedure Process_Command
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array);

   procedure Data_In
     (Storage_Address : System.Address;
      Data_Length     : out A0B.Types.Unsigned_32);

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
