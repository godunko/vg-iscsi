--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

with A0B.Types;

with SCSI.Commands.SBC;

package Target.File is

   function Last_LBA return A0B.Types.Unsigned_64;

   function Data_Length
     (Transfer_Length : A0B.Types.Unsigned_32) return A0B.Types.Unsigned_64;

   procedure Read
     (Descriptor      : SCSI.Commands.SBC.READ_Command_Descriptor;
      Storage_Address : System.Address;
      Data_Length     : out A0B.Types.Unsigned_32);

end Target.File;
