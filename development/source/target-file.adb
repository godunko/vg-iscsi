--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Target.File is

   Capacity     : constant := 1_024 * 1_024 * 1_024;
   Block_Length : constant := 512;

   --------------
   -- Last_LBA --
   --------------

   function Last_LBA return A0B.Types.Unsigned_64 is
      use type A0B.Types.Unsigned_64;

   begin
      return Capacity / Block_Length - 1;
   end Last_LBA;

end Target.File;
