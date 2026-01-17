--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

package iSCSI.Target.Login is

   type Login_Handler is private;

   procedure Process
     (Header_Address        : System.Address;
      Request_Data_Address  : System.Address;
      Response_Data_Address : System.Address);

private

   type Login_Handler is record
      null;
   end record;

end iSCSI.Target.Login;
