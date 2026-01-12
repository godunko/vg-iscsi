--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package iSCSI.Types with Pure is

   type Opcode_Type is private;

   NOP_Out                               : constant Opcode_Type;
   SCSI_Command                          : constant Opcode_Type;
   SCSI_Task_Management_Function_Request : constant Opcode_Type;
   Login_Request                         : constant Opcode_Type;
   Text_Request                          : constant Opcode_Type;
   SCSI_Data_Out                         : constant Opcode_Type;
   Logout_Request                        : constant Opcode_Type;
   SNACK_Request                         : constant Opcode_Type;

private

   type Opcode_Type is mod 2 ** 5 with Size => 5;

   NOP_Out                               : constant Opcode_Type := 16#00#;
   SCSI_Command                          : constant Opcode_Type := 16#01#;
   SCSI_Task_Management_Function_Request : constant Opcode_Type := 16#02#;
   Login_Request                         : constant Opcode_Type := 16#03#;
   Text_Request                          : constant Opcode_Type := 16#04#;
   SCSI_Data_Out                         : constant Opcode_Type := 16#05#;
   Logout_Request                        : constant Opcode_Type := 16#06#;
   SNACK_Request                         : constant Opcode_Type := 16#10#;

end iSCSI.Types;
