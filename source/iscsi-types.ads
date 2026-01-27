--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package iSCSI.Types with Pure is

   type Opcode_Type is private;

   NOP_Out                                : constant Opcode_Type;
   SCSI_Command                           : constant Opcode_Type;
   SCSI_Task_Management_Function_Request  : constant Opcode_Type;
   Login_Request                          : constant Opcode_Type;
   Text_Request                           : constant Opcode_Type;
   SCSI_Data_Out                          : constant Opcode_Type;
   Logout_Request                         : constant Opcode_Type;
   SNACK_Request                          : constant Opcode_Type;

   NOP_In                                 : constant Opcode_Type;
   SCSI_Response                          : constant Opcode_Type;
   SCSI_Task_Management_Function_Response : constant Opcode_Type;
   Login_Response                         : constant Opcode_Type;
   Text_Response                          : constant Opcode_Type;
   SCSI_Data_In                           : constant Opcode_Type;
   Logout_Response                        : constant Opcode_Type;
   Ready_To_Transfer                      : constant Opcode_Type;
   Asynchronous_Message                   : constant Opcode_Type;
   Reject                                 : constant Opcode_Type;

   type Stage is private;

   SecurityNegotiation         : constant Stage;
   LoginOperationalNegotiation : constant Stage;
   FullFeaturePhase            : constant Stage;

private

   type Opcode_Type is mod 2 ** 6 with Size => 6;

   NOP_Out                                : constant Opcode_Type := 16#00#;
   SCSI_Command                           : constant Opcode_Type := 16#01#;
   SCSI_Task_Management_Function_Request  : constant Opcode_Type := 16#02#;
   Login_Request                          : constant Opcode_Type := 16#03#;
   Text_Request                           : constant Opcode_Type := 16#04#;
   SCSI_Data_Out                          : constant Opcode_Type := 16#05#;
   Logout_Request                         : constant Opcode_Type := 16#06#;
   SNACK_Request                          : constant Opcode_Type := 16#10#;
   NOP_In                                 : constant Opcode_Type := 16#20#;
   SCSI_Response                          : constant Opcode_Type := 16#21#;
   SCSI_Task_Management_Function_Response : constant Opcode_Type := 16#22#;
   Login_Response                         : constant Opcode_Type := 16#23#;
   Text_Response                          : constant Opcode_Type := 16#24#;
   SCSI_Data_In                           : constant Opcode_Type := 16#25#;
   Logout_Response                        : constant Opcode_Type := 16#26#;
   Ready_To_Transfer                      : constant Opcode_Type := 16#31#;
   Asynchronous_Message                   : constant Opcode_Type := 16#32#;
   Reject                                 : constant Opcode_Type := 16#3F#;

   type Stage is mod 2 ** 2 with Size => 2;

   SecurityNegotiation         : constant Stage := 0;
   LoginOperationalNegotiation : constant Stage := 1;
   FullFeaturePhase            : constant Stage := 3;

end iSCSI.Types;
