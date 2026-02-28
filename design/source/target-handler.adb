--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with System.Storage_Elements;

with Ada.Text_IO; use Ada.Text_IO;

with A0B.Types.Big_Endian;

with SCSI.Commands.SBC;
with SCSI.Commands.SPC;
with SCSI.Decoders.SBC.READ_6;
with SCSI.Decoders.SBC.READ_10;
with SCSI.Decoders.SBC.WRITE_6;
with SCSI.Decoders.SBC.WRITE_10;
with SCSI.Decoders.SPC.INQUIRY;
with SCSI.Decoders.SPC.MODE_SENSE_6;
with SCSI.Decoders.SPC.REPORT_LUNS;
with SCSI.SBC4.CDB;
with SCSI.SBC4.Data;
with SCSI.SPC5.CDB;
with SCSI.SPC5.Data;
with SCSI.SPC5.Mode;
with SCSI.SPC5.VPD;

with Target.File;

package body Target.Handler is

   USB_MSC_BOOT_CDB_Length  : constant := 12;
   iSCSI_CDB_Minumum_Length : constant := 16;

   function Decode_READ_CAPACITY_16
     (CDB_Storage : SCSI.SPC5.CDB.SERVICE_ACTION_IN_16_CDB;
      Descriptor  : out SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor)
      return Boolean;

   function Decode_TEST_UNIT_READY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor)
      return Boolean;

   procedure Failure_INVALID_COMMAND_OPERATION_CODE;

   function Failure_INVALID_FIELD_IN_CDB return Boolean;

   procedure Execute_INQUIRY
     (Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor);

   procedure Execute_MODE_SENSE
     (Descriptor : SCSI.Commands.SPC.MODE_SENSE_Command_Descriptor);

   procedure Execute_READ
     (Descriptor : SCSI.Commands.SBC.READ_Command_Descriptor);

   procedure Execute_READ_CAPACITY
     (Descriptor : SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor);

   procedure Execute_REPORT_LUNS
     (Descriptor : SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor);

   procedure Execute_TEST_UNIT_READY
     (Descriptor : SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor);

   procedure Execute_WRITE
     (Descriptor : SCSI.Commands.SBC.WRITE_Command_Descriptor);

   procedure Encode_READ_CAPACITY_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   procedure Encode_MODE_SENSE_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   procedure Encode_REPORT_LUNS_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   procedure Encode_Standard_INQUIRY_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   procedure Encode_Supported_VPD_Pages
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   type Operation_Kind is
     (None, Inquiry, Mode_Sense, Read, Read_Capacity, Report_Luns, Write);

   type Operation_Information (Kind : Operation_Kind := None) is record
      Status            : SCSI.SAM5.STATUS;
      Sense             : SCSI.SAM5.Sense_Data;
      Write_Data_Length : A0B.Types.Unsigned_64;
      Read_Data_Length  : A0B.Types.Unsigned_64;

      case Kind is
         when None =>
            null;

         when Inquiry =>
            Inquiry : SCSI.Commands.SPC.INQUIRY_Command_Descriptor;

         when Mode_Sense =>
            Mode_Sense : SCSI.Commands.SPC.MODE_SENSE_Command_Descriptor;

         when Read =>
            Read : SCSI.Commands.SBC.READ_Command_Descriptor;

         when Read_Capacity =>
            Read_Capacity : SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor;

         when Report_Luns =>
            Report_Luns : SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;

         when Write =>
            Write : SCSI.Commands.SBC.WRITE_Command_Descriptor;
      end case;
   end record;

   type SCSI_Decoder is
     new SCSI.Decoders.SPC.INQUIRY.INQUIRY_Decoder
       and SCSI.Decoders.SPC.MODE_SENSE_6.MODE_SENSE_6_Decoder
       and SCSI.Decoders.SBC.READ_6.READ_6_Decoder
       and SCSI.Decoders.SBC.READ_10.READ_10_Decoder
       and SCSI.Decoders.SPC.REPORT_LUNS.REPORT_LUNS_Decoder
       and SCSI.Decoders.SBC.WRITE_6.WRITE_6_Decoder
       and SCSI.Decoders.SBC.WRITE_10.WRITE_10_Decoder with null record;

   overriding function Length_Check_Mode
     (Self : SCSI_Decoder) return SCSI.Decoders.Length_Check_Modes;

   overriding procedure Set_INVALID_FIELD_IN_CDB
     (Self : in out SCSI_Decoder;
      Byte : A0B.Types.Unsigned_16);

   overriding procedure Set_INVALID_FIELD_IN_CDB
     (Self : in out SCSI_Decoder;
      Byte : A0B.Types.Unsigned_16;
      Bit  : A0B.Types.Unsigned_3);

   Operation : Operation_Information;

   type VPD_Builder is access procedure
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32);

   VPD : constant array (SCSI.SPC5.VPD_Page_Code) of VPD_Builder :=
     [SCSI.SPC5.Supported_VPD_Pages_VPD_Page_Code =>
        Encode_Supported_VPD_Pages'Access,
      others => null];

   Maximum_VPD : constant := 32;

   -------------
   -- Data_In --
   -------------

   procedure Data_In
     (Storage_Address : System.Address;
      Data_Length     : out A0B.Types.Unsigned_32) is
   begin
      Put_Line (">>> Data-In <<<");
      Put_Line (Operation'Image);
      Put_Line ("===============");

      case Operation.Kind is
         when None =>
            raise Program_Error;

         when Inquiry =>
            if Operation.Inquiry.EVPD then
               VPD (Operation.Inquiry.PAGE_CODE)
                 (Allocation_Length => Operation.Inquiry.ALLOCATION_LENGTH,
                  Storage_Address   => Storage_Address,
                  Data_Length       => Data_Length);

            else
               Encode_Standard_INQUIRY_Data
                 (Allocation_Length => Operation.Inquiry.ALLOCATION_LENGTH,
                  Storage_Address   => Storage_Address,
                  Data_Length       => Data_Length);
            end if;

         when Mode_Sense =>
            Encode_MODE_SENSE_Data
              (Allocation_Length => Operation.Mode_Sense.ALLOCATION_LENGTH,
               Storage_Address   => Storage_Address,
               Data_Length       => Data_Length);

         when Read =>
            Target.File.Read
              (Descriptor      => Operation.Read,
               Storage_Address => Storage_Address,
               Data_Length     => Data_Length);

         when Read_Capacity =>
            Encode_READ_CAPACITY_Data
              (Allocation_Length => Operation.Read_Capacity.ALLOCATION_LENGTH,
               Storage_Address   => Storage_Address,
               Data_Length       => Data_Length);

         when Report_Luns =>
            Encode_REPORT_LUNS_Data
              (Allocation_Length => Operation.Report_Luns.ALLOCATION_LENGTH,
               Storage_Address   => Storage_Address,
               Data_Length       => Data_Length);

         when Write =>
            raise Program_Error;
      end case;
   end Data_In;

   --------------
   -- Data_Out --
   --------------

   procedure Data_Out
     (Buffer_Offset   : A0B.Types.Unsigned_32;
      Storage_Address : System.Address;
      Data_Length     : A0B.Types.Unsigned_32) is
   begin
      Put_Line (">>> Data-Out <<<");
      Put_Line (Operation'Image);
      Put_Line ("===============");

      case Operation.Kind is
         when None =>
            raise Program_Error;

         when Inquiry =>
            raise Program_Error;

         when Mode_Sense =>
            raise Program_Error;

         when Read =>
            raise Program_Error;

         when Read_Capacity =>
            raise Program_Error;

         when Report_Luns =>
            raise Program_Error;

         when Write =>
            Target.File.Write
              (Descriptor      => Operation.Write,
               Buffer_Offset   => Buffer_Offset,
               Storage_Address => Storage_Address,
               Data_Length     => Data_Length);
      end case;
   end Data_Out;

   -----------------------------
   -- Decode_READ_CAPACITY_16 --
   -----------------------------

   function Decode_READ_CAPACITY_16
     (CDB_Storage : SCSI.SPC5.CDB.SERVICE_ACTION_IN_16_CDB;
      Descriptor  : out SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor)
      return Boolean
   is
      use type A0B.Types.Reserved_1;
      use type A0B.Types.Reserved_3;
      use type A0B.Types.Reserved_7;
      use type A0B.Types.Reserved_8;

   begin
      declare
         CDB : constant SCSI.SBC4.CDB.READ_CAPACITY_16_CDB
           with Import, Address => CDB_Storage'Address;

      begin
         if CDB.Reserved_1_7_5 /= A0B.Types.Zero
           or CDB.Reserved_14_7_1 /= A0B.Types.Zero
         then
            return Failure_INVALID_FIELD_IN_CDB;
         end if;

         if CDB.Obsolete_2 /= A0B.Types.Zero
           or CDB.Obsolete_3 /= A0B.Types.Zero
           or CDB.Obsolete_4 /= A0B.Types.Zero
           or CDB.Obsolete_5 /= A0B.Types.Zero
           or CDB.Obsolete_6 /= A0B.Types.Zero
           or CDB.Obsolete_7 /= A0B.Types.Zero
           or CDB.Obsolete_8 /= A0B.Types.Zero
           or CDB.Obsolete_9 /= A0B.Types.Zero
           or CDB.Obsolete_14_0_0 /= A0B.Types.Zero
         then
            return Failure_INVALID_FIELD_IN_CDB;
         end if;

         --  XXX CONTROL is not validated/decoded

         Descriptor :=
           (Variant           => SCSI.Commands.SBC.READ_CAPACITY_16,
            SERVICE_ACTION    => CDB.SERVICE_ACTION,
            ALLOCATION_LENGTH => CDB.ALLOCATION_LENGTH.Value);
      end;

      return True;
   end Decode_READ_CAPACITY_16;

   ----------------------------
   -- Decode_TEST_UNIT_READY --
   ----------------------------

   function Decode_TEST_UNIT_READY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor)
      return Boolean
   is
      use type A0B.Types.Reserved_8;

   begin
      case Length_Check is
         when Default =>
            if CDB_Storage'Length
                 /= SCSI.SPC5.CDB.TEST_UNIT_READY_CDB_Length
            then
               return Failure_INVALID_FIELD_IN_CDB;
            end if;

         when USB_MSC_BOOT =>
            if CDB_Storage'Length /= SCSI.SPC5.CDB.TEST_UNIT_READY_CDB_Length
              and CDB_Storage'Length /= USB_MSC_BOOT_CDB_Length
            then
               return Failure_INVALID_FIELD_IN_CDB;
            end if;

         when iSCSI =>
            if CDB_Storage'Length /= iSCSI_CDB_Minumum_Length then
               return Failure_INVALID_FIELD_IN_CDB;
            end if;
      end case;

      declare
         CDB : constant SCSI.SPC5.CDB.TEST_UNIT_READY_CDB
           with Import, Address => CDB_Storage'Address;

      begin
         if CDB.Reserved_1 /= A0B.Types.Zero
           or CDB.Reserved_2 /= A0B.Types.Zero
           or CDB.Reserved_3 /= A0B.Types.Zero
           or CDB.Reserved_4 /= A0B.Types.Zero
         then
            return Failure_INVALID_FIELD_IN_CDB;
         end if;

         --  XXX CONTROL is not validated/decoded

         Descriptor := (null record);
      end;

      return True;
   end Decode_TEST_UNIT_READY;

   ----------------------------
   -- Encode_MODE_SENSE_Data --
   ----------------------------

   procedure Encode_MODE_SENSE_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32) is
   begin
      case Operation.Mode_Sense.Variant is
         when SCSI.Commands.SPC.MODE_SENSE_6 =>
            declare
               Data : SCSI.SPC5.Mode.MODE_6_Header
                 with Import, Address => Storage_Address;

            begin
               Data :=
                 (MODE_DATA_LENGTH          => 3,
                  MEDIUM_TYPE               => 0,
                  DEVICE_SPECIFIC_PARAMETER => 0,
                  BLOCK_DESCRIPTOR_LENGTH   => 0);

               Data_Length :=
                 A0B.Types.Unsigned_32'Min
                   (Allocation_Length,
                    SCSI.SPC5.Mode.MODE_6_Header_Length);
            end;

         when SCSI.Commands.SPC.MODE_SENSE_10 =>
            raise Program_Error;
      end case;
   end Encode_MODE_SENSE_Data;

   -------------------------------
   -- Encode_READ_CAPACITY_Data --
   -------------------------------

   procedure Encode_READ_CAPACITY_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32) is
   begin
      case Operation.Read_Capacity.Variant is
         when SCSI.Commands.SBC.READ_CAPACITY_10 =>
            raise Program_Error;

         when SCSI.Commands.SBC.READ_CAPACITY_16 =>
            declare
               Data : SCSI.SBC4.Data.READ_CAPACITY_16_Data
                 with Import, Address => Storage_Address;

            begin
               Data :=
                 (RETURNED_LOGICAL_BLOCK_ADDRESS             =>
                    (Value => Target.File.Last_LBA),
                  BLOCK_LENGTH_IN_BYTES                      => (Value => 512),
                  P_TYPE                                     => 0,
                  PROT_EN                                    => False,
                  P_I_EXPONENT                               => 0,
                  LOGICAL_BLOCKS_PER_PHYSICAL_BLOCK_EXPONENT => 0,
                  LOWEST_ALIGNED_LOGICAL_BLOCK_ADDRESS       => (Value => 0),
                  others => <>);

               Data_Length :=
                 A0B.Types.Unsigned_32'Min
                   (Allocation_Length,
                    SCSI.SBC4.Data.READ_CAPACITY_16_Data_Length);
            end;
      end case;
   end Encode_READ_CAPACITY_Data;

   -----------------------------
   -- Encode_REPORT_LUNS_Data --
   -----------------------------

   procedure Encode_REPORT_LUNS_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32)
   is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_32;
      pragma Warnings (Off, "use clause for type ""*"" has no effect");
      --  XXX FSF GCC 15
      use type System.Storage_Elements.Storage_Offset;
      pragma Warnings (On, "use clause for type ""*"" has no effect");

      Header : SCSI.SPC5.Data.REPORT_LUNS_Data_Header
        with Import, Address => Storage_Address;
      LUNs   : array (A0B.Types.Unsigned_8 range 0 .. 0)
        of A0B.Types.Big_Endian.Unsigned_64
          with Import,
               Address =>
                 Storage_Address + SCSI.SPC5.Data.REPORT_LUNS_Data_Header_Length;

   begin
      if Operation.Report_Luns.SELECT_REPORT /= 0 then
         raise Program_Error;
      end if;

      LUNs (0) := (Value => 0);
      Header := (LUN_LIST_LENGTH => (Value => 8), others => <>);

      Data_Length :=
        A0B.Types.Unsigned_32'Min
          (Allocation_Length,
           SCSI.SPC5.Data.REPORT_LUNS_Data_Header_Length + 8);
   end Encode_REPORT_LUNS_Data;

   ----------------------------------
   -- Encode_Standard_INQUIRY_Data --
   ----------------------------------

   procedure Encode_Standard_INQUIRY_Data
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32)
   is
      SAM5        : constant := 16#00A0#;
      SBC4        : constant := 16#0600#;
      SPC5        : constant := 16#05C0#;
      USB_MSC_BOT : constant := 16#1730#;

      Data : SCSI.SPC5.Data.INQUIRY_Data
        with Import, Address => Storage_Address;

   begin
      Data :=
        (PERIPHERAL_QUALIFIER      => 0,
         PERIPHERAL_DEVICE_TYPE    => 0,
         RMB                       => True,
         LU_CONG                   => False,
         VERSION                   => 16#07#,
         NORMACA                   => False,
         HISUP                     => False,
         RESPONSE_DATA_FORMAT      => 16#02#,
         ADDITIONAL_LENGTH         => <>,
         SCCS                      => False,
         TPGS                      => 0,
         T3PC                      => False,
         PROTECT                   => False,
         ENCSERV                   => False,
         VS_6_5_5                  => False,
         MULTIP                    => False,
         CMDQUE                    => False,
         VS_7_0_0                  => False,
         T10_VENDOR_IDENTIFICATION => "VG      ",
         PRODUCT_IDENTIFICATION    => "CNC.Net         ",
         PRODUCT_REVISION_LEVEL    => "0.01",
         VERSION_DESCRIPTOR_1      => (Value => SAM5),
         VERSION_DESCRIPTOR_2      => (Value => 0),
         VERSION_DESCRIPTOR_3      => (Value => USB_MSC_BOT),
         VERSION_DESCRIPTOR_4      => (Value => SPC5),
         VERSION_DESCRIPTOR_5      => (Value => SBC4),
         VERSION_DESCRIPTOR_6      => (Value => 0),
         VERSION_DESCRIPTOR_7      => (Value => 0),
         VERSION_DESCRIPTOR_8      => (Value => 0),
         others                    => <>);

      Data_Length :=
        A0B.Types.Unsigned_32'Min
          (Allocation_Length,
           SCSI.SPC5.Data.INQUIRY_Data_Length);
   end Encode_Standard_INQUIRY_Data;

   --------------------------------
   -- Encode_Supported_VPD_Pages --
   --------------------------------

   procedure Encode_Supported_VPD_Pages
     (Allocation_Length : A0B.Types.Unsigned_32;
      Storage_Address   : System.Address;
      Data_Length       : out A0B.Types.Unsigned_32)
   is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_32;
      pragma Warnings (Off, "use clause for type ""*"" has no effect");
      --  XXX FSF GCC 15
      use type System.Storage_Elements.Storage_Offset;
      pragma Warnings (On, "use clause for type ""*"" has no effect");

      Header : SCSI.SPC5.VPD.VPD_Page_Header
        with Import, Address => Storage_Address;
      Pages  : array (A0B.Types.Unsigned_8 range 0 .. Maximum_VPD - 1)
        of SCSI.SPC5.VPD_Page_Code
          with Import,
               Address =>
                 Storage_Address + SCSI.SPC5.VPD.VPD_Page_Header_Length;
      Count  : A0B.Types.Unsigned_8 := 0;

   begin
      for J in VPD'Range loop
         if VPD (J) /= null then
            Pages (Count) := J;
            Count := @ + 1;
         end if;
      end loop;

      Header :=
        (PERIPHERAL_QUALIFIER   => 0,
         PERIPHERAL_DEVICE_TYPE => 0,
         PAGE_CODE              =>
           SCSI.SPC5.Supported_VPD_Pages_VPD_Page_Code,
         PAGE_LENGTH            => (Value => A0B.Types.Unsigned_16 (Count)));

      Data_Length :=
        A0B.Types.Unsigned_32'Min
          (Allocation_Length,
           SCSI.SPC5.VPD.VPD_Page_Header_Length
             + A0B.Types.Unsigned_32 (Count));
   end Encode_Supported_VPD_Pages;

   ---------------------
   -- Execute_INQUIRY --
   ---------------------

   procedure Execute_INQUIRY
     (Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor) is
   begin
      Put_Line ("SCSI: INQUIRY EVPD " & Descriptor.EVPD'Image);
      Put_Line ("SCSI: INQUIRY PAGE CODE" & Descriptor.PAGE_CODE'Image);
      Put_Line ("SCSI: INQUIRY ALLOCATION LENGTH" & Descriptor.ALLOCATION_LENGTH'Image);

      if Descriptor.EVPD then
         if VPD (Descriptor.PAGE_CODE) = null then
            Operation :=
              (Kind              => None,
               Status            => SCSI.SAM5.CHECK_CONDITION,
               Sense             => SCSI.SPC5.INVALID_FIELD_IN_CDB,
               Write_Data_Length => 0,
               Read_Data_Length  => 0);

            return;
         end if;
      --
      --  else
      --     Operation := (Inquiry, Descriptor);
      end if;

      Operation :=
        (Kind              => Inquiry,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  =>
           A0B.Types.Unsigned_64 (Descriptor.ALLOCATION_LENGTH),
         Inquiry           => Descriptor);
   end Execute_INQUIRY;

   ------------------------
   -- Execute_MODE_SENSE --
   ------------------------

   procedure Execute_MODE_SENSE
     (Descriptor : SCSI.Commands.SPC.MODE_SENSE_Command_Descriptor)
   is
      use type A0B.Types.Unsigned_2;
      use type A0B.Types.Unsigned_8;
      use type SCSI.SPC5.Mode_Page_Code;

   begin
      if Descriptor.PC /= 0
        or Descriptor.SUBPAGE_CODE /= 0
        or Descriptor.PAGE_CODE /= SCSI.SPC5.All_Pages
      then
         Operation :=
           (Kind              => None,
            Status            => SCSI.SAM5.CHECK_CONDITION,
            Sense             => SCSI.SPC5.INVALID_FIELD_IN_CDB,
            Write_Data_Length => 0,
            Read_Data_Length  => 0);

         return;
      end if;

      Operation :=
        (Kind              => Mode_Sense,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  =>
           A0B.Types.Unsigned_64 (Descriptor.ALLOCATION_LENGTH),
         Mode_Sense        => Descriptor);
   end Execute_MODE_SENSE;

   ------------------
   -- Execute_READ --
   ------------------

   procedure Execute_READ
     (Descriptor : SCSI.Commands.SBC.READ_Command_Descriptor) is
   begin
      Operation :=
        (Kind              => Read,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  =>
           Target.File.Data_Length (Descriptor.TRANSFER_LENGTH),
         Read              => Descriptor);
   end Execute_READ;

   ---------------------------
   -- Execute_READ_CAPACITY --
   ---------------------------

   procedure Execute_READ_CAPACITY
     (Descriptor : SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor) is
   begin
      Operation :=
        (Kind              => Read_Capacity,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  =>
           A0B.Types.Unsigned_64 (Descriptor.ALLOCATION_LENGTH),
         Read_Capacity     => Descriptor);
   end Execute_READ_CAPACITY;

   -------------------------
   -- Execute_REPORT_LUNS --
   -------------------------

   procedure Execute_REPORT_LUNS
     (Descriptor : SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor)
   is
      use type A0B.Types.Unsigned_8;

   begin
      if Descriptor.SELECT_REPORT /= 0 then
         raise Program_Error;
      end if;

      Operation :=
        (Kind              => Report_Luns,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  =>
           A0B.Types.Unsigned_64 (Descriptor.ALLOCATION_LENGTH),
         Report_Luns       => Descriptor);
   end Execute_REPORT_LUNS;

   -----------------------------
   -- Execute_TEST_UNIT_READY --
   -----------------------------

   procedure Execute_TEST_UNIT_READY
     (Descriptor : SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor)
   is
      pragma Unreferenced (Descriptor);

   begin
      Operation :=
        (Kind              => None,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length => 0,
         Read_Data_Length  => 0);
   end Execute_TEST_UNIT_READY;

   -------------------
   -- Execute_WRITE --
   -------------------

   procedure Execute_WRITE
     (Descriptor : SCSI.Commands.SBC.WRITE_Command_Descriptor) is
   begin
      Operation :=
        (Kind              => Write,
         Status            => SCSI.SAM5.GOOD,
         Sense             => SCSI.SPC5.NO_SENSE,
         Write_Data_Length =>
           Target.File.Data_Length (Descriptor.TRANSFER_LENGTH),
         Read_Data_Length  => 0,
         Write             => Descriptor);
   end Execute_WRITE;

   --------------------------------------------
   -- Failure_INVALID_COMMAND_OPERATION_CODE --
   --------------------------------------------

   procedure Failure_INVALID_COMMAND_OPERATION_CODE is
   begin
      Operation :=
        (Kind              => None,
         Status            => SCSI.SAM5.CHECK_CONDITION,
         Sense             => SCSI.SPC5.INVALID_COMMAND_OPERATION_CODE,
         Write_Data_Length => 0,
         Read_Data_Length  => 0);
   end Failure_INVALID_COMMAND_OPERATION_CODE;

   ----------------------------------
   -- Failure_INVALID_FIELD_IN_CDB --
   ----------------------------------

   function Failure_INVALID_FIELD_IN_CDB return Boolean is
   begin
      Operation :=
        (Kind              => None,
         Status            => SCSI.SAM5.CHECK_CONDITION,
         Sense             => SCSI.SPC5.INVALID_FIELD_IN_CDB,
         Write_Data_Length => 0,
         Read_Data_Length  => 0);

      return False;
   end Failure_INVALID_FIELD_IN_CDB;

   -----------------
   -- Has_Data_In --
   -----------------

   function Has_Data_In return Boolean is
   begin
      case Operation.Kind is
         when None =>
            return False;

         when Inquiry =>
            return True;

         when Mode_Sense =>
            return True;

         when Read =>
            return True;

         when Read_Capacity =>
            return True;

         when Report_Luns =>
            return True;

         when Write =>
            raise Program_Error;
      end case;
   end Has_Data_In;

   -----------------------
   -- Length_Check_Mode --
   -----------------------

   overriding function Length_Check_Mode
     (Self : SCSI_Decoder) return SCSI.Decoders.Length_Check_Modes is
   begin
      return SCSI.Decoders.iSCSI;
   end Length_Check_Mode;

   ---------------------
   -- Process_Command --
   ---------------------

   procedure Process_Command
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array)
   is
      Decoder : SCSI_Decoder;

   begin
      if CDB_Storage'Length = 0 then
         raise Program_Error;
         --  XXX INVALID_FIELD_IN_CDB

         --  return;
      end if;

      declare
         Operation : constant SCSI.SAM5.OPERATION_CODE
           with Import, Address => CDB_Storage'Address;

      begin
         case Operation is
            when SCSI.SPC5.INQUIRY =>
               declare
                  Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor;

               begin
                  if Decoder.Decode_INQUIRY (CDB_Storage, Descriptor) then
                     Execute_INQUIRY (Descriptor);
                  end if;
               end;

            when SCSI.SPC5.MODE_SENSE_6 =>
               declare
                  Descriptor :
                    SCSI.Commands.SPC.MODE_SENSE_Command_Descriptor;

               begin
                  if Decoder.Decode_MODE_SENSE_6 (CDB_Storage, Descriptor) then
                     Execute_MODE_SENSE (Descriptor);
                  end if;
               end;

            when SCSI.SBC4.READ_6 =>
               declare
                  Descriptor : SCSI.Commands.SBC.READ_Command_Descriptor;

               begin
                  if Decoder.Decode_READ_6 (CDB_Storage, Descriptor) then
                     Execute_READ (Descriptor);
                  end if;
               end;

            when SCSI.SBC4.READ_10 =>
               declare
                  Descriptor : SCSI.Commands.SBC.READ_Command_Descriptor;

               begin
                  if Decoder.Decode_READ_10 (CDB_Storage, Descriptor) then
                     Execute_READ (Descriptor);
                  end if;
               end;

            when SCSI.SPC5.REPORT_LUNS =>
               declare
                  Descriptor :
                    SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;

               begin
                  if Decoder.Decode_REPORT_LUNS (CDB_Storage, Descriptor) then
                     Execute_REPORT_LUNS (Descriptor);
                  end if;
               end;

            when SCSI.SPC5.SERVICE_ACTION_IN_16 =>
               if CDB_Storage'Length
                    /= SCSI.SPC5.CDB.SERVICE_ACTION_IN_16_CDB_Length
               then
                  --  Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

                  raise Program_Error;
               end if;

               declare
                  CDB : constant SCSI.SPC5.CDB.SERVICE_ACTION_IN_16_CDB
                    with Import, Address => CDB_Storage'Address;

               begin
                  case CDB.SERVICE_ACTION is
                     when SCSI.SBC4.READ_CAPACITY_16 =>
                        declare
                           Descriptor :
                             SCSI.Commands.SBC.READ_CAPACITY_Command_Descriptor;

                        begin
                           if Decode_READ_CAPACITY_16 (CDB, Descriptor) then
                              Execute_READ_CAPACITY (Descriptor);
                           end if;
                        end;

                     when others =>
                        raise Program_Error;
                  end case;
               end;

            when SCSI.SPC5.TEST_UNIT_READY =>
               declare
                  Descriptor :
                    SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor;

               begin
                  if Decode_TEST_UNIT_READY (CDB_Storage, Descriptor) then
                     Execute_TEST_UNIT_READY (Descriptor);

                  else
                     raise Program_Error;
                     --  Command_Decode_Failure (Sense);
                  end if;
               end;

            when SCSI.SBC4.WRITE_6 =>
               declare
                  Descriptor : SCSI.Commands.SBC.WRITE_Command_Descriptor;

               begin
                  if Decoder.Decode_WRITE_6 (CDB_Storage, Descriptor) then
                     Execute_WRITE (Descriptor);
                  end if;
               end;

            when SCSI.SBC4.WRITE_10 =>
               declare
                  Descriptor : SCSI.Commands.SBC.WRITE_Command_Descriptor;

               begin
                  if Decoder.Decode_WRITE_10 (CDB_Storage, Descriptor) then
                     Execute_WRITE (Descriptor);
                  end if;
               end;

            when others =>
               Failure_INVALID_COMMAND_OPERATION_CODE;
         end case;
      end;
   end Process_Command;

   ----------------------
   -- Read_Data_Length --
   ----------------------

   function Read_Data_Length return A0B.Types.Unsigned_64 is
   begin
      return Operation.Read_Data_Length;
   end Read_Data_Length;

   -----------
   -- Sense --
   -----------

   function Sense return SCSI.SAM5.Sense_Data is
   begin
      return Operation.Sense;
   end Sense;

   ------------------------------
   -- Set_INVALID_FIELD_IN_CDB --
   ------------------------------

   overriding procedure Set_INVALID_FIELD_IN_CDB
     (Self : in out SCSI_Decoder;
      Byte : A0B.Types.Unsigned_16) is
   begin
      Operation :=
        (Kind              => None,
         Status            => SCSI.SAM5.CHECK_CONDITION,
         Sense             => SCSI.SPC5.INVALID_FIELD_IN_CDB,
         Write_Data_Length => 0,
         Read_Data_Length  => 0);
   end Set_INVALID_FIELD_IN_CDB;

   ------------------------------
   -- Set_INVALID_FIELD_IN_CDB --
   ------------------------------

   overriding procedure Set_INVALID_FIELD_IN_CDB
     (Self : in out SCSI_Decoder;
      Byte : A0B.Types.Unsigned_16;
      Bit  : A0B.Types.Unsigned_3) is
   begin
      Operation :=
        (Kind              => None,
         Status            => SCSI.SAM5.CHECK_CONDITION,
         Sense             => SCSI.SPC5.INVALID_FIELD_IN_CDB,
         Write_Data_Length => 0,
         Read_Data_Length  => 0);
   end Set_INVALID_FIELD_IN_CDB;

   ------------
   -- Status --
   ------------

   function Status return SCSI.SAM5.STATUS is
   begin
      return Operation.Status;
   end Status;

   -----------------------
   -- Write_Data_Length --
   -----------------------

   function Write_Data_Length return A0B.Types.Unsigned_64 is
   begin
      return Operation.Write_Data_Length;
   end Write_Data_Length;

end Target.Handler;
