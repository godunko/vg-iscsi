--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with System.Storage_Elements;

with Ada.Text_IO; use Ada.Text_IO;

with A0B.Types.Big_Endian;

with SCSI.Commands.SPC;
with SCSI.SAM5;
with SCSI.SPC5.CDB;
with SCSI.SPC5.Data;
with SCSI.SPC5.VPD;

package body Target.Handler is

   USB_MSC_BOOT_CDB_Length  : constant := 12;
   iSCSI_CDB_Minumum_Length : constant := 16;

   function Decode_INQUIRY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.INQUIRY_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean;

   function Decode_REPORT_LUNS
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean;

   function Decode_TEST_UNIT_READY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean;

   procedure Execute_INQUIRY
     (Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor);

   procedure Execute_REPORT_LUNS
     (Descriptor : SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor);

   procedure Execute_TEST_UNIT_READY
     (Descriptor : SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor);

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

   type Operation_Kind is (None, Inquiry, Report_Luns);

   type Operation_Information (Kind : Operation_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Inquiry =>
            Inquiry : SCSI.Commands.SPC.INQUIRY_Command_Descriptor;

         when Report_Luns =>
            Report_Luns : SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;
      end case;
   end record;

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

         when Report_Luns =>
            Encode_REPORT_LUNS_Data
              (Allocation_Length => Operation.Report_Luns.ALLOCATION_LENGTH,
               Storage_Address   => Storage_Address,
               Data_Length       => Data_Length);
      end case;
   end Data_In;

   --------------------
   -- Decode_INQUIRY --
   --------------------

   function Decode_INQUIRY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.INQUIRY_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean
   is
      use type A0B.Types.Reserved_1;
      use type A0B.Types.Reserved_6;
      use type SCSI.SPC5.VPD_Page_Code;

   begin
      case Length_Check is
         when Default =>
            if CDB_Storage'Length /= SCSI.SPC5.CDB.INQUIRY_CDB_Length then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;

         when USB_MSC_BOOT =>
            if CDB_Storage'Length /= SCSI.SPC5.CDB.INQUIRY_CDB_Length
              and CDB_Storage'Length /= USB_MSC_BOOT_CDB_Length
            then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;

         when iSCSI =>
            if CDB_Storage'Length /= iSCSI_CDB_Minumum_Length then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;
      end case;

      declare
         CDB : constant SCSI.SPC5.CDB.INQUIRY_CDB
           with Import, Address => CDB_Storage'Address;

      begin
         --  Reserved fields

         if CDB.Reserved_1_7_2 /= A0B.Types.Zero then
            Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

            return False;
         end if;

         --  Obsolete fields

         if CDB.Obsolete_1_1_1 /= A0B.Types.Zero then
            Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

            return False;
         end if;

         --  PAGE_CODE can be specified for EVPD only

         if CDB.PAGE_CODE /= 0 and CDB.EVPD = False then
            Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

            return False;
         end if;

         --  XXX CONTROL is not validated/decoded

         Descriptor :=
           (EVPD              => CDB.EVPD,
            PAGE_CODE         => CDB.PAGE_CODE,
            ALLOCATION_LENGTH =>
              A0B.Types.Unsigned_32 (CDB.ALLOCATION_LENGTH.Value));
      end;

      Sense := SCSI.SPC5.NO_SENSE;

      return True;
   end Decode_INQUIRY;

   ------------------------
   -- Decode_REPORT_LUNS --
   ------------------------

   function Decode_REPORT_LUNS
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean
   is
      use type A0B.Types.Reserved_8;

   begin
      case Length_Check is
         when Default | USB_MSC_BOOT =>
            if CDB_Storage'Length /= SCSI.SPC5.CDB.REPORT_LUNS_CDB_Length then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;

         when iSCSI =>
            if CDB_Storage'Length /= iSCSI_CDB_Minumum_Length then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;
      end case;

      declare
         CDB : constant SCSI.SPC5.CDB.REPORT_LUNS_CDB
           with Import, Address => CDB_Storage'Address;

      begin
         if CDB.Reserved_1 /= A0B.Types.Zero
           or CDB.Reserved_3 /= A0B.Types.Zero
           or CDB.Reserved_4 /= A0B.Types.Zero
           or CDB.Reserved_5 /= A0B.Types.Zero
           or CDB.Reserved_10 /= A0B.Types.Zero
         then
            Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

            return False;
         end if;

         --  XXX CONTROL is not validated/decoded

         Descriptor :=
           (ALLOCATION_LENGTH => CDB.ALLOCATION_LENGTH.Value,
            SELECT_REPORT     => CDB.SELECT_REPORT);
      end;

      return True;
   end Decode_REPORT_LUNS;

   ----------------------------
   -- Decode_TEST_UNIT_READY --
   ----------------------------

   function Decode_TEST_UNIT_READY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean
   is
      use type A0B.Types.Reserved_8;

   begin
      case Length_Check is
         when Default =>
            if CDB_Storage'Length
                 /= SCSI.SPC5.CDB.TEST_UNIT_READY_CDB_Length
            then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;

         when USB_MSC_BOOT =>
            if CDB_Storage'Length /= SCSI.SPC5.CDB.TEST_UNIT_READY_CDB_Length
              and CDB_Storage'Length /= USB_MSC_BOOT_CDB_Length
            then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
            end if;

         when iSCSI =>
            if CDB_Storage'Length /= iSCSI_CDB_Minumum_Length then
               Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

               return False;
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
            Sense := SCSI.SPC5.INVALID_FIELD_IN_CDB;

            return False;
         end if;

         --  XXX CONTROL is not validated/decoded

         Descriptor := (null record);
      end;

      return True;
   end Decode_TEST_UNIT_READY;

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
            raise Program_Error;
         end if;
      --
      --  else
      --     Operation := (Inquiry, Descriptor);
      end if;

      Operation := (Inquiry, Descriptor);
   end Execute_INQUIRY;

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

      Operation := (Report_Luns, Descriptor);
   end Execute_REPORT_LUNS;

   -----------------------------
   -- Execute_TEST_UNIT_READY --
   -----------------------------

   procedure Execute_TEST_UNIT_READY
     (Descriptor : SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor)
   is
      pragma Unreferenced (Descriptor);

   begin
      Operation := (Kind => None);
   end Execute_TEST_UNIT_READY;

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

         when Report_Luns =>
            return True;
      end case;
      --  return Operation.Kind in Inquiry | Report_Luns;
   end Has_Data_In;

   ---------------------
   -- Process_Command --
   ---------------------

   procedure Process_Command
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array) is
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
                  use type SCSI.SAM5.Sense_Data;

                  Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor;
                  Sense      : SCSI.SAM5.Sense_Data;

               begin
                  if Decode_INQUIRY (CDB_Storage, Descriptor, Sense) then
                     pragma Assert (Sense = SCSI.SPC5.NO_SENSE);

                     Execute_INQUIRY (Descriptor);

                  else
                     raise Program_Error;
                     --  Command_Decode_Failure (Sense);
                  end if;
               end;

            when SCSI.SPC5.REPORT_LUNS =>
               declare
                  use type SCSI.SAM5.Sense_Data;

                  Descriptor :
                    SCSI.Commands.SPC.REPORT_LUNS_Command_Descriptor;
                  Sense      : SCSI.SAM5.Sense_Data;

               begin
                  if Decode_REPORT_LUNS
                    (CDB_Storage, Descriptor, Sense)
                  then
                     pragma Assert (Sense = SCSI.SPC5.NO_SENSE);

                     Execute_REPORT_LUNS (Descriptor);

                  else
                     raise Program_Error;
                     --  Command_Decode_Failure (Sense);
                  end if;
               end;

            when SCSI.SPC5.TEST_UNIT_READY =>
               declare
                  use type SCSI.SAM5.Sense_Data;

                  Descriptor :
                    SCSI.Commands.SPC.TEST_UNIT_READY_Command_Descriptor;
                  Sense      : SCSI.SAM5.Sense_Data;

               begin
                  if Decode_TEST_UNIT_READY
                    (CDB_Storage, Descriptor, Sense)
                  then
                     pragma Assert (Sense = SCSI.SPC5.NO_SENSE);

                     Execute_TEST_UNIT_READY (Descriptor);

                  else
                     raise Program_Error;
                     --  Command_Decode_Failure (Sense);
                  end if;
               end;

            when others =>
               raise Program_Error;
         end case;
      end;
   end Process_Command;

end Target.Handler;
