--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO; use Ada.Text_IO;

with SCSI.Commands.SPC;
with SCSI.SAM5;
with SCSI.SPC5.CDB;
with SCSI.SPC5.Data;

package body Target.Handler is

   USB_MSC_BOOT_CDB_Length  : constant := 12;
   iSCSI_CDB_Minumum_Length : constant := 16;

   function Decode_INQUIRY
     (CDB_Storage : A0B.Types.Arrays.Unsigned_8_Array;
      Descriptor  : out SCSI.Commands.SPC.INQUIRY_Command_Descriptor;
      Sense       : out SCSI.SAM5.Sense_Data) return Boolean;

   procedure Execute_INQUIRY
     (Descriptor : SCSI.Commands.SPC.INQUIRY_Command_Descriptor);

   type Operation_Kind is (None, Inquiry);

   type Operation_Information (Kind : Operation_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Inquiry =>
            Inquiry : SCSI.Commands.SPC.INQUIRY_Command_Descriptor;
      end case;
   end record;

   Operation : Operation_Information;

   -------------
   -- Data_In --
   -------------

   procedure Data_In
     (Storage_Address : System.Address;
      Data_Length     : out A0B.Types.Unsigned_32)
   is
   begin
      case Operation.Kind is
         when None =>
            raise Program_Error;

         when Inquiry =>
            declare
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
                   (Operation.Inquiry.ALLOCATION_LENGTH,
                    SCSI.SPC5.Data.INQUIRY_Data_Length);
            end;
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
      use type A0B.Types.Unsigned_8;

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

         Descriptor :=
           (EVPD              => CDB.EVPD,
            PAGE_CODE         => CDB.PAGE_CODE,
            ALLOCATION_LENGTH =>
              A0B.Types.Unsigned_32 (CDB.ALLOCATION_LENGTH.Value));
      end;

      Sense := SCSI.SPC5.NO_SENSE;

      return True;
   end Decode_INQUIRY;

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
         raise Program_Error;

      else
         Operation := (Inquiry, Descriptor);
      end if;
   end Execute_INQUIRY;

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

            when others =>
               raise Program_Error;
         end case;
      end;
   end Process_Command;

end Target.Handler;
