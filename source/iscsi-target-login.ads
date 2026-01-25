--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;

with A0B.Types;

with iSCSI.Text;

package iSCSI.Target.Login is

   type Boolean_Value_Kind is (None, Error, Value);

   type Boolean_Value (Kind : Boolean_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : Boolean;
      end case;
   end record;

   type Numerical_Value_Kind is (None, Error, Value);

   type Numerical_Value (Kind : Numerical_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : A0B.Types.Unsigned_64;
      end case;
   end record;

   type Binary_Value_Kind is (None, Error, Value);

   type Binary_Value_16 (Kind : Binary_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : A0B.Types.Unsigned_16;
      end case;
   end record;

   type Name_Value_Kind is (None, Error, Value);

   type Name_Value (Kind : Name_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : iSCSI.Text.Segment;
      end case;
   end record;

   subtype Local_Name_Value is Name_Value;
   subtype TargetAddress_Value is Name_Value;
   subtype SendTargets_Value is Name_Value;

   type List_Of_Values_Kind is (None, Error, Value);

   type List_Of_Values (Kind : List_Of_Values_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : iSCSI.Text.Segment;
      end case;
   end record;

   type SessionType is (Discovery, Normal);

   type SessionType_Value_Kind is (None, Error, Value);

   type SessionType_Value (Kind : SessionType_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Error =>
            null;

         when Value =>
            Value : SessionType;
      end case;
   end record;

   type Decoded_Operational_Parameters is record
      HeaderDigest             : List_Of_Values;
      DataDigest               : List_Of_Values;
      MaxConnections           : Numerical_Value;
      SendTargets              : Name_Value;
      TargetName               : Name_Value;
      InitiatorName            : Name_Value;
      TargetAlias              : Local_Name_Value;
      InitiatorAlias           : Local_Name_Value;
      TargetAddress            : TargetAddress_Value;
      TargetPortalGroupTag     : Binary_Value_16;
      InitialR2T               : Boolean_Value;
      ImmediateData            : Boolean_Value;
      MaxRecvDataSegmentLength : Numerical_Value;
      MaxBurstLength           : Numerical_Value;
      FirstBurstLength         : Numerical_Value;
      DefaultTime2Wait         : Numerical_Value;
      DefaultTime2Retain       : Numerical_Value;
      MaxOutstandingR2T        : Numerical_Value;
      DataPDUInOrder           : Boolean_Value;
      DataSequenceInOrder      : Boolean_Value;
      ErrorRecoveryLevel       : Numerical_Value;
      SessionType              : SessionType_Value;

      OFMarker                 : Boolean_Value;
      IFMarker                 : Boolean_Value;
      OFMarkInt                : Numerical_Value;
      IFMarkInt                : Numerical_Value;
   end record;

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
