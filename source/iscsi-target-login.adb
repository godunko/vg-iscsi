--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Text_IO;
with System.Storage_Elements;

with A0B.Types;

with iSCSI.PDUs;
with iSCSI.Text;

package body iSCSI.Target.Login is

   DIGIT_ZERO             : constant := 16#30#;
   DIGIT_NINE             : constant := 16#39#;
   LATIN_CAPITAL_LETTER_A : constant := 16#41#;
   LATIN_CAPITAL_LETTER_F : constant := 16#46#;
   LATIN_CAPITAL_LETTER_X : constant := 16#58#;
   LATIN_SMALL_LETTER_A   : constant := 16#61#;
   LATIN_SMALL_LETTER_F   : constant := 16#66#;
   LATIN_SMALL_LETTER_X   : constant := 16#78#;

   DataDigest_String           : constant String := "DataDigest";
   DataPDUInOrder_String       : constant String := "DataPDUInOrder";
   DataSequenceInOrder_String  : constant String := "DataSequenceInOrder";
   DefaultTime2Retain_String   : constant String := "DefaultTime2Retain";
   DefaultTime2Wait_String     : constant String := "DefaultTime2Wait";
   Discovery_String            : constant String := "Discovery";
   ErrorRecoveryLevel_String   : constant String := "ErrorRecoveryLevel";
   FirstBurstLength_String     : constant String := "FirstBurstLength";
   HeaderDigest_String         : constant String := "HeaderDigest";
   IFMarker_String             : constant String := "IFMarker";
   IFMarkInt_String            : constant String := "IFMarkInt";
   ImmediateData_String        : constant String := "ImmediateData";
   InitialR2T_String           : constant String := "InitialR2T";
   InitiatorAlias_String       : constant String := "InitiatorAlias";
   InitiatorName_String        : constant String := "InitiatorName";
   iSCSIProtocolLevel_String   : constant String := "iSCSIProtocolLevel";
   MaxBurstLength_String       : constant String := "MaxBurstLength";
   MaxConnections_String       : constant String := "MaxConnections";
   MaxOutstandingR2T_String    : constant String := "MaxOutstandingR2T";
   MaxRecvDataSegmentLength_String : constant String :=
                                                    "MaxRecvDataSegmentLength";
   No_String                   : constant String := "No";
   None_String                 : constant String := "None";
   Normal_String               : constant String := "Normal";
   NotUnderstood_String        : constant String := "NotUnderstood";
   OFMarker_String             : constant String := "OFMarker";
   OFMarkInt_String            : constant String := "OFMarkInt";
   Reject_String               : constant String := "Reject";
   SendTargets_String          : constant String := "SendTargets";
   SessionType_String          : constant String := "SessionType";
   TargetAddress_String        : constant String := "TargetAddress";
   TargetAlias_String          : constant String := "TargetAlias";
   TargetName_String           : constant String := "TargetName";
   TargetPortalGroupTag_String : constant String := "TargetPortalGroupTag";
   TaskReporting_String        : constant String := "TaskReporting";
   X_Hash_Prefix_String        : constant String := "X#";
   X_Minus_Prefix_String       : constant String := "X-";
   X_NodeArchitecture_String   : constant String := "X#NodeArchitecture";
   Y_Hash_Prefix_String        : constant String := "Y#";
   Y_Minus_Prefix_String       : constant String := "Y-";
   Yes_String                  : constant String := "Yes";
   Z_Hash_Prefix_String        : constant String := "Z#";
   Z_Minus_Prefix_String       : constant String := "Z-";

   DataDigest_Key               : constant
     iSCSI.Text.UTF8_String (1 .. DataDigest_String'Length)
       with Import, Address => DataDigest_String'Address;
   DataPDUInOrder_Key           : constant
     iSCSI.Text.UTF8_String (1 .. DataPDUInOrder_String'Length)
       with Import, Address => DataPDUInOrder_String'Address;
   DataSequenceInOrder_Key      : constant
     iSCSI.Text.UTF8_String (1 .. DataSequenceInOrder_String'Length)
       with Import, Address => DataSequenceInOrder_String'Address;
   DefaultTime2Retain_Key       : constant
     iSCSI.Text.UTF8_String (1 .. DefaultTime2Retain_String'Length)
       with Import, Address => DefaultTime2Retain_String'Address;
   DefaultTime2Wait_Key         : constant
     iSCSI.Text.UTF8_String (1 .. DefaultTime2Wait_String'Length)
       with Import, Address => DefaultTime2Wait_String'Address;
   ErrorRecoveryLevel_Key       : constant
     iSCSI.Text.UTF8_String (1 .. ErrorRecoveryLevel_String'Length)
       with Import, Address => ErrorRecoveryLevel_String'Address;
   FirstBurstLength_Key         : constant
     iSCSI.Text.UTF8_String (1 .. FirstBurstLength_String'Length)
       with Import, Address => FirstBurstLength_String'Address;
   HeaderDigest_Key             : constant
     iSCSI.Text.UTF8_String (1 .. HeaderDigest_String'Length)
       with Import, Address => HeaderDigest_String'Address;
   IFMarker_Key                 : constant
     iSCSI.Text.UTF8_String (1 .. IFMarker_String'Length)
       with Import, Address => IFMarker_String'Address;
   IFMarkInt_Key                : constant
     iSCSI.Text.UTF8_String (1 .. IFMarkInt_String'Length)
       with Import, Address => IFMarkInt_String'Address;
   ImmediateData_Key            : constant
     iSCSI.Text.UTF8_String (1 .. ImmediateData_String'Length)
       with Import, Address => ImmediateData_String'Address;
   InitialR2T_Key               : constant
     iSCSI.Text.UTF8_String (1 .. InitialR2T_String'Length)
       with Import, Address => InitialR2T_String'Address;
   InitiatorAlias_Key           : constant
     iSCSI.Text.UTF8_String (1 .. InitiatorAlias_String'Length)
       with Import, Address => InitiatorAlias_String'Address;
   InitiatorName_Key            : constant
     iSCSI.Text.UTF8_String (1 .. InitiatorName_String'Length)
       with Import, Address => InitiatorName_String'Address;
   iSCSIProtocolLevel_Key       : constant
     iSCSI.Text.UTF8_String (1 .. iSCSIProtocolLevel_String'Length)
       with Import, Address => iSCSIProtocolLevel_String'Address;
   MaxBurstLength_Key           : constant
     iSCSI.Text.UTF8_String (1 .. MaxBurstLength_String'Length)
       with Import, Address => MaxBurstLength_String'Address;
   MaxConnections_Key           : constant
     iSCSI.Text.UTF8_String (1 .. MaxConnections_String'Length)
       with Import, Address => MaxConnections_String'Address;
   MaxOutstandingR2T_Key        : constant
     iSCSI.Text.UTF8_String (1 .. MaxOutstandingR2T_String'Length)
       with Import, Address => MaxOutstandingR2T_String'Address;
   MaxRecvDataSegmentLength_Key : constant
     iSCSI.Text.UTF8_String (1 .. MaxRecvDataSegmentLength_String'Length)
       with Import, Address => MaxRecvDataSegmentLength_String'Address;
   OFMarker_Key                 : constant
     iSCSI.Text.UTF8_String (1 .. OFMarker_String'Length)
       with Import, Address => OFMarker_String'Address;
   OFMarkInt_Key                : constant
     iSCSI.Text.UTF8_String (1 .. OFMarkInt_String'Length)
       with Import, Address => OFMarkInt_String'Address;
   SendTargets_Key              : constant
     iSCSI.Text.UTF8_String (1 .. SendTargets_String'Length)
       with Import, Address => SendTargets_String'Address;
   SessionType_Key              : constant
     iSCSI.Text.UTF8_String (1 .. SessionType_String'Length)
       with Import, Address => SessionType_String'Address;
   TargetAddress_Key            : constant
     iSCSI.Text.UTF8_String (1 .. TargetAddress_String'Length)
       with Import, Address => TargetAddress_String'Address;
   TargetAlias_Key              : constant
     iSCSI.Text.UTF8_String (1 .. TargetAlias_String'Length)
       with Import, Address => TargetAlias_String'Address;
   TargetName_Key               : constant
     iSCSI.Text.UTF8_String (1 .. TargetName_String'Length)
       with Import, Address => TargetName_String'Address;
   TargetPortalGroupTag_Key     : constant
     iSCSI.Text.UTF8_String (1 .. TargetPortalGroupTag_String'Length)
       with Import, Address => TargetPortalGroupTag_String'Address;
   TaskReporting_Key            : constant
     iSCSI.Text.UTF8_String (1 .. TaskReporting_String'Length)
       with Import, Address => TaskReporting_String'Address;
   X_Hash_Prefix_Key            : constant
     iSCSI.Text.UTF8_String (1 .. X_Hash_Prefix_String'Length)
       with Import, Address => X_Hash_Prefix_String'Address;
   X_Minus_Prefix_Key           : constant
     iSCSI.Text.UTF8_String (1 .. X_Minus_Prefix_String'Length)
       with Import, Address => X_Minus_Prefix_String'Address;
   X_NodeArchitecture_Key       : constant
     iSCSI.Text.UTF8_String (1 .. X_NodeArchitecture_String'Length)
       with Import, Address => X_NodeArchitecture_String'Address;
   Y_Hash_Prefix_Key            : constant
     iSCSI.Text.UTF8_String (1 .. Y_Hash_Prefix_String'Length)
       with Import, Address => Y_Hash_Prefix_String'Address;
   Y_Minus_Prefix_Key           : constant
     iSCSI.Text.UTF8_String (1 .. Y_Minus_Prefix_String'Length)
       with Import, Address => Y_Minus_Prefix_String'Address;
   Z_Hash_Prefix_Key            : constant
     iSCSI.Text.UTF8_String (1 .. Z_Hash_Prefix_String'Length)
       with Import, Address => Z_Hash_Prefix_String'Address;
   Z_Minus_Prefix_Key           : constant
     iSCSI.Text.UTF8_String (1 .. Z_Minus_Prefix_String'Length)
       with Import, Address => Z_Minus_Prefix_String'Address;

   Discovery_Value     : constant
     iSCSI.Text.UTF8_String (1 .. Discovery_String'Length)
       with Import, Address => Discovery_String'Address;
   No_Value            : constant
     iSCSI.Text.UTF8_String (1 .. No_String'Length)
       with Import, Address => No_String'Address;
   None_Value          : constant
     iSCSI.Text.UTF8_String (1 .. None_String'Length)
       with Import, Address => None_String'Address;
   Normal_Value        : constant
     iSCSI.Text.UTF8_String (1 .. Normal_String'Length)
       with Import, Address => Normal_String'Address;
   NotUnderstood_Value : constant
     iSCSI.Text.UTF8_String (1 .. NotUnderstood_String'Length)
       with Import, Address => NotUnderstood_String'Address;
   Reject_Value        : constant
     iSCSI.Text.UTF8_String (1 .. Reject_String'Length)
       with Import, Address => Reject_String'Address;
   Yes_Value           : constant
     iSCSI.Text.UTF8_String (1 .. Yes_String'Length)
       with Import, Address => Yes_String'Address;

   type Optional_Slice (Is_Specified : Boolean := False) is record
      case Is_Specified is
         when False =>
            null;

         when True =>
            Value : iSCSI.Text.Segment;
      end case;
   end record;

   function To_String (Item : iSCSI.Text.UTF8_String) return String;

   type Boolean_Value_Kind is (None, Reject, Value);

   type Boolean_Value (Kind : Boolean_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Reject =>
            null;

         when Value =>
            Value : Boolean;
      end case;
   end record;

   type Numeric_Value_Kind is (None, Reject, Value);

   type Numeric_Value (Kind : Numeric_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Reject =>
            null;

         when Value =>
            Value : A0B.Types.Unsigned_64;
      end case;
   end record;

   procedure Decode_Boolean_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Boolean_Value);
   --  Decode `boolean-value`.

   procedure Decode_Numerical_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Numeric_Value);
   --  Decode `numerical-value`. Set `Kind` of the `Decoded` to `Reject` in
   --  case of errors:
   --    - empty input string
   --    - illegal character in string
   --    - value out of range (greater than 2**64 - 1)

   --------------------------
   -- Decode_Boolean_Value --
   --------------------------

   procedure Decode_Boolean_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Boolean_Value)
   is
      use type iSCSI.Text.UTF8_String;

   begin
      Ada.Text_IO.Put_Line (To_String (Image));

      if Image = No_Value then
         Decoded := (Kind => Value, Value => False);

      elsif Image = Yes_Value then
         Decoded := (Kind => Value, Value => True);

      else
         Decoded := (Kind => Reject);
      end if;
   end Decode_Boolean_Value;

   ----------------------------
   -- Decode_Numerical_Value --
   ----------------------------

   procedure Decode_Numerical_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Numeric_Value)
   is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_64;

      Index : Positive := Image'First;
      Digit : A0B.Types.Unsigned_64;

   begin
      Ada.Text_IO.Put_Line (To_String (Image));

      if Image'Length = 0 then
         Decoded := (Kind => Reject);

         return;
      end if;

      if Image'Length > 2
        and then
          (Image (Index) = DIGIT_ZERO
             and Image (Index + 1)
               in LATIN_CAPITAL_LETTER_X | LATIN_SMALL_LETTER_X)
      then
         --  `hex-constant`

         Index   := @ + 2;
         Decoded := (Kind => Value, Value => 0);

         loop
            if Image (Index) in DIGIT_ZERO .. DIGIT_NINE then
               Digit := A0B.Types.Unsigned_64 (Image (Index) - DIGIT_ZERO);

            elsif Image (Index)
              in LATIN_CAPITAL_LETTER_A .. LATIN_CAPITAL_LETTER_F
            then
               Digit :=
                 A0B.Types.Unsigned_64
                   (Image (Index) - LATIN_CAPITAL_LETTER_A + 10);

            elsif Image (Index)
              in LATIN_SMALL_LETTER_A .. LATIN_SMALL_LETTER_F
            then
               Digit :=
                 A0B.Types.Unsigned_64
                   (Image (Index) - LATIN_SMALL_LETTER_A + 10);

            else
               Decoded := (Kind => Reject);

               return;
            end if;

            if Decoded.Value > A0B.Types.Unsigned_64'Last / 16 then
               Decoded := (Kind => Reject);

               return;
            end if;

            Decoded.Value := @ + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;

      else
         --  `decimal-constant`

         Decoded := (Kind => Value, Value => 0);

         loop
            if Image (Index) in DIGIT_ZERO .. DIGIT_NINE then
               Digit := A0B.Types.Unsigned_64 (Image (Index) - DIGIT_ZERO);

            else
               Decoded := (Kind => Reject);

               return;
            end if;

            if Decoded.Value > (A0B.Types.Unsigned_64'Last - Digit) / 10 then
               Decoded := (Kind => Reject);

               return;
            end if;

            Decoded.Value := @ + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;
      end if;
   end Decode_Numerical_Value;

   -------------
   -- Process --
   -------------

   procedure Process
     (Header_Address        : System.Address;
      Request_Data_Address  : System.Address;
      Response_Data_Address : System.Address)
   is
      use type iSCSI.Text.UTF8_String;

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : iSCSI.Text.UTF8_String);

      ----------------------
      -- Append_Key_Value --
      ----------------------

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : iSCSI.Text.UTF8_String) is
      begin
         Ada.Text_IO.Put ('`');
         Ada.Text_IO.Put (To_String (Key));
         Ada.Text_IO.Put ("` => `");
         Ada.Text_IO.Put (To_String (Value));
         Ada.Text_IO.Put ('`');
         Ada.Text_IO.New_Line;
      end Append_Key_Value;

      Header : constant iSCSI.PDUs.Login_Request_Header
        with Import, Address => Header_Address;
      Parser : iSCSI.Text.Parser;

      type Session_Type_Kinds is (Default, Reject, Discovery, Normal);

      type Digest_Kinds is (Reject, None);

      DataDigest_Value     : Optional_Slice;
      HeaderDigest_Value   : Optional_Slice;
      InitiatorAlias_Value : Optional_Slice;
      InitiatorName_Value  : Optional_Slice;
      SessionType_Value    : Optional_Slice;

      Session_Type       : Session_Type_Kinds := Default;
      Header_Digest      : Digest_Kinds := None;
      Data_Digest        : Digest_Kinds := None;
      DefaultTime2Retain : Numeric_Value;
      DefaultTime2Wait   : Numeric_Value;
      IFMarker           : Boolean_Value;
      OFMarker           : Boolean_Value;

   begin
      iSCSI.Text.Initialize
        (Parser,
         Request_Data_Address,
         System.Storage_Elements.Storage_Count (Header.DataSegmentLength));

      while iSCSI.Text.Forward (Parser) loop
         declare
            Key   : constant iSCSI.Text.UTF8_String :=
              iSCSI.Text.Text (iSCSI.Text.Key (Parser));
            Value : constant Optional_Slice :=
              (True, iSCSI.Text.Value (Parser));

         begin
            if Key = DataDigest_Key then
               DataDigest_Value := Value;

            elsif Key = DataPDUInOrder_Key then
               raise Program_Error;

            elsif Key = DataSequenceInOrder_Key then
               raise Program_Error;

            elsif Key = DefaultTime2Retain_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), DefaultTime2Retain);

            elsif Key = DefaultTime2Wait_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), DefaultTime2Wait);

            elsif Key = ErrorRecoveryLevel_Key then
               raise Program_Error;

            elsif Key = FirstBurstLength_Key then
               raise Program_Error;

            elsif Key = HeaderDigest_Key then
               HeaderDigest_Value := Value;

            elsif Key = IFMarker_Key then
               Decode_Boolean_Value (iSCSI.Text.Value (Parser), IFMarker);

            elsif Key = IFMarkInt_Key then
               raise Program_Error;

            elsif Key = ImmediateData_Key then
               raise Program_Error;

            elsif Key = InitialR2T_Key then
               raise Program_Error;

            elsif Key = InitiatorAlias_Key then
               InitiatorAlias_Value := Value;

            elsif Key = InitiatorName_Key then
               InitiatorName_Value := Value;

            elsif Key = iSCSIProtocolLevel_Key then
               raise Program_Error;

            elsif Key = MaxBurstLength_Key then
               raise Program_Error;

            elsif Key = MaxConnections_Key then
               raise Program_Error;

            elsif Key = MaxOutstandingR2T_Key then
               raise Program_Error;

            elsif Key = MaxRecvDataSegmentLength_Key then
               raise Program_Error;

            elsif Key = OFMarker_Key then
               Decode_Boolean_Value (iSCSI.Text.Value (Parser), OFMarker);

            elsif Key = OFMarkInt_Key then
               raise Program_Error;

            elsif Key = SendTargets_Key then
               raise Program_Error;

            elsif Key = SessionType_Key then
               SessionType_Value := Value;

            elsif Key = TargetAddress_Key then
               raise Program_Error;

            elsif Key = TargetAlias_Key then
               raise Program_Error;

            elsif Key = TargetName_Key then
               raise Program_Error;

            elsif Key = TargetPortalGroupTag_Key then
               raise Program_Error;

            elsif Key = TaskReporting_Key then
               raise Program_Error;

            elsif Key = X_NodeArchitecture_Key then
               raise Program_Error;

            elsif Key'Length >= X_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + X_Minus_Prefix_Key'Length - 1)
                         = X_Minus_Prefix_Key
            then
               raise Program_Error;

            elsif Key'Length >= Y_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + Y_Minus_Prefix_Key'Length - 1)
                         = Y_Minus_Prefix_Key
            then
               raise Program_Error;

            elsif Key'Length >= Z_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + Z_Minus_Prefix_Key'Length - 1)
                         = Z_Minus_Prefix_Key
            then
               raise Program_Error;

            else
               raise Program_Error;
            end if;
         end;
      end loop;

      --
      --  Validation
      --

      --  SessionType

      if SessionType_Value.Is_Specified then
         if SessionType_Value.Value = Discovery_Value then
            Session_Type := Discovery;

         elsif SessionType_Value.Value = Normal_Value then
            Session_Type := Normal;

         else
            Session_Type := Reject;

            Append_Key_Value (SessionType_Key, Reject_Value);

            raise Program_Error;
            --  XXX 0209 Session type not supported !!!
         end if;
      end if;

      --  DataDigest

      if DataDigest_Value.Is_Specified then
         --  XXX DataDigest is a list of values, not supported yet !!!
         --
         --  Parse value in the list and select appropriate.

         if DataDigest_Value.Value /= None_Value then
            Data_Digest := Reject;
         end if;
      end if;

      --  HeaderDigest

      if HeaderDigest_Value.Is_Specified then
         --  XXX HeaderDigest is a list of values, not supported yet !!!
         --
         --  Parse value in the list and select appropriate.

         if HeaderDigest_Value.Value /= None_Value then
            Header_Digest := Reject;
         end if;
      end if;

      --  InitiatorName

      if not InitiatorName_Value.Is_Specified then
         --  `InitiatorName` must be present always.

         raise Program_Error;
         --  XXX 0207 Missing parameter !!!
      end if;

      --
      --  Response text
      --

      case Data_Digest is
         when Reject =>
            Append_Key_Value (DataDigest_Key, Reject_Value);
            Data_Digest := None;  --  Use default value

            --  Alternatively, negotiation can be terminated.

         when None =>
            Append_Key_Value (DataDigest_Key, None_Value);
      end case;

      case Header_Digest is
         when Reject =>
            Append_Key_Value (HeaderDigest_Key, Reject_Value);
            Header_Digest := None;  --  Use default value

            --  Alternatively, negotiation can be terminated.

         when None =>
            Append_Key_Value (HeaderDigest_Key, None_Value);
      end case;
   end Process;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : iSCSI.Text.UTF8_String) return String is
      Result : constant String (1 .. Item'Length)
        with Import, Address => Item'Address;

   begin
      return Result;
   end To_String;

end iSCSI.Target.Login;
