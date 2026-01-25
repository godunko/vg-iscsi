--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Text_IO;
with System.Storage_Elements;

with iSCSI.PDUs;

package body iSCSI.Target.Login is

   Configured_MaxConnections           : constant := 1;
   Configured_InitialR2T               : constant Boolean := True;
   Configured_ImmediateData            : constant Boolean := False;
   Configured_MaxRecvDataSegmentLength : constant := 8_192;
   Configured_MaxBurstLength           : constant := 262_144;
   Configured_FirstBurstLength         : constant := 65_536;

   PLUS_SIGN              : constant := 16#2B#;
   SOLIDUS                : constant := 16#2F#;
   DIGIT_ZERO             : constant := 16#30#;
   DIGIT_NINE             : constant := 16#39#;
   EQUALS_SIGN            : constant := 16#3D#;
   LATIN_CAPITAL_LETTER_A : constant := 16#41#;
   LATIN_CAPITAL_LETTER_B : constant := 16#42#;
   LATIN_CAPITAL_LETTER_F : constant := 16#46#;
   LATIN_CAPITAL_LETTER_X : constant := 16#58#;
   LATIN_CAPITAL_LETTER_Z : constant := 16#5A#;
   LATIN_SMALL_LETTER_A   : constant := 16#61#;
   LATIN_SMALL_LETTER_B   : constant := 16#62#;
   LATIN_SMALL_LETTER_F   : constant := 16#66#;
   LATIN_SMALL_LETTER_X   : constant := 16#78#;
   LATIN_SMALL_LETTER_Z   : constant := 16#7A#;

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
   Irrelevant_String           : constant String := "Irrelevant";
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
   Irrelevant_Value    : constant
     iSCSI.Text.UTF8_String (1 .. Irrelevant_String'Length)
       with Import, Address => Irrelevant_String'Address;
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

   function To_String (Item : iSCSI.Text.UTF8_String) return String;

   procedure Decode_Boolean_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Boolean_Value);
   --  Decode `boolean-value`.

   procedure Decode_Numerical_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Numerical_Value);
   --  Decode `numerical-value`. Set `Kind` of the `Decoded` to `Reject` in
   --  case of errors:
   --    - empty input string
   --    - illegal character in string
   --    - value out of range (greater than 2**64 - 1)

   procedure Decode_Numerical_Value
     (Segment : iSCSI.Text.Segment;
      First   : A0B.Types.Unsigned_64;
      Last    : A0B.Types.Unsigned_64;
      Decoded : out Numerical_Value);
   --  Decode `numerical-value`. Set `Kind` of the `Decoded` to `Reject` in
   --  case of errors:
   --    - empty input string
   --    - illegal character in string
   --    - value out of given range

   procedure Decode_Binary_Value_16_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out Binary_Value_16);
   --  Decode `16-bit-binary-value`.

   procedure Decode_iSCSI_Name_Value
     (Image   : iSCSI.Text.Segment;
      Decoded : out Name_Value);
   --  Decode `iSCSI-name-value`.
   --
   --  XXX No checks are implemented yet.

   procedure Decode_iSCSI_Local_Name_Value
     (Image   : iSCSI.Text.Segment;
      Decoded : out Local_Name_Value);
   --  Decode `iSCSI-local-name-value`.
   --
   --  XXX No checks are implemented yet.

   procedure Decode_List_Of_Values
     (Image   : iSCSI.Text.Segment;
      Decoded : out List_Of_Values);
   --  Decode `list-of-values`.
   --
   --  XXX No checks are implemented yet.

   procedure Decode_SessionType_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out SessionType_Value);
   --  Decode `<Discovery|Normal>`

   procedure Decode_SendTargets_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out SendTargets_Value);
   --  Decode `SendTargets`

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : iSCSI.Text.UTF8_String);

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : Boolean);

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : Natural);

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : A0B.Types.Unsigned_24);

   procedure Set_Error_Initiator_Error is null;
   --  XXX Set error code to 2000 "Initiator error"

   procedure Set_Error_Unsupported_Version is null;
   --  XXX Set error code to 0205 "The requested iSCSI version range is not
   --  supported by the target."

   procedure Set_Error_Missing_Parameter is null;
   --  XXX Set error code to 0207 "Missing parameters"

   procedure Set_Error_Session_Type_Not_Supported is null;
   --  XXX Set 0209 "Session type not supported" code !!!

   procedure Validate (Decoded : Decoded_Operational_Parameters);

   procedure Validate_Discovery_Session
     (Decoded : Decoded_Operational_Parameters);

   procedure Validate_Normal_Session
     (Decoded : Decoded_Operational_Parameters);

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

   ----------------------
   -- Append_Key_Value --
   ----------------------

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : Boolean) is
   begin
      Append_Key_Value
        (Key, (case Value is when False => No_Value, when True => Yes_Value));
   end Append_Key_Value;

   ----------------------
   -- Append_Key_Value --
   ----------------------

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : Natural)
   is
      Image_String : constant String := Natural'Image (Value);
      Image        : constant iSCSI.Text.UTF8_String (Image_String'Range)
        with Import, Address => Image_String'Address;

   begin
      Append_Key_Value (Key, Image (Image'First + 1 .. Image'Last));
   end Append_Key_Value;

   ----------------------
   -- Append_Key_Value --
   ----------------------

   procedure Append_Key_Value
     (Key   : iSCSI.Text.UTF8_String;
      Value : A0B.Types.Unsigned_24)
   is
      Image_String : constant String := A0B.Types.Unsigned_24'Image (Value);
      Image        : constant iSCSI.Text.UTF8_String (Image_String'Range)
        with Import, Address => Image_String'Address;

   begin
      Append_Key_Value (Key, Image (Image'First + 1 .. Image'Last));
   end Append_Key_Value;

   ----------------------------------
   -- Decode_Binary_Value_16_Value --
   ----------------------------------

   procedure Decode_Binary_Value_16_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out Binary_Value_16)
   is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_16;

      Image : constant iSCSI.Text.UTF8_String :=
        iSCSI.Text.Text (Segment);
      Index : Positive := Image'First;
      Digit : A0B.Types.Unsigned_16;

   begin
      Ada.Text_IO.Put_Line (To_String (Image));

      if Image'Length = 0 then
         Decoded := (Kind => Error);

         return;
      end if;

      if Image'Length > 2
        and then
          (Image (Index) = DIGIT_ZERO
             and Image (Index + 1)
               in LATIN_CAPITAL_LETTER_B | LATIN_SMALL_LETTER_B)
      then
         --  `base64-constant`

         --  XXX It is naive implementation, most probably it doesn't work as
         --  expected:
         --   * 16-bit value should be encoded as single 24-bits unit
         --     (4 characters group): 3 characters followed by one '=' padding
         --     character
         --   * value is in big-endian format

         Index   := @ + 2;
         Decoded := (Kind => Value, Value => 0);

         loop
            if Image (Index)
              in LATIN_CAPITAL_LETTER_A .. LATIN_CAPITAL_LETTER_Z
            then
               Digit :=
                 A0B.Types.Unsigned_16
                   (Image (Index) - LATIN_CAPITAL_LETTER_A);

            elsif Image (Index)
              in LATIN_SMALL_LETTER_A .. LATIN_SMALL_LETTER_Z
            then
               Digit :=
                 A0B.Types.Unsigned_16
                   (Image (Index) - LATIN_SMALL_LETTER_A + 26);

            elsif Image (Index) in DIGIT_ZERO .. DIGIT_NINE then
               Digit :=
                 A0B.Types.Unsigned_16 (Image (Index) - DIGIT_ZERO + 52);

            elsif Image (Index) = PLUS_SIGN then
               Digit := 62;

            elsif Image (Index) = SOLIDUS then
               Digit := 63;

            elsif Image (Index) = EQUALS_SIGN then
               --  XXX It must appear only once, there is no check for invalid
               --  length of the data.

               return;

            else
               Decoded := (Kind => Error);

               return;
            end if;

            if Decoded.Value > (A0B.Types.Unsigned_16'Last - Digit) / 64 then
               Decoded := (Kind => Error);

               return;
            end if;

            Decoded.Value := @ * 64 + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;

      elsif Image'Length > 2
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
               Digit := A0B.Types.Unsigned_16 (Image (Index) - DIGIT_ZERO);

            elsif Image (Index)
              in LATIN_CAPITAL_LETTER_A .. LATIN_CAPITAL_LETTER_F
            then
               Digit :=
                 A0B.Types.Unsigned_16
                   (Image (Index) - LATIN_CAPITAL_LETTER_A + 10);

            elsif Image (Index)
              in LATIN_SMALL_LETTER_A .. LATIN_SMALL_LETTER_F
            then
               Digit :=
                 A0B.Types.Unsigned_16
                   (Image (Index) - LATIN_SMALL_LETTER_A + 10);

            else
               Decoded := (Kind => Error);

               return;
            end if;

            if Decoded.Value > A0B.Types.Unsigned_16'Last / 16 then
               Decoded := (Kind => Error);

               return;
            end if;

            Decoded.Value := @ * 16 + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;

      else
         --  `decimal-constant`

         Decoded := (Kind => Value, Value => 0);

         loop
            if Image (Index) in DIGIT_ZERO .. DIGIT_NINE then
               Digit := A0B.Types.Unsigned_16 (Image (Index) - DIGIT_ZERO);

            else
               Decoded := (Kind => Error);

               return;
            end if;

            if Decoded.Value > (A0B.Types.Unsigned_16'Last - Digit) / 10 then
               Decoded := (Kind => Error);

               return;
            end if;

            Decoded.Value := @ * 10 + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;
      end if;
   end Decode_Binary_Value_16_Value;

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
         Decoded := (Kind => Error);
      end if;
   end Decode_Boolean_Value;

   -----------------------------------
   -- Decode_iSCSI_Local_Name_Value --
   -----------------------------------

   procedure Decode_iSCSI_Local_Name_Value
     (Image   : iSCSI.Text.Segment;
      Decoded : out Local_Name_Value) renames Decode_iSCSI_Name_Value;

   -----------------------------
   -- Decode_iSCSI_Name_Value --
   -----------------------------

   procedure Decode_iSCSI_Name_Value
     (Image   : iSCSI.Text.Segment;
      Decoded : out Name_Value) is
   begin
      --  XXX checks are not implemented !!!

      Decoded := (Kind => Value, Value => Image);
   end Decode_iSCSI_Name_Value;

   ---------------------------
   -- Decode_List_Of_Values --
   ---------------------------

   procedure Decode_List_Of_Values
     (Image   : iSCSI.Text.Segment;
      Decoded : out List_Of_Values) is
   begin
      --  XXX checks are not implemented !!!

      Decoded := (Kind => Value, Value => Image);
   end Decode_List_Of_Values;

   ----------------------------
   -- Decode_Numerical_Value --
   ----------------------------

   procedure Decode_Numerical_Value
     (Image   : iSCSI.Text.UTF8_String;
      Decoded : out Numerical_Value)
   is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_64;

      Index : Positive := Image'First;
      Digit : A0B.Types.Unsigned_64;

   begin
      Ada.Text_IO.Put_Line (To_String (Image));

      if Image'Length = 0 then
         Decoded := (Kind => Error);

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
               Decoded := (Kind => Error);

               return;
            end if;

            if Decoded.Value > A0B.Types.Unsigned_64'Last / 16 then
               Decoded := (Kind => Error);

               return;
            end if;

            Decoded.Value := @ * 16 + Digit;
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
               Decoded := (Kind => Error);

               return;
            end if;

            if Decoded.Value > (A0B.Types.Unsigned_64'Last - Digit) / 10 then
               Decoded := (Kind => Error);

               return;
            end if;

            Decoded.Value := @ * 10 + Digit;
            Index         := @ + 1;

            exit when Index > Image'Last;
         end loop;
      end if;
   end Decode_Numerical_Value;

   ----------------------------
   -- Decode_Numerical_Value --
   ----------------------------

   procedure Decode_Numerical_Value
     (Segment : iSCSI.Text.Segment;
      First   : A0B.Types.Unsigned_64;
      Last    : A0B.Types.Unsigned_64;
      Decoded : out Numerical_Value)
   is
      Image : constant iSCSI.Text.UTF8_String :=
        iSCSI.Text.Text (Segment);

   begin
      Decode_Numerical_Value (Image, Decoded);

      if Decoded.Kind = Value
        and then Decoded.Value not in First .. Last
      then
         Decoded := (Kind => Error);
      end if;
   end Decode_Numerical_Value;

   ------------------------------
   -- Decode_SendTargets_Value --
   ------------------------------

   procedure Decode_SendTargets_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out SendTargets_Value) renames Decode_iSCSI_Name_Value;

   ------------------------------
   -- Decode_SessionType_Value --
   ------------------------------

   procedure Decode_SessionType_Value
     (Segment : iSCSI.Text.Segment;
      Decoded : out SessionType_Value)
   is
      use type iSCSI.Text.UTF8_String;

      Image : constant iSCSI.Text.UTF8_String :=
        iSCSI.Text.Text (Segment);

   begin
      if Image = Discovery_Value then
         Decoded := (Kind => Value, Value => Discovery);

      elsif Image = Normal_Value then
         Decoded := (Kind => Value, Value => Normal);

      else
         Decoded := (Kind => Error);
      end if;
   end Decode_SessionType_Value;

   --------------------------------
   -- Decode_TargetAddress_Value --
   --------------------------------

   procedure Decode_TargetAddress_Value
     (Image   : iSCSI.Text.Segment;
      Decoded : out TargetAddress_Value) renames Decode_iSCSI_Name_Value;

   -------------
   -- Process --
   -------------

   procedure Process
     (Header_Address        : System.Address;
      Request_Data_Address  : System.Address;
      Response_Data_Address : System.Address with Unreferenced)
   is
      use type A0B.Types.Unsigned_64;
      use type iSCSI.Text.UTF8_String;

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : A0B.Types.Unsigned_64);

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : String);

      ----------------------
      -- Append_Key_Value --
      ----------------------

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : String)
      is
         V : constant iSCSI.Text.UTF8_String (1 .. Value'Length)
           with Import, Address => Value'Address;

      begin
         Append_Key_Value (Key, V);
      end Append_Key_Value;

      ----------------------
      -- Append_Key_Value --
      ----------------------

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : A0B.Types.Unsigned_64) is
      begin
         Append_Key_Value (Key, A0B.Types.Unsigned_64'Image (Value));
      end Append_Key_Value;

      Header : constant iSCSI.PDUs.Login_Request_Header
        with Import, Address => Header_Address;
      Parser : iSCSI.Text.Parser;

      DefaultTime2Retain       : Numerical_Value;
      DefaultTime2Wait         : Numerical_Value;
      IFMarker                 : Boolean_Value;
      OFMarker                 : Boolean_Value;
      ErrorRecoveryLevel       : Numerical_Value;
      MaxRecvDataSegmentLength : Numerical_Value;

      Decoded : Decoded_Operational_Parameters;

   begin
      iSCSI.Text.Initialize
        (Parser,
         Request_Data_Address,
         System.Storage_Elements.Storage_Count (Header.DataSegmentLength));

      while iSCSI.Text.Forward (Parser) loop
         declare
            Key     : constant iSCSI.Text.UTF8_String :=
              iSCSI.Text.Text (iSCSI.Text.Key (Parser));
            Segment : constant iSCSI.Text.Segment := iSCSI.Text.Value (Parser);
            Text    : constant iSCSI.Text.UTF8_String :=
              iSCSI.Text.Value (Parser);

         begin
            if Key = DataDigest_Key then
               Decode_List_Of_Values (Segment, Decoded.DataDigest);

            elsif Key = DataPDUInOrder_Key then
               Decode_Boolean_Value (Text, Decoded.DataPDUInOrder);

            elsif Key = DataSequenceInOrder_Key then
               Decode_Boolean_Value (Text, Decoded.DataSequenceInOrder);

            elsif Key = DefaultTime2Retain_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), DefaultTime2Retain);
               Decode_Numerical_Value
                 (Segment,
                  0,
                  3_600,
                  Decoded.DefaultTime2Retain);

            elsif Key = DefaultTime2Wait_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), DefaultTime2Wait);
               Decode_Numerical_Value
                 (Segment,
                  0,
                  3_600,
                  Decoded.DefaultTime2Wait);

            elsif Key = ErrorRecoveryLevel_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), ErrorRecoveryLevel);
               Decode_Numerical_Value
                 (Segment, 0, 2, Decoded.ErrorRecoveryLevel);

            elsif Key = FirstBurstLength_Key then
               Decode_Numerical_Value
                 (Segment,
                  512,
                  2**24 - 1,
                  Decoded.FirstBurstLength);

            elsif Key = HeaderDigest_Key then
               Decode_List_Of_Values
                 (iSCSI.Text.Value (Parser), Decoded.HeaderDigest);

            elsif Key = IFMarker_Key then
               Decode_Boolean_Value (iSCSI.Text.Value (Parser), IFMarker);
               Decode_Boolean_Value (Text, Decoded.IFMarker);

            elsif Key = IFMarkInt_Key then
               Decode_Numerical_Value (Segment, 1, 65_535, Decoded.IFMarkInt);

            elsif Key = ImmediateData_Key then
               Decode_Boolean_Value
                 (iSCSI.Text.Value (Parser), Decoded.ImmediateData);

            elsif Key = InitialR2T_Key then
               Decode_Boolean_Value
                 (iSCSI.Text.Value (Parser), Decoded.InitialR2T);

            elsif Key = InitiatorAlias_Key then
               Decode_iSCSI_Local_Name_Value
                 (Segment, Decoded.InitiatorAlias);

            elsif Key = InitiatorName_Key then
               Decode_iSCSI_Name_Value (Segment, Decoded.InitiatorName);

            elsif Key = iSCSIProtocolLevel_Key then
               Decode_Numerical_Value
                 (Segment, 0, 31, Decoded.iSCSIProtocolLevel);
               --  Range is specified by [RFC7144]

            elsif Key = MaxBurstLength_Key then
               Decode_Numerical_Value
                 (Segment,
                  512,
                  2**24 - 1,
                  Decoded.MaxBurstLength);

            elsif Key = MaxConnections_Key then
               Decode_Numerical_Value
                 (Segment, 1, 65_535, Decoded.MaxConnections);

            elsif Key = MaxOutstandingR2T_Key then
               Decode_Numerical_Value
                 (Segment,
                  0,
                  65_535,
                  Decoded.MaxOutstandingR2T);

            elsif Key = MaxRecvDataSegmentLength_Key then
               Decode_Numerical_Value
                 (iSCSI.Text.Value (Parser), MaxRecvDataSegmentLength);
               Decode_Numerical_Value
                 (Segment,
                  512,
                  2**24 - 1,
                  Decoded.MaxRecvDataSegmentLength);

            elsif Key = OFMarker_Key then
               Decode_Boolean_Value (iSCSI.Text.Value (Parser), OFMarker);
               Decode_Boolean_Value (Text, Decoded.OFMarker);

            elsif Key = OFMarkInt_Key then
               Decode_Numerical_Value (Segment, 1, 65_535, Decoded.OFMarkInt);

            elsif Key = SendTargets_Key then
               Decode_SendTargets_Value (Segment, Decoded.SendTargets);

            elsif Key = SessionType_Key then
               Decode_SessionType_Value (Segment, Decoded.SessionType);

            elsif Key = TargetAddress_Key then
               Decode_TargetAddress_Value (Segment, Decoded.TargetAddress);

            elsif Key = TargetAlias_Key then
               Decode_iSCSI_Local_Name_Value (Segment, Decoded.TargetAlias);

            elsif Key = TargetName_Key then
               Decode_iSCSI_Name_Value (Segment, Decoded.TargetName);

            elsif Key = TargetPortalGroupTag_Key then
               Decode_Binary_Value_16_Value
                 (Segment, Decoded.TargetPortalGroupTag);

            elsif Key = TaskReporting_Key then
               Decode_List_Of_Values (Segment, Decoded.TaskReporting);

            elsif Key = X_NodeArchitecture_Key then
               Decode_List_Of_Values (Segment, Decoded.X_NodeArchitecture);

            elsif Key'Length >= X_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + X_Minus_Prefix_Key'Length - 1)
                         = X_Minus_Prefix_Key
            then
               for J in Decoded.NotUnderstood'Range loop
                  if iSCSI.Text.Is_Null (Decoded.NotUnderstood (J)) then
                     Decoded.NotUnderstood (J) := iSCSI.Text.Key (Parser);

                     exit;
                  end if;
               end loop;

            elsif Key'Length >= Y_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + Y_Minus_Prefix_Key'Length - 1)
                         = Y_Minus_Prefix_Key
            then
               for J in Decoded.NotUnderstood'Range loop
                  if iSCSI.Text.Is_Null (Decoded.NotUnderstood (J)) then
                     Decoded.NotUnderstood (J) := iSCSI.Text.Key (Parser);

                     exit;
                  end if;
               end loop;

            elsif Key'Length >= Z_Minus_Prefix_Key'Length
              and then Key (Key'First
                              .. Key'First + Z_Minus_Prefix_Key'Length - 1)
                         = Z_Minus_Prefix_Key
            then
               for J in Decoded.NotUnderstood'Range loop
                  if iSCSI.Text.Is_Null (Decoded.NotUnderstood (J)) then
                     Decoded.NotUnderstood (J) := iSCSI.Text.Key (Parser);

                     exit;
                  end if;
               end loop;

            else
               for J in Decoded.NotUnderstood'Range loop
                  if iSCSI.Text.Is_Null (Decoded.NotUnderstood (J)) then
                     Decoded.NotUnderstood (J) := iSCSI.Text.Key (Parser);

                     exit;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      --
      --  Validation
      --

      --  DefaultTime2Retain

      if DefaultTime2Retain.Kind = None then
         DefaultTime2Retain := (Kind => Value, Value => 20);

      elsif DefaultTime2Retain.Kind = Value
        and then DefaultTime2Retain.Value not in 0 .. 3_600
      then
         DefaultTime2Retain := (Kind => Error);
      end if;

      --  DefaultTime2Wait

      if DefaultTime2Wait.Kind = None then
         DefaultTime2Wait := (Kind => Value, Value => 2);

      elsif DefaultTime2Wait.Kind = Value
        and then DefaultTime2Wait.Value not in 0 .. 3_600
      then
         DefaultTime2Wait := (Kind => Error);
      end if;

      --  IFMarker                 : Boolean_Value;
      --  OFMarker                 : Boolean_Value;
      --  ErrorRecoveryLevel       : Numeric_Value;
      --  MaxRecvDataSegmentLength : Numeric_Value;

      --
      --  Response text
      --

      case DefaultTime2Retain.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (DefaultTime2Retain_Key, Reject_Value);

         when Value =>
            Append_Key_Value
              (DefaultTime2Retain_Key, DefaultTime2Retain.Value);
      end case;

      case DefaultTime2Wait.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (DefaultTime2Wait_Key, Reject_Value);

         when Value =>
            Append_Key_Value
              (DefaultTime2Wait_Key, DefaultTime2Wait.Value);
      end case;

      --  IFMarker                 : Boolean_Value;
      --  OFMarker                 : Boolean_Value;
      --  ErrorRecoveryLevel       : Numeric_Value;
      --  MaxRecvDataSegmentLength : Numeric_Value;

      --

      Validate (Decoded);
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

   --------------
   -- Validate --
   --------------

   procedure Validate (Decoded : Decoded_Operational_Parameters) is
      SessionType : iSCSI.Target.Login.SessionType := Normal;

   begin
      --  SessionType
      --
      --  Declarative: no target's response, except value decode error

      case Decoded.SessionType.Kind is
         when None =>
            --  Default

            null;

         when Error =>
            Append_Key_Value (SessionType_Key, Reject_Value);

            Set_Error_Session_Type_Not_Supported;

         when Value =>
            SessionType := Decoded.SessionType.Value;
      end case;

      case SessionType is
         when Discovery =>
            Validate_Discovery_Session (Decoded);

         when Normal =>
            Validate_Normal_Session (Decoded);
      end case;
   end Validate;

   --------------------------------
   -- Validate_Discovery_Session --
   --------------------------------

   procedure Validate_Discovery_Session
     (Decoded : Decoded_Operational_Parameters)
   is
      use type iSCSI.Text.Segment;

      --  iSCSIProtocolLevel                 : Natural  := RFC7143;
      --  MaxConnections                     : Positive := 1;
      TargetName                         : iSCSI.Text.Segment
        with Unreferenced;
      InitiatorName                      : iSCSI.Text.Segment
        with Unreferenced;
      InitiatorAlias                     : iSCSI.Text.Segment
        with Unreferenced;
      --  InitialR2T                         : Boolean  := True;
      --  ImmediateData                      : Boolean := True;
      Initiator_MaxRecvDataSegmentLength : A0B.Types.Unsigned_24 := 8_192
        with Unreferenced;
      Target_MaxRecvDataSegmentLength    : A0B.Types.Unsigned_24 := 8_192;
      --  MaxBurstLength                     : A0B.Types.Unsigned_24 := 262_144;
      --  FirstBurstLength                   : A0B.Types.Unsigned_24 := 65_536;

   begin
      --  iSCSIProtocolLevel, irrelevant when SessionType = Discovery

      case Decoded.iSCSIProtocolLevel.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (iSCSIProtocolLevel_Key, Reject_Value);

         when Value =>
            Append_Key_Value (iSCSIProtocolLevel_Key, Irrelevant_Value);
      end case;

      --  ImmediateData, irrelevant when SessionType = Discovery

      case Decoded.ImmediateData.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (ImmediateData_Key, Reject_Value);

         when Value =>
            Append_Key_Value (ImmediateData_Key, Irrelevant_Value);
      end case;

      --  InitialR2T, irrelevant when SessionType = Discovery

      case Decoded.InitialR2T.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (InitialR2T_Key, Reject_Value);

         when Value =>
            Append_Key_Value (InitialR2T_Key, Irrelevant_Value);
      end case;

      --  MaxBurstLength, irrelevant when SessionType = Discovery

      case Decoded.MaxBurstLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxBurstLength_Key, Reject_Value);

         when Value =>
            Append_Key_Value (MaxBurstLength_Key, Irrelevant_Value);
      end case;

      --  DataDigest
      --
      --  XXX Doesn't support list of values.
      --  XXX Accept `None` only.
      --  XXX Share code with `Discovery` session processing.

      case Decoded.DataDigest.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (DataDigest_Key, Reject_Value);

            --  XXX Should error be reported ???

         when Value =>
            if Decoded.DataDigest.Value = None_Value then
               Append_Key_Value (DataDigest_Key, None_Value);

            else
               Append_Key_Value (DataDigest_Key, Reject_Value);
            end if;
      end case;

      --  FirstBurstLength, irrelevant when SessionType = Discovery

      case Decoded.FirstBurstLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (FirstBurstLength_Key, Reject_Value);

         when Value =>
            Append_Key_Value (FirstBurstLength_Key, Irrelevant_Value);
      end case;

      --  HeaderDigest
      --
      --  XXX Doesn't support list of values.
      --  XXX Accept `None` only.
      --  XXX Share code with `Normal` session processing.

      case Decoded.HeaderDigest.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (HeaderDigest_Key, Reject_Value);

            --  XXX Should error be reported ???

         when Value =>
            if Decoded.HeaderDigest.Value = None_Value then
               Append_Key_Value (HeaderDigest_Key, None_Value);

            else
               Append_Key_Value (HeaderDigest_Key, Reject_Value);
            end if;
      end case;

      --  InitiatorAlias, Declarative, optional

      case Decoded.InitiatorAlias.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (InitiatorAlias_Key, Reject_Value);

         when Value =>
            InitiatorAlias := Decoded.InitiatorAlias.Value;
      end case;

      --  InitiatorName, Declarative, must be provided

      case Decoded.InitiatorName.Kind is
         when None =>
            Set_Error_Missing_Parameter;

         when Error =>
            Append_Key_Value (InitiatorName_Key, Reject_Value);

         when Value =>
            InitiatorName := Decoded.InitiatorName.Value;
      end case;

      --  MaxConnections, irrelevant when SessionType = Discovery

      case Decoded.MaxConnections.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxConnections_Key, Reject_Value);

         when Value =>
            Append_Key_Value (MaxConnections_Key, Irrelevant_Value);
      end case;

      --  MaxRecvDataSegmentLength, Declarative

      case Decoded.MaxRecvDataSegmentLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxRecvDataSegmentLength_Key, Reject_Value);

         when Value =>
            Initiator_MaxRecvDataSegmentLength :=
              A0B.Types.Unsigned_24 (Decoded.MaxRecvDataSegmentLength.Value);
      end case;

      --  SendTargets, irrelevant in Login Request

      case Decoded.SendTargets.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (SendTargets_Key, Reject_Value);

         when Value =>
            Append_Key_Value (SendTargets_Key, Irrelevant_Value);
      end case;

      --  TargetAddress, never send by the initiator

      case Decoded.TargetAddress.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetAddress_Key, Reject_Value);

         when Value =>
            Append_Key_Value (TargetAddress_Key, Reject_Value);
      end case;

      --  TargetAlias, never send by the initiator

      case Decoded.TargetAlias.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetAlias_Key, Reject_Value);

         when Value =>
            Append_Key_Value (TargetAlias_Key, Reject_Value);
      end case;

      --  TargetName, Declarative, optional when SessionType = Discovery

      case Decoded.TargetName.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetName_Key, Reject_Value);

         when Value =>
            TargetName := Decoded.TargetName.Value;
      end case;

      --  TargetPortalGroupTag, never send by the initiator, target must
      --  provide it on Login.

      case Decoded.TargetPortalGroupTag.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetPortalGroupTag_Key, Reject_Value);

            Set_Error_Initiator_Error;

         when Value =>
            Append_Key_Value (TargetPortalGroupTag_Key, Reject_Value);

            Set_Error_Initiator_Error;
      end case;

      --  DefaultTime2Wait         : Numerical_Value;
      --  DefaultTime2Retain       : Numerical_Value;
      --  MaxOutstandingR2T        : Numerical_Value;
      --  DataPDUInOrder           : Boolean_Value;
      --  DataSequenceInOrder      : Boolean_Value;
      --  ErrorRecoveryLevel       : Numerical_Value;
      --
      --  --  [RFC3720], obsolete in [RFC7143]
      --
      --  OFMarker                 : Boolean_Value;
      --  IFMarker                 : Boolean_Value;
      --  OFMarkInt                : Numerical_Value;
      --  IFMarkInt                : Numerical_Value;
      --
      --  --  [RFC7143]
      --
      --  TaskReporting            : List_Of_Values;
      --  X_NodeArchitecture       : List_Of_Values;
      --
      --  NotUnderstood            : Segment_Array (1 .. 8);

      --  Send some parameters

      --  if Decoded.TargetAlias.Kind = None then
      --     Append_Key_Value (TargetAlias_Key, "VG iSCSI target");
      --     --  XXX Send it only when configured !!!
      --  end if;

      --  Append_Key_Value (TargetAddress_Key, "127.0.0.1");
      --  XXX Send on redirect and on `SendTargets` only

      if Decoded.TargetPortalGroupTag.Kind = None then
         Append_Key_Value (TargetPortalGroupTag_Key, Natural'(1));
      end if;

      if Decoded.MaxRecvDataSegmentLength.Kind /= Error then
         Target_MaxRecvDataSegmentLength :=
           Configured_MaxRecvDataSegmentLength;
         Append_Key_Value
           (MaxRecvDataSegmentLength_Key, Target_MaxRecvDataSegmentLength);
      end if;

      raise Program_Error;
   end Validate_Discovery_Session;

   -----------------------------
   -- Validate_Normal_Session --
   -----------------------------

   procedure Validate_Normal_Session
     (Decoded : Decoded_Operational_Parameters)
   is
      use type A0B.Types.Unsigned_24;
      use type iSCSI.Text.Segment;

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : String);

      ----------------------
      -- Append_Key_Value --
      ----------------------

      procedure Append_Key_Value
        (Key   : iSCSI.Text.UTF8_String;
         Value : String)
      is
         V : constant iSCSI.Text.UTF8_String (1 .. Value'Length)
           with Import, Address => Value'Address;

      begin
         Append_Key_Value (Key, V);
      end Append_Key_Value;

      RFC7143 : constant := 1;
      RFC7144 : constant := 2;

      iSCSIProtocolLevel                 : Natural  := RFC7143;
      MaxConnections                     : Positive := 1;
      TargetName                         : iSCSI.Text.Segment
        with Unreferenced;
      InitiatorName                      : iSCSI.Text.Segment
        with Unreferenced;
      InitiatorAlias                     : iSCSI.Text.Segment
        with Unreferenced;
      InitialR2T                         : Boolean  := True;
      ImmediateData                      : Boolean := True;
      Initiator_MaxRecvDataSegmentLength : A0B.Types.Unsigned_24 := 8_192
        with Unreferenced;
      Target_MaxRecvDataSegmentLength    : A0B.Types.Unsigned_24 := 8_192;
      MaxBurstLength                     : A0B.Types.Unsigned_24 := 262_144;
      FirstBurstLength                   : A0B.Types.Unsigned_24 := 65_536;

   begin
      --  iSCSIProtocolLevel

      case Decoded.iSCSIProtocolLevel.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (iSCSIProtocolLevel_Key, Reject_Value);

            Set_Error_Unsupported_Version;

         when Value =>
            iSCSIProtocolLevel :=
              Natural'Min
                (Natural (Decoded.iSCSIProtocolLevel.Value),
                 RFC7144);
            Append_Key_Value (iSCSIProtocolLevel_Key, iSCSIProtocolLevel);
      end case;

      --  ImmediateData

      case Decoded.ImmediateData.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (ImmediateData_Key, Reject_Value);

         when Value =>
            ImmediateData :=
              Configured_ImmediateData and Decoded.ImmediateData.Value;
            Append_Key_Value (ImmediateData_Key, ImmediateData);
      end case;

      --  InitialR2T

      case Decoded.InitialR2T.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (InitialR2T_Key, Reject_Value);

         when Value =>
            InitialR2T :=
              Configured_InitialR2T or Decoded.InitialR2T.Value;
            Append_Key_Value (InitialR2T_Key, InitialR2T);
      end case;

      --  MaxBurstLength

      case Decoded.MaxBurstLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxBurstLength_Key, Reject_Value);

         when Value =>
            MaxBurstLength :=
              A0B.Types.Unsigned_24'Min
                (Configured_MaxBurstLength,
                 A0B.Types.Unsigned_24
                   (Decoded.MaxBurstLength.Value));
            Append_Key_Value (MaxBurstLength_Key, MaxBurstLength);
      end case;

      --  DataDigest
      --
      --  XXX Doesn't support list of values.
      --  XXX Accept `None` only.
      --  XXX Share code with `Discovery` session processing.

      case Decoded.DataDigest.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (DataDigest_Key, Reject_Value);

            --  XXX Should error be reported ???

         when Value =>
            if Decoded.DataDigest.Value = None_Value then
               Append_Key_Value (DataDigest_Key, None_Value);

            else
               Append_Key_Value (DataDigest_Key, Reject_Value);
            end if;
      end case;

      --  FirstBurstLength,
      --  irrelevant when InitialR2T=Yes and ImmediateData=No

      case Decoded.FirstBurstLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (FirstBurstLength_Key, Reject_Value);

         when Value =>
            if InitialR2T and not ImmediateData then
               Append_Key_Value (FirstBurstLength_Key, Irrelevant_Value);

            else
               FirstBurstLength :=
                 A0B.Types.Unsigned_24'Min
                   (Configured_FirstBurstLength,
                    A0B.Types.Unsigned_24 (Decoded.FirstBurstLength.Value));
               Append_Key_Value (FirstBurstLength_Key, FirstBurstLength);

               if FirstBurstLength > MaxBurstLength then
                  Set_Error_Initiator_Error;
               end if;
            end if;
      end case;

      --  HeaderDigest
      --
      --  XXX Doesn't support list of values.
      --  XXX Accept `None` only.
      --  XXX Share code with `Discovery` session processing.

      case Decoded.HeaderDigest.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (HeaderDigest_Key, Reject_Value);

            --  XXX Should error be reported ???

         when Value =>
            if Decoded.HeaderDigest.Value = None_Value then
               Append_Key_Value (HeaderDigest_Key, None_Value);

            else
               Append_Key_Value (HeaderDigest_Key, Reject_Value);
            end if;
      end case;

      --  InitiatorAlias, Declarative, optional

      case Decoded.InitiatorAlias.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (InitiatorAlias_Key, Reject_Value);

         when Value =>
            InitiatorAlias := Decoded.InitiatorAlias.Value;
      end case;

      --  InitiatorName, Declarative, must be provided

      case Decoded.InitiatorName.Kind is
         when None =>
            Set_Error_Missing_Parameter;

         when Error =>
            Append_Key_Value (InitiatorName_Key, Reject_Value);

         when Value =>
            InitiatorName := Decoded.InitiatorName.Value;
      end case;

      --  MaxConnections

      case Decoded.MaxConnections.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxConnections_Key, Reject_Value);

         when Value =>
            MaxConnections :=
              Positive'Min
                (Positive (Decoded.MaxConnections.Value),
                 Configured_MaxConnections);
            Append_Key_Value (MaxConnections_Key, MaxConnections);
      end case;

      --  MaxRecvDataSegmentLength, Declarative

      case Decoded.MaxRecvDataSegmentLength.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (MaxRecvDataSegmentLength_Key, Reject_Value);

         when Value =>
            Initiator_MaxRecvDataSegmentLength :=
              A0B.Types.Unsigned_24 (Decoded.MaxRecvDataSegmentLength.Value);
      end case;

      --  SendTargets, irrelevant in Login Request

      case Decoded.SendTargets.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (SendTargets_Key, Reject_Value);

         when Value =>
            Append_Key_Value (SendTargets_Key, Irrelevant_Value);
      end case;

      --  TargetAddress, never send by the initiator

      case Decoded.TargetAddress.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetAddress_Key, Reject_Value);

         when Value =>
            Append_Key_Value (TargetAddress_Key, Reject_Value);
      end case;

      --  TargetAlias, never send by the initiator

      case Decoded.TargetAlias.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetAlias_Key, Reject_Value);

         when Value =>
            Append_Key_Value (TargetAlias_Key, Reject_Value);
            --  XXX Is it correct when TargetAlias is configured ???
      end case;

      --  TargetName, Declarative, required when SessionType = Normal

      case Decoded.TargetName.Kind is
         when None =>
            Set_Error_Missing_Parameter;

         when Error =>
            Append_Key_Value (TargetName_Key, Reject_Value);

         when Value =>
            TargetName := Decoded.TargetName.Value;
      end case;

      --  TargetPortalGroupTag, never send by the initiator, target must
      --  provide it on Login.

      case Decoded.TargetPortalGroupTag.Kind is
         when None =>
            null;

         when Error =>
            Append_Key_Value (TargetPortalGroupTag_Key, Reject_Value);

            Set_Error_Initiator_Error;

         when Value =>
            Append_Key_Value (TargetPortalGroupTag_Key, Reject_Value);

            Set_Error_Initiator_Error;
      end case;

      --  DefaultTime2Wait         : Numerical_Value;
      --  DefaultTime2Retain       : Numerical_Value;
      --  MaxOutstandingR2T        : Numerical_Value;
      --  DataPDUInOrder           : Boolean_Value;
      --  DataSequenceInOrder      : Boolean_Value;
      --  ErrorRecoveryLevel       : Numerical_Value;
      --
      --  --  [RFC3720], obsolete in [RFC7143]
      --
      --  OFMarker                 : Boolean_Value;
      --  IFMarker                 : Boolean_Value;
      --  OFMarkInt                : Numerical_Value;
      --  IFMarkInt                : Numerical_Value;
      --
      --  --  [RFC7143]
      --
      --  TaskReporting            : List_Of_Values;
      --  X_NodeArchitecture       : List_Of_Values;
      --
      --  NotUnderstood            : Segment_Array (1 .. 8);

      --  Send some parameters

      if Decoded.TargetAlias.Kind = None then
         Append_Key_Value (TargetAlias_Key, "VG iSCSI target");
         --  XXX Send it only when configured !!!
      end if;

      --  Append_Key_Value (TargetAddress_Key, "127.0.0.1");
      --  XXX Send on redirect and on `SendTargets` only

      if Decoded.TargetPortalGroupTag.Kind = None then
         Append_Key_Value (TargetPortalGroupTag_Key, Natural'(1));
      end if;

      if Decoded.MaxRecvDataSegmentLength.Kind /= Error then
         Target_MaxRecvDataSegmentLength :=
           Configured_MaxRecvDataSegmentLength;
         Append_Key_Value
           (MaxRecvDataSegmentLength_Key, Target_MaxRecvDataSegmentLength);
      end if;

      raise Program_Error;
   end Validate_Normal_Session;

end iSCSI.Target.Login;
