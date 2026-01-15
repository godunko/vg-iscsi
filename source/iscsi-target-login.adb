--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Storage_Elements;

with iSCSI.PDUs;
with iSCSI.Text;

package body iSCSI.Target.Login is

   DataDigest_String           : constant String := "DataDigest";
   DataPDUInOrder_String       : constant String := "DataPDUInOrder";
   DataSequenceInOrder_String  : constant String := "DataSequenceInOrder";
   DefaultTime2Retain_String   : constant String := "DefaultTime2Retain";
   DefaultTime2Wait_String     : constant String := "DefaultTime2Wait";
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
   OFMarker_String             : constant String := "OFMarker";
   OFMarkInt_String            : constant String := "OFMarkInt";
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

   -------------
   -- Process --
   -------------

   procedure Process
     (Header_Address : System.Address;
      Data_Address   : System.Address)
   is
      use type iSCSI.Text.UTF8_String;

      Header : constant iSCSI.PDUs.Login_Request_Header
        with Import, Address => Header_Address;
      Parser : iSCSI.Text.Parser;

   begin
      iSCSI.Text.Initialize
        (Parser,
         Data_Address,
         System.Storage_Elements.Storage_Count (Header.DataSegmentLength));

      while iSCSI.Text.Forward (Parser) loop
         declare
            Key : constant iSCSI.Text.UTF8_String :=
              iSCSI.Text.Text (iSCSI.Text.Key (Parser));

         begin
            if Key = DataDigest_Key then
               raise Program_Error;

            elsif Key = DataPDUInOrder_Key then
               raise Program_Error;

            elsif Key = DataSequenceInOrder_Key then
               raise Program_Error;

            elsif Key = DefaultTime2Retain_Key then
               raise Program_Error;

            elsif Key = DefaultTime2Wait_Key then
               raise Program_Error;

            elsif Key = ErrorRecoveryLevel_Key then
               raise Program_Error;

            elsif Key = FirstBurstLength_Key then
               raise Program_Error;

            elsif Key = HeaderDigest_Key then
               raise Program_Error;

            elsif Key = IFMarker_Key then
               raise Program_Error;

            elsif Key = IFMarkInt_Key then
               raise Program_Error;

            elsif Key = ImmediateData_Key then
               raise Program_Error;

            elsif Key = InitialR2T_Key then
               raise Program_Error;

            elsif Key = InitiatorAlias_Key then
               raise Program_Error;

            elsif Key = InitiatorName_Key then
               raise Program_Error;

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
               raise Program_Error;

            elsif Key = OFMarkInt_Key then
               raise Program_Error;

            elsif Key = SendTargets_Key then
               raise Program_Error;

            elsif Key = SessionType_Key then
               raise Program_Error;

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
   end Process;

end iSCSI.Target.Login;
