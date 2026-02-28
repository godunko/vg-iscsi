--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C;
with Interfaces.C_Streams;

package body Target.File is

   Block_Length : constant := 512;

   File : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.NULL_Stream;

   procedure Open_File;

   --  procedure Close_File;

   ----------------
   -- Close_File --
   ----------------

   --  procedure Close_File is
   --     use type Interfaces.C_Streams.FILEs;
   --
   --  begin
   --     if File /= Interfaces.C_Streams.NULL_Stream then
   --        if Interfaces.C_Streams.fclose (File) /= 0 then
   --           raise Program_Error;
   --        end if;
   --
   --        File := Interfaces.C_Streams.NULL_Stream;
   --     end if;
   --  end Close_File;

   -----------------
   -- Data_Length --
   -----------------

   function Data_Length
     (Transfer_Length : A0B.Types.Unsigned_32) return A0B.Types.Unsigned_64
   is
      use type A0B.Types.Unsigned_64;

   begin
      return Block_Length * A0B.Types.Unsigned_64 (Transfer_Length);
   end Data_Length;

   --------------
   -- Last_LBA --
   --------------

   function Last_LBA return A0B.Types.Unsigned_64 is
      use type A0B.Types.Unsigned_64;

   begin
      Open_File;

      if Interfaces.C_Streams.fseek64
           (File, 0, Interfaces.C_Streams.SEEK_END) /= 0
      then
         raise Program_Error;
      end if;

      return
        A0B.Types.Unsigned_64 (Interfaces.C_Streams.ftell64 (File)) / Block_Length - 1;
   end Last_LBA;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File is
      use type Interfaces.C_Streams.FILEs;

   begin
      if File = Interfaces.C_Streams.NULL_Stream then
         declare
            filename : Interfaces.C.char_array := Interfaces.C.To_C ("disk.img");
            filemode : Interfaces.C.char_array := Interfaces.C.To_C ("r+b");

         begin
            File :=
              Interfaces.C_Streams.fopen (filename'Address, filemode'Address);
         end;
      end if;
   end Open_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Descriptor     : SCSI.Commands.SBC.READ_Command_Descriptor;
      Data_In_Buffer : in out SCSI.Buffers.Data_Buffer'Class)
   is
      use type A0B.Types.Unsigned_32;
      use type A0B.Types.Unsigned_64;
      use type Interfaces.C_Streams.size_t;

      Length  : constant A0B.Types.Unsigned_32 :=
        Descriptor.TRANSFER_LENGTH * Block_Length;
      Address : System.Address;

   begin
      Open_File;

      if Data_In_Buffer.Allocate (Length, Address) then
         if Interfaces.C_Streams.fseek64
           (File,
            Interfaces.C_Streams.int64
              (Descriptor.LOGICAL_BLOCK_ADDRESS * Block_Length),
            Interfaces.C_Streams.SEEK_SET) /= 0
         then
            raise Program_Error;
         end if;

         if Interfaces.C_Streams.fread
           (buffer => Address,
            index  => 0,
            size   => Block_Length,
            count  => Interfaces.C_Streams.size_t (Descriptor.TRANSFER_LENGTH),
            stream => File)
              /= Interfaces.C_Streams.size_t (Descriptor.TRANSFER_LENGTH)
         then
            raise Program_Error;
         end if;

      else
         raise Program_Error;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Descriptor      : SCSI.Commands.SBC.WRITE_Command_Descriptor;
      Buffer_Offset   : A0B.Types.Unsigned_32;
      Storage_Address : System.Address;
      Data_Length     : A0B.Types.Unsigned_32)
   is
      use type A0B.Types.Unsigned_64;
      use type Interfaces.C_Streams.size_t;

   begin
      Open_File;

      if Interfaces.C_Streams.fseek64
        (File,
         Interfaces.C_Streams.int64
           (Descriptor.LOGICAL_BLOCK_ADDRESS * Block_Length
            + A0B.Types.Unsigned_64 (Buffer_Offset)),
         Interfaces.C_Streams.SEEK_SET) /= 0
      then
         raise Program_Error;
      end if;

      if Interfaces.C_Streams.fwrite
        (buffer => Storage_Address,
         size   => 1,
         count  => Interfaces.C_Streams.size_t (Data_Length),
         stream => File) /= Interfaces.C_Streams.size_t (Data_Length)
      then
         raise Program_Error;
      end if;

      if Interfaces.C_Streams.fflush (File) /= 0 then
         raise Program_Error;
      end if;
   end Write;

end Target.File;
