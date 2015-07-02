pragma License (GPL);
------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: GNU GPLv3 or any later as published by Free Software Foundation --
-- (see COPYING file)                                                       --
--                                                                          --
--                    Copyright © 2014 - 2015, darkestkhan                  --
------------------------------------------------------------------------------
--  This Program is Free Software: You can redistribute it and/or modify    --
--  it under the terms of The GNU General Public License as published by    --
--    the Free Software Foundation, either version 3 of the license, or     --
--                (at Your option) any later version.                       --
--                                                                          --
--      This Program is distributed in the hope that it will be useful,     --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of      --
--      MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the        --
--              GNU General Public License for more details.                --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--   along with this program. If not, see <http://www.gnu.org/licenses/>.   --
------------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- Small program which uses OpenAL for playing RIFF WAVE files.          --
  ---------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Oto.AL;
with Oto.ALC;
with Oto.ALUT;
use Oto;
procedure Play_WAVE is

  ---------------------------------------------------------------------------

  package CLI renames Ada.Command_Line;
  package TIO renames Ada.Text_IO;
  package UByte_IO is new Ada.Sequential_IO (AL.UByte);

  ---------------------------------------------------------------------------

  use type AL.Bool;
  use type ALC.Enum;

  type Floats is array (Positive range <>) of Float;
  type UBytes is array (Positive range <>) of AL.UByte;
  type UBytes_Access is access UBytes;

  procedure Free is new Ada.Unchecked_Deallocation (UBytes, UBytes_Access);

  -- Not the best way to represent wav file but does work.
  type WAV is
  record
    Format: AL.Enum;
    Data  : UBytes_Access;
    Samples_At: Integer;
    Size  : AL.SizeI;
    Freq  : AL.SizeI;
  end record;

  ---------------------------------------------------------------------------
  -- Procedure for parsing RIFF WAVE file.
  -- WARNING: Extensive use of object overlays.

  Not_A_WAV_File: exception;
  Unsupported_Format: exception;

  procedure Read (Name: in String; Item: in out WAV)
  is
    FD: UByte_IO.File_Type;
    Data_Block_At: Integer;
  begin
    Free (Item.Data); -- Free data beforehand

    UByte_IO.Open (FD, UByte_IO.In_File, Name);

    pragma Assert (AL.UByte'Size = Character'Size);

    Check_If_RIFF: declare
      RIFF_ID : UBytes (1 .. 4);
      RIFF    : String (1 .. 4);
      for RIFF'Address use RIFF_ID'Address;
    begin
      for K in RIFF_ID'Range loop
        UByte_IO.Read (FD, RIFF_ID (K));
      end loop;

      if RIFF /= "RIFF" then
        UByte_IO.Close (FD);
        raise Not_A_WAV_File;
      end if;
    end Check_If_RIFF;

    Get_File_Size: declare
      Size_Bytes: UBytes (1 .. 4);
      Size      : Integer;
      for Size'Address use Size_Bytes'Address;
    begin
      for K in Size_Bytes'Range loop
        UByte_IO.Read (FD, Size_Bytes (K));
      end loop;

      Item.Size := Size;
      TIO.Put_Line ("File Size is :" & Integer'Image (Item.Size + 8));
    end Get_File_Size;

    Get_Content:
    begin
      Item.Data := new UBytes (1 .. Item.Size);
      for K in Item.Data.all'Range loop
        UByte_IO.Read (FD, Item.Data.all (K));
      end loop;
      UByte_IO.Close (FD);
    end Get_Content;

    Check_If_WAVE: declare
      WAVE      : String (1 .. 4);
      for WAVE'Address use Item.Data.all (1)'Address;
    begin
      if WAVE /= "WAVE" then
        raise Not_A_WAV_File;
      end if;
    end Check_If_WAVE;

    Check_If_fmt_Present: declare
      fmt       : String (1 .. 4);
      for fmt'Address use Item.Data.all (5)'Address;
    begin
      if fmt /= "fmt " then
        raise Not_A_WAV_File;
      end if;
    end Check_If_fmt_Present;

    Parse_fmt: declare
      use type AL.Short;

      Subchunk1_Size: Integer;
      for Subchunk1_Size'Address use Item.Data.all (9)'Address;
      Audio_Format  : AL.Short;
      for Audio_Format'Address use Item.Data.all (13)'Address;
      Num_Channels  : AL.Short;
      for Num_Channels'Address use Item.Data.all (15)'Address;
      Sample_Rate   : Integer;
      for Sample_Rate'Address use Item.Data.all (17)'Address;
      Byte_Rate     : Integer;
      for Byte_Rate'Address use Item.Data.all (21)'Address;
      Block_Align   : AL.Short;
      for Block_Align'Address use Item.Data.all (25)'Address;
      Bits_Per_Sample : AL.Short;
      for Bits_Per_Sample'Address use Item.Data.all (27)'Address;
    begin
      TIO.Put_Line ("Subchunk_Size:" & Integer'Image (Subchunk1_Size));
      Data_Block_At := 13 + Subchunk1_Size;
      TIO.Put_Line ("Audio Format :" & AL.Short'Image (Audio_Format));
      TIO.Put_Line ("Num Channels :" & AL.Short'Image (Num_Channels));
      TIO.Put_Line ("Sample Rate  :" & Integer'Image (Sample_Rate));
      Item.Freq := Sample_Rate;
      TIO.Put_Line ("Byte Rate    :" & Integer'Image (Byte_Rate));
      TIO.Put_Line ("Block Align  :" & AL.Short'Image (Block_Align));
      TIO.Put_Line ("Bits/Sample  :" & AL.Short'Image (Bits_Per_Sample));

      if Audio_Format /= 1 then
        raise Unsupported_Format with "Compressed WAVE not supported";
      end if;

      if Num_Channels = 1 then
        if Bits_Per_Sample = 8 then
          Item.Format := AL.AL_FORMAT_MONO8;
        elsif Bits_Per_Sample = 16 then
          Item.Format := AL.AL_FORMAT_MONO16;
        else
          raise Unsupported_Format
            with "MONO" & AL.Short'Image (Bits_Per_Sample);
        end if;
      elsif Num_Channels = 2 then
        if Bits_Per_Sample = 8 then
          Item.Format := AL.AL_FORMAT_STEREO8;
        elsif Bits_Per_Sample = 16 then
          Item.Format := AL.AL_FORMAT_STEREO16;
        else
          raise Unsupported_Format
            with "STEREO" & AL.Short'Image (Bits_Per_Sample);
        end if;
      else
        raise Unsupported_Format
          with "Too many channels" & AL.Short'Image (Num_Channels);
      end if;

      TIO.Put_Line ("Item.Format: " & AL.Enum'Image (Item.Format));
    end Parse_fmt;

    Get_Sample_Start_And_Size: declare
      data: String (1 .. 4);
      for data'Address use Item.Data.all (Data_Block_At)'Address;
      Data_Size: Integer;
      for Data_Size'Address use Item.Data.all (Data_Block_At + 4)'Address;
    begin
      if data /= "data" then
        raise Not_A_WAV_File;
      end if;

      Item.Size := AL.SizeI (Data_Size);
      Item.Samples_At := Data_Block_At + 8;
      TIO.Put_Line ("Data Size :" & Integer'Image (Data_Size));
      TIO.Put_Line ("Samples At:" & Integer'Image (Item.Samples_At));
    end Get_Sample_Start_And_Size;
  end Read;

  ---------------------------------------------------------------------------

  Info: AL.Bool;
  Error: AL.Enum;

  Buffer: AL.UInt;
  Source: AL.UInt;

  -- Source position and velocity
  Source_Pos: constant Floats := (0.0, 0.0, 0.0);
  Source_Vel: constant Floats := (0.0, 0.0, 0.0);

  -- Listener position, velocity and orientation
  Listener_Pos: constant Floats := (0.0, 0.0, 0.0);
  Listener_Vel: constant Floats := (0.0, 0.0, 0.0);
  Listener_Ori: constant Floats := (0.0, 0.0, -1.0, 0.0, 1.0, 0.0);

  File_Content: WAV;
begin
  -- Create OpenAL context.
  Info := ALUT.Init (ALUT.Null_String_Array);
  if Info = AL.AL_FALSE then
    raise Program_Error with "ALUT.Init failed to create OpenAL context." &
                              AL.Enum'Image (AL.Get_Error);
  end if;

  if CLI.Argument_Count /= 1 then
    raise Program_Error with "Incorrect argument passed to program.";
  end if;

  -- Read WAV file.
  Read (CLI.Argument (1), File_Content);

  -- Generate buffer and bind data to it.
  AL.Gen_Buffers (1, Buffer'Address);
  Error := AL.Get_Error;
  if Error /= AL.AL_NO_ERROR then
    raise Program_Error
      with "Error when generating buffer:" & AL.Enum'Image (Error);
  end if;
  AL.Buffer_Data
    ( Buffer,
      File_Content.Format,
      File_Content.Data.all (File_Content.Samples_At)'Address,
      File_Content.Size,
      File_Content.Freq
    );

  -- Bind buffer with source.
  AL.Gen_Sources (1, Source'Address);
  Error := AL.Get_Error;
  if Error /= AL.AL_NO_ERROR then
    raise Program_Error
      with "Error when generating source:" & AL.Enum'Image (Error);
  end if;

  AL.Source (Source, AL.AL_BUFFER, AL.Int (Buffer));
  AL.Source (Source, AL.AL_PITCH, 1.0);
  AL.Source (Source, AL.AL_GAIN, 1.0);
  AL.Source_FV (Source, AL.AL_POSITION, Source_Pos'Address);
  AL.Source_FV (Source, AL.AL_VELOCITY, Source_Vel'Address);
  AL.Source (Source, AL.AL_LOOPING, AL.Int (AL.AL_TRUE));
  Error := AL.Get_Error;
  if Error /= AL.AL_NO_ERROR then
    raise Program_Error
      with "Error when setting source properties:" & AL.Enum'Image (Error);
  end if;

  -- Set listener values.
  AL.Listener_FV (AL.AL_POSITION, Listener_Pos'Address);
  AL.Listener_FV (AL.AL_VELOCITY, Listener_Vel'Address);
  AL.Listener_FV (AL.AL_ORIENTATION, Listener_Ori'Address);
  Error := AL.Get_Error;
  if Error /= AL.AL_NO_ERROR then
    raise Program_Error
      with "Error when setting listener properties:" & AL.Enum'Image (Error);
  end if;

  -- Actually play WAVE file.
  AL.Source_Play (Source);

  declare
    C: Character;
  begin
    TIO.New_Line (2);
    TIO.Put_Line ("Press 'q' to quit this program.");
    loop
      TIO.Get_Immediate (C);
      case C is
      when 'q' => exit;
      when others => null;
      end case;
    end loop;
  end;

  -- Kill all data.
  AL.Delete_Sources (1, Source'Address);
  AL.Delete_Buffers (1, Buffer'Address);
  Error := AL.Get_Error;
  if Error /= AL.AL_NO_ERROR then
    raise Program_Error
      with "Error when destroying buffers:" & ALUT.Get_Error_String (Error);
  end if;

  Info := ALUT.Quit;
  if Info = AL.AL_FALSE then
    raise Program_Error
      with "ALUT.Quit failed to destroy OpenAL context. ALUT Error ID:" &
            ALUT.Get_Error_String (ALUT.Get_Error);
  end if;
end Play_WAVE;
