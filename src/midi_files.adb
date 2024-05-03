--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Directories;
with Ada.Sequential_IO;

with Interfaces;

with Embedded;       use Embedded;

package body Midi_Files is

   procedure Create_Notes_Off_MIDI is
      package IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);
      File_Emb       : constant Content_Type := Get_Content (Notes_Off_Embedded);
      MIDI_Filename  : constant String := Notes_Off_Embedded;
      MIDI_File      : IO.File_Type;
   begin
      if Ada.Directories.Exists (MIDI_Filename) then
         Ada.Directories.Delete_File (MIDI_Filename);
      end if;
      IO.Create (File => MIDI_File, Name => MIDI_Filename);
      for Val of File_Emb.Content.all loop
         IO.Write (MIDI_File, Interfaces.Unsigned_8 (Val));
      end loop;
      IO.Close (MIDI_File);
   end Create_Notes_Off_MIDI;

end Midi_Files;