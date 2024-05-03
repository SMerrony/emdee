--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Track is

   function Ends_With (Source, Pattern : String) return Boolean is
   begin
      return
         Pattern'Length <= Source'Length and then
         Source (Source'Last - Pattern'Length + 1 .. Source'Last) = Pattern;
   end Ends_With;

   function Guess_Media_Type (Filename : String) return Media_Type is
      Result      : Media_Type := UNKNOWN;
      UC_Filename : constant String := To_Upper (Filename);
   begin
      if Ends_With (UC_Filename, "MP3") then
         Result := MP3;
      elsif Ends_With (UC_Filename, "OGG") then
         Result := OGG;
      elsif Ends_With (UC_Filename, "WAV") then
         Result := WAV;
      elsif Ends_With (UC_Filename, "FLAC") then
         Result := FLAC;
      elsif Ends_With (UC_Filename, "MID") then
         Result := MIDI;
      elsif Ends_With (UC_Filename, "MIDI") then
         Result := MIDI;
      elsif Ends_With (UC_Filename, "SMF") then
         Result := MIDI;
      elsif Ends_With (UC_Filename, "KAR") then
         Result := MIDI;
      end if;
      return Result;
   end Guess_Media_Type;

end Track;