--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Track;

package body Track is

   function Ends_With (Source, Pattern : String) return Boolean is
   begin
      return Pattern'Length <= Source'Length
      and then Source (Source'Last - Pattern'Length + 1 .. Source'Last) = Pattern;
   end Ends_With;

   function Guess_Media_Type (Filename : String) return Media_Type is
      Result : Media_Type := UNKNOWN;
   begin
      if Ends_With (Filename, "MP3") then
         Result := MP3;
      elsif Ends_With (Filename, "mp3") then
         Result := MP3;
      elsif Ends_With (Filename, "OGG") then
         Result := OGG;
      elsif Ends_With (Filename, "ogg") then
         Result := OGG;
      elsif Ends_With (Filename, "WAV") then
         Result := WAV;
      elsif Ends_With (Filename, "wav") then
         Result := WAV;
      elsif Ends_With (Filename, "MID") then
         Result := MIDI;
      elsif Ends_With (Filename, "mid") then
         Result := MIDI;
      elsif Ends_With (Filename, "midi") then
         Result := MIDI;
      end if;
      return Result;
   end Guess_Media_Type;

end Track;