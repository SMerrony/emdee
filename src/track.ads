--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Track is

   type Media_Type is (FLAC, MIDI, MP3, OGG, WAV, UNKNOWN);

   type Track_T is record
      Title,
      Path,
      Comment   : Unbounded_String;
      Volume    : Integer;
      Skip      : Boolean;
      File_Type : Media_Type;
   end record;

   function Guess_Media_Type (Filename : String) return Media_Type;

end Track;