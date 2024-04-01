--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Track is

   type Known_Types is (MIDI, MP3, OGG, WAV);

   type Track_T is record
      Title,
      Path,
      Comment   : Unbounded_String;
      Tempo,
      Volume    : Float;
      Skip      : Boolean;
      File_Type : Known_Types;
   end record;

end Track;