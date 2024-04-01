--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package Players is

   type Players_T is record
      Config_Desc : Unbounded_String;
      MIDI_Player,
      MP3_Player,
      OGG_Player,
      WAV_Player  : Unbounded_String;
   end record;

   Active_Players_Config : Players_T;

   Default_Players_File : constant String := "players.toml";

   --  TOML exceptions
   Could_Not_Parse,
   Duplicate_Configuration,
   Incomplete_Configuration,
   Unknown_Configuration_Item : exception;

   procedure Load_Player_Config (Filename : String);

end Players;