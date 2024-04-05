--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Glib.Spawn;

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

   --  Player Process exceptions
   Already_Playing,
   Player_Error,
   Unknown_Media_Type : exception;

   Currently_Playing_Track : Integer := 1;

   Player_PID : aliased Glib.Spawn.GPid := 0;

   procedure Load_Player_Config (Filename : String);
   procedure Play_Track;
   procedure Stop_Playing;

   function Player_Active return Boolean; --  May update PID

end Players;