--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Glib.Spawn;

package Players is

   --  Player Process exceptions
   Already_Playing,
   Player_Error,
   PulseAudio_Not_Found,
   Unknown_Media_Type : exception;

   --  PulseAudio_Env_Dir : constant String := "XDG_RUNTIME_DIR";

   Currently_Playing_Track : Integer := 1;

   Player_PID : aliased Glib.Spawn.GPid := 0;

   procedure Play_Track;
   procedure Stop_Playing;
   function  Player_Active return Boolean; --  May update PID

private

end Players;