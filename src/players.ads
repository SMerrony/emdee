--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Glib.Spawn;

package Players is

   Silence_Emb_Name : constant String := "silence_1s.mp3";
   Silence_Tmp_Name : constant String := "eMDee_1s_silence.mp3";

   --  Player Process exceptions
   Already_Playing,
   Player_Error,
   PulseAudio_Not_Found,
   Unknown_Media_Type : exception;

   --  PulseAudio_Env_Dir : constant String := "XDG_RUNTIME_DIR";

   Currently_Playing_Track : Integer := 1;

   Player_PID : aliased Glib.Spawn.GPid := 0;
   
   procedure Create_1s_Silence_MP3;
   procedure Play_Track;
   procedure Stop_Playing;
   function  Player_Active return Boolean; --  May update PID

private

end Players;