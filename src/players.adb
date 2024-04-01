--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with TOML;            use TOML;
with TOML.File_IO;

package body Players is

   procedure Load_Player_Config (Filename : String) is
      Toml_Parse_Result : Read_Result;
   begin
      Toml_Parse_Result := TOML.File_IO.Load_File (Filename);
      if not Toml_Parse_Result.Success then
         raise Could_Not_Parse with To_String (Toml_Parse_Result.Message);
      end if;

      declare
         Val : TOML_Value;
      begin
         Val := Get_Or_Null (Toml_Parse_Result.Value, "players");
         if Val.Is_Null then
            raise Incomplete_Configuration with "Missing [players] section";
         end if;

         if Has (Val, "description") then
            Active_Players_Config.Config_Desc := As_Unbounded_String (Get (Val, "description"));
         else
            raise Incomplete_Configuration with "Config Description not configured";
         end if;

         if Has (Val, "midi_player") then
            Active_Players_Config.MIDI_Player := As_Unbounded_String (Get (Val, "midi_player"));
         else
            raise Incomplete_Configuration with "MIDI Player not configured";
         end if;

         if Has (Val, "mp3_player") then
            Active_Players_Config.MP3_Player := As_Unbounded_String (Get (Val, "mp3_player"));
         else
            raise Incomplete_Configuration with "MP3 Player not configured";
         end if;

         if Has (Val, "ogg_player") then
            Active_Players_Config.OGG_Player := As_Unbounded_String (Get (Val, "ogg_player"));
         else
            raise Incomplete_Configuration with "OGG Player not configured";
         end if;

         if Has (Val, "wav_player") then
            Active_Players_Config.WAV_Player := As_Unbounded_String (Get (Val, "wav_player"));
         else
            raise Incomplete_Configuration with "WAV Player not configured";
         end if;
         Active_Players_Config.WAV_Player := As_Unbounded_String (Get (Val, "wav_player"));

      end; --  declare
   end Load_Player_Config;

end Players;