--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Directories;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

with Glib.Error;
with Glib.Spawn;     use Glib.Spawn;

with Gtkada.Types;

with Interfaces.C;

with TOML;           use TOML;
with TOML.File_IO;

with Players;        use Players;
with Session;        use Session;
with Track;          use Track;

package body Players is

   package C renames Interfaces.C;

   function system (command : C.char_array) return C.int
     with Import, Convention => C;

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

   --  function Play_MIDI (Filename : String) return Process_Id is
   --     PID : Process_Id;
   --  begin
   --     PID := Non_Blocking_Spawn (Program_Name => To_String (Active_Players_Config.WAV_Player),
   --                                Args => (new String'("-q"), new String'(Filename)));
   --     return PID;
   --  end Play_MIDI;

   function Spawn_Async is
      new Generic_Spawn_Async (User_Data => Integer);

   procedure Play_Track is
      use Glib;
      Track : constant Track_T := Active_Session.Tracks (Currently_Playing_Track);
      Media_File : constant String := To_String (Track.Path);

      --  OK : Boolean;
      Okay : Glib.Gboolean;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Gtkada.Types.Chars_Ptr_Array := (0 .. 5 => <>);
      Env  : aliased Gtkada.Types.Chars_Ptr_Array := (0 .. 0 => <>);

      procedure Prepare_Mpg123_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         argv (0) := Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player));
         argv (1) := Gtkada.Types.New_String ("-q");
         argv (2) := Gtkada.Types.New_String ("-o");
         argv (3) := Gtkada.Types.New_String ("jack");
         --  argv (3) := Gtkada.Types.New_String ("--no-control");
         argv (4) := Gtkada.Types.New_String (Media_File);
         argv (5) := Gtkada.Types.Null_Ptr;
      end Prepare_Mpg123_Arguments;

      procedure Prepare_Env (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         argv (0) := Gtkada.Types.Null_Ptr;
      end Prepare_Env;

   begin

      --  -- THIS WORKS...
      --  OK := Spawn_Command_Line_Async (Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player) &
      --                                                           " " & Media_File), Err);
      --  -- ...THAT WORKED

      case Track.File_Type is
         when MIDI =>
null;
         when MP3  =>
            Prepare_Mpg123_Arguments (Argv);
         when OGG  =>
null;
         when WAV  =>
null;
         when UNKNOWN => raise Unknown_Media_Type;
      end case;

      Prepare_Env (Env);

      Okay := Spawn_Async (Working_Directory => Gtkada.Types.Null_Ptr,
                           Argv => Argv'Access,
                           Envp => Env'Access,
                           Flags => G_Spawn_Search_Path, --  + G_Spawn_Do_Not_Reap_Child,
                           Child_Setup => null,
                           Data => null,
                           Child_Pid => Player_PID'Access,
                           Error => PErr'Access
                           );

      if Okay = 0 then
         Ada.Text_IO.Put_Line ("ERROR");
      end if;

      Ada.Text_IO.Put_Line ("DEBUG: PID:" & Player_PID'Image);

   end Play_Track;

   function Player_Active return Boolean is
      Running : Boolean := False;
   begin
      if Player_PID /= 0 then
         Running := Ada.Directories.Exists ("/proc/" & Trim (Player_PID'Image, Left));
         if not Running then
            Player_PID := 0;  --  N.B. Clear the PID if the process has died
         end if;
      end if;
      return Running;
   end Player_Active;

   procedure Stop_Playing is
      command : aliased constant C.char_array := C.To_C ("kill " & Player_PID'Image);
      Unused_rc : C.int;
   begin
      --  This is just gross - there must be a better way...
      Unused_rc := system (command);
      Glib.Spawn.Spawn_Close_Pid (Player_PID);
   end Stop_Playing;

end Players;