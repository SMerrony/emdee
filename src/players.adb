--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib.Error;
with Glib.Spawn;     use Glib.Spawn;

with Gtkada.Types;

with Interfaces.C;

with Players;        use Players;
with Session;        use Session;
with Track;          use Track;

package body Players is

   package C renames Interfaces.C;

   function system (command : C.char_array) return C.int
     with Import, Convention => C;

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
      Argv : aliased Gtkada.Types.Chars_Ptr_Array := (0 .. 15 => <>);
      Env  : aliased Gtkada.Types.Chars_Ptr_Array := (0 .. 5 => <>);

      procedure Prepare_Ffplay_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         argv (0) := Gtkada.Types.New_String ("ffplay");
         argv (1) := Gtkada.Types.New_String ("-hide_banner");
         argv (2) := Gtkada.Types.New_String ("-nodisp");
         argv (3) := Gtkada.Types.New_String ("-autoexit");
         argv (4) := Gtkada.Types.New_String ("-loglevel");
         argv (5) := Gtkada.Types.New_String ("quiet");
         argv (6) := Gtkada.Types.New_String (Media_File);
         argv (7) := Gtkada.Types.Null_Ptr;
      end Prepare_Ffplay_Arguments;

      --  procedure Prepare_Mpg123_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      --  begin
      --     argv (0) := Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player));
      --     argv (1) := Gtkada.Types.New_String ("-q");
      --     argv (2) := Gtkada.Types.New_String ("-o");
      --     argv (3) := Gtkada.Types.New_String ("pulse");
      --     --  argv (3) := Gtkada.Types.New_String ("--no-control");
      --     argv (4) := Gtkada.Types.New_String (Media_File);
      --     argv (5) := Gtkada.Types.Null_Ptr;
      --  end Prepare_Mpg123_Arguments;

      --  procedure Prepare_Paplay_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      --  begin
      --     argv (0) := Gtkada.Types.New_String (To_String (Active_Players_Config.WAV_Player));
      --     --  argv (1) := Gtkada.Types.New_String ("-q");
      --     --  argv (2) := Gtkada.Types.New_String ("-o");
      --     --  argv (3) := Gtkada.Types.New_String ("jack");
      --     --  argv (3) := Gtkada.Types.New_String ("--no-control");
      --     argv (1) := Gtkada.Types.New_String (Media_File);
      --     argv (2) := Gtkada.Types.Null_Ptr;
      --  end Prepare_Paplay_Arguments;

      procedure Prepare_Env (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         if not Ada.Environment_Variables.Exists (PulseAudio_Env_Dir) then
            raise PulseAudio_Not_Found;
         end if;
         argv (0) := Gtkada.Types.New_String (PulseAudio_Env_Dir & "=" & Ada.Environment_Variables.Value (PulseAudio_Env_Dir));
         argv (1) := Gtkada.Types.Null_Ptr;
      end Prepare_Env;

   begin

      --  -- THIS WORKS...
      --  OK := Spawn_Command_Line_Async (Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player) &
      --                                                           " " & Media_File), Err);
      --  -- ...THAT WORKED

      case Track.File_Type is
         when FLAC | MP3 | OGG | WAV =>
            Prepare_Ffplay_Arguments (Argv);
         when MIDI =>
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
            Player_PID := 0;  --  N.B. Clear the PID if the process has finished
         end if;
      end if;
      return Running;
   end Player_Active;

   procedure Stop_Playing is
      command : aliased constant C.char_array := C.To_C ("kill " & Player_PID'Image);
      Unused_rc : C.int;
   begin
      if Player_Active then
         --  This is just gross - there must be a better way...
         Unused_rc := system (command);
         Glib.Spawn.Spawn_Close_Pid (Player_PID);
         Player_PID := 0;
      end if;
   end Stop_Playing;

end Players;