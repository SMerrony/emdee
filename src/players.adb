--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Directories;
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

   function Prepare_Ffplay_Arguments (Media_File : String;
                                      Volume : Integer)
                                      return Gtkada.Types.Chars_Ptr_Array is
      Volume_Str : constant String := Trim (Volume'Image, Left);
      argv : Gtkada.Types.Chars_Ptr_Array (0 .. 15);
   begin
      argv (0) := Gtkada.Types.New_String ("ffplay");
      argv (1) := Gtkada.Types.New_String ("-hide_banner");
      argv (2) := Gtkada.Types.New_String ("-nodisp");
      argv (3) := Gtkada.Types.New_String ("-autoexit");
      argv (4) := Gtkada.Types.New_String ("-loglevel");
      argv (5) := Gtkada.Types.New_String ("quiet");
      argv (6) := Gtkada.Types.New_String ("-volume");
      argv (7) := Gtkada.Types.New_String (Volume_Str);
      argv (8) := Gtkada.Types.New_String (Media_File);
      argv (9) := Gtkada.Types.Null_Ptr;
      return argv;
   end Prepare_Ffplay_Arguments;

   procedure Play_Track is
      use Glib;
      Track      : constant Track_T := Active_Session.Tracks (Currently_Playing_Track);
      Media_File : constant String := To_String (Track.Path);
      Okay : Glib.Gboolean;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Gtkada.Types.Chars_Ptr_Array := (0 .. 15 => <>);

   begin
      case Track.File_Type is
         when FLAC | MP3 | OGG | WAV =>
            --  Prepare_Ffplay_Arguments (Argv);
            Argv := Prepare_Ffplay_Arguments (Media_File, Track.Volume);
         when MIDI =>
null;
         when UNKNOWN => raise Unknown_Media_Type;
      end case;

      Okay := Spawn_Async (Working_Directory => Gtkada.Types.Null_Ptr,
                           Argv => Argv'Access,
                           Envp => null,  --  Inherit our env, critical that XDG_RUNTIME_DIR exists
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