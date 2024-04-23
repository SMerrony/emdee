--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

--  N.B. This is the WINDOWS-SPECIFIC Players package body

with Ada.Directories;

with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;           use Glib;
with Glib.Error;
with Glib.Spawn;     use Glib.Spawn;

with Gtkada.Types;   use Gtkada.Types;

with Interfaces;
with Interfaces.C;

with Win32.Winbase;

with Midi_Files;
with Players;        use Players;
with Session;        use Session;
with Track;          use Track;

package body Players is

   package C renames Interfaces.C;

   function system (command : C.char_array) return C.int
     with Import, Convention => C;

   function Spawn_Async is
      new Generic_Spawn_Async (User_Data => Integer);
   --  function Spawn_Async_With_Fds is
   --     new Generic_Spawn_Async_With_Fds (User_Data => Integer);

   function Prepare_Ffplay_Arguments (Media_File : String; Volume : Integer)
                                      return Chars_Ptr_Array is
      Volume_Str : constant String := Trim (Volume'Image, Left);
      Argv_Arr : Chars_Ptr_Array (0 .. 15);
   begin
      Argv_Arr (0) := New_String ("ffplay");
      Argv_Arr (1) := New_String ("-hide_banner");
      Argv_Arr (2) := New_String ("-nodisp");
      Argv_Arr (3) := New_String ("-autoexit");
      Argv_Arr (4) := New_String ("-loglevel");
      Argv_Arr (5) := New_String ("quiet");
      Argv_Arr (6) := New_String ("-volume");
      Argv_Arr (7) := New_String (Volume_Str);
      Argv_Arr (8) := New_String (Media_File);
      Argv_Arr (9) := Null_Ptr;
      return Argv_Arr;
   end Prepare_Ffplay_Arguments;

   function Prepare_Aplaymidi_Arguments (Midi_Port : String; Media_File : String) return Chars_Ptr_Array is
      Argv_Arr : Chars_Ptr_Array (0 .. 15);
   begin
      Argv_Arr (0) := New_String ("aplaymidi");
      Argv_Arr (1) := New_String ("-p");
      Argv_Arr (2) := New_String (Midi_Port);
      Argv_Arr (3) := New_String (Media_File);
      Argv_Arr (4) := Null_Ptr;
      return Argv_Arr;
   end Prepare_Aplaymidi_Arguments;

   procedure Play_Track is
      Track      : constant Track_T := Sess.Tracks (Currently_Playing_Track);
      Media_File : constant String := To_String (Track.Path);
      Okay : Gboolean;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Chars_Ptr_Array := (0 .. 15 => <>);
   begin
      case Track.File_Type is
         when FLAC | MP3 | OGG | WAV =>
            Argv := Prepare_Ffplay_Arguments (Media_File, Track.Volume);
            Okay := Spawn_Async (Working_Directory => Null_Ptr,
                                 Argv => Argv'Access,
                                 Envp => null,  --  Inherit our env
                                 Flags => G_Spawn_Search_Path, --  + G_Spawn_Do_Not_Reap_Child,
                                 Child_Setup => null,
                                 Data => null,
                                 Child_Pid => Player_PID'Access,
                                 Error => PErr'Access
                                 );
         when MIDI =>
            Argv := Prepare_Aplaymidi_Arguments (To_String (Sess.MIDI_Port), Media_File);
            Okay := Spawn_Async (Working_Directory => Null_Ptr,
                                 Argv => Argv'Access,
                                 Envp => null,  --  Inherit our env
                                 Flags => G_Spawn_Search_Path, --  + G_Spawn_Do_Not_Reap_Child,
                                 Child_Setup => null,
                                 Data => null,
                                 Child_Pid => Player_PID'Access,
                                 Error => PErr'Access
                                 );
         when UNKNOWN => raise Unknown_Media_Type;
      end case;

      if Okay = 0 then
         Ada.Text_IO.Put_Line ("PLAYER ERROR");
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
      --  Unused_rc : C.int;
      Unused_Okay : Gboolean;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Chars_Ptr_Array := (0 .. 15 => <>);
   begin
      if Player_Active then
         --  This is just gross - there must be a better way...
         --  Unused_rc := system (command);
         Glib.Spawn.Spawn_Close_Pid (Player_PID);
         --  Player_PID := 0;
         if Sess.Tracks (Currently_Playing_Track).File_Type = MIDI then
            Argv := Prepare_Aplaymidi_Arguments (To_String (Sess.MIDI_Port), Midi_Files.Notes_Off_Embedded);
            Unused_Okay := Spawn_Async (Working_Directory => Null_Ptr,
                                 Argv => Argv'Access,
                                 Envp => null,  --  Inherit our env
                                 Flags => G_Spawn_Search_Path, --  + G_Spawn_Do_Not_Reap_Child,
                                 Child_Setup => null,
                                 Data => null,
                                 Child_Pid => Player_PID'Access,
                                 Error => PErr'Access
                                 );
         end if;
      end if;
   end Stop_Playing;

end Players;