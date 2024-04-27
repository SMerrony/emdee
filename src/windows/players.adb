--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

--  N.B. This is the WINDOWS-SPECIFIC Players package body

with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Glib;           use Glib;
with Glib.Error;
with Glib.Spawn;     use Glib.Spawn;

with Gtkada.Types;   use Gtkada.Types;

with Interfaces;
with Interfaces.C;

with System;

with Win32;
with Win32.Winbase;
with Win32.Winnt;

with Midi_Files;
with Players;        use Players;
with Session;        use Session;
with Track;          use Track;

package body Players is

   function Spawn_Async is
      new Generic_Spawn_Async (User_Data => Integer);

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

   Proc_Info : aliased Win32.Winbase.PROCESS_INFORMATION;
   Proc_Handle : Win32.Winnt.HANDLE;

   function To_LPSTR is
      new Ada.Unchecked_Conversion (System.Address, Win32.LPSTR);

   procedure Launch_FFplayer  (Media_File : String; Volume : Integer) is
      use Interfaces.C;
      use type Win32.BOOL;
      use Win32.Winbase;
      Start_Info : aliased STARTUPINFO;
      Okay       : Win32.BOOL;
      Volume_Str : constant String := Trim (Volume'Image, Left);
      Command_Str : constant String := "ffplay -hide_banner -nodisp -autoexit -loglevel quiet -volume " &
                                       Volume_Str & " " & Media_File;
      Command_CA : char_array := To_C (Command_Str);
   begin
      Ada.Text_IO.Put_Line ("DEBUG: FFplay command line: " & Command_Str);
      Start_Info.cb          := (STARTUPINFOA'Size) / System.Storage_Unit;
      Start_Info.lpReserved  :=   null;
      Start_Info.lpDesktop   :=   null;
      Start_Info.lpTitle     :=   null;
      Start_Info.dwFlags     :=   0;
      Start_Info.cbReserved2 :=   0;
      Start_Info.lpReserved2 :=   null;
      Okay := CreateProcess (
         null,                --  No module name
         To_LPSTR (Command_CA'Address),
         null,                --  Proc handle not inheritable
         null,                --  Thread handle not inheritable
         Win32.FALSE,         --  Do not inherit handles
         0,                   --  No creation flags
         System.Null_Address, --  Use our environment
         null,                --  Use current directory
         Start_Info'Unchecked_Access,
         Proc_Info'Unchecked_Access
      );
      if Okay = Win32.FALSE then
         Ada.Text_IO.Put_Line ("PLAYER LAUNCH ERROR");
      else
         Proc_Handle := Proc_Info.hProcess;
         Ada.Text_IO.Put_Line ("DEBUG: Player PID: " & Proc_Info.dwProcessId'Image);
      end if;
   end Launch_FFplayer;

   procedure Play_Track is
      Track      : constant Track_T := Sess.Tracks (Currently_Playing_Track);
      Media_File : constant String := To_String (Track.Path);
      Okay : Gboolean;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Chars_Ptr_Array := [0 .. 15 => <>];
   begin
      case Track.File_Type is
         when FLAC | MP3 | OGG | WAV =>
            Launch_FFplayer (Media_File, Track.Volume);
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
   end Play_Track;

   function Player_Active return Boolean is
      use Win32;
      use Interfaces.C;
      Check_OK  : Win32.BOOL;
      Exit_Code : aliased Win32.DWORD;
   begin
      if Proc_Info.dwProcessId = 0 then
         return False;
      end if;
      Check_OK := Win32.Winbase.GetExitCodeProcess (Proc_Info.hProcess, Exit_Code'Unchecked_Access);
      --  Ada.Text_IO.Put_Line ("DEBUG: Check_OK: " & Check_OK'Image & " Exit_Code: " & Exit_Code'Image);
      if Check_OK = 0 then
         return False;     --  Could not check given handle
      elsif Exit_Code = Win32.Winbase.STILL_ACTIVE then  --  STILL_ACTIVE = 259
         return True;
      else
         Proc_Info.dwProcessId := 0;
         return False;
      end if;
   end Player_Active;

   procedure Stop_Playing is
      Unused_Okay : Gboolean;
      Unused_Bool : Win32.BOOL;
      PErr : aliased Glib.Error.GError;
      Argv : aliased Chars_Ptr_Array := [0 .. 15 => <>];
   begin
      if Player_Active then
         Unused_Bool := Win32.Winbase.TerminateProcess (Proc_Handle, 0);
         Proc_Info.dwProcessId := 0;
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