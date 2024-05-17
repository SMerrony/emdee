--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

--  N.B. This is the WINDOWS-SPECIFIC Players package body

with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;
with Interfaces.C;

with System;

with Win32;
with Win32.Winbase;
with Win32.Winnt;

with Embedded;
with GUI;
with Midi_Files;
with Players;        use Players;
with Session;        use Session;
with Track;          use Track;

package body Players is

   procedure Create_1s_Silence_MP3 is
      MP3_Emb : constant Embedded.Content_Type := Embedded.Get_Content (Silence_Emb_Name);
      package IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);
      MP3_File : IO.File_Type;
   begin
      if not Ada.Directories.Exists (Silence_Tmp_Name) then
         IO.Create (File => MP3_File, Name => Silence_Tmp_Name);
         for Val of MP3_Emb.Content.all loop
            IO.Write (MP3_File, Interfaces.Unsigned_8 (Val));
         end loop;
         IO.Close (MP3_File);
      end if;
   end Create_1s_Silence_MP3;   

   Proc_Info : aliased Win32.Winbase.PROCESS_INFORMATION;
   Proc_Handle : Win32.Winnt.HANDLE;

   function To_LPSTR is
      new Ada.Unchecked_Conversion (System.Address, Win32.LPSTR);

   function Launch_FFplayer  (Media_File : String; Volume : Integer) return Boolean is
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
      if GUI.Verbose then
         Ada.Text_IO.Put_Line ("DEBUG: FFplay command line: " & Command_Str);
      end if;
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
         return False;
      else
         Proc_Handle := Proc_Info.hProcess;
         if GUI.Verbose then
            Ada.Text_IO.Put_Line ("DEBUG: Player PID: " & Proc_Info.dwProcessId'Image);
         end if;
      end if;
      return True;
   end Launch_FFplayer;

   function Launch_PlaySMF  (Media_File : String; MIDI_Port : String) return Boolean is
      use Interfaces.C;
      use type Win32.BOOL;
      use Win32.Winbase;
      Start_Info : aliased STARTUPINFO;
      Okay       : Win32.BOOL;
      Command_Str : constant String := "playsmf --out " &MIDI_Port & " " & Media_File;
      Command_CA : char_array := To_C (Command_Str);
   begin
      Ada.Text_IO.Put_Line ("DEBUG: PlasySMF command line: " & Command_Str);
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
         return False;
      else
         Proc_Handle := Proc_Info.hProcess;
         Ada.Text_IO.Put_Line ("DEBUG: Player PID: " & Proc_Info.dwProcessId'Image);
      end if;
      return True;
   end Launch_PlaySMF;

   procedure Play_Silence (Secs : Positive) is
   --  N.B. This proc will cause the GUI to hang for 'Secs
      Unused_Bool : Boolean;
   begin
      for S in 1 .. Secs loop
         Unused_Bool := Launch_FFplayer (Silence_Tmp_Name, 0);
         loop
            delay 0.1;
            exit when not Player_Active;
         end loop;
      end loop;
   end Play_Silence;

   procedure Play_Track is
      Track      : constant Track_T := Sess.Tracks (Currently_Playing_Track);
      Media_File : constant String := To_String (Track.Path);
      Okay : Boolean;
   begin
      case Track.File_Type is
         when NONE =>
            null;
         when FLAC | MP3 | OGG | WAV =>
            if Sess.Lead_In_Silence > 0 then
               Play_Silence (Sess.Lead_In_Silence);
            end if;
            Okay := Launch_FFplayer (Media_File, Track.Volume);
         when MIDI =>
            Okay := Launch_PlaySMF (Media_File, To_String (Sess.MIDI_Port));
         when UNKNOWN => raise Unknown_Media_Type;
      end case;

      if not Okay then
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
      Unused_Okay : Boolean;
      Unused_Bool : Win32.BOOL;
   begin
      if Player_Active then
         Unused_Bool := Win32.Winbase.TerminateProcess (Proc_Handle, 0);
         Proc_Info.dwProcessId := 0;
         if Sess.Tracks (Currently_Playing_Track).File_Type = MIDI then
            Unused_Okay := Launch_PlaySMF (Midi_Files.Notes_Off_Embedded, To_String (Sess.MIDI_Port));
         end if;
      end if;
   end Stop_Playing;

end Players;