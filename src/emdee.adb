--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.OS_Lib;

with GUI;

procedure Emdee is

   Arg_Ix   : Natural := 1;

   procedure Print_Help is
   begin
      Put_Line (GUI.App_Title &  " - " & GUI.App_Comment);
      Put_Line ("Usage:");
      Put_Line ("  -h | -help           Display this usage message");
      --  Put_Line ("  -sessconf <filename> Load the specified session configuration");
      --  Put_Line ("  -sysconf <filename>  Use the specified system configuration");
      Put_Line ("  -version             Show the version of emdee and exit");
   end Print_Help;

begin
   while Arg_Ix <= Argument_Count loop
      if Argument (Arg_Ix) = "-version" then
         Put_Line ("emdee version " & GUI.App_SemVer);
         GNAT.OS_Lib.OS_Exit (0);
      elsif Argument (Arg_Ix) = "-h" or else Argument (Arg_Ix) = "-help" then
         Print_Help;
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      Arg_Ix := Arg_Ix + 1;
   end loop;

   GUI.Launch;

end Emdee;