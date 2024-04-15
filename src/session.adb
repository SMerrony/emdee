--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Text_IO;

with TOML.File_IO;

package body Session is

   procedure Load_Session (Filename : String) is
      Toml_Parse_Result : Read_Result;
   begin
      Toml_Parse_Result := TOML.File_IO.Load_File (Filename);
      if not Toml_Parse_Result.Success then
         raise Could_Not_Parse with To_String (Toml_Parse_Result.Message);
      end if;

      Active_Session.Tracks.Clear;

      declare
         Top_Keys : constant TOML.Key_Array := Toml_Parse_Result.Value.Keys;
      begin

         for TK in Top_Keys'Range loop

            if To_String (Top_Keys (TK)) = "session" then
               declare
                  Session_Table : constant TOML_Value := Get (Toml_Parse_Result.Value, "session");
               begin
                  Active_Session.Desc      := As_Unbounded_String (Get (Session_Table, "description"));
                  Active_Session.Comment   := As_Unbounded_String (Get (Session_Table, "comment"));
                  if Has (Session_Table, "midiport") then
                     Active_Session.MIDI_Port := As_Unbounded_String (Get (Session_Table, "midiport"));
                  else
                     Active_Session.MIDI_Port := Null_Unbounded_String;
                  end if;
                  --  Active_Session.Updated   := As_Local_Datetime (Get (Session_Table, "updated"));
               end;

            elsif To_String (Top_Keys (TK)) = "track" then
               declare
                  Track_Array : constant TOML_Value := Get (Toml_Parse_Result.Value, "track");
                  Num_Tracks  : constant Natural    := Length (Track_Array);
                  Toml_Track  : TOML_Value;
                  Track        : Track_T;
               begin
                  for T in 1 .. Num_Tracks loop
                     Toml_Track := Item (Track_Array, T);
                     Track.Title   := As_Unbounded_String (Get (Toml_Track, "title"));
                     if Has (Toml_Track, "path") then
                        Track.Path    := As_Unbounded_String (Get (Toml_Track, "path"));
                        Track.File_Type := Guess_Media_Type (To_String (Track.Path));
                     else
                        Track.Path := Null_Unbounded_String;
                     end if;
                     if Has (Toml_Track, "comment") then
                        Track.Comment := As_Unbounded_String (Get (Toml_Track, "comment"));
                     else
                        Track.Comment := Null_Unbounded_String;
                     end if;
                     if Has (Toml_Track, "volume") then
                        Track.Volume := Integer (As_Integer (Get (Toml_Track, "volume")));
                     else
                        Track.Volume := 100;
                     end if;
                     if Has (Toml_Track, "skip") then
                        Track.Skip := As_Boolean (Get (Toml_Track, "skip"));
                     else
                        Track.Skip := False;
                     end if;
                     Active_Session.Tracks.Append (Track);
                  end loop;
               end; --  declare

            end if;

         end loop; --  Top_Keys loop

      end; --  declare
   end Load_Session;

   procedure Save_Session (Filename : String) is
      Toml_Sess           : TOML_Value; --  The overall TOML structure
      Toml_Session_Table,
      Toml_Track_Array,
      Toml_Track_Table    : TOML_Value;
      File                : Ada.Text_IO.File_Type;
   begin

      Ada.Text_IO.Create (File => File, Mode => Ada.Text_IO.Out_File, Name => Filename);

      Toml_Sess := Create_Table; --  Top-level container

      Toml_Session_Table := Create_Table;
      Toml_Session_Table.Set (Key => "description", Entry_Value => Create_String (Value => Active_Session.Desc));
      Toml_Session_Table.Set (Key => "comment", Entry_Value => Create_String (Value => Active_Session.Comment));
      Toml_Session_Table.Set (Key => "midiport", Entry_Value => Create_String (Value => Active_Session.MIDI_Port));
      --  TODO add "updated" field
      Toml_Sess.Set (Key => "session", Entry_Value => Toml_Session_Table);

      Toml_Track_Array := Create_Array;
      for Track of Active_Session.Tracks loop
         Toml_Track_Table := Create_Table;
         Toml_Track_Table.Set (Key => "title", Entry_Value => Create_String (Value => Track.Title));
         Toml_Track_Table.Set (Key => "path", Entry_Value => Create_String (Value => Track.Path));
         Toml_Track_Table.Set (Key => "comment", Entry_Value => Create_String (Value => Track.Comment));
         Toml_Track_Table.Set (Key => "volume", Entry_Value => Create_Integer (Value => Any_Integer (Track.Volume)));
         Toml_Track_Table.Set (Key => "skip", Entry_Value => Create_Boolean (Value => Track.Skip));
         Toml_Track_Array.Append (Item => Toml_Track_Table);
      end loop;
      Toml_Sess.Set (Key => "track", Entry_Value => Toml_Track_Array);

      TOML.File_IO.Dump_To_File (Toml_Sess, File);
      Ada.Text_IO.Close (File);
   end Save_Session;

end Session;