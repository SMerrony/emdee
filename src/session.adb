--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

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
                  Active_Session.Desc    := As_Unbounded_String (Get (Session_Table, "description"));
                  Active_Session.Comment := As_Unbounded_String (Get (Session_Table, "comment"));
                  Active_Session.Updated := As_Local_Datetime (Get (Session_Table, "updated"));
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
                     if Has (Toml_Track, "tempo") then
                        Track.Tempo := Float (As_Float (Get (Toml_Track, "tempo")).Value);
                     else
                        Track.Tempo := 1.0;
                     end if;
                     if Has (Toml_Track, "volume") then
                        Track.Volume := Float (As_Float (Get (Toml_Track, "volume")).Value);
                     else
                        Track.Volume := 1.0;
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

end Session;