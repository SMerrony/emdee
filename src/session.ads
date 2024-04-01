--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with TOML;      use TOML;

with Track;     use Track;

package Session is

   package Track_Vectors is new Ada.Containers.Vectors (
      Index_Type => Natural,
      Element_Type => Track_T
   );

   type Session_T is record
      Desc,
      Comment : Unbounded_String;
      Updated : TOML.Any_Local_Datetime;
      Tracks  : Track_Vectors.Vector;
   end record;

   Active_Session : Session_T;

   --  TOML exceptions
   Could_Not_Parse,
   Duplicate_Configuration,
   Incomplete_Configuration,
   Unknown_Configuration_Item : exception;

   procedure Load_Session (Filename : String);

end Session;