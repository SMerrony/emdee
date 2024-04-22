--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Gtk.Grid;                use Gtk.Grid;

package GUI.Tracks is

   Tracks_Grid : Gtk_Grid;

   function  Create_Tracks_Grid return Gtk_Grid;
   procedure Clear_Tracks_Display;
   procedure Display_Empty_Track (Track_Row : Glib.Gint);
   procedure Display_Tracks;

end GUI.Tracks;