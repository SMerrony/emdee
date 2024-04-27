--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Gtk.Button;

package GUI.Live_Controls is

   Play_Btn, Stop_Btn, Previous_Btn, Next_Btn : Gtk.Button.Gtk_Button;

   function Create_Live_Controls_Grid return Gtk_Grid;

end GUI.Live_Controls;