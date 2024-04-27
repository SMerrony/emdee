--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Gdk.Pixbuf;

with Glib.Main;

with Gtk.Application;         use Gtk.Application;
with Gtk.Application_Window;  use Gtk.Application_Window;
with Gtk.Box;
with Gtk.Button;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Css_Provider;        use Gtk.Css_Provider;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Grid;                use Gtk.Grid;
with Gtk.Label;               use Gtk.Label;
with Gtk.Radio_Menu_Item;     use Gtk.Radio_Menu_Item;
with Gtk.Style_Provider;
with Gtk.Widget;              use Gtk.Widget;

package GUI is

   package SB_Timeout_P is new Glib.Main.Generic_Sources (Gtk.Box.Gtk_Box);

   App_SemVer    : constant String := "0.1.0";  --  TODO Update Version each release!
   App_Title     : constant String := "eMDee";
   App_ID        : constant String := "fr.merrony." & App_Title;
   App_Comment   : constant String := "Musical Director's Assistant";
   App_Author    : constant String := "Stephen Merrony";
   App_Copyright : constant String := "Copyright (C)2024 S.Merrony";
   App_Icon      : constant String := "emdee.ico";
   App_CSS       : constant String := "main.css";
   App_Website   : constant String := "https://github.com/SMerrony/emdee";

   Max_Tracks  : constant Integer := 99;

   --  Track display column ordering
   Row_Col     : constant Glib.Gint := 0;
   Select_Col  : constant Glib.Gint := 1;
   Title_Col   : constant Glib.Gint := 2;
   Skip_Col    : constant Glib.Gint := 3;
   Comment_Col : constant Glib.Gint := 4;
   Vol_Col     : constant Glib.Gint := 5;
   File_Col    : constant Glib.Gint := 6;
   Down_Col    : constant Glib.Gint := 7;
   Up_Col      : constant Glib.Gint := 8;
   Del_Col     : constant Glib.Gint := 9;

   type Select_Btn_Arr_T is array (1 .. Max_Tracks) of Gtk.Button.Gtk_Button;

   App         : Gtk_Application;
   Main_Window : Gtk_Application_Window;
   Icon_PB     : Gdk.Pixbuf.Gdk_Pixbuf;
   Main_Box    : Gtk.Box.Gtk_Box;
   Session_Header_Grid : Gtk.Grid.Gtk_Grid;
   Select_Btn_Arr : Select_Btn_Arr_T;
   Track_Modifiers_Check_Item : Gtk_Check_Menu_Item;
   Show_Track_Modifiers : Boolean := False;

   Session_Desc_Entry,
   Session_Comment_Entry : Gtk_Entry;
   New_Track_Entry_Row   : Glib.Gint;

   Shutting_Down : Boolean := False;

   --  Status Bar items...
   Active_Label,
   Players_Label : Gtk_Label;
   SB_Update_MS  : Glib.Guint := 250;
   SB_Timeout    : Glib.Main.G_Source_Id := 0;

   Currently_Selected_Track : Integer := -1;
   Currently_Active : Boolean := False;

   type Font_Size is (S, M, L, XL, XXL);
   View_S_Radio_Item, View_M_Radio_Item,
   View_L_Radio_Item, View_XL_Radio_Item,
   View_XXL_Radio_Item : Gtk_Radio_Menu_Item;

   Current_Font_Size : Font_Size := M;

   Fixed_CSS_Str : constant String := "grid { padding: 10px; }" & ASCII.LF &
                              "grid label { margin: 8px; }"     & ASCII.LF &
                              "entry#highlit { color: red; }"   & ASCII.LF &
                              "spinbutton#highlit { color: red; }";

   CSS_Provider : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;

   function Build_CSS (Size : Font_Size) return String;

   procedure Launch;

private

   procedure Select_Next_Track;
   procedure Select_Previous_Track;
   --  procedure Display_Tracks;
   --  procedure Display_Empty_Track (Track_Row : Glib.Gint);

   --  procedure Clear_Tracks_Display;
   procedure Update_Text_Fields;
   procedure Apply_Css (Widget   : not null access Gtk_Widget_Record'Class;
                        Provider : Gtk.Style_Provider.Gtk_Style_Provider);

end GUI;