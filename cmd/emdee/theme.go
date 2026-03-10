// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"image/color"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/theme"
)

const (
	GuiNormal = 14 // M in the TOML config
	GuiLarge  = 20 // L
	GuiXLarge = 28 // XL
)

var GuiSize = GuiNormal

type emdeeTheme struct{}

var _ fyne.Theme = (*emdeeTheme)(nil)

func (t *emdeeTheme) Color(name fyne.ThemeColorName, variant fyne.ThemeVariant) color.Color {
	return theme.DefaultTheme().Color(name, variant)
}

func (t *emdeeTheme) Icon(name fyne.ThemeIconName) fyne.Resource {
	return theme.DefaultTheme().Icon(name)
}

func (t *emdeeTheme) Font(style fyne.TextStyle) fyne.Resource {
	return theme.DefaultTheme().Font(style)
}

func (t *emdeeTheme) Size(name fyne.ThemeSizeName) float32 {
	switch name {
	case theme.SizeNameText:
		return float32(GuiSize)
	default:
		return theme.DefaultTheme().Size(name)
	}
}
