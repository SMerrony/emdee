// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"fmt"
	"log"
	"os/exec"
	"runtime"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/dialog"
)

var (
	viewNormalItem *fyne.MenuItem
	viewLargeItem  *fyne.MenuItem
	viewXLItem     *fyne.MenuItem
)

func buildMenu() (mainMenu *fyne.MainMenu) {
	newItem := fyne.NewMenuItem("New Session...", func() {})
	openItem := fyne.NewMenuItem("Open Session...", func() { openSession() })
	saveItem := fyne.NewMenuItem("Save Session", func() {})
	saveAsItem := fyne.NewMenuItem("Save Session As...", func() {})
	fileMenu := fyne.NewMenu("File", newItem, openItem, fyne.NewMenuItemSeparator(), saveItem, saveAsItem)

	// viewSmallItem := fyne.NewMenuItem("View Small", func() {})
	// viewSmallItem.Checked = false
	viewNormalItem = fyne.NewMenuItem("View Normal", func() {
		GuiSize = GuiNormal
		viewNormalItem.Checked = true
		viewLargeItem.Checked = false
		viewXLItem.Checked = false
		if currentSession != nil {
			currentSession.Session.FontSize = "M"
			currentSession.Session.IsDirty = true
		}
		emdeeApp.Settings().SetTheme(&emdeeTheme{})
	})
	viewNormalItem.Checked = true

	viewLargeItem = fyne.NewMenuItem("View Large", func() {
		GuiSize = GuiLarge
		viewNormalItem.Checked = false
		viewLargeItem.Checked = true
		viewXLItem.Checked = false
		if currentSession != nil {
			currentSession.Session.FontSize = "L"
			currentSession.Session.IsDirty = true
		}
		emdeeApp.Settings().SetTheme(&emdeeTheme{})
	})

	viewXLItem = fyne.NewMenuItem("View X-Large", func() {
		GuiSize = GuiXLarge
		viewNormalItem.Checked = false
		viewLargeItem.Checked = false
		viewXLItem.Checked = true
		if currentSession != nil {
			currentSession.Session.FontSize = "XL"
			currentSession.Session.IsDirty = true
		}
		emdeeApp.Settings().SetTheme(&emdeeTheme{})
	})

	// viewXXLItem := fyne.NewMenuItem("View XX-Large", func() {})
	// viewXXLItem.Checked = false
	viewSessionEditingItem := fyne.NewMenuItem("Session Editing Mode", func() {})
	viewSessionEditingItem.Checked = false
	viewMenu := fyne.NewMenu("View", viewNormalItem, viewLargeItem, viewXLItem,
		fyne.NewMenuItemSeparator(), viewSessionEditingItem,
	)

	midiSettingsItem := fyne.NewMenuItem("MIDI Settings...", func() {})
	midiMenu := fyne.NewMenu("MIDI", midiSettingsItem)

	onlineHelpItem := fyne.NewMenuItem("Online Help", func() { openBrowser(helpURL) })
	aboutItem := fyne.NewMenuItem("About", helpAbout)
	helpMenu := fyne.NewMenu("Help", onlineHelpItem, aboutItem)

	mainMenu = fyne.NewMainMenu(fileMenu, viewMenu, midiMenu, helpMenu)
	return mainMenu
}

func openBrowser(url string) {
	var err error

	switch runtime.GOOS {
	case "linux":
		err = exec.Command("xdg-open", url).Start()
	case "windows":
		err = exec.Command("rundll32", "url.dll,FileProtocolHandler", url).Start()
	case "darwin":
		err = exec.Command("open", url).Start()
	default:
		err = fmt.Errorf("unsupported platform")
	}
	if err != nil {
		log.Fatal(err)
	}
}

func helpAbout() {
	info := fmt.Sprintf("%s\n\n%s\n\n%s\n\n%s", appTitle, appSemVer, appWebsite, appCopyright)
	dialog.ShowInformation("About", info, mainWindow)
}
