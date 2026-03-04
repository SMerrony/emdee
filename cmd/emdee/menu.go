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
	"fyne.io/fyne/v2/storage"
)

var (
	viewNormalItem         *fyne.MenuItem
	viewLargeItem          *fyne.MenuItem
	viewXLItem             *fyne.MenuItem
	viewSessionEditingItem *fyne.MenuItem
)

func buildMenu() (mainMenu *fyne.MainMenu) {
	newItem := fyne.NewMenuItem("New Session...", func() {})
	openItem := fyne.NewMenuItem("Open Session...", fileOpen)
	saveItem := fyne.NewMenuItem("Save Session", fileSave)
	saveAsItem := fyne.NewMenuItem("Save Session As...", fileSaveAs)
	fileMenu := fyne.NewMenu("File",
		newItem,
		openItem,
		fyne.NewMenuItemSeparator(),
		saveItem,
		saveAsItem)

	// viewSmallItem := fyne.NewMenuItem("View Small", func() {})
	// viewSmallItem.Checked = false
	viewNormalItem = fyne.NewMenuItem("View Normal", func() {
		GuiSize = GuiNormal
		viewNormalItem.Checked = true
		viewLargeItem.Checked = false
		viewXLItem.Checked = false
		if currentSession != nil {
			currentSession.Session.FontSize = "M"
			currentSession.Session.isDirty = true
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
			currentSession.Session.isDirty = true
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
			currentSession.Session.isDirty = true
		}
		emdeeApp.Settings().SetTheme(&emdeeTheme{})
	})

	// viewXXLItem := fyne.NewMenuItem("View XX-Large", func() {})
	// viewXXLItem.Checked = false
	viewSessionEditingItem = fyne.NewMenuItem("Session Editing", func() {
		viewSessionEditingItem.Checked = !viewSessionEditingItem.Checked
		trackEditMode = viewSessionEditingItem.Checked
		if tracksBox != nil {
			content.Remove(tracksBox)
			tracksBox = buildSessionRows()
			content.Add(tracksBox)
			content.Refresh()
		}
		// content.Refresh()
	})
	viewSessionEditingItem.Checked = false
	viewMenu := fyne.NewMenu("View",
		viewNormalItem,
		viewLargeItem,
		viewXLItem,
		fyne.NewMenuItemSeparator(),
		viewSessionEditingItem,
	)

	midiSettingsItem := fyne.NewMenuItem("MIDI Settings...", func() {})
	midiMenu := fyne.NewMenu("MIDI", midiSettingsItem)

	onlineHelpItem := fyne.NewMenuItem("Online Help", func() { openBrowser(helpURL) })
	aboutItem := fyne.NewMenuItem("About", helpAbout)
	helpMenu := fyne.NewMenu("Help",
		onlineHelpItem,
		aboutItem)

	mainMenu = fyne.NewMainMenu(fileMenu, viewMenu, midiMenu, helpMenu)
	return mainMenu
}

// opens a file dialog to select a session file, then loads and displays the session data in the UI
func fileOpen() {
	sd := dialog.NewFileOpen(func(urirc fyne.URIReadCloser, e error) {
		if urirc != nil {
			loadAndShowSession(urirc.URI().Path())
		}
	}, mainWindow)
	sd.Resize(fyne.Size{Width: 600, Height: 600})
	sd.SetConfirmText("Open")
	sd.SetFilter(storage.NewExtensionFileFilter([]string{".toml", ".TOML"}))
	sd.Show()
}

func fileSave() {
	if currentSession != nil {
		if currentSession.Session.filePath != "" {
			if err := currentSession.Save(currentSession.Session.filePath); err != nil {
				dialog.ShowError(err, mainWindow)
				log.Printf("ERROR: Could not save Session file %s\n", currentSession.Session.filePath)
			} else {
				currentSession.Session.isDirty = false
			}
		} else {
			fileSaveAs()
		}
	}
}

func fileSaveAs() {
	sd := dialog.NewFileSave(func(urirc fyne.URIWriteCloser, e error) {
		if urirc != nil {
			path := urirc.URI().Path()
			if err := currentSession.Save(path); err != nil {
				dialog.ShowError(err, mainWindow)
				log.Printf("ERROR: Could not save Session file %s\n", path)
			} else {
				currentSession.Session.filePath = path
				currentSession.Session.isDirty = false
			}
		}
	}, mainWindow)
	sd.Resize(fyne.Size{Width: 600, Height: 600})
	sd.SetConfirmText("Save")
	sd.Show()
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
