// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"emdee/internal/players"
	"fmt"
	"log"
	"os/exec"
	"runtime"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/storage"
	"fyne.io/fyne/v2/widget"
)

var (
	viewNormalItem         *fyne.MenuItem
	viewLargeItem          *fyne.MenuItem
	viewXLItem             *fyne.MenuItem
	viewSessionEditingItem *fyne.MenuItem
)

func buildMenu() (mainMenu *fyne.MainMenu) {
	newItem := fyne.NewMenuItem("New Session...", fileNew)
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
		config.Session.FontSize = "M"
		setSessionDirty(true)
		showSession()
		mainWindow.Resize(fyne.Size{Width: 20, Height: 20})
	})
	viewNormalItem.Checked = true

	viewLargeItem = fyne.NewMenuItem("View Large", func() {
		GuiSize = GuiLarge
		config.Session.FontSize = "L"
		setSessionDirty(true)
		showSession()
		mainWindow.Resize(fyne.Size{Width: 20, Height: 20})
	})

	viewXLItem = fyne.NewMenuItem("View X-Large", func() {
		GuiSize = GuiXLarge
		config.Session.FontSize = "XL"
		setSessionDirty(true)
		showSession()
		mainWindow.Resize(fyne.Size{Width: 20, Height: 20})
	})

	// viewXXLItem := fyne.NewMenuItem("View XX-Large", func() {})
	// viewXXLItem.Checked = false

	viewSessionEditingItem = fyne.NewMenuItem("Session Editing", func() {
		viewSessionEditingItem.Checked = !viewSessionEditingItem.Checked
		trackEditMode = viewSessionEditingItem.Checked
		if tracksBox != nil {
			content.Remove(tracksBox)
		}
		tracksBox = buildTracksDisplay()
		content.Add(tracksBox)
		content.Refresh()
		mainWindow.Resize(fyne.Size{Width: 20, Height: 20}) // Force the window to recalculate its size to accommodate the new track display layout
	})
	viewSessionEditingItem.Checked = false
	viewMenu := fyne.NewMenu("View",
		viewNormalItem,
		viewLargeItem,
		viewXLItem,
		fyne.NewMenuItemSeparator(),
		viewSessionEditingItem,
	)

	midiListPortsItem := fyne.NewMenuItem("List Ports", func() {
		log.Print(players.ListMidiOuts()) // TODO remove this when tested on Windows
		text := widget.NewLabel(players.ListMidiOuts())
		text.TextStyle = fyne.TextStyle{Monospace: true}
		dialog.ShowCustom("MIDI Ports", "OK", text, mainWindow)
	})
	midiSettingsItem := fyne.NewMenuItem("MIDI Settings...", midiPortChooser)
	midiMenu := fyne.NewMenu("MIDI", midiListPortsItem, midiSettingsItem)

	onlineHelpItem := fyne.NewMenuItem("Online Help", func() { openBrowser(helpURL) })
	aboutItem := fyne.NewMenuItem("About", helpAbout)
	helpMenu := fyne.NewMenu("Help",
		onlineHelpItem,
		aboutItem)

	mainMenu = fyne.NewMainMenu(fileMenu, viewMenu, midiMenu, helpMenu)
	return mainMenu
}

func promptToSaveIfDirty(after func()) {
	if sessionIsDirty() {
		cd := dialog.NewConfirm("Unsaved Changes", "You have unsaved changes. Do you want to save before proceeding?",
			func(save bool) {
				if save {
					fileSave()
				}
				after()
			}, mainWindow)
		cd.Show()
	} else {
		after()
	}
}

func fileNew() {
	promptToSaveIfDirty(func() {
		clearSessionDisplayAndData()
		mainWindow.SetTitle(appTitle + " - (No Session Loaded)")
		viewSessionEditingItem.Checked = true
		trackEditMode = true
		showSession()
		mainWindow.Resize(fyne.Size{Width: 20, Height: 20})
	})
}

// opens a file dialog to select a session file, then loads and displays the session data in the UI
func fileOpen() {
	promptToSaveIfDirty(func() {
		od := dialog.NewFileOpen(func(urirc fyne.URIReadCloser, e error) {
			if urirc != nil {
				clearSessionDisplayAndData()
				loadAndShowSession(urirc.URI().Path())
				setSessionDirty(false)
			}
		}, mainWindow)
		od.Resize(fyne.Size{Width: 600, Height: 600})
		od.SetConfirmText("Open")
		od.SetFilter(storage.NewExtensionFileFilter([]string{".toml", ".TOML"}))
		od.Show()
	})

}

func fileSave() {
	if getSessionFilePath() != "" {
		if err := config.Save(getSessionFilePath()); err != nil {
			dialog.ShowError(err, mainWindow)
			log.Printf("ERROR: Could not save Session file %s\n", getSessionFilePath())
		} else {
			setSessionDirty(false)
		}
	} else {
		fileSaveAs()
	}
}

func fileSaveAs() {
	sd := dialog.NewFileSave(func(urirc fyne.URIWriteCloser, e error) {
		if urirc != nil {
			path := urirc.URI().Path()
			if err := config.Save(path); err != nil {
				dialog.ShowError(err, mainWindow)
				log.Printf("ERROR: Could not save Session file %s\n", path)
			} else {
				setSessionFilePath(path)
				setSessionDirty(false)
			}
		}
	}, mainWindow)
	sd.Resize(fyne.Size{Width: 600, Height: 600})
	sd.SetConfirmText("Save")
	sd.Show()
}

func midiPortChooser() {
	portEntry := widget.NewEntry()
	portEntry.Text = config.Session.MidiPort
	items := []*widget.FormItem{widget.NewFormItem("Port:", portEntry)}
	dialog.ShowForm("MIDI Port", "Save", "Cancel", items, func(b bool) {
		if b {
			config.Session.MidiPort = portEntry.Text
			setSessionDirty(true)
		}
	}, mainWindow)
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
