// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"fmt"
	"log"
	"strconv"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
)

type row struct {
	id             int
	title, comment string
	volume         int
	skip           bool
	selectorBtn    *widget.Button
}

var (
	sessionNameEntry  *widget.Entry
	sessionNotesEntry *widget.Entry

	sessBox *fyne.Container

	rows []row
)

// openSession opens a file dialog to select a session file, then loads and displays the session data in the UI
func openSession() {
	sd := dialog.NewFileOpen(func(urirc fyne.URIReadCloser, e error) {
		if urirc != nil {
			loadAndShowSession(urirc.URI().Path())
		}
	}, mainWindow)
	sd.Resize(fyne.Size{Width: 600, Height: 600})
	sd.SetConfirmText("Open")
	sd.Show()
}

func loadAndShowSession(path string) {
	var err error
	currentSession, err = loadSession(path)
	if err != nil {
		dialog.ShowError(err, mainWindow)
		log.Printf("ERROR: Could not load Session file %s\n", path)
	} else {
		// fmt.Printf("%#v\n", currentSession)
		switch currentSession.Session.FontSize {
		case "M":
			GuiSize = GuiNormal
			viewNormalItem.Checked = true
			viewLargeItem.Checked = false
			viewXLItem.Checked = false
		case "L":
			GuiSize = GuiLarge
			viewNormalItem.Checked = false
			viewLargeItem.Checked = true
			viewXLItem.Checked = false
		case "XL":
			GuiSize = GuiXLarge
			viewNormalItem.Checked = false
			viewLargeItem.Checked = false
			viewXLItem.Checked = true
		default:
			GuiSize = GuiNormal
			viewNormalItem.Checked = true
			viewLargeItem.Checked = false
			viewXLItem.Checked = false
		}
		mainWindow.SetTitle(fmt.Sprintf("%s - %s", appTitle, currentSession.Session.Name))
		updateSessionHeader(currentSession.Session.Name, currentSession.Session.Notes)
		updateSessionRows()

		playButton.Enable()
		previousButton.Enable()
		nextButton.Enable()

		// TODO Update UI with loaded session data
		// TODO Update MIDI settings with loaded session data
		// TODO Update track list with loaded session data
		// TODO Update font size with loaded session data
	}
}

func buildSessionHeader() (sessionHeader *fyne.Container) {
	sessionLabel := widget.NewLabel("Session:")
	sessionNameEntry = widget.NewEntry()
	notesLabel := widget.NewLabel("Notes:")
	sessionNotesEntry = widget.NewEntry()
	sessionHeader = container.New(layout.NewFormLayout(), sessionLabel, sessionNameEntry, notesLabel, sessionNotesEntry)
	return sessionHeader
}

func updateSessionHeader(title string, notes string) {
	sessionNameEntry.SetText(title)
	sessionNotesEntry.SetText(notes)
}

func scaleFactor() float32 {
	if currentSession != nil {
		switch currentSession.Session.FontSize {
		case "M":
			return 1.0
		case "L":
			return GuiLarge / GuiNormal
		case "XL":
			return GuiXLarge / GuiNormal
		default:
			return 1.0
		}
	} else {
		return 1.0
	}
}

func updateSelection() {
	for _, r := range rows {
		if r.id == activeTrackIx {
			r.selectorBtn.Importance = widget.HighImportance
		} else {
			r.selectorBtn.Importance = widget.MediumImportance
		}
		r.selectorBtn.Refresh()
	}
}

func updateSessionRows() {
	for rowid, track := range currentSession.Tracks {
		selectorBtn := widget.NewButtonWithIcon(">", nil, func() {
			activeTrackIx = rowid
			updateSelection()
		})
		rows = append(rows, row{
			id:          rowid,
			title:       track.Title,
			comment:     track.Comment,
			volume:      track.Volume,
			skip:        track.Skip,
			selectorBtn: selectorBtn,
		})
		rowBox := container.NewHBox()
		rowBox.Add(widget.NewLabel(strconv.Itoa(rowid + 1)))
		rowBox.Add(selectorBtn)
		titleEntry := NewMinSizeableEntry(300 * scaleFactor())
		titleEntry.SetText(track.Title)
		rowBox.Add(titleEntry)
		skipCheck := widget.NewCheck("", func(b bool) {
			currentSession.Tracks[rowid].Skip = b
			currentSession.Session.IsDirty = true
		})
		skipCheck.SetChecked(track.Skip)
		rowBox.Add(skipCheck)
		commentEntry := NewMinSizeableEntry(200 * scaleFactor())
		commentEntry.SetText(track.Comment)
		rowBox.Add(commentEntry)
		volumeEntry := NewMinSizeableEntry(50 * scaleFactor())
		volumeEntry.SetText(strconv.Itoa(track.Volume))
		rowBox.Add(volumeEntry)
		rowBox.Add(widget.NewButtonWithIcon("-", nil, nil))
		rowBox.Add(widget.NewButtonWithIcon("+", nil, nil))
		sessBox.Add(rowBox)
	}
}

func buildSessionBody() (sessionBody *fyne.Container) {
	sessBox = container.NewVBox()
	hdrBox := container.NewHBox()
	hdrBox.Add(widget.NewLabel("#"))
	hdrBox.Add(widget.NewLabel(" ")) // Placeholder for selector column
	hdrBox.Add(NewMinSizeableLabel("Title", 300*scaleFactor()))
	hdrBox.Add(NewMinSizeableLabel("Skip", 40*scaleFactor()))
	hdrBox.Add(NewMinSizeableLabel("Comment", 200*scaleFactor()))
	hdrBox.Add(NewMinSizeableLabel("Volume", 50*scaleFactor()))
	sessBox.Add(hdrBox)
	return sessBox
}

// updates to the UI after a track has finished playing
func playerFinished() {
	playButton.Enable()
	stopButton.Disable()
	if activeTrackIx < len(currentSession.Tracks)-1 {
		for {
			activeTrackIx++
			if !currentSession.Tracks[activeTrackIx].Skip ||
				activeTrackIx >= len(currentSession.Tracks)-1 {
				break
			}
		}
		updateSelection()
	}
}
