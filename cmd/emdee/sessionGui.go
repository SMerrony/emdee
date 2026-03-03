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
	"fyne.io/fyne/v2/storage"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
)

type row struct {
	id             int
	title, comment string
	volume         int
	play           bool
	path           string
	selectorBtn    *widget.Button
}

var (
	sessionNameEntry  *widget.Entry
	sessionNotesEntry *widget.Entry

	sessBox *fyne.Container

	trackEditMode bool = false

	rows []row
)

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
		tracksBox = buildSessionRows()
		content.Add(tracksBox)
		content.Refresh()
		// content.Add(buildSessionRows())

		playButton.Enable()
		previousButton.Enable()
		nextButton.Enable()

		// TODO Update UI with loaded session data
		// TODO Update MIDI settings with loaded session data
		// TODO Update track list with loaded session data
		// TODO Update font size with loaded session data
	}
}

// The sessionHeader holds the session name and notes fields, which are displayed at the top of the
// UI and can be edited by the user. Changes to these fields are tracked in the currentSession struct
// and marked as dirty if they differ from the loaded values.
// The sesssionHeader is the same for both the performance and editing modes.
func buildSessionHeader() (sessionHeader *fyne.Container) {
	sessionLabel := widget.NewLabel("Session:")
	sessionNameEntry = widget.NewEntry()
	sessionNameEntry.OnChanged = func(s string) {
		currentSession.Session.Name = s
		currentSession.Session.isDirty = true
		mainWindow.SetTitle(fmt.Sprintf("%s - %s", appTitle, currentSession.Session.Name))
	}
	notesLabel := widget.NewLabel("Notes:")
	sessionNotesEntry = widget.NewEntry()
	sessionNotesEntry.OnChanged = func(s string) {
		currentSession.Session.Notes = s
		currentSession.Session.isDirty = true
	}
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

// Update the GUI whenever the active track changes.
func updateTrackSelection() {
	for _, r := range rows {
		if r.id == activeTrackIx {
			r.selectorBtn.Importance = widget.HighImportance
		} else {
			r.selectorBtn.Importance = widget.MediumImportance
		}
		r.selectorBtn.Refresh()
	}
}

func buildSessionRows() *fyne.Container {
	rows = nil // Clear existing row data
	tracksBox = container.NewVBox()
	for rowid, track := range currentSession.Tracks {
		selectorBtn := widget.NewButtonWithIcon("", theme.NavigateNextIcon(), func() {
			activeTrackIx = rowid
			updateTrackSelection()
		})
		rows = append(rows, row{
			id:          rowid,
			title:       track.Title,
			comment:     track.Comment,
			volume:      track.Volume,
			play:        track.Play,
			selectorBtn: selectorBtn,
		})
		rowBox := container.NewHBox()
		rowBox.Add(widget.NewLabel(strconv.Itoa(rowid + 1)))
		rowBox.Add(selectorBtn)
		titleEntry := NewMinSizeableEntry(300 * scaleFactor())
		titleEntry.SetText(track.Title)
		rowBox.Add(titleEntry)
		playCheck := widget.NewCheck("", func(b bool) {
			currentSession.Tracks[rowid].Play = b
			currentSession.Session.isDirty = true
		})
		playCheck.SetChecked(track.Play)
		rowBox.Add(playCheck)
		commentEntry := NewMinSizeableEntry(200 * scaleFactor())
		commentEntry.SetText(track.Comment)
		rowBox.Add(commentEntry)
		volumeEntry := NewMinSizeableEntry(60 * scaleFactor())
		volumeEntry.SetText(strconv.Itoa(track.Volume))

		rowBox.Add(volumeEntry)
		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeDownIcon(), nil))
		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeUpIcon(), nil))

		if trackEditMode {
			LeadInEntry := NewMinSizeableEntry(40 * scaleFactor())
			LeadInEntry.SetText(strconv.Itoa(track.LeadIn))
			rowBox.Add(LeadInEntry)
			rowBox.Add(widget.NewButtonWithIcon("", theme.FolderOpenIcon(), nil))
			clearBtn := widget.NewButtonWithIcon("", theme.ContentClearIcon(), nil)
			rowBox.Add(clearBtn)
			tmpWidth := clearBtn.MinSize().Width
			if rowid < len(currentSession.Tracks)-1 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveDownIcon(), nil))
			} else {
				rowBox.Add(NewMinSizeableLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}
			if rowid > 0 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveUpIcon(), nil))
			} else {
				rowBox.Add(NewMinSizeableLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}
			rowBox.Add(widget.NewButtonWithIcon("", theme.DeleteIcon(), func() {
				currentSession.Tracks = append(currentSession.Tracks[:rowid], currentSession.Tracks[rowid+1:]...)
				currentSession.Session.isDirty = true
				content.Remove(tracksBox)
				tracksBox = buildSessionRows()
				content.Add(tracksBox)
				content.Refresh()
			}))
		}

		tracksBox.Add(rowBox)
	}
	// TODO Add empty row at the end for adding new tracks in editing mode
	if trackEditMode {
		newRow := row{}
		rowBox := container.NewHBox()
		rowBox.Add(widget.NewLabel(strconv.Itoa(len(currentSession.Tracks) + 1)))
		selectorBtn := widget.NewButtonWithIcon("", theme.NavigateNextIcon(), nil)
		selectorBtn.Disable()
		rowBox.Add(selectorBtn)

		titleEntry := NewMinSizeableEntry(300 * scaleFactor())
		titleEntry.SetPlaceHolder("(Track Title)*")
		titleEntry.OnChanged = func(s string) {
			newRow.title = s
		}
		rowBox.Add(titleEntry)

		playCheck := widget.NewCheck("", nil)
		playCheck.OnChanged = func(b bool) {
			newRow.play = b
		}
		playCheck.SetChecked(true)
		rowBox.Add(playCheck)

		commentEntry := NewMinSizeableEntry(200 * scaleFactor())
		commentEntry.SetPlaceHolder("(Optional Comment)")
		commentEntry.OnChanged = func(s string) {
			newRow.comment = s
		}
		rowBox.Add(commentEntry)

		volumeEntry := NewMinSizeableEntry(60 * scaleFactor())
		volumeEntry.SetText(strconv.Itoa(100))
		volumeEntry.OnChanged = func(s string) {
			newRow.volume, _ = strconv.Atoi(s)
		}
		rowBox.Add(volumeEntry)

		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeDownIcon(), nil))
		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeUpIcon(), nil))

		LeadInEntry := NewMinSizeableEntry(40 * scaleFactor())
		LeadInEntry.SetText(strconv.Itoa(0))
		rowBox.Add(LeadInEntry)
		rowBox.Add(widget.NewButtonWithIcon("", theme.FolderOpenIcon(), func() {
			fd := dialog.NewFileOpen(func(reader fyne.URIReadCloser, err error) {
				if err != nil {
					dialog.ShowError(err, mainWindow)
					return
				}
				if reader == nil {
					// log.Println("Cancelled")
					return
				}
				newRow.path = reader.URI().Path()
				reader.Close()
			}, mainWindow)
			fd.SetFilter(storage.NewExtensionFileFilter([]string{".mp3", ".wav", ".ogg", ".flac", ".midi", ".MP3", ".WAV", ".OGG", ".FLAC", ".MIDI"}))
			fd.SetConfirmText("Select")
			fd.Show()
		}))
		clearBtn := widget.NewButtonWithIcon("", theme.ContentClearIcon(), nil)
		rowBox.Add(clearBtn)
		// tmpWidth := clearBtn.MinSize().Width
		trackSaveBtn := widget.NewButton("Save", func() {
			if titleEntry.Text == "" {
				dialog.ShowInformation("Validation Error", "Track title is required.", mainWindow)
				return
			}
			vol, err := strconv.Atoi(volumeEntry.Text)
			if err != nil {
				dialog.ShowError(fmt.Errorf("Invalid volume value: %s", volumeEntry.Text), mainWindow)
				return
			}
			leadIn, err := strconv.Atoi(LeadInEntry.Text)
			if err != nil {
				dialog.ShowError(fmt.Errorf("Invalid lead-in value: %s", LeadInEntry.Text), mainWindow)
				return
			}
			newTrack := Track{
				Title:   titleEntry.Text,
				Comment: commentEntry.Text,
				Path:    newRow.path,
				Volume:  vol,
				Play:    playCheck.Checked,
				LeadIn:  leadIn,
			}
			currentSession.Tracks = append(currentSession.Tracks, newTrack)
			currentSession.Session.isDirty = true
			content.Remove(tracksBox)
			tracksBox = buildSessionRows()
			content.Add(tracksBox)
			content.Refresh()
		})

		rowBox.Add(trackSaveBtn)

		tracksBox.Add(rowBox)
	}
	return tracksBox
}

// // FIXME Do we really need this?
// func buildSessionRowsHeader() (sessionBody *fyne.Container) {
// 	sessBox = container.NewVBox()
// 	hdrBox := container.NewHBox()
// 	hdrBox.Add(widget.NewLabel(" ")) // Placeholder for row number column
// 	hdrBox.Add(widget.NewLabel(" ")) // Placeholder for selector column
// 	hdrBox.Add(NewMinSizeableLabel("Title", 300*scaleFactor()))
// 	hdrBox.Add(NewMinSizeableLabel("Skip", 40*scaleFactor()))
// 	hdrBox.Add(NewMinSizeableLabel("Comment", 200*scaleFactor()))
// 	hdrBox.Add(NewMinSizeableLabel("Volume", 50*scaleFactor()))
// 	sessBox.Add(hdrBox)
// 	return sessBox
// }

// updates to the UI after a track has finished playing
func playerFinished() {
	playButton.Enable()
	stopButton.Disable()
	if activeTrackIx < len(currentSession.Tracks)-1 {
		for {
			activeTrackIx++
			if currentSession.Tracks[activeTrackIx].Play ||
				activeTrackIx >= len(currentSession.Tracks)-1 {
				break
			}
		}
		updateTrackSelection()
	}
}
