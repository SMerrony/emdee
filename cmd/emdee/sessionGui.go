// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	cw "emdee/internal/customwidgets"
	"emdee/internal/players"

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
	skip           bool
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
		sessionFilePath = path
		mainWindow.SetTitle(fmt.Sprintf("%s - %s", appTitle, sessionFilePath))
		updateSessionHeader(currentSession.Session.Name, currentSession.Session.Notes)
		tracksBox = buildTracksDisplay()
		content.Add(tracksBox)
		content.Refresh()

		sessionDirty = false

		playButton.Enable()
		previousButton.Enable()
		nextButton.Enable()

		// TODO Update MIDI settings with loaded session data
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
		sessionDirty = true
	}
	notesLabel := widget.NewLabel("Notes:")
	sessionNotesEntry = widget.NewEntry()
	sessionNotesEntry.OnChanged = func(s string) {
		currentSession.Session.Notes = s
		sessionDirty = true
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

func buildTracksDisplay() *fyne.Container {
	// log.Println("Building tracks display")
	rows = nil // Clear existing row data
	tracksBox = container.NewVBox()
	tracksBox.Add(buildTracksDisplayHeader())
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
			skip:        track.Skip,
			selectorBtn: selectorBtn,
		})

		rowBox := container.NewHBox()

		rowBox.Add(cw.NewMinWidthLabel(strconv.Itoa(rowid+1), 40*scaleFactor()))

		rowBox.Add(selectorBtn)

		titleEntry := cw.NewMinWidthEntry(300 * scaleFactor())
		titleEntry.SetText(track.Title)
		titleEntry.OnChanged = func(s string) {
			currentSession.Tracks[rowid].Title = s
			sessionDirty = true
		}
		rowBox.Add(titleEntry)

		skipCheck := widget.NewCheck("", func(b bool) {
			currentSession.Tracks[rowid].Skip = b
			sessionDirty = true
		})
		skipCheck.SetChecked(track.Skip)
		skipCheck.OnChanged = func(b bool) {
			currentSession.Tracks[rowid].Skip = b
			sessionDirty = true
		}
		rowBox.Add(skipCheck)

		commentEntry := cw.NewMinWidthEntry(200 * scaleFactor())
		commentEntry.SetText(track.Comment)
		commentEntry.OnChanged = func(s string) {
			currentSession.Tracks[rowid].Comment = s
			sessionDirty = true
		}
		rowBox.Add(commentEntry)

		switch players.GuessMediaType(currentSession.Tracks[rowid].Path) {
		case players.MediaNone:
			rowBox.Add(cw.NewMinWidthLabel("No Media", 140*scaleFactor()))
		case players.MediaAudio, players.MediaUnknown:
			volumeEntry := cw.NewMinWidthEntry(60 * scaleFactor())
			volumeEntry.SetText(strconv.Itoa(track.Volume))
			volumeEntry.OnChanged = func(s string) {
				currentSession.Tracks[rowid].Volume, _ = strconv.Atoi(s)
				sessionDirty = true
			}
			rowBox.Add(volumeEntry)

			rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeDownIcon(), func() {
				if currentSession.Tracks[rowid].Volume >= 10 {
					currentSession.Tracks[rowid].Volume -= 5
					volumeEntry.SetText(strconv.Itoa(currentSession.Tracks[rowid].Volume))
					sessionDirty = true
				}
			}))
			rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeUpIcon(), func() {
				currentSession.Tracks[rowid].Volume += 5
				if currentSession.Tracks[rowid].Volume <= 95 {
					currentSession.Tracks[rowid].Volume += 5
					volumeEntry.SetText(strconv.Itoa(currentSession.Tracks[rowid].Volume))
					sessionDirty = true
				}
			}))
		case players.MediaMIDI:
			rowBox.Add(cw.NewMinWidthLabel("MIDI", 140*scaleFactor()))
		}

		if trackEditMode {
			LeadInEntry := cw.NewMinWidthEntry(40 * scaleFactor())
			LeadInEntry.SetText(strconv.Itoa(track.LeadIn))
			rowBox.Add(LeadInEntry)
			rowBox.Add(widget.NewButtonWithIcon("", theme.FolderOpenIcon(), nil))
			clearBtn := widget.NewButtonWithIcon("", theme.ContentClearIcon(), nil)
			rowBox.Add(clearBtn)

			tmpWidth := clearBtn.MinSize().Width
			if rowid < len(currentSession.Tracks)-1 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveDownIcon(), func() {
					currentSession.swapTracks(rowid, rowid+1)
					content.Remove(tracksBox)
					tracksBox = buildTracksDisplay()
					content.Add(tracksBox)
					content.Refresh()
					sessionDirty = true
				}))
			} else {
				rowBox.Add(cw.NewMinWidthLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}

			if rowid > 0 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveUpIcon(), func() {
					currentSession.swapTracks(rowid, rowid-1)
					content.Remove(tracksBox)
					tracksBox = buildTracksDisplay()
					content.Add(tracksBox)
					content.Refresh()
					sessionDirty = true
				}))
			} else {
				rowBox.Add(cw.NewMinWidthLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}

			rowBox.Add(widget.NewButtonWithIcon("", theme.DeleteIcon(), func() {
				currentSession.Tracks = append(currentSession.Tracks[:rowid], currentSession.Tracks[rowid+1:]...)
				sessionDirty = true
				content.Remove(tracksBox)
				tracksBox = buildTracksDisplay()
				content.Add(tracksBox)
				content.Refresh()
			}))
		}

		tracksBox.Add(rowBox)
	}

	if trackEditMode {
		newRow := row{}

		rowBox := container.NewHBox()
		rowBox.Add(cw.NewMinWidthLabel(strconv.Itoa(len(currentSession.Tracks)+1), 40*scaleFactor()))

		selectorBtn := widget.NewButtonWithIcon("", theme.NavigateNextIcon(), nil)
		selectorBtn.Disable()
		rowBox.Add(selectorBtn)

		titleEntry := cw.NewMinWidthEntry(300 * scaleFactor())
		titleEntry.SetPlaceHolder("(Track Title - Required)")
		titleEntry.OnChanged = func(s string) {
			newRow.title = s
		}
		rowBox.Add(titleEntry)

		skipCheck := widget.NewCheck("", nil)
		skipCheck.OnChanged = func(b bool) {
			newRow.skip = b
		}
		skipCheck.SetChecked(false)
		rowBox.Add(skipCheck)

		commentEntry := cw.NewMinWidthEntry(200 * scaleFactor())
		commentEntry.SetPlaceHolder("(Optional Comment)")
		commentEntry.OnChanged = func(s string) {
			newRow.comment = s
		}
		rowBox.Add(commentEntry)

		volumeEntry := cw.NewMinWidthEntry(60 * scaleFactor())
		volumeEntry.SetText(strconv.Itoa(100))
		newRow.volume = 100
		volumeEntry.OnChanged = func(s string) {
			newRow.volume, _ = strconv.Atoi(s)
		}
		rowBox.Add(volumeEntry)

		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeDownIcon(), func() {
			if newRow.volume >= 10 {
				newRow.volume -= 5
				volumeEntry.SetText(strconv.Itoa(newRow.volume))
			}
		}))

		rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeUpIcon(), func() {
			if newRow.volume <= 95 {
				newRow.volume += 5
				volumeEntry.SetText(strconv.Itoa(newRow.volume))
			}
		}))

		LeadInEntry := cw.NewMinWidthEntry(40 * scaleFactor())
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
		trackSaveBtn := widget.NewButton("Add to Session", func() {
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
				Skip:    skipCheck.Checked,
				LeadIn:  leadIn,
			}
			currentSession.Tracks = append(currentSession.Tracks, newTrack)
			sessionDirty = true
			content.Remove(tracksBox)
			tracksBox = buildTracksDisplay()
			content.Add(tracksBox)
			content.Refresh()
		})

		rowBox.Add(trackSaveBtn)

		tracksBox.Add(rowBox)
	}
	return tracksBox
}

func buildTracksDisplayHeader() *fyne.Container {
	// log.Println("Building tracks display header")
	hdrBox := container.NewHBox()
	hdrBox.Add(cw.NewMinWidthLabel(" ", 40*scaleFactor())) // Placeholder for row number column
	hdrBox.Add(cw.NewMinWidthLabel(" ", 35*scaleFactor())) // Placeholder for selector column
	hdrBox.Add(cw.NewMinWidthLabel("Title", 300*scaleFactor()))
	hdrBox.Add(cw.NewMinWidthLabel("Skip", 40*scaleFactor()))
	hdrBox.Add(cw.NewMinWidthLabel("Comment", 200*scaleFactor()))
	hdrBox.Add(cw.NewMinWidthLabel("Volume", 140*scaleFactor()))
	if trackEditMode {
		hdrBox.Add(cw.NewMinWidthLabel("Lead In", 40*scaleFactor()))
	}
	return hdrBox
}

// updates to the UI after a track has finished playing
func playerFinished() {
	playerActive = false
	playButton.Enable()
	stopButton.Disable()
	if activeTrackIx < len(currentSession.Tracks)-1 {
		for {
			activeTrackIx++
			if !currentSession.Tracks[activeTrackIx].Skip &&
				activeTrackIx <= len(currentSession.Tracks)-1 {
				break
			}
		}
		updateTrackSelection()
	}
}
