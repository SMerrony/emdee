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
	tracksBox         *fyne.Container
	content           *fyne.Container
	trackEditMode     bool = false

	rows []row
)

func setupWindow(w fyne.Window) {
	w.Resize(fyne.NewSize(900, 600))
	w.SetMainMenu(buildMenu())
	status := container.NewVBox(buildPlayerControls(), buildStatusBox())
	content = container.NewBorder(buildSessionHeader(), status, nil, nil, nil)
	w.SetContent(content)
}

func showSession() {
	mainWindow.SetTitle(fmt.Sprintf("%s - %s", appTitle, getSessionFilePath()))
	mainWindow.SetMainMenu(buildMenu())
	switch GuiSize {
	case GuiNormal:
		viewNormalItem.Checked = true
		viewLargeItem.Checked = false
		viewXLItem.Checked = false
	case GuiLarge:
		viewNormalItem.Checked = false
		viewLargeItem.Checked = true
		viewXLItem.Checked = false
	case GuiXLarge:
		viewNormalItem.Checked = false
		viewLargeItem.Checked = false
		viewXLItem.Checked = true
	default:
		viewNormalItem.Checked = true
		viewLargeItem.Checked = false
		viewXLItem.Checked = false
	}
	status := container.NewVBox(buildPlayerControls(), buildStatusBox())
	content = container.NewBorder(buildSessionHeader(), status, nil, nil, nil)
	updateSessionHeader(config.Session.Name, config.Session.Notes)
	tracksBox = buildTracksDisplay()
	content.Add(tracksBox)
	updateTrackSelection()
	mainWindow.SetContent(content)
}

func loadAndShowSession(path string) {
	var err error
	config, err = loadConfig(path)
	if err != nil {
		dialog.ShowError(err, mainWindow)
		log.Printf("ERROR: Could not load Session file %s\n", path)
	} else {
		setSessionFilePath(path)
		setActiveTrackIx(-1)
		switch config.Session.FontSize {
		case "M":
			GuiSize = GuiNormal
		case "L":
			GuiSize = GuiLarge
		case "XL":
			GuiSize = GuiXLarge
		default:
			GuiSize = GuiNormal
		}
		showSession()
		setSessionDirty(false)
		setPlayerButtonsAvailability()
	}
}

func clearSessionDisplayAndData() {
	initLiveData()
	tracksBox = nil
	content.RemoveAll()
	content.Refresh()
}

// The sessionHeader holds the session name and notes fields, which are displayed at the top of the
// UI and can be edited by the user. Changes to these fields are tracked in the currentSession struct
// and marked as dirty if they differ from the loaded values.
// The sesssionHeader is the same for both the performance and editing modes.
func buildSessionHeader() (sessionHeader *fyne.Container) {
	sessionLabel := widget.NewLabel("Session:")
	sessionNameEntry = widget.NewEntry()
	sessionNameEntry.OnChanged = func(s string) {
		config.Session.Name = s
		setSessionDirty(true)
	}
	notesLabel := widget.NewLabel("Notes:")
	sessionNotesEntry = widget.NewEntry()
	sessionNotesEntry.OnChanged = func(s string) {
		config.Session.Notes = s
		setSessionDirty(true)
	}
	sessionHeader = container.New(layout.NewFormLayout(), sessionLabel, sessionNameEntry, notesLabel, sessionNotesEntry)
	return sessionHeader
}

func updateSessionHeader(title string, notes string) {
	sessionNameEntry.SetText(title)
	sessionNotesEntry.SetText(notes)
}

func scaleFactor() float32 {
	if config != nil {
		switch config.Session.FontSize {
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

// Update the track selector buttons
func updateTrackSelection() {
	for _, r := range rows {
		if r.id == getActiveTrackIx() {
			r.selectorBtn.Importance = widget.HighImportance
		} else {
			r.selectorBtn.Importance = widget.MediumImportance
		}
		r.selectorBtn.Refresh()
	}
	setPlayerButtonsAvailability()
}

func buildTracksDisplay() *fyne.Container {
	// log.Println("Building tracks display")
	tracksBox = container.NewVBox()
	tracksBox.Add(buildTracksDisplayHeader())
	for rowid, track := range config.Tracks {
		selectorBtn := widget.NewButtonWithIcon("", theme.NavigateNextIcon(), func() {
			setActiveTrackIx(rowid)
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
			config.Tracks[rowid].Title = s
			setSessionDirty(true)
		}
		rowBox.Add(titleEntry)

		skipCheck := widget.NewCheck("", func(b bool) {
			config.Tracks[rowid].Skip = b
			setSessionDirty(true)
		})
		skipCheck.SetChecked(track.Skip)
		skipCheck.OnChanged = func(b bool) {
			config.Tracks[rowid].Skip = b
			setSessionDirty(true)
		}
		rowBox.Add(skipCheck)

		commentEntry := cw.NewMinWidthEntry(200 * scaleFactor())
		commentEntry.SetText(track.Comment)
		commentEntry.OnChanged = func(s string) {
			config.Tracks[rowid].Comment = s
			setSessionDirty(true)
		}
		rowBox.Add(commentEntry)

		switch players.GuessMediaType(config.Tracks[rowid].Path) {
		case players.MediaNone:
			rowBox.Add(cw.NewMinWidthLabel("No Media", 140*scaleFactor()))
		case players.MediaMIDI:
			rowBox.Add(cw.NewMinWidthLabel("MIDI", 140*scaleFactor()))
		case players.MediaAudio, players.MediaUnknown:
			volumeEntry := cw.NewMinWidthEntry(60 * scaleFactor())
			volumeEntry.SetText(strconv.Itoa(track.Volume))
			volumeEntry.OnChanged = func(s string) {
				config.Tracks[rowid].Volume, _ = strconv.Atoi(s)
				setSessionDirty(true)
			}
			rowBox.Add(volumeEntry)

			rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeDownIcon(), func() {
				if config.Tracks[rowid].Volume >= 10 {
					config.Tracks[rowid].Volume -= 5
					volumeEntry.SetText(strconv.Itoa(config.Tracks[rowid].Volume))
					setSessionDirty(true)
				}
			}))
			rowBox.Add(widget.NewButtonWithIcon("", theme.VolumeUpIcon(), func() {
				config.Tracks[rowid].Volume += 5
				if config.Tracks[rowid].Volume <= 95 {
					config.Tracks[rowid].Volume += 5
					volumeEntry.SetText(strconv.Itoa(config.Tracks[rowid].Volume))
					setSessionDirty(true)
				}
			}))
		}

		if trackEditMode {
			LeadInEntry := cw.NewMinWidthEntry(40 * scaleFactor())
			LeadInEntry.SetText(strconv.Itoa(track.LeadIn))
			rowBox.Add(LeadInEntry)
			rowBox.Add(widget.NewButtonWithIcon("", theme.FolderOpenIcon(), nil))
			clearBtn := widget.NewButtonWithIcon("", theme.ContentClearIcon(), nil)
			rowBox.Add(clearBtn)

			tmpWidth := clearBtn.MinSize().Width
			if rowid < len(config.Tracks)-1 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveDownIcon(), func() {
					config.swapTracks(rowid, rowid+1)
					content.Remove(tracksBox)
					tracksBox = buildTracksDisplay()
					content.Add(tracksBox)
					content.Refresh()
					setSessionDirty(true)
				}))
			} else {
				rowBox.Add(cw.NewMinWidthLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}

			if rowid > 0 {
				rowBox.Add(widget.NewButtonWithIcon("", theme.MoveUpIcon(), func() {
					config.swapTracks(rowid, rowid-1)
					content.Remove(tracksBox)
					tracksBox = buildTracksDisplay()
					content.Add(tracksBox)
					content.Refresh()
					setSessionDirty(true)
				}))
			} else {
				rowBox.Add(cw.NewMinWidthLabel(" ", tmpWidth)) // Placeholder to keep buttons aligned
			}

			rowBox.Add(widget.NewButtonWithIcon("", theme.DeleteIcon(), func() {
				config.Tracks = append(config.Tracks[:rowid], config.Tracks[rowid+1:]...)
				setSessionDirty(true)
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
		rowBox.Add(cw.NewMinWidthLabel(strconv.Itoa(len(config.Tracks)+1), 40*scaleFactor()))

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
			config.Tracks = append(config.Tracks, newTrack)
			setSessionDirty(true)
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
	setPlayerActive(false)
	if getActiveTrackIx() < len(config.Tracks)-1 {
		for {
			setActiveTrackIx(getActiveTrackIx() + 1)
			if !config.Tracks[getActiveTrackIx()].Skip &&
				getActiveTrackIx() <= len(config.Tracks)-1 {
				break
			}
		}
		updateTrackSelection()
		setPlayerButtonsAvailability()
	}
}
