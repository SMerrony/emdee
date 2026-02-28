// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"os/exec"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
)

var (
	playButton, stopButton, previousButton, nextButton *widget.Button
	playerCmd                                          *exec.Cmd = nil
)

func buildPlayerControls() (playerControls *fyne.Container) {

	playButton = widget.NewButton("Play", play)
	playButton.Disable()
	stopButton = widget.NewButton("Stop", stop)
	stopButton.Disable()
	previousButton = widget.NewButton("Previous", previous)
	previousButton.Disable()
	nextButton = widget.NewButton("Next", next)
	nextButton.Disable()
	playerControls = container.New(layout.NewGridLayout(4), playButton, stopButton, previousButton, nextButton)
	return playerControls
}

func play() {
	if currentSession == nil || activeTrackIx < 0 || activeTrackIx >= len(currentSession.Tracks) {
		dialog.ShowInformation("No Track Selected", "Please select a track to play.", mainWindow)
		return
	}
	track := currentSession.Tracks[activeTrackIx]
	if track.Skip {
		dialog.ShowInformation("Track Skipped", "This track is marked to be skipped. Please uncheck the skip box to play it.", mainWindow)
		return
	}
	// TODO handle MIDI files, different OSes, etc.
	cmd, err := startFFPlay(track.Path, track.Volume)
	if err != nil {
		dialog.ShowError(err, mainWindow)
	} else {
		playerCmd = cmd
		stopButton.Enable()
		playButton.Disable()
		previousButton.Disable()
		nextButton.Disable()
	}
}

func stop() {
	if playerActive {
		stopPlayer(playerCmd)
		stopButton.Disable()
		playButton.Enable()
		previousButton.Enable()
		nextButton.Enable()
	}
}

func next() {
	if activeTrackIx < len(currentSession.Tracks)-1 {
		activeTrackIx++
		updateSelection()
	}
}

func previous() {
	if activeTrackIx > 0 {
		activeTrackIx--
		updateSelection()
	}
}
