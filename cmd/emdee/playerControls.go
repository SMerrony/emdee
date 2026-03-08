// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	cw "emdee/internal/customwidgets"
	"emdee/internal/players"

	"log"

	"os/exec"
	"runtime"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
)

var (
	playButton                             *cw.MinHeightButton
	stopButton, previousButton, nextButton *widget.Button
	playerCmd                              *exec.Cmd = nil
)

func buildPlayerControls() (playerControls *fyne.Container) {

	playButton = cw.NewMinHeightButton("Play", 80*scaleFactor())
	playButton.SetIcon(theme.MediaPlayIcon())
	playButton.OnTapped = play
	playButton.Disable()

	stopButton = widget.NewButtonWithIcon("Stop", theme.MediaStopIcon(), stop)
	stopButton.Disable()
	previousButton = widget.NewButtonWithIcon("Previous", theme.MediaSkipPreviousIcon(), previous)
	previousButton.Disable()
	nextButton = widget.NewButtonWithIcon("Next", theme.MediaSkipNextIcon(), next)
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
		dialog.ShowInformation("Track Skip", "This track is marked be skipped. Please uncheck the skip box to play it.", mainWindow)
		return
	}
	// TODO handle MIDI files, different OSes, etc.
	var cmd *exec.Cmd
	var err error
	switch players.GuessMediaType(currentSession.Tracks[activeTrackIx].Path) {
	case players.MediaAudio:
		// ffplay is used everywhere (!)
		cmd, err = players.StartPlayer(players.PlayerFfplayer, track.Path, track.Volume, "")
	case players.MediaMIDI:
		switch runtime.GOOS {
		case "linux":
			cmd, err = players.StartPlayer(players.PlayerAplaymidi, track.Path, 100, currentSession.Session.MidiPort)
		case "windows":
			cmd, err = players.StartPlayer(players.PlayerPlaysmf, track.Path, 100, currentSession.Session.MidiPort)
		default:
			dialog.ShowInformation("Unsupported Platform", "Emdee cannot yet play MIDI files on this Operating System", mainWindow)
			return
		}
	}
	if err != nil {
		dialog.ShowError(err, mainWindow)
	} else {
		playerActive = true
		go monitorForCmdFinished(cmd)
		playerCmd = cmd
		stopButton.Enable()
		playButton.Disable()
		previousButton.Disable()
		nextButton.Disable()
	}
}

// Check for command termination. Must be run in a separate goroutine
func monitorForCmdFinished(cmd *exec.Cmd) {
	if cmd == nil {
		return
	}
	err := cmd.Wait()
	if err != nil && err.Error() != "signal: killed" {
		log.Printf("Command finished with error: %v", err)
	}
	fyne.DoAndWait(playerFinished) // Update UI in main thread
}

func stop() {
	if playerActive {
		players.StopPlayer(playerCmd)
		playerActive = false
		stopButton.Disable()
		playButton.Enable()
		previousButton.Enable()
		nextButton.Enable()
	}
}

func next() {
	if activeTrackIx < len(currentSession.Tracks)-1 {
		activeTrackIx++
		updateTrackSelection()
	}
}

func previous() {
	if activeTrackIx > 0 {
		activeTrackIx--
		updateTrackSelection()
	}
}
