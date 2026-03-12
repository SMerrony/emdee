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
)

var (
	playButton, stopButton, previousButton, nextButton *cw.MinHeightButton
	playerCmd                                          *exec.Cmd = nil
)

func checkPlayers() {
	switch runtime.GOOS {
	case "linux":
		if !players.CheckPlayerFound(players.PlayerFfplay) {
			dialog.ShowInformation("Audio Player", "Cannot find '"+players.PlayerFfplay.String()+"' on path, audio playback will not work", mainWindow)
		}
		if !players.CheckPlayerFound(players.PlayerAplaymidi) {
			dialog.ShowInformation("MIDI Player", "Cannot find '"+players.PlayerAplaymidi.String()+"' on path, MIDI file playback will not work", mainWindow)
		}
	case "windows":
		if !players.CheckPlayerFound(players.PlayerFfplay) {
			dialog.ShowInformation("Audio Player", "Cannot find '"+players.PlayerFfplay.String()+"' on path, audio playback will not work", mainWindow)
		}
		if !players.CheckPlayerFound(players.PlayerPlaysmf) {
			dialog.ShowInformation("MIDI Player", "Cannot find '"+players.PlayerPlaysmf.String()+"' on path, MIDI file playback will not work", mainWindow)
		}
	default:
	}
}

func buildPlayerControls() (playerControls *fyne.Container) {
	playButton = cw.NewMinHeightButton("Play", 70*scaleFactor())
	playButton.SetIcon(theme.MediaPlayIcon())
	playButton.OnTapped = play
	playButton.Disable()

	stopButton = cw.NewMinHeightButton("Stop", 70*scaleFactor()) // Use NewMinHeightButton
	stopButton.SetIcon(theme.MediaStopIcon())
	stopButton.OnTapped = stop
	stopButton.Disable()

	previousButton = cw.NewMinHeightButton("Previous", 70*scaleFactor()) // Use NewMinHeightButton
	previousButton.SetIcon(theme.MediaSkipPreviousIcon())
	previousButton.OnTapped = previous
	previousButton.Disable()

	nextButton = cw.NewMinHeightButton("Next", 70*scaleFactor()) // Use NewMinHeightButton
	nextButton.SetIcon(theme.MediaSkipNextIcon())
	nextButton.OnTapped = next
	nextButton.Disable()

	playerControls = container.New(layout.NewGridLayout(4), playButton, stopButton, previousButton, nextButton)

	return playerControls
}

func play() {
	if config == nil || getActiveTrackIx() < 0 || getActiveTrackIx() >= len(config.Tracks) {
		dialog.ShowInformation("No Track Selected", "Please select a track to play.", mainWindow)
		return
	}
	track := config.Tracks[getActiveTrackIx()]
	if track.Skip {
		dialog.ShowInformation("Track Skip", "This track is marked be skipped. Please uncheck the skip box to play it.", mainWindow)
		return
	}
	// TODO handle MIDI files, different OSes, etc.
	var cmd *exec.Cmd
	var err error
	switch players.GuessMediaType(track.Path) {
	case players.MediaAudio:
		// ffplay is used everywhere (!)
		cmd, err = players.StartPlayer(players.PlayerFfplay, track.Path, track.Volume, "")
	case players.MediaMIDI:
		switch runtime.GOOS {
		case "linux":
			cmd, err = players.StartPlayer(players.PlayerAplaymidi, track.Path, 100, config.Session.MidiPort)
		case "windows":
			cmd, err = players.StartPlayer(players.PlayerPlaysmf, track.Path, 100, config.Session.MidiPort)
		default:
			dialog.ShowInformation("Unsupported Platform", "Emdee cannot yet play MIDI files on this Operating System", mainWindow)
			return
		}
	}
	if err != nil {
		dialog.ShowError(err, mainWindow)
	} else {
		setPlayerActive(true)
		go monitorForCmdFinished(cmd)
		playerCmd = cmd
		setPlayerButtonsAvailability()
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
	if playerIsActive() {
		players.StopPlayer(playerCmd)
		setPlayerActive(false)
		setPlayerButtonsAvailability()
	}
}

func next() {
	if getActiveTrackIx() < len(config.Tracks)-1 {
		setActiveTrackIx((getActiveTrackIx() + 1))
		updateTrackSelection()
		setPlayerButtonsAvailability()
	}
}

func previous() {
	if getActiveTrackIx() > 0 {
		setActiveTrackIx((getActiveTrackIx() - 1))
		updateTrackSelection()
		setPlayerButtonsAvailability()
	}
}

func setPlayerButtonsAvailability() {
	if playerIsActive() {
		stopButton.Enable()
		playButton.Disable()
		previousButton.Disable()
		nextButton.Disable()
	} else {
		if getActiveTrackIx() == -1 {
			stopButton.Disable()
			playButton.Disable()
			previousButton.Disable()
			nextButton.Disable()
		} else {
			stopButton.Disable()
			playButton.Enable()
			if getActiveTrackIx() > 0 {
				previousButton.Enable()
			} else {
				previousButton.Disable()
			}
			if getActiveTrackIx() < len(config.Tracks)-1 {
				nextButton.Enable()
			} else {
				nextButton.Disable()
			}
		}
	}
}
