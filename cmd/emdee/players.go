// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"log"
	"os/exec"
	"strconv"

	"fyne.io/fyne/v2"
)

// On Linux, we use ffplay for FLAC, MP3, OGG and WAV files.
func startFFPlay(mediaFilePath string, volume int) (*exec.Cmd, error) {
	cmd := exec.Command("ffplay", "-hide_banner", "-nodisp", "-autoexit", "-loglevel", "quiet", "-volume", strconv.Itoa(volume), mediaFilePath)
	err := cmd.Start()
	if err != nil {
		log.Printf("Error starting ffplay: %v", err)
		return nil, err
	}
	playerActive = true
	// playingStatusChanged <- true
	go monitorForCmdFinished(cmd)
	return cmd, nil
}

// Check for command termination. Must be run in a separate goroutine
func monitorForCmdFinished(cmd *exec.Cmd) {
	if cmd == nil {
		playerActive = false
		// playingStatusChanged <- true
		return
	}
	err := cmd.Wait()
	if err != nil && err.Error() != "signal: killed" {
		log.Printf("Command finished with error: %v", err)
	}
	playerActive = false
	fyne.Do(playerFinished) // Update UI in main thread
	// playingStatusChanged <- true
}

func stopPlayer(cmd *exec.Cmd) error {
	if cmd != nil && cmd.Process != nil {
		err := cmd.Process.Kill()
		if err != nil {
			log.Printf("Error stopping player: %v	", err)
			return err
		}
	}
	return nil
}
