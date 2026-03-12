// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package players

import (
	"log"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
)

type MediaType int

const (
	MediaNone MediaType = iota
	MediaAudio
	MediaMIDI
	MediaUnknown
)

func GuessMediaType(mediaPath string) MediaType {
	if mediaPath == "" {
		return MediaNone
	}
	ucPath := strings.ToUpper(mediaPath)
	if strings.HasSuffix(ucPath, ".MP3") ||
		strings.HasSuffix(ucPath, ".OGG") ||
		strings.HasSuffix(ucPath, ".WAV") ||
		strings.HasSuffix(ucPath, ".FLAC") {
		return MediaAudio
	}
	if strings.HasSuffix(ucPath, ".MID") ||
		strings.HasSuffix(ucPath, ".MIDI") ||
		strings.HasSuffix(ucPath, ".SMF") ||
		strings.HasSuffix(ucPath, ".KAR") {
		return MediaMIDI
	}
	return MediaUnknown
}

type KnownPlayers int

const (
	PlayerFfplay KnownPlayers = iota
	PlayerAplaymidi
	PlayerPlaysmf
)

func (s KnownPlayers) String() string {
	switch s {
	case PlayerAplaymidi:
		return "aplaymidi"
	case PlayerFfplay:
		return "ffplay"
	case PlayerPlaysmf:
		return "playsmf"
	}
	return "unknown (internal error)"
}

func CheckPlayerFound(player KnownPlayers) bool {
	_, err := exec.LookPath(player.String())
	return err == nil
}

func StartPlayer(player KnownPlayers, path string, volume int, midiPort string) (cmd *exec.Cmd, err error) {
	switch player {
	case PlayerFfplay:
		cmd = exec.Command(player.String(), "-hide_banner", "-nodisp", "-autoexit", "-loglevel", "quiet", "-volume", strconv.Itoa(volume), path)
	case PlayerAplaymidi:
		cmd = exec.Command(player.String(), "-p", midiPort, path)
	case PlayerPlaysmf:
		cmd = exec.Command(player.String(), "--out", midiPort, path)
	}
	err = cmd.Start()
	if err != nil {
		log.Printf("Error starting ffplay: %v", err)
		return nil, err
	}
	return cmd, nil
}

func StopPlayer(cmd *exec.Cmd) error {
	if cmd != nil && cmd.Process != nil {
		err := cmd.Process.Kill()
		if err != nil {
			log.Printf("Error stopping player: %v ", err)
			return err
		}
	}
	return nil
}

func ListMidiOuts() (result string) {
	switch runtime.GOOS {
	case "linux":
		cmd := exec.Command("aplaymidi", "-l")
		out, err := cmd.Output()
		if err != nil {
			result = err.Error()
		} else {
			result = string(out)
		}
	case "windows":
		cmd := exec.Command("lsmidiouts")
		out, err := cmd.Output()
		if err != nil {
			result = err.Error()
		} else {
			result = string(out)
		}
	default:
		result = "Cannot list MIDI ports on this Operating System"
	}
	return result
}
