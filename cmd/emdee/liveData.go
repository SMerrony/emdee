// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"emdee/internal/players"
	"sync"
)

type liveDataT struct {
	mutex           sync.RWMutex
	sessionDirty    bool
	sessionFilePath string
	playerActive    bool
	activeTrackIx   int
	mediaType       players.MediaType
}

var liveData liveDataT

func initLiveData() {
	liveData.mutex.Lock()
	liveData.sessionDirty = false
	liveData.sessionFilePath = ""
	liveData.playerActive = false
	liveData.activeTrackIx = -1
	liveData.mutex.Unlock()
}

// SessionIsDirty returns the value of sessionDirty.
func sessionIsDirty() bool {
	liveData.mutex.RLock()
	defer liveData.mutex.RUnlock()
	return liveData.sessionDirty
}

// SetSessionDirty sets the value of sessionDirty.
func setSessionDirty(dirty bool) {
	liveData.mutex.Lock()
	defer liveData.mutex.Unlock()
	liveData.sessionDirty = dirty
}

// GetSessionFilePath returns the value of sessionFilePath.
func getSessionFilePath() string {
	liveData.mutex.RLock()
	defer liveData.mutex.RUnlock()
	return liveData.sessionFilePath
}

// SetSessionFilePath sets the value of sessionFilePath.
func setSessionFilePath(path string) {
	liveData.mutex.Lock()
	defer liveData.mutex.Unlock()
	liveData.sessionFilePath = path
}

// playerIsActive returns the value of playerActive.
func playerIsActive() bool {
	liveData.mutex.RLock()
	defer liveData.mutex.RUnlock()
	return liveData.playerActive
}

// SetPlayerActive sets the value of playerActive.
func setPlayerActive(active bool) {
	liveData.mutex.Lock()
	defer liveData.mutex.Unlock()
	liveData.playerActive = active
}

// getActiveTrackIx() returns the index of the currently active track.
func getActiveTrackIx() int {
	liveData.mutex.RLock()
	defer liveData.mutex.RUnlock()
	return liveData.activeTrackIx
}

func setActiveTrackIx(t int) {
	liveData.mutex.Lock()
	defer liveData.mutex.Unlock()
	liveData.activeTrackIx = t
}

func getMediaType() players.MediaType {
	liveData.mutex.RLock()
	defer liveData.mutex.RUnlock()
	return liveData.mediaType
}

func setMediaType(mt players.MediaType) {
	liveData.mutex.Lock()
	defer liveData.mutex.Unlock()
	liveData.mediaType = mt
}
