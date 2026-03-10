// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"strconv"
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
)

var statusLabel *widget.Label

func buildStatusBox() (statBox *fyne.Container) {
	statusLabel = widget.NewLabel("No session loaded")
	statBox = container.NewHBox(layout.NewSpacer(), statusLabel, layout.NewSpacer())
	go func() {
		ticker := time.NewTicker(statusUpdatePeriodMs * time.Millisecond)
		for range ticker.C {
			fyne.Do(func() {
				statusText := ""
				if playerIsActive() {
					statusText = "Playing Track " + strconv.Itoa(getActiveTrackIx()+1) +
						" - " + config.Tracks[getActiveTrackIx()].Title
				} else {
					statusText = "(Not Playing)"
				}
				if sessionIsDirty() {
					statusText = statusText + " - Unsaved changes"
				}
				statusLabel.SetText(statusText)
			})
		}
	}()
	return statBox
}
