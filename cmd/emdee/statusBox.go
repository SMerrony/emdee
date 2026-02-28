// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
)

func buildStatusBox() (statBox *fyne.Container) {
	statusLabel = widget.NewLabel("No session loaded")
	statBox = container.NewHBox(layout.NewSpacer(), statusLabel, layout.NewSpacer())
	go func() {
		ticker := time.NewTicker(statusUpdatePeriodMs * time.Millisecond)
		for range ticker.C {
			updateStatusBox()
		}
	}()
	return statBox
}

func updateStatusBox() {
	fyne.Do(func() {
		if currentSession == nil {
			statusLabel.SetText("No session loaded")
		} else if playerActive {
			statusLabel.SetText("(Playing)")
		} else {
			statusLabel.SetText("(Not Playing)")
		}
	})
}
