// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/widget"
)

// Customised Label widget with a minimum width
type MinSizeableLabel struct {
	widget.Label
	minWidth float32
}

func NewMinSizeableLabel(text string, minWidth float32) *MinSizeableLabel {
	label := &MinSizeableLabel{minWidth: minWidth}
	label.ExtendBaseWidget(label)
	label.SetText(text)
	return label
}

func (l *MinSizeableLabel) MinSize() fyne.Size {
	min := l.Label.MinSize()
	min.Width = l.minWidth // Set a minimum width for the label
	return min
}

// Customised Entry widget with a minimum width
type MinSizeableEntry struct {
	widget.Entry
	minWidth float32
}

func NewMinSizeableEntry(minWidth float32) *MinSizeableEntry {
	entry := &MinSizeableEntry{minWidth: minWidth}
	entry.ExtendBaseWidget(entry)
	return entry
}

func (e *MinSizeableEntry) MinSize() fyne.Size {
	min := e.Entry.MinSize()
	min.Width = e.minWidth // Set a minimum width for the entry
	return min
}
