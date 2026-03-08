// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package customwidgets

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/widget"
)

// Customised Label widget with a minimum width
type MinWidthLabel struct {
	widget.Label
	minWidth float32
}

func NewMinWidthLabel(text string, minWidth float32) *MinWidthLabel {
	label := &MinWidthLabel{minWidth: minWidth}
	label.ExtendBaseWidget(label)
	label.SetText(text)
	label.Alignment = fyne.TextAlignCenter
	return label
}

func (l *MinWidthLabel) MinSize() fyne.Size {
	min := l.Label.MinSize()
	min.Width = l.minWidth // Set a minimum width for the label
	return min
}

// Customised Entry widget with a minimum width
type MinWidthEntry struct {
	widget.Entry
	minWidth float32
}

func NewMinWidthEntry(minWidth float32) *MinWidthEntry {
	entry := &MinWidthEntry{minWidth: minWidth}
	entry.ExtendBaseWidget(entry)
	return entry
}

func (e *MinWidthEntry) MinSize() fyne.Size {
	min := e.Entry.MinSize()
	min.Width = e.minWidth // Set a minimum width for the entry
	return min
}

// Customised Button widget with a minimum height
type MinHeightButton struct {
	widget.Button
	minHeight float32
}

func NewMinHeightButton(text string, minHeight float32) *MinHeightButton {
	button := &MinHeightButton{minHeight: minHeight}
	button.SetText(text)
	button.ExtendBaseWidget(button)
	return button
}

func (b *MinHeightButton) MinSize() fyne.Size {
	min := b.Button.MinSize()
	min.Height = b.minHeight // Set a minimum height for the button
	return min
}
