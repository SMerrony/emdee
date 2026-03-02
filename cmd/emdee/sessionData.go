// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	"os"

	"github.com/BurntSushi/toml"
)

type Config struct {
	Session Session `toml:"session"`
	Tracks  []Track `toml:"track"`
}

type Session struct {
	Name     string `toml:"name"`
	Notes    string `toml:"notes"`
	FontSize string `toml:"fontsize"`
	MidiPort string `toml:"midiport"`
	filePath string // Not stored in the TOML, used to track from where the session was loaded
	isDirty  bool   // Not stored in the TOML, used to track whether the session has unsaved changes
}

type Track struct {
	Comment string `toml:"comment"`
	Path    string `toml:"path"`
	Play    bool   `toml:"play"`
	Title   string `toml:"title"`
	Volume  int    `toml:"volume"`
	LeadIn  int    `toml:"leadin"`
}

func loadSession(path string) (*Config, error) {
	if _, err := os.Stat(path); err != nil {
		return nil, err
	}
	var config Config
	if _, err := toml.DecodeFile(path, &config); err != nil {
		return nil, err
	}
	config.Session.filePath = path
	config.Session.isDirty = false
	return &config, nil
}

func saveSession(path string, config *Config) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	encoder := toml.NewEncoder(f)
	if err := encoder.Encode(config); err != nil {
		return err
	}
	return nil
}
