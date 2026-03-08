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
}

type Track struct {
	Title   string `toml:"title"`
	Skip    bool   `toml:"skip"`
	Comment string `toml:"comment"`
	Path    string `toml:"path"`
	Volume  int    `toml:"volume"`
	LeadIn  int    `toml:"leadin"`
}

func newConfig() *Config {
	return &Config{}
}

func loadSession(path string) (*Config, error) {
	if _, err := os.Stat(path); err != nil {
		return nil, err
	}
	var config Config
	if _, err := toml.DecodeFile(path, &config); err != nil {
		return nil, err
	}
	return &config, nil
}

func (conf *Config) Save(path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	encoder := toml.NewEncoder(f)
	if err := encoder.Encode(conf); err != nil {
		return err
	}
	return nil
}

func (conf *Config) swapTracks(i, j int) {
	conf.Tracks[i], conf.Tracks[j] = conf.Tracks[j], conf.Tracks[i]
}
