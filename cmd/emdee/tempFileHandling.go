// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import "os"

func writeTempFile(filename string, data []byte) (string, error) {
	f, err := os.CreateTemp("", filename)
	if err != nil {
		return "", err
	}
	_, err = f.Write(data)
	if err != nil {
		return "", err
	}
	return f.Name(), nil
}

func deleteTempFile(filename string) {
	os.Remove(filename)
}
