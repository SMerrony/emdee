// SPDX-License-Identifier: GPL-3.0-or-later
// SPDX-FileCopyrightText: Copyright 2026 Stephen Merrony

package main

import (
	_ "embed"
	"flag"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
)

const (
	appTitle     = "eMDee"
	appComment   = "Live performance and rehearsal assistant for musical directors"
	appCopyright = "Copyright ©2026 S.Merrony"
	appSemVer    = "v0.3.0" // TODO Update SemVer on each release!
	appWebsite   = "https://github.com/SMerrony/emdee"
	helpURL      = "https://github.com/SMerrony/emdee/blob/main/doc/UserGuide.md"

	statusUpdatePeriodMs = 500
)

var (
	emdeeApp   fyne.App
	mainWindow fyne.Window

	silentFileName, allNotesOffFileName string
)

var (
	sessionFlag = flag.String("session", "", "Path to session file to load on startup")
	versionFlag = flag.Bool("version", false, "Display version number and exit")
)

//go:generate fyne bundle -o bundled.go emdee_icon.png
//go:generate fyne bundle -o bundled.go -append silence_1s.mp3
//go:generate fyne bundle -o bundled.go -append all_notes_off.mid

func main() {
	flag.Parse()
	if *versionFlag {
		println(appTitle, appSemVer)
		return
	}

	emdeeApp = app.New()
	emdeeApp.SetIcon(resourceEmdeeiconPng)
	emdeeApp.Settings().SetTheme(&emdeeTheme{})
	initLiveData()
	mainWindow = emdeeApp.NewWindow(appTitle + " - (No Session Loaded)")

	setupWindow(mainWindow)

	if *sessionFlag != "" {
		loadAndShowSession(*sessionFlag)
	}

	checkPlayers()
	var err error
	silentFileName, err = writeTempFile("silent1sec*.mp3", resourceSilence1sMp3Data)
	defer deleteTempFile(silentFileName)
	allNotesOffFileName, err = writeTempFile("allNotesOff*.midi", resourceAllnotesoffMidData)
	defer deleteTempFile(allNotesOffFileName)

	if err != nil {
		panic(err)
	}

	mainWindow.SetCloseIntercept(func() {
		promptToSaveIfDirty(func() {
			mainWindow.Close()
		})
	})

	mainWindow.ShowAndRun()
}
