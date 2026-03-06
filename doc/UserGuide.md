# eMDee User Guide

`eMDee` is a live performance and rehearsal tool for musical directors which removes 
the need to have folders of tracks and different media players open in order to play backing tracks for performance groups such as singers, choirs, and theatre-groups.

The MD can plan in advance the order of performance; later, `eMDee` will facilitate the playing of each track in the specfied order during the performance.  The volume of each track can be adjusted, and tracks can be skipped etc.

- [eMDee User Guide](#emdee-user-guide)
  - [Installation](#installation)
  - [Terminology](#terminology)
  - [Starting the Application](#starting-the-application)
  - [Menu](#menu)
    - [File](#file)
    - [View](#view)
    - [MIDI](#midi)
    - [Help](#help)
  - [Creating a New Session](#creating-a-new-session)
  - [Adding Tracks](#adding-tracks)
    - [Title](#title)
    - [Skip](#skip)
    - [Comment](#comment)
    - [Volume](#volume)
    - [Increase/Decrease Volume](#increasedecrease-volume)
    - [Lead-in](#lead-in)
    - [Media File Chooser](#media-file-chooser)


## Installation
See the project [README.](../README.md)

## Terminology
* Track - usually an audio or MIDI file, typically the accompaniment for a single piece of music.  You can also add tracks with no associated media file, eg. as a placeholder for an unaccompanied item
* Session - a collection of tracks arranged in a specific order for a performance, rehearsal, gig, etc.

## Starting the Application
`eMDee` may be started in two ways...
1. With no arguments - in which case an empty session is shown
2. With the `-sesssion` argument followed by an existing session TOML file - in which case the specified session is loaded and displayed. Eg. `./emdee -session Christmas26.toml`

## Menu
### File
No surprises here; you can create new sessions, open existing ones, save your session etc.

### View
Here you can select one of three GUI sizes for eMDee.  It's often useful to have a large (or XL) interface
on laptops or tablets being used in performance.  The chosen view size is saved whenever you save a session.

You can also switch between performance (normal) and Session Editing mode from this menu.

### MIDI
### Help

## Creating a New Session
First, ensure you are in the session editing mode either by choosing `Session Editing` from the `View` menu or via the `File | New` menu option - an empty track will appear on the display.

Enter a descriptive name for the session in the top (`Session`) field.  Optionally, add further notes about the session in the `Notes` field.

Now proceed to add tracks to your session...

## Adding Tracks
The procedure for adding tracks is the same whether you are creating a new session or changing an existing one.

You must be in `Session Editing` mode.

From left to right...

### Title
Enter the title of the track (compulsory) in the first field.

### Skip
The check-box is to indicate that the track should be skipped, check it if you want the track to be skipped in performance - it will also be checked if there is no media file for the track.

### Comment
Next is an optional comment field.  Often this is used for a short *aide memoire* for the MD.

### Volume
Following the comment is the volume field - this is entered as a percentage of the maximum volume. 

### Increase/Decrease Volume
The next two buttons decrease and increase the volume by 5% respectively.

### Lead-in
Enter the number of seconds lead-in (silence) you want played before the track starts playing.
This is very useful if your sound system needs a second or two to "wake up" (eg. some optical or Bluetooth connections), or if you simply need a little time to prepare yourself before the track actually starts.

### Media File Chooser
Hitting the folder button will bring up a file chooser for you to associate a media file (eg. .wav, .mp3, .flac, .midi, etc.) with the current track.


