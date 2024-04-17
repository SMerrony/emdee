<a href="https://github.com/SMerrony/emdee">
    <img src="https://github.com/SMerrony/emdee/blob/main/doc/emdee_icon.png" alt="eMDee logo" title="eMDee" align="right" height="60" />
</a>

# eMDee Musical Director's Assistant

![GitHub License](https://img.shields.io/github/license/SMerrony/emdee) ![GitHub Issues or Pull Requests](https://img.shields.io/github/issues/SMerrony/emdee) ![GitHub top language](https://img.shields.io/github/languages/top/SMerrony/emdee)




`eMDee` is a live performance assistant for musical directors which removes 
the need to have folders of tracks and command-line windows open in order to play backing tracks for performance groups such as singers, choirs, and theatre-groups.

The MD can plan in advance the order of performance; later, `eMDee` will facilitate the playing of each track in the specfied order during the performance.

Additional controls may be added to specific tracks such as changing the volume level.

![eMDee main screen, ready to perform!](doc/eMDee_0_1_0_Loaded.png)

`eMDee` is designed for, and probably only runs on, GNU Linux systems such as Mint, Debian, Ubuntu, etc.

## Core Features
* Create 'session' of 'tracks'
* Tracks may be audio (FLAC, MP3, OGG, WAV) or MIDI files
* Tracks may be re-ordered
* Tracks may be marked for skipping in performance
* Controls are provided to Play, Stop
* Audio tracks may have a volume modifie
* Dummy or 'placeholder' tracks may be inserted to remind MD of a cappella pieces etc.

Third-party 'helper' applications are used to actually play the tracks, currently...
* `ffplay` for FLAC, MP3, OGG and WAV
* `aplaymidi` for MIDI files on an external device such as a synthesizer, keyboard, or digital piano

You must ensure those applications are installed on your system for `eMDee` to work.

When a MIDI file is manually stopped from playing, an all-notes-off MIDI file is sent to the player to prevent stuck notes.

## Session Editing

![eMDee main screen, editing tracks](doc/eMDee_0_1_0_Editing.png)