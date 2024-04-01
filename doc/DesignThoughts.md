# Design Thoughts for 'eMDee'

## Intended Use
The idea behind `eMDee` is to provide a live performance assistant for musical directors which removes 
the need to have folders of tracks and command-line windows open in order to play backing tracks for live
performance groups such as singers, choirs, and theatre-groups.

The MD can plan in advance the order of performance and `eMDee` will facilitate the playing of each track in the specfied order.  

Additional controls may be added to specific tracks such as volume boost, tempo change, etc.

Easy-to-access performance controls are provided to stop, pause and restart the current track.

## Core Functionality
* Create 'session' of 'tracks'
* Tracks may be audio or MIDI files
* Tracks may be re-ordered
* Tracks may be marked for skipping in performance
* Controls are provided to Play Next, Stop, Pause, and Restart
* Audio tracks may have a volume modifier
* MIDI tracks may have a tempo modifier
* Dummy or 'placeholder' tracks may be inserted to remind MD of a cappella pieces etc.

Third-party 'helper' applications are used to actually play the tracks, initially...
* `mpg123` for MP3
* `ogg123` for OGG
* `aplay` for WAV files
* `aplaymidi` for MIDI files on an external device such as a synthesizer, keyboard, or digital piano

## Session Definition
```
# Sample eMDee session TOML file

[session]
name = "MJC Summer Concert - Choir"
comment = "For the MJC Summer concert in the main hall (my laptop, their piano, main stage PA)"
updated = 2024-03-29T08:50:12

[[tracks]]
title = "C'est si bon"
path = "/home/steve/Desktop/MJC_Saix_MP3/Accompaniments/CestSiBon_SAH_Accpt.mid"
comment = "A little faster in the dry acoustic"
tempo = 1.05                 # increase tempo by 5%

[[tracks]]
title = "C'est Magnifique"
skip = true                 # this track will be skipped 
path = "/home/steve/Desktop/MJC_Saix_MP3/Accompaniments/CestMagnifique_Accpt.mp3"
comment = "*Good for encore!*"
volume = 0.95               # reduce volume by 5%

[[tracks]]
title = "Mon coq est mort"
comment = "No accompaniment, give note F"

[[tracks]]
title = "La Javanaise"
path = "/home/steve/Desktop/MJC_Saix_MP3/Accompaniments/LaJavanaise_Accpt.mp3"

```

## Players Configuration

The idea is that you probably only have one or two `players.toml` setups as
most of this configuration will be fairly common across your sessions.

If a `players.toml` file is found when `eMDee` starts, then it will be loaded as the 
default players configuration.

``` 
# Sample eMDee players configuration TOML file
[players]
description = "Normal laptop/USB-MIDI setup"
mp3_player = "mpg123"               # no path specified, must be on $PATH
ogg_player = "ogg123"
wav_player = "/usr/bin/aplay"       # specific path specified
midi_player = "aplaymidi -p 20:0"   # port argument supplied
```
