# eMDee Musical Director's Assistant

The idea behind `eMDee` is to provide a live performance assistant for musical directors which removes 
the need to have folders of tracks and command-line windows open in order to play backing tracks for live
performance groups such as singers, choirs, and theatre-groups.

The MD can plan in advance the order of performance; later, `eMDee` will facilitate the playing of each track in the specfied order during the performance.

Additional controls may be added to specific tracks such as volume boost, tempo change, etc.

## Core Features
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

