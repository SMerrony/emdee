# eMDee User Guide

`eMDee` is a live performance and rehearsal tool for musical directors which removes 
the need to have folders of tracks and different media players open in order to play backing tracks for performance groups such as singers, choirs, and theatre-groups.

The MD can plan in advance the order of performance; later, `eMDee` will facilitate the playing of each track in the specfied order during the performance.  The volume of each track can be adjusted, and tracks can be skipped etc.

## Installation
See the project [README.](../README.md)

## Terminology
* Track - an audio or MIDI file, usually the accompaniment for a single piece of music
* Session - a collection of tracks arranged in a specific order for a performance, rehearsal, etc.

## Starting the Application
`eMDee` may be started in two ways...
1. With no arguments - in which case an empty session is shown
2. With the `-sesssion` argument followed by an existing session TOML file - in which case the specified session is loaded and displayed. Eg. `./emdee -session Christmas26.toml`