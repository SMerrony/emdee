# Developer's Notes

## Embedded Resources

The embedded icon was generated with this command, run from the top-level dir...
`are --lang=Ada -o src --resource=Embedded --name-access --fileset='**/*.*' share/emdee`

## Examine Style (CSS) of Running Application
`GTK_DEBUG=interactive bin/emdee`

## Browse Available Icons for Button etc.
`Icon Browser` from Mint menu, or `gtk3-icon-browser`

## Required Libraries
* libcsfml-dev and its dependents

On 2024-04-03, current Linux Mint I had to specify the exact
version of the crate via...
`alr with asfml=2.5.1`

## Pain Points

### Spawn
The whole `spawn_async` thing took ages to figure out.  As usual, it seemed impossible to find a simple example to follow.  

There's no complementary function to kill a process spawned by the above function.

? Maybe `aShell` would make life easier?

### Players
It turns out that PulseAudio is not 'visible' to non-desktop applications.
Jack therefore becomes mandatory, which seems a shame.