# Developer's Notes

## Embedded Resources

The embedded icon was generated with this command, run from the top-level dir...
`are --lang=Ada -o src --resource=Embedded --name-access --fileset='**/*.*' share/emdee`

## Examine Style (CSS) of Running Application
`GTK_DEBUG=interactive bin/emdee`

## Browse Available Icons for Button etc.
`Icon Browser` from Mint menu, or `gtk3-icon-browser`

## Required Libraries

## Pain Points

### Spawn
The whole `spawn_async` thing took ages to figure out.  As usual, it seemed impossible to find a simple example to follow.  

For future referencem this simple example worked...
```
      --  -- THIS WORKS...
      --  OK := Spawn_Command_Line_Async (Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player) &
      --                                                           " " & Media_File), Err);
      --  -- ...THAT WORKED
```

There's no complementary function to kill a process spawned by the above function.

? Maybe `aShell` would make life easier?
