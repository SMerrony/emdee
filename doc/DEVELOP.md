# Developer's Notes

## Embedded Resources

The embedded icon was generated with this command, run from the top-level dir...
`are --lang=Ada -o src --resource=Embedded --name-access --fileset='**/*.*' share/emdee`

The all notes off midi file was created using csvmidi.

## Examine Style (CSS) of Running Application
`GTK_DEBUG=interactive bin/emdee`

## Browse Available Icons for Button etc.
`Icon Browser` from Mint menu, or `gtk3-icon-browser`

## Required Libraries

## Pain Points

### Spawn
The whole `spawn_async` thing took ages to figure out.  As usual, it seemed impossible to find a simple example to follow.  

For future reference, this simple example worked...
```
      --  -- THIS WORKS...
      OK := Spawn_Command_Line_Async (Gtkada.Types.New_String (To_String (Active_Players_Config.MP3_Player) &
                                                                 " " & Media_File), Err);
      --  -- ...THAT WORKED
```

This also worked...
```
   function Spawn_Async is
      new Generic_Spawn_Async (User_Data => Integer);

...

      Okay := Spawn_Async (Working_Directory => Gtkada.Types.Null_Ptr,
                           Argv => Argv'Access,
                           Envp => null,  --  Inherit our env, critical that XDG_RUNTIME_DIR exists
                           Flags => G_Spawn_Search_Path, --  + G_Spawn_Do_Not_Reap_Child,
                           Child_Setup => null,
                           Data => null,
                           Child_Pid => Player_PID'Access,
                           Error => PErr'Access
                           );
```

There's no complementary function to kill a process spawned by the above function.

### PulseAudio Volume
It seems a new virtual interface is created for every app that plays music, the master volume control does nothing to those interfaces, so it's useless.

### Windows
To get Alire to build win32ada I had to add

```,"-gnateDTARGET=Win32"```

to the `emdee_config.gpr` file.

