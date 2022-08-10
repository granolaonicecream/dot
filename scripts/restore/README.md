# Window Restore Script

Restore (unminimize a window) by its ID.  This exists to fill the functional gap of xdotool being unable to restore a window without also focusing it.  

Since most window managers give focus to a window activation request, xdotool can't just bring the window back into view without other consequences.  For example, tiling manager layouts may treat an activation request as special and relayout the window differently than its last position in the layout.

TODO: this might be worth making an MR to the xdotool repository.
