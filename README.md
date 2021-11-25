# janet-editor-elf

Helpful Bits for Janet Support in Editors

## Background

An attempt to provide bits outside of an editor to help with Janet
support in editors.  The idea is that externalization can:

* Reduce duplicated implementation effort among editors
* Increase consistent behavior across editors
* Achieve some level of support sooner than with "native" support in
  editors with less developed support for Janet

## What Can It Do?

* Indentation
* Wrapping and unwrapping `tracev` and `comment` forms

## Planned

* Determine expression before the cursor
* Check for [left margin convention](doc/left-margin-convention.md) 
  compliance

## Editor Support

At the moment, there is only [integration with Emacs](doc/emacs.md).

The author hopes that it will be practical to support other editors
such as Neovim, Kakoune, Freja, and possibly VSCode.

## Credits

* bakpakin - janet, spork/fmt, etc.
* llmII - discussion and testing
* pyrmont - discussion
* saikyun - discussion and freja
