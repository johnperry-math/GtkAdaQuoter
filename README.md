# GtkAdaQuoter

A "short-term" project to learn some GtkAda, while also obtaining a useful tool in the process.
I even ended up contributing quite a bit of material based on this to the
[Ada Programming wikibook](https://en.m.wikibooks.org/wiki/Ada_Programming)'s
[section on GtkAda](https://en.m.wikibooks.org/wiki/Ada_Programming/Libraries/GUI/GtkAda#).

## What it does

* Reads from a json-formatted file (currently hardcoded to `~/signatures/all_signatures.json`).
  * Expects an array of quote objects.
  * For each object, expects a field for `quotation`, and optional fields for `author`, `source`, and `speaker`.
* Pops up a window with the quotes arranged in rows, different fields in columns.
* Allows one to:
  * add a row beneath the currently highlighted row;
  * delete the currently highlighted row;
  * save all the quotes to a file determined by a file chooser dialog.

## To do

### Done!
- [x] Add a file chooser dialog for opening the file.
- [x] Add a "quit" button with mnemonic.
- [x] ~~Add icons to buttons.~~ Figure out why buttons won't display icons.
  - Turns out this is due to a design decision by GTK developers,
    Who Know Better Than You (TM).
- [x] Choose better default window dimensions.
- [x] Decent behavior when the default input file isn't found.
- [x] Decent behavior when the input file chooser dialog is canceled.
- [x] ~~Better default column widths.~~ OBE by configuration file
- [x] Enable a configuration file to remember:

     - [x] column widths;
     - [x] ~~previously-used paths for opening, saving;~~
       - Decided to rely on usability of GTK's file chooser.
     - [x] window dimensions.

- [x] Enable a Ctrl-Q shortcut.
- [x] Enable a Ctrl-W shortcut.
- [x] Enable drag-and-drop reordering of quotes.
- [x] ~~Fix button alignment.~~
      Figure out why the button bar does not expand to window edges.
  - Turns out this is due to GTK's unintuitive `GTK_Attach_Options`.
    I managed to get it to hit the edges (see commented code)
    but it's not pretty.
- [x] Fix the bug that arises when tabbing away from an active but un-edited cell.
  - Turns out GTK **really** wants the `GTK_Tree_View` to grab focus
    after setting the focus on a particular cell, and for some reason
    the `Tab` button doesn't do this properly.
- [x] Fix the bug where closing the window raises GTK alarms.
- [x] When editing is complete, enable automatic navigation to next cell.

## See also

The JsonQuoter submodule is used to obtain, manipulate, and store the quotes.

## License

Public domain, insofar as this is compatible with the libraries used.
