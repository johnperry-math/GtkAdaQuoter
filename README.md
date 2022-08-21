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

(_maybe_)

- [ ] Add a file chooser dialog for opening the file.
- [ ] Enable a configuration file so that previously-used paths are remembered.
- [ ] Enable tabbing from one cell to another.
- [ ] Enable drag-and-drop reordering of quotes.

### Done!
- [x] Choose better default window dimensions.

## See also

The JsonQuoter submodule is used to obtain, manipulate, and store the quotes.

## License

Public domain, insofar as this is compatible with the libraries used.
