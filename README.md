# rda-documentor
Auto-generated improved documentation for the Resource Description and Access ontology,
in the form of a small babashka webserver that can be accessed locally.

## Running the server
- [Install babashka](https://book.babashka.org/#_installation)
- Download the [RDA CSV ontology](https://github.com/RDARegistry/RDA-Vocabularies/tree/master/csv/Elements).
  I recommend using something like [this tool](https://download-directory.github.io/) to download just the directory.
- Build the database: `bb make-db`
- Start a repl
- In `server.clj`, run `(start-server!)`
