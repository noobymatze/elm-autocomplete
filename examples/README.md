# Examples

There are multiple examples for you to explore in the `./src` directory.

## Pick one to build

#### Dead simple autocomplete

Default autocomplete based on Github mentions CSS: `./src/SimpleExample.elm`

To build :`make simple`

#### Simple CSS Class Styling

A simple, styled example: `./src/StyledExample.elm`.

To build: `make styled`


#### HTTP

An HTTP fetching example: `./src/RemoteExample.elm`

To build: `make remote`


#### Typeahead Example

An advanced styling example: `./src/AdvancedExample.elm`

To build: `make typeahead`

#### Controlled Example

A textarea with controlled autocompletes inside: `./src/ControlledExample.elm`

To build: `make controlled`

Or, any of these without `make`:

`elm make --output build/elm.js src/<some_example.elm>`

## Run it

After building one of these, simply open `example.html` in your favorite browser!
