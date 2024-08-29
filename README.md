[![jurl CI](https://github.com/gfarfanb/jurl/actions/workflows/maven.yml/badge.svg)](https://github.com/gfarfanb/jurl/actions/workflows/maven.yml)

# jurl
CLI Java application to provide an API test/client tool.


## What do you need for execute it

- [JRE](https://www.java.com/en/download/)/[JDK](https://www.oracle.com/java/technologies/downloads/) ~> 8


## Installation

- Choose and download a [release version](https://github.com/gfarfanb/jurl/releases) (ZIP file *jurl-X.X.X.zip*)
- Decompress ZIP file and test it

```bat
rem Windows
jurl -h
```

```sh
# Linux
./jurl -h
```

- (Optional) Set *JURL_HOME* environment variable. In the case you want to put the *jurl-X.X.X.jar*
in a different location

```bat
rem Windows
set "JURL_HOME=..."
```

```sh
# Linux
export JURL_HOME="..."
```


## How to use it

Define a spec file (no specific extension is required)

**pokemon-api**
```text
@default-request = pokemon


### [request] pokemon
GET https://pokeapi.co/api/v2/pokemon/{{name}}

output pokemonId = {{OUT/id}}


### [request] encounters
@method = GET
@host = https://pokeapi.co
@basePath = /api/v2
@endpoint = /pokemon/{{pokemonId}}/encounters
```
> [PokéAPI - The RESTful Pokémon API](https://pokeapi.co/)

Execute request
```sh
./jurl -s name "gengar" pokemon-api
```
> According to the spec, after the request the variable `pokemonId` will have the ID of the requested Pokémon

Execute a request by name
```sh
./jurl -n encounters pokemon-api
```


### Config tricks

**config.json**
```json
{
    "openEditorCommand": "\"C:\\Program Files\\Sublime Text\\subl.exe\" \"{{~win-separator~HTTP/response.path}}\""
}
```

## Contribution

Follow the [Contribution guidelines](.github/CONTRIBUTING.md) for this project.


## License

Copyright © 2023-2024, [Giovanni Farfán B.](https://github.com/gfarfanb). Released under the 
[MIT License](./LICENSE).
