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

### As executable by system

- Set *JURL_HOME* environment variable based on the location of the *jurl-X.X.X.jar*
- Add *JURL_HOME* to *PATH* enrivonment variable

**Windows**

Edit the environment variables

| Variable | Value |
| --- | --- |
| JURL_HOME | *jurl JAR location* |
| Path | %Path%;%JURL_HOME% |

```bat
rem Restart command prompt
jurl -h
```

**Linux**

Add/modify the environment variables in *~/.bashrc* file

```sh
vim ~/.bashrc
# vim>
# export JURL_HOME="<jurl-jar-location>"
# export PATH=$PATH:$JURL_HOME

source ~/.bashrc

# Set execution permissions to *jurl* bash file
chmod +x $JURL_HOME/jurl

jurl -h
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
jurl -s name "gengar" pokemon-api
```
> According to the spec, after the request the variable `pokemonId` will have the ID of the requested Pokémon

Execute a request by name
```sh
jurl -n encounters pokemon-api
```


### Config tricks

Create or edit the `<WORKING_DIR>/config/config.json` file to specify default values:

| Field | Value |
| --- | --- |
| **openEditorCommand** | <ul><li>`/usr/bin/subl \"{{HTTP/response.path}}\"`</li><li>`\"%PROGRAMFILES%\\Sublime Text\\subl.exe\" \"{{~win-separator~HTTP/response.path}}\"`</li></ul> |
| **downloadsLocation** | <ul><li>`$HOME/Downloads`</li><li>`%USERPROFILE%\\Downloads`</li></ul> |

> Example:
> ```json
> {
>     "openEditorCommand": "/usr/bin/subl \"{{HTTP/response.path}}\"",
>     "downloadsLocation": "$HOME/Downloads"
> }
> ```

## Contribution

Follow this project's [Contribution guidelines](.github/CONTRIBUTING.md).


## License

Copyright © 2023-2024, [Giovanni Farfán B.](https://github.com/gfarfanb). Released under the 
[MIT License](./LICENSE).
