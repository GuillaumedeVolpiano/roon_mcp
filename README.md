# roon-mcp

A [Model Context Protocol](https://modelcontextprotocol.io) server, on
stdio, that lets an assistant play music on a [Roon](https://roon.app)
system: list the zones, search the library for tracks, queue them, and
control playback.

This is an independent, unofficial project, not affiliated with or endorsed
by Roon Labs. It is built on
[roon-api](https://github.com/GuillaumedeVolpiano/roon-api) and
[effectful-mcp](https://github.com/GuillaumedeVolpiano/effectful-mcp), and
speaks MCP revision 2026-07-28, and the earlier revisions to the clients
that only know them.

## Tools

| Tool | What it does |
|---|---|
| `list_zones` | The zones, what plays in each, their outputs with their volume, their playback options, and what each allows. |
| `get_queue` | The next items of the queue of a zone. |
| `search_tracks` | Several searches at once, one per track; candidates with an id, a title and artists. |
| `queue_tracks` | Candidates into the queue of a zone, in order: at the end (`queue`, the default), after the current track (`add_next`), or in place of the queue, the first at once (`play_now`). Reports each track. |
| `control` | Play, pause, stop, next, previous. |
| `browse_library` | The library by albums, artists, playlists, genres, composers and internet radios, or a search of all of it; opens the lists it finds, page by page. |
| `play_item` | What `browse_library` found (a track, an album, a playlist, an artist, a radio) into a zone: `queue` (the default), `add_next`, `play_now`, `start_radio` or `shuffle`. |
| `set_volume` | The volume of a zone or of one of its outputs: a level, a move, or mute. |
| `seek` | A position in the track that plays, or a move from the current one. |
| `set_playback_options` | Shuffle, Roon Radio after the queue, and the loop. |
| `transfer_zone` | What plays in a zone, with its queue, to another zone. |
| `get_image` | An image of Roon, such as a cover, from an `image_key` given by the other tools. |

Zones are named by their id or their name.

## Setting up

    cabal build roon-mcp
    roon-mcp register

`roon-mcp register` connects to the Roon Server and waits until you enable
"roon-mcp" in Roon, under Settings, Extensions. It does so once: the token
it receives is kept in `$XDG_STATE_HOME/roon-api/`.

Then declare the server to the MCP client, for instance Claude Code:

    claude mcp add roon -- /path/to/roon-mcp

`ROON_HOST` and `ROON_PORT` choose the server. Without `ROON_HOST`,
roon-mcp looks for one on the local network when it starts, by SOOD;
`ROON_CORE`, a name or a unique id, chooses among several. If none answers,
it uses `127.0.0.1:9330`. `ROON_TRACE=1` describes every frame exchanged with Roon
on stderr, which MCP leaves free for logs.

## How it works

Each call opens a connection to Roon, does its work and closes it, so the
server keeps no state. Roon keeps one connection per extension, so calls
take their connections one at a time. A candidate id carries the browsing
session where Roon keeps the result of its search, so that searching and
queueing can be two calls.

## Building

roon-mcp depends on roon-api, effectful-mcp, effectful-streamly,
effectful-streamly-http,
effectful-websockets, streamly-http, streamly-network-udp and
streamly-websockets, not yet on Hackage. `cabal.project` fetches them
from GitHub, at their releases. To build against copies checked out next to
this repository instead, pass `--project-file=cabal.project.siblings`.

Tested with GHC 9.12.4 and Roon Server 2.75.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
