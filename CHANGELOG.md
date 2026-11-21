# Revision history for roon-mcp

## 0.2.0.3 -- 2026-11-21

* The server also answers the clients of the earlier MCP revisions, such
  as the Claude desktop application (effectful-mcp 0.1.1.0).

## 0.2.0.2 -- 2026-11-21

* Built on roon-api 0.2.0.2.

## 0.2.0.1 -- 2026-11-21

* `play_item` plays an album found by a search (fixed in roon-api
  0.2.0.1).

## 0.2.0.0 -- 2026-11-21

* `browse_library` lists and searches the library; `play_item` plays what
  it found, whole albums and playlists included.
* `set_volume`, `seek`, `set_playback_options` and `transfer_zone`.
* `list_zones` gives the outputs with their volume and the playback
  options of each zone. Its `outputs` are now objects, no longer names.
* The server gives Roon and MCP clients the version of this package.
* Built on roon-api 0.2.

## 0.1.0.0 -- 2026-11-21

* MCP server on stdio with the tools `list_zones`, `get_queue`,
  `search_tracks`, `queue_tracks` and `control`.
* `get_image` returns an image of Roon, such as a cover, from the
  `image_key` that the other tools give.
* `roon-mcp register` to enable the extension once.
* `ROON_TRACE` describes every frame exchanged with Roon on stderr.
* Without `ROON_HOST`, the Roon Server is discovered on the local network
  at start; `ROON_CORE` chooses among several.
