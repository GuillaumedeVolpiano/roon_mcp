# Revision history for roon-mcp

## 0.1.0.0 -- unreleased

* MCP server on stdio with the tools `list_zones`, `get_queue`,
  `search_tracks`, `queue_tracks` and `control`.
* `get_image` returns an image of Roon, such as a cover, from the
  `image_key` that the other tools give.
* `roon-mcp register` to enable the extension once.
* `ROON_TRACE` describes every frame exchanged with Roon on stderr.
* Without `ROON_HOST`, the Roon Server is discovered on the local network
  at start; `ROON_CORE` chooses among several.
