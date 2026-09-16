# HTTP Streaming Sample (browser-interactive)

A small Indy Direct server (port 8080) that demonstrates the explicit
socket-level streaming writers behind a **browser UI** (`www/index.html`):

- **SSE** (`TMVCSSEWriter`, `text/event-stream`) — `/api/chat` streams text token
  by token; `/api/progress` drives a live progress bar. Consumed in the page
  with the browser `EventSource` API.
- **JSON Lines** (`TMVCJSONLWriter`, `application/x-ndjson`) — `/api/people`
  fills a table row by row as the lines arrive.
- **CSV** (`TMVCCSVWriter`, `text/csv`) — `/api/people-csv` streams rows.

Open `http://localhost:8080/static/index.html` to see all four demos run live
in the browser.

## Relation to the other streaming material

This sample is the **interactive / browser-facing** angle. For a single
reference that shows *every* incremental streaming mechanism side by side
(JSON-array writer over four sources, SSE, JSONL, CSV, the declarative
`function: TMVCStreamedResponse` chunked path, and the buffered
`function: TDataSet` contrast), with `curl` commands, see:

- **`samples/streamed_array_writer/`** — the comprehensive showcase.
- **`docs/incremental-streaming.md`** — the full guide: when to use each
  mechanism, how to activate it, framing, backends and disconnection handling.
