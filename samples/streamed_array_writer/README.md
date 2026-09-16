# Streamed Array Writer — Comprehensive Streaming Showcase

Single-file reference for every incremental (socket-level) streaming mechanism
in DelphiMVCFramework. Nine endpoints, three wire-format families plus two
declarative paths, all driven from the same SQLite `people` table (200k rows)
or from an in-memory / file source.

## Endpoints at a glance

| Route | Mechanism | Action shape | Wire format | Content-Length | Notes |
|---|---|---|---|---|---|
| `GET /stream/dataset` | `TMVCJSONArrayWriter` | `procedure` | `application/json` | no | DB cursor (raw column names), row by row |
| `GET /stream/dbobjects` | `TMVCJSONArrayWriter` | `procedure` | `application/json` | no | DB cursor → `TPerson`, serialized with object rules |
| `GET /stream/objectlist` | `TMVCJSONArrayWriter` | `procedure` | `application/json` | no | In-memory `TObjectList<TPerson>` |
| `GET /stream/enumeration` | `TMVCJSONArrayWriter` | `procedure` | `application/json` | no | Lazy `TEnumerable<TPerson>` backed by a file |
| `GET /stream/sse` | `TMVCSSEWriter` | `procedure` | `text/event-stream` | no | One named `person` event per row; final `done` event |
| `GET /stream/jsonl` | `TMVCJSONLWriter` | `procedure` | `application/x-ndjson` | no | One JSON object per line (NDJSON / JSON Lines) |
| `GET /stream/csv` | `TMVCCSVWriter` | `procedure` | `text/csv` | no | Header from `TPerson` RTTI; one CSV row per DB row |
| `GET /stream/datasetfunc` | framework (serializer) | `function: TDataSet` | `application/json` | **yes** | Zero streaming code; full JSON buffered in memory |
| `GET /stream/datasetstreamed` | framework (chunked) | `function: TMVCStreamedResponse` | `application/json` | no | Declarative + chunked; flat framework RAM |

All streaming-writer endpoints have no `Content-Length` because the total size
is not known before streaming starts. `/stream/datasetfunc` is the contrast
case: the framework serializes the whole result into a memory buffer, so it can
set `Content-Length` before writing the response.

## TMVCJSONArrayWriter — source-agnostic

The four `/stream/{dataset,dbobjects,objectlist,enumeration}` rows all use the
same `TMVCJSONArrayWriter`. `Send(const AJSONValue: string)` takes any complete
JSON value and doesn't care where it came from, so the same writer drives:

- a forward-only FireDAC cursor (raw record or hydrated object)
- a `TObjectList<T>` already in memory
- a `TEnumerable<T>` that reads a file line by line (never loaded in full)

The point: the wire format and the push loop are independent of the source.
Swap the source (DB cursor, file, network stream) without touching the writer
or the serializer call.

## Run

```
StreamedArrayWriterSample.exe
```

On first launch it creates `people.db` (SQLite, 200k rows) and
`people_feed.csv` (200k lines, feed for the enumeration demo), then listens
on **http://localhost:8991** (Indy Direct).

```bash
# JSON array — no Content-Length proves the body is streamed
curl -s -D - http://localhost:8991/stream/dataset       -o out.json
curl -s -D - http://localhost:8991/stream/dbobjects     -o out.json
curl -s -D - http://localhost:8991/stream/objectlist    -o out.json
curl -s -D - http://localhost:8991/stream/enumeration   -o out.json

# Server-Sent Events — text/event-stream, one event per row
curl -s -D - http://localhost:8991/stream/sse           -o out.txt

# JSON Lines — one JSON object per line
curl -s -D - http://localhost:8991/stream/jsonl         -o out.ndjson

# CSV — header + one row per person
curl -s -D - http://localhost:8991/stream/csv           -o out.csv

# buffered: this one HAS Content-Length
curl -s -D - http://localhost:8991/stream/datasetfunc   -o out.json

# declarative + chunked: no Content-Length, flat framework RAM
curl -s -D - http://localhost:8991/stream/datasetstreamed -o out.json
```

## Explicit writer vs. returning a TDataSet vs. TMVCStreamedResponse

| | `function: TDataSet` | `function: TMVCStreamedResponse` | `TMVCJSONArrayWriter` / SSE / JSONL / CSV |
|---|---|---|---|
| Who serializes | the framework | the framework | you, one value per `Send()` |
| JSON DOM built | no (rows → bytes) | no | no |
| DB-side RAM | bounded (unidirectional cursor) | bounded | bounded |
| **Framework-side RAM** | **whole JSON payload** | **one record** | **one record** |
| `Content-Length` | **yes** | no | no |
| Backends | all (WebBroker / Indy / HTTP.sys) | Indy Direct + HTTP.sys | Indy-based only |
| Code to write | zero | zero | you drive the loop |
| Wire format | JSON array | JSON array | any (JSON array / SSE / JSONL / CSV) |

`/stream/datasetfunc` (return `TDataSet`) is "streaming **serialization** into a
buffer" — the serializer never builds a DOM, but the complete JSON payload does
exist in a `TMemoryStream` before the first byte is sent. Right choice for
moderate result sets: works on all backends, adds `Content-Length`, needs no
code. Use an explicit writer when you need flat **framework**-side RAM (very
large or unbounded results), a non-JSON wire format (SSE, CSV, NDJSON), or a
source that is not a single dataset.

`/stream/datasetstreamed` (return `TMVCStreamedResponse`) gives you declarative
syntax with true socket streaming: the framework writes chunked transfer
directly to the Indy socket, keeps framework RAM flat, and requires no loop
code. It works on Indy Direct and HTTP.sys but not on WebBroker. The trade-off
vs. the explicit `TMVCJSONArrayWriter`: you cannot add per-row logic (joins,
filtering, mapping to domain objects) — you just return a dataset.

## Further reading

See `docs/incremental-streaming.md` for the full guide covering writer
internals, backpressure, disconnection handling and HTTP.sys differences.

## Requirements

All streaming-writer endpoints take over the raw Indy socket, so they require
an **Indy-based backend** (Indy Direct or WebBroker hosted by
`TIdHTTPWebBrokerBridge`). They cannot stream over an HTTP.sys socket.

`/stream/datasetfunc` and `/stream/datasetstreamed` go through the normal
response path (`datasetfunc` on all backends; `datasetstreamed` on Indy and
HTTP.sys but not WebBroker).
