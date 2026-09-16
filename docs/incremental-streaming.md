# Incremental (socket-level) streaming in DelphiMVCFramework

This document lists **every** way to stream a response body to the client
incrementally — bytes leaving the server as they are produced, with **flat
server RAM** (the full payload is never held in memory) — and exactly how to
**activate** each one.

There are two activation styles:

- **A. Imperative** — you open an explicit *writer* inside a `procedure` action
  and push to the socket yourself.
- **B. Declarative** — you write a normal `function` action that **returns a
  specific type** (`TMVCStreamedResponse`) and the framework streams it for you.

A third style (**C**) is included only as a contrast: returning a `TDataSet` /
`TObjectList` is serialized with no DOM but **buffered framework-side** and sent
with `Content-Length` — it is *not* socket-incremental.

> **Backend note.** Socket-level streaming requires a backend that exposes the
> raw connection. The explicit writers (A) require an **Indy-based** backend
> (Indy Direct, or WebBroker hosted by `TIdHTTPWebBrokerBridge`). The
> declarative `TMVCStreamedResponse` (B) works on **Indy Direct** (chunked +
> keep-alive) and **HTTP.sys** (close-delimited); on WebBroker it raises HTTP
> 501 *before any byte is sent*.

---

## A. Imperative — explicit socket writers

**Unit:** `MVCFramework.SSE.Writer`
**Action shape:** `procedure` (this is the one sanctioned exception to the
"controller actions are always `function`" rule — the writer takes over the
socket, so there is nothing to return).

### Shared mechanics (all four writers)

1. Construct the writer with the controller `Context`:
   `lW := TMVCxxxWriter.Create(Context);`
   The constructor immediately writes the HTTP status line + headers to the
   socket (`Connection: close`, `Cache-Control: no-cache`, **no
   `Content-Length`**) and sets `Context.Response.StreamingHandled := True` so
   the engine does not append a second response after the action returns.
2. Call `Send(...)` once per element/event. Each call writes **straight to the
   socket** — only the current element lives in memory.
3. Poll `Connected: Boolean` to detect a client that went away and stop early.
4. `Free` the writer (the destructor disconnects the TCP socket, giving the
   client a clean EOF). `TMVCJSONArrayWriter` also exposes `Close` to emit the
   closing `]` before the object goes out of scope.
5. **Source-agnostic:** you decide how each element is serialized —
   `Serializer.SerializeDataSetRecord(ds)`, `Serializer.SerializeObject(obj)`,
   or any raw string.

If the backend is not Indy-based, the writer constructor raises
`EMVCException` (HTTP 500) before anything is sent.

### A.1 `TMVCSSEWriter` — Server-Sent Events (`text/event-stream`)

For browser `EventSource` / live event feeds (chat tokens, progress, notifications).

```pascal
[MVCPath('/sse/clock')]
[MVCHTTPMethod([httpGET])]
[MVCProduces('text/event-stream')]
procedure Clock;
// ...
procedure TMyController.Clock;
var
  lSSE: TMVCSSEWriter;
  I: Integer;
begin
  lSSE := TMVCSSEWriter.Create(Context);   // optional: (Context, charset, retryMS)
  try
    for I := 1 to 10 do
    begin
      if not lSSE.Connected then Break;
      lSSE.Send('tick', FormatDateTime('hh:nn:ss', Now), IntToStr(I)); // event,data,id
      Sleep(1000);
    end;
    lSSE.Send('done', '');
  finally
    lSSE.Free;
  end;
end;
```
API: `Send(AEvent, AData: string; AId: string='')`, `Send(AData: string)` (an
unnamed `message` event), `SendComment(AText)` (heartbeat, ignored by the client).

### A.2 `TMVCJSONLWriter` — JSON Lines / NDJSON (`application/x-ndjson`)

One complete JSON document per line. Ideal for log/event tails and `fetch()`
readers that parse line-by-line.

```pascal
lJSONL := TMVCJSONLWriter.Create(Context);
try
  while not ds.Eof do
  begin
    if not lJSONL.Connected then Break;
    lJSONL.Send(Serializer.SerializeDataSetRecord(ds)); // newline appended for you
    ds.Next;
  end;
finally
  lJSONL.Free;
end;
```
API: `Send(AJSONLine: string)`.

### A.3 `TMVCJSONArrayWriter` — one JSON array, element by element (`application/json`)

Emits a single, valid JSON array (`[` … `,` … `]`) so an ordinary JSON client
receives one normal array, while the server holds only the current element.

```pascal
lW := TMVCJSONArrayWriter.Create(Context);
try
  while not ds.Eof do
  begin
    if not lW.Connected then Break;
    lW.Send(Serializer.SerializeDataSetRecord(ds)); // commas inserted automatically
    ds.Next;
  end;
finally
  lW.Free;   // emits the closing "]" (or call lW.Close explicitly)
end;
```
API: `Send(AJSONValue: string)` (any complete JSON value), `Close`.

### A.4 `TMVCCSVWriter` — CSV rows (`text/csv`)

Header row emitted on the first `Send`; columns derived from the object's RTTI
(or a `TClass`/settings passed to the constructor).

```pascal
lW := TMVCCSVWriter.Create(Context, TPerson); // header from TPerson's properties
try
  while not ds.Eof do
  begin
    if not lW.Connected then Break;
    lW.Send(PersonFromRow(ds)); // one TObject per row
    ds.Next;
  end;
finally
  lW.Free;
end;
```
API: `Send(AObject: TObject)`, `IgnoredAttributes` (set before the first `Send`),
plus constructor overloads for a fixed class and `TMVCCSVSerializerSettings`.

**When to use A:** you need a specific wire format (SSE/NDJSON/CSV), or the
source is not a single dataset (merged sources, generators, per-element logic),
or you want explicit control of the loop. Indy-based backends only.

---

## B. Declarative — `function` returning `TMVCStreamedResponse`

**Units:** helper on `TMVCController` (in `MVCFramework`); concrete wrapper in
`MVCFramework.Serializer.Streaming.DataSet`.
**Action shape:** ordinary `function` — no socket code.

Return the result of the inherited **`StreamDataSet`** helper:

```pascal
uses MVCFramework.Serializer.Streaming.DataSet; // only if you reference the type name

[MVCPath('/customers')]
[MVCHTTPMethod([httpGET])]
function GetCustomers: TMVCStreamedResponse;
begin
  Result := StreamDataSet(
    TMVCActiveRecord.SelectUnidirectionalDataSet('SELECT * FROM customers ORDER BY id', []),
    TMVCNameCase.ncLowerCase);   // ANameCase, AOwnsDataSet=True, AIgnoredFields=nil
end;
```

Helper signature (inherited from `TMVCRenderer`, callable unqualified in any action):

```pascal
function StreamDataSet(const ADataSet: TDataSet;
  const ANameCase: TMVCNameCase = ncLowerCase;
  const AOwnsDataSet: Boolean = True;
  const AIgnoredFields: TMVCIgnoredList = nil): TMVCStreamedResponse;
```

What happens:

- The engine recognizes the returned `TMVCStreamedResponse` and renders it via
  the active backend's chunk writer (`IMVCChunkedResponseWriter` from the
  response). It produces **one JSON array** (`application/json`), **no
  `Content-Length`**.
- Serialization is **record-by-record** into a fixed 64 KB buffer flushed as it
  fills → **flat server RAM** regardless of row count.
- Iteration is **forward-only** (`while not Eof do … Next`, no `First`, no
  bookmarks), so a unidirectional/forward-only FireDAC cursor works (use
  `SelectUnidirectionalDataSet`, or a `TFDQuery` with
  `FetchOptions.Mode := fmOnDemand; FetchOptions.Unidirectional := True`).
- The wrapper **owns and frees the dataset** after rendering (`AOwnsDataSet`
  default `True`). The request-scoped connection (e.g. from
  `TMVCActiveRecordMiddleware`) stays alive for the whole stream.
- **Per backend:**
  - **Indy Direct** → `Transfer-Encoding: chunked` + **keep-alive**.
  - **HTTP.sys** → incremental body **close-delimited** (connection closed at
    the end; no keep-alive). Consumable by every standard client.
  - **WebBroker / unsupported** → `CreateChunkedWriter` raises **HTTP 501
    before any byte**, handled by the normal error pipeline.
- **Error handling:** a failure *before* the first byte propagates as a normal
  error response. A failure *mid-stream* (headers already sent) aborts the
  stream, leaves the body truncated, logs via `LogE`, and lets the connection
  close — no second/500 response is spliced on.

**When to use B:** you have a (large/forward-only) dataset and want a one-liner,
declarative action with flat RAM and broad backend support (Indy + HTTP.sys),
without writing any socket code.

> `TMVCStreamedResponse` is an abstract base; `TMVCStreamedDataSet` is the only
> concrete subclass today. New streamed sources (lists, CSV, …) can subclass it
> later without touching the engine.

---

## C. (Contrast) Returning `TDataSet` / `TObjectList` — *not* socket-incremental

```pascal
function GetCustomers: TDataSet;            // or TObjectList<TCustomer>, or OKResponse(ds)
begin
  Result := TMVCActiveRecord.SelectUnidirectionalDataSet('SELECT * FROM customers', []);
end;
```

The framework serializes this with `TMVCStreamingJsonSerializer` — **no JSON
DOM, no intermediate string** — but **into a `TMemoryStream`**, then sends it
with a **`Content-Length`**. So:

- **DB-side** RAM is bounded if the cursor is forward-only/unidirectional;
- **framework-side** RAM equals the **whole JSON payload** (it is buffered to
  measure it);
- works on **every** backend (WebBroker / Indy / HTTP.sys), zero code.

Use this for small/moderate result sets where a `Content-Length` and universal
backend support matter more than flat framework-side RAM. For very large or
unbounded results, use **A** or **B**.

---

## Decision matrix

| Goal | Activate | Action | Format | Content-Length | Flat framework RAM | Backends |
|------|----------|--------|--------|----------------|--------------------|----------|
| Browser live events | `TMVCSSEWriter` | `procedure` | SSE | no (close) | yes | Indy-based |
| Line-delimited stream | `TMVCJSONLWriter` | `procedure` | NDJSON | no (close) | yes | Indy-based |
| One big JSON array, custom loop | `TMVCJSONArrayWriter` | `procedure` | JSON array | no (close) | yes | Indy-based |
| CSV download | `TMVCCSVWriter` | `procedure` | CSV | no (close) | yes | Indy-based |
| Large dataset, declarative | `StreamDataSet → TMVCStreamedResponse` | `function` | JSON array | no (chunked/close) | yes | Indy Direct + HTTP.sys |
| Small/moderate dataset, any backend | return `TDataSet`/`TObjectList` | `function` | JSON | **yes** | no (payload buffered) | all |

**Rules of thumb**
- Need a specific wire format or a non-dataset source → **A** (explicit writer).
- Have a dataset and want a declarative one-liner with flat RAM on Indy/HTTP.sys → **B**.
- Small result set, want `Content-Length` and every backend → **C**.

---

## Reference

| Unit | Provides |
|------|----------|
| `MVCFramework.SSE.Writer` | `TMVCSSEWriter`, `TMVCJSONLWriter`, `TMVCJSONArrayWriter`, `TMVCCSVWriter` (base `TMVCStreamWriter`) |
| `MVCFramework` | `IMVCChunkedResponseWriter`, `TMVCStreamedResponse`, `TMVCController.StreamDataSet`, `TMVCWebResponse.CreateChunkedWriter` |
| `MVCFramework.Serializer.Streaming.DataSet` | `TMVCStreamedDataSet` (forward-only, flat-RAM record-by-record) |
| `MVCFramework.Serializer.Streaming` | `TMVCStreamingJsonSerializer` (used by style C; no DOM, buffered) |

Working sample: `samples/streamed_array_writer/` — endpoints for every style
above side by side, with `curl` commands showing the presence/absence of
`Content-Length`.
