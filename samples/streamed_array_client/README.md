# Streamed Array Client

Console client for the streaming endpoints of `samples/streamed_array_writer`.
It shows what the client side gains from a response that is sent one item at a
time: the same 200,000 people are read once as a JSON array and once as JSON
Lines (`application/x-ndjson`), and the peak memory of the process is printed.

| Mode | How the body is handled | Peak memory (200,000 rows) |
|---|---|---|
| `ndjson` | each line is parsed and dropped while it arrives | about 11 MB |
| `array` | the document must be complete before it can be parsed | about 64 MB |

Both modes read the same data and print the same checksum (the sum of the ages).
Figures measured on a Win32 debug build over the loopback; yours will differ,
the ratio is the point.

## Run

```
StreamedArrayWriterSample.exe          (in samples\streamed_array_writer)

StreamClient.exe ndjson http://localhost:8991/stream/items
StreamClient.exe array  http://localhost:8991/stream/items
StreamClient.exe ndjson http://localhost:8991/stream/itemsfile
```

Run each mode in its own process: the peak memory is a property of the process.

## How it works

`THTTPClient.Get(url, stream)` writes the body into the stream while it
arrives. `TLineStream` is a write-only `TStream` that cuts the bytes into lines,
parses each complete line with `TJsonObject.Parse`, and keeps only the
unfinished tail. `TMVCRESTClient` is not used here on purpose: it buffers the
whole response before returning it.
