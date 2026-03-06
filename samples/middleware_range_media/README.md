# Range Media Middleware Sample

DMVCFramework sample demonstrating HTTP Range request support (RFC 7233) for
serving audio and video files with seeking capability in HTML5 media elements.

## What this sample shows

- Configuring the Range Media middleware via `UseRangeMediaMiddleware`
- Serving media files with `206 Partial Content` responses
- HTML5 `<audio>` and `<video>` elements with seeking support
- Directory traversal protection

## Prerequisites

- Delphi 12+
- DMVCFramework (latest)

## Setup

1. **Place media files** in the `media/` folder (e.g. `sample.mp3`, `sample.mp4`).

2. **Build and run** the project in Delphi.

3. **Open** `http://localhost:8080` in your browser.

## How it works

The `TMVCRangeMediaMiddleware` intercepts GET requests matching a URL prefix
(`/media` in this sample) and serves the corresponding files from a document root
with full HTTP Range support:

- **No Range header** -> `200 OK` with the complete file
- **Valid Range header** -> `206 Partial Content` with the requested byte range
- **Invalid range** -> `416 Range Not Satisfiable`

This is essential for HTML5 `<audio>` and `<video>` elements, which use Range
requests to enable seeking without downloading the entire file.

## Test with curl

```bash
# Full file
curl -v http://localhost:8080/media/sample.mp3

# First 64 KB (partial content)
curl -v -H "Range: bytes=0-65535" http://localhost:8080/media/sample.mp3

# Last 1000 bytes
curl -v -H "Range: bytes=-1000" http://localhost:8080/media/sample.mp3
```

## Endpoints

| Path | Description |
|------|-------------|
| `GET /` | Media player page with audio/video elements |
| `GET /media/<file>` | Serves media files with Range support |

## Notes

- Only single-range requests are supported (no multipart ranges).
- The middleware resolves paths against the document root and blocks directory
  traversal attempts.
- Supported MIME types include MP3, MP4, OGG, OPUS, WAV, FLAC, AAC, WebM, MKV,
  AVI, and MOV. Unknown extensions are served as `application/octet-stream`.
