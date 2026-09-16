# Range Media Middleware Sample

DMVCFramework sample demonstrating HTTP Range request support (RFC 7233) for
serving audio and video files with seeking capability in HTML5 media elements.

## What this sample shows

- Configuring `UseRangeMediaMiddleware` with a URL prefix and a document root
- `206 Partial Content` responses with correct `Content-Range` headers
- HTML5 `<audio>` and `<video>` with real seeking (not a full-download workaround)
- An interactive **Range Request Inspector** in the browser UI
- Directory traversal protection

## Prerequisites

- Delphi 12+
- DMVCFramework (latest)
- curl (built into Windows 10 / 11; available on Linux / macOS by default)

## Setup

### 1. Download sample media files

Run the provided script from the project directory:

```bat
# Windows (curl is built into Windows 10 / 11)
get_samples.bat

# Linux / macOS
bash get_samples.sh
```

The scripts download two files into the `media/` folder using `curl`:

| File | Source | License |
|------|--------|---------|
| `media/sample.ogg` | Beethoven — Moonlight Sonata sequenced (Wikimedia Commons) | Public Domain |
| `media/sample.mp4` | Big Buck Bunny 320x180 — © Blender Foundation | CC BY 3.0 |

### 2. Build and run

Open `RangeMediaSample.dproj` in Delphi, build, and run.

### 3. Open the browser

Navigate to `http://localhost:8080`.

The page includes:
- Audio player for `sample.ogg`
- Video player for `sample.mp4`
- **Range Request Inspector** — fire 200, 206, 416, and 404 requests live
  and inspect the response headers directly in the browser

## How it works

`TMVCRangeMediaMiddleware` intercepts `GET` requests matching a URL prefix
and serves the corresponding files from a document root with full HTTP Range
support:

| Request | Response |
|---------|----------|
| No `Range` header | `200 OK` — full file |
| `Range: bytes=0-65535` | `206 Partial Content` + `Content-Range` |
| `Range: bytes=-1024` | `206` — last 1024 bytes (suffix range) |
| `Range: bytes=5000-` | `206` — from byte 5000 to EOF (open-ended) |
| Invalid range | `416 Range Not Satisfiable` |
| File not found | `404 Not Found` |

### Middleware registration

```pascal
FEngine.AddMiddleware(
  UseRangeMediaMiddleware('/media', 'media')
  //                      ^         ^
  //                      URL       folder relative to exe
  //                      prefix
);
```

## Test with curl

```bash
# Full file
curl -I http://localhost:8080/media/sample.ogg

# First 64 KB
curl -v -H "Range: bytes=0-65535" http://localhost:8080/media/sample.ogg

# Last 1 KB (suffix range)
curl -v -H "Range: bytes=-1024" http://localhost:8080/media/sample.ogg

# Open-ended range (from byte 50000 to EOF)
curl -v -H "Range: bytes=50000-" http://localhost:8080/media/sample.ogg

# Invalid range -> 416
curl -v -H "Range: bytes=9999999-" http://localhost:8080/media/sample.ogg
```

## Notes

- Only **single-range** requests are supported (no multipart/byteranges).
- Directory traversal is blocked — only files inside the configured document
  root are accessible.
- Supported MIME types: MP3, MP4, M4A, OGG, OPUS, WAV, FLAC, AAC, WebM,
  MKV, AVI, MOV. Unknown extensions are served as `application/octet-stream`.
- `Content-Encoding: identity` is set to prevent compression middleware from
  breaking `Content-Range` semantics.
