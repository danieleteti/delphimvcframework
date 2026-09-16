#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# get_samples.sh
# Downloads royalty-free sample media files for the Range Media Middleware demo.
#
# Output:
#   media/sample.mp3  - Beethoven: Moonlight Sonata (Wikimedia Commons, Public Domain)
#   media/sample.mp4  - Big Buck Bunny 320x180, CC BY 3.0, (c) Blender Foundation
#
# Requirements: curl (usually pre-installed on macOS and most Linux distros)
# ---------------------------------------------------------------------------

set -euo pipefail

if ! command -v curl &>/dev/null; then
  echo "ERROR: curl not found."
  echo ""
  echo "  Ubuntu/Debian:  sudo apt install curl"
  echo "  macOS:          curl is pre-installed; if missing: brew install curl"
  exit 1
fi

mkdir -p media

echo "Downloading media/sample.ogg ..."
echo "  Beethoven: Moonlight Sonata - Wikimedia Commons (Public Domain)"
curl -L --progress-bar -o media/sample.ogg \
  "https://upload.wikimedia.org/wikipedia/commons/9/96/Beethoven_Moonlight_sonata_sequenced.ogg"

echo ""
echo "Downloading media/sample.mp4 ..."
echo "  Big Buck Bunny 320x180, CC BY 3.0, (c) Blender Foundation"
curl -L --progress-bar -o media/sample.mp4 \
  "https://download.blender.org/peach/bigbuckbunny_movies/BigBuckBunny_320x180.mp4"

echo ""
echo "Done.  Files saved in the media/ folder."
echo ""
echo "  Attribution:"
echo "    audio: Beethoven - Moonlight Sonata, Wikimedia Commons (Public Domain)"
echo "           https://commons.wikimedia.org/wiki/File:Beethoven_Moonlight_Sonata.ogg"
echo "    video: Big Buck Bunny, CC BY 3.0, (c) copyright 2008 Blender Foundation"
echo "           https://www.bigbuckbunny.org"
echo ""
echo "Open http://localhost:8080 after starting the server."
