#!/bin/bash

# Anahata Android TV Streamer
# Streams the screen to a local network URL that can be opened on a TV.

PORT=8080
IP=$(hostname -I | awk '{print $1}')

echo "Starting Android TV Stream..."
echo "Open this URL on your TV (VLC or Browser): http://$IP:$PORT"

# This creates a simple MPEG-TS stream over HTTP
ffmpeg -f x11grab -video_size 1920x1080 -framerate 30 -i :0.0 \
       -f pulse -i default \
       -c:v libx264 -preset ultrafast -tune zerolatency -b:v 2000k \
       -f mpegts "http://0.0.0.0:$PORT"
