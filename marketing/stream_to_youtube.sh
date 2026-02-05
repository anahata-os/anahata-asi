#!/bin/bash

# Anahata Live Streamer
# Streams the NetBeans window to YouTube using ffmpeg.

# --- CONFIGURATION ---
# Replace with your actual YouTube Stream Key
STREAM_KEY="YOUR_YOUTUBE_STREAM_KEY"
RTMP_URL="rtmp://a.rtmp.youtube.com/live2/$STREAM_KEY"

# Resolution and Frame Rate
RES="1920x1080"
FPS="30"

# Audio Device (PulseAudio default)
AUDIO_DEVICE="default"

echo "Starting Anahata Live Stream..."
echo "Target: $RTMP_URL"

# ffmpeg command:
# -f x11grab: Capture X11 display
# -s: Set resolution
# -i :0.0: Display 0, screen 0
# -f pulse: Capture audio from PulseAudio
# -c:v libx264: Use H.264 video codec
# -preset veryfast: Balance between CPU usage and quality
# -b:v 3000k: Video bitrate (3 Mbps)
# -pix_fmt yuv420p: Required for most streaming services
# -g 60: Keyframe interval (2 seconds at 30 fps)
# -c:a aac: Use AAC audio codec
# -f flv: Output format for RTMP

ffmpeg -f x11grab -video_size $RES -framerate $FPS -i :0.0 \
       -f pulse -i $AUDIO_DEVICE \
       -c:v libx264 -preset veryfast -b:v 3000k -maxrate 3000k -bufsize 6000k \
       -pix_fmt yuv420p -g 60 \
       -c:a aac -b:a 128k \
       -f flv "$RTMP_URL"
