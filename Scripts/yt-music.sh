#!/bin/bash

url="https://www.youtube.com/playlist?list=PLZGNjNozIky0F5FyR_aVK4uChqYZr3oQD"
directory="$HOME/Music/%(title)s.%(ext)s"
format="wav"
quality=0

youtube-dl -o $directory -x --audio-format $format --audio-quality $quality --prefer-ffmpeg $url 
