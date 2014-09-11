This is for those times where you have subtitles in subrip format (.srt) and they are almost okay, but they are just slightly screwed up, and you'd like to fix them.

> subripmod --help

move all subtitles in srt file backwards 90 seconds, then take all the subtitles and slow them down to 96% speed, relative to the video.
> subripmod -i bad.srt -o good.srt --offset -90000 --speedup 0.966
