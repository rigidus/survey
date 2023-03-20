tesseract bw.png test2 --psm 6 -l rus+eng lstmbox
tesseract bw.png test -l rus+eng
ffmpeg \
  -framerate 60 \
  -pattern_type glob \
  -i 'tmp.*.png' \
  -r 15 \
  -vf scale=512:-1 \
  out.gif \
;
