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

docker build -t core --build-arg DISPLAY  .

xauth extract - $DISPLAY > ~/.Xauthority

docker run  -it --network=host --env DISPLAY=$DISPLAY  --privileged  --volume="$HOME/.Xauthority:/root/.Xauthority:rw"  -v /tmp/.X11-unix:/tmp/.X11-unix --rm -v ${HOME}/out:/survey/out core
