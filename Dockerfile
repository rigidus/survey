FROM fukamachi/sbcl:2.3.2-debian
ARG DISPLAY
ENV DISPLAY $DISPLAY
RUN apt update && apt install -y tesseract-ocr tesseract-ocr-rus tesseract-ocr-eng ffmpeg libpng-dev gcc && rm -rf /var/lib/apt/lists/*
WORKDIR /survey
COPY . .
RUN sbcl --load run.lisp --non-interactive --eval "(save-lisp-and-die \"core\" :toplevel #'core-main :executable t)"
ENTRYPOINT ["./core"]
