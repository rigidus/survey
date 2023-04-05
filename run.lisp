(ql:quickload "bordeaux-threads")
(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png")
(ql:quickload "anaphora")
(use-package :anaphora)
(ql:quickload "cl-tesseract")
(ql:quickload :cl-ppcre)
(ql:quickload "alexandria")

(defparameter *OUTLOCK*
  (bt:make-recursive-lock "output-lock"))

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defmacro dbg (msg &rest params)
  "debug output with lock"
  `(bt:with-recursive-lock-held (*OUTLOCK*)
     (format t ,msg ,@params)
     (finish-output)))

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (xlib:display-force-output ,display)))
       (xlib:close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (xlib:display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (xlib:screen-root ,screen)))
         ,@body))))

(defun x-size ()
  (with-default-screen (s)
    (values
     (xlib:screen-width s)
     (xlib:screen-height s))))

(defparameter *default-x* 0)
(defparameter *default-y* 0)
(defparameter *default-width* 800)
(defparameter *default-height* 600)

(defun init-defaults ()
  (multiple-value-bind (width height)
      (x-size)
    (setf *default-width* width
          *default-height* height
          *default-x* 0
          *default-y* 0)))

;; (init-defaults)

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(defun x-snapshot (&key (x *default-x*) (y *default-y*)
                     (width  *default-width*) (height *default-height*)
                     path)
  ;; "Return RGB data array (The dimensions correspond to the height, width,
  ;; and pixel components, see comments in x-snapsearch for more details),
  ;; or write to file (PNG only), depend on if you provide the path keyword"
  (with-default-window (w)
    (let ((image
           (raw-image->png
            (xlib:get-raw-image w :x x :y y
                                :width width :height height
                                :format :z-pixmap)
            width height)
          ))
      (if path
          (let* ((ext (pathname-type path))
                 (path
                  (if ext
                      path
                      (concatenate 'string path ".png")))
                 (png? (or (null ext) (equal ext "png"))))
            (cond
              (png? (zpng:write-png image path))
              (t (error "Only PNG file is supported"))))
          ;; else - return image array
          (zpng:data-array image)))))

;; (x-snapshot
;;  :path "x-snapshot-true-color-telegram.png")



(defmacro pin (yy xx aa bb cc)
  `(progn
     (setf (aref new ,yy ,xx 0) ,aa)
     (setf (aref new ,yy ,xx 1) ,bb)
     (setf (aref new ,yy ,xx 2) ,cc)))

(declaim (inline copy-png))

(declaim (ftype (function (string string) fixnum)
                copy-png))

(setf *features*
      (remove :packrest *features*))

;; (pushnew :packrest *features*)


(defun conv-8x8 (img)
  (declare (optimize (speed 3))
           (optimize (safety 0)))
  (let* ((width      (the fixnum (logand -8 (png:image-width img))))
         (height     (the fixnum (logand -8 (png:image-height img))))
         (channels   (the fixnum (png:image-channels img)))
         (bit-depth  (the fixnum (png:image-bit-depth img)))
         (packflag   (make-array `(,(ash height -3) ,(ash width -3))
                                 :element-type 'fixnum))
         (packline   (make-array `(,(ash height -3) ,(ash width -3) ,channels)
                                 :element-type '(unsigned-byte 8))))
    ;; packflag - это массив флагов равных нулю, когда в квадрате 8*8 есть только один цвет
    ;; packline - это массив этих цветов в обрабатываемой строке пикселей
    (declare  (type fixnum width)     (type fixnum height)
              (type fixnum channels)  (type fixnum bit-depth))
    (do* ((yy1      0  (+ 1 yy1))
          (yy-8-p   0  (logand yy1 7)) ;; true когда y-координата кратна 8
          (yy3      0  (ash yy1 -3)))  ;; y-индекс в массиве packflag
         ((>= yy1 height))
      (declare (type fixnum yy1) (type fixnum yy-8-p) (type fixnum yy3))
      (do* ((xx1      0  (+ 1 xx1))
            (xx-8-p   0  (logand xx1 7)) ;; true когда x-координата кратна 8
            (xx3      0  (ash xx1 -3)))  ;; x-индекс в массиве packflag
           ((>= xx1 width))
        (declare (type fixnum xx1) (type fixnum xx-8-p) (type fixnum xx3))
        (block internal-loop
          (if (= 0 xx-8-p yy-8-p)
              (progn ;; then - левый верхний угол квадрата 8x8
                ;; (pin yy1 xx1 0 255 0) ;; ~~~~~~~ GREEN
                (setf (aref packflag yy3 xx3) 0) ;; set packflag = 0
                (dotimes (zz channels)           ;; save to packline
                  (setf (aref packline yy3 xx3 zz)
                        (aref img yy1 xx1 zz))))
              (progn ;; else - любая другая точка кроме верхнего левого угла
                ;; (pin yy1 xx1 0 70 0)  ;; ~~~~~~~ DARK-GREEN
                ;; (dotimes (zz channels) ;;~~~~~~~~~~~~~~[:::::::::::::]~~~~~
                ;;   (setf (aref new yy1 xx1 zz)
                ;;         (aref img yy1 xx1 zz)))
                (if (not (= 0 (aref packflag yy3 xx3)))
                    ;; цвет в этом квадрате 8*8 уже менялся -
                    ;; нет смысла проверять опять
                    (progn
                      ;; (pin yy1 xx1 0 0 70)  ;; ~~~~~~~ DARK-BLUE
                      )
                    ;; else - цвет ранее не менялся - имеет смысл проверить сейчас
                    ;; отличается ли цвет текущего обрабатываемого пикселя от эталона
                    (progn
                      (dotimes (zz channels)
                        (if (= (aref packline yy3 xx3 zz)
                               (aref img yy1 xx1 zz))
                            (progn ;; в
                              ;; (pin yy1 xx1 70 0 0)  ;; ~~~~~~~ DARK-RED
                              )
                            ;; else - любой канал не равен -> весь квадрат к черту
                            (progn
                              ;; (pin yy1 xx1 157 157 0) ;; ~~~~~~~ DARK-YELLOW
                              (setf (aref packflag yy3 xx3)
                                    (+ (ash (logand yy1 7) 3) ;; [bits:00yyyxxx]
                                       (logand xx1 7))))))
                      ;; Если мы здесь, то цвет не поменялся после проверки текущего
                      ;; пикселя - тут мы пока не делаем ничего
                      (progn nil)
                      ))))) ;; end of internal-loop
        ) ;; end of do-cycle for x
      )  ;; end of do-cycle for y
    (values
     packflag packline height width channels bit-depth)))

(defmacro light-packline-color ()
  `(logior #b10000000
           (aref packline yy3 xx3 zz)))

(defmacro rgb-times (&body body)
  `(dotimes (zz channels)
     (setf (aref new yy1 xx1 zz)
           ,@body)))

;; (macroexpand-1 '(rgb-times (light-packline-color)))


(defun re-conv-8x8 (packflag packline height width channels bit-depth &optional old)
  (declare (optimize (speed 3))
           (optimize (safety 0)))
  (let* ((new (png:make-image height width channels bit-depth)))
    ;; Восстанавливаем информацию из packflag и packline
    (do* ((yy1      0  (+ 1 yy1))
          (yy-8-p   0  (logand yy1 7))
          (yy3      0  (ash yy1 -3)))
         ((>= yy1 height))
      (declare (type fixnum yy1) (type fixnum yy-8-p) (type fixnum yy3))
      (do* ((xx1      0  (+ 1 xx1))
            (xx-8-p   0  (logand xx1 7))
            (xx3      0  (ash xx1 -3)))
           ((>= xx1 width))
        (declare (type fixnum xx1) (type fixnum xx-8-p) (type fixnum xx3))
        (let* ((packflag (aref packflag yy3 xx3))
               (bb7 (logand #b10000000 packflag))
               (bb6 (logand #b01000000 packflag))
               (b_5 (logand #b00111111 packflag))
               (yy4 (ash b_5 -3))
               (xx4 (logand b_5 #b111))
               (y-chg (+ yy4 (logand yy1 -8)))  ;; Y изменения цвета
               (x-chg (+ xx4 (logand xx1 -8)))) ;; X изменения цвета
          (if (= 0 b_5)
              ;; 8x8-квадрат залит полностью - выведем его цвет с
              ;; дитерингом осветляя каждый второй пиксель, чтобы
              ;; показать что это фон и получить масштабную сетку
              ;; (! дитеринг отключен для скорости !)
              (rgb-times
                (aref packline yy3 xx3 zz) ;; вместо дитеринга
                ;; (if (or (not (= 0 (logand yy1 #b010)))
                ;;         (not (= 0 (logand xx1 #b010))))
                ;;     (aref packline yy3 xx3 zz)
                ;;     ;; else
                ;;     (logand 255 (+ 20 (aref packline yy3 xx3 zz))))
                )
              ;; else: 8x8-квадрат не залит полностью
              (if (and (< yy1 y-chg)
                       (< xx3 x-chg))
                  ;; заливаем первые пиксели цветом из packline
                  (progn ;; filled part
                    (rgb-times
                      (aref packline yy3 xx3 zz)))
                  ;; else: а остальные
                  ;; пропускаем если у нас нет исходного изображения,
                  ;; иначе берем пиксели из исходника, но обнуляем
                  ;; их младшие биты, чтобы цвет остальных пикселей
                  ;; отличался от того что мы взяли из packline для
                  ;; залитого участка квадрата
                  ;; (! отключено для скорости !)
                  (when old
                    (rgb-times
                      (aref old yy1 xx1 zz)
                      ;; (logand 255
                      ;;         ;; we need only 1 bit for channel
                      ;;         (logior #b00011111
                      ;;                 (aref old yy1 xx1 zz)))
                      ))))
          ;; Особые случаи: Если у нас есть флаги bb7 и bb6
          (when (not (= 0 bb7))
            (rgb-times
              (if (and (= 1 zz)
                       (or (= (logand yy1 #b111) (logand xx1 #b111))
                           (= 0 (logand yy1 #b111))
                           (= 0 (logand xx1 #b111))))
                  255
                  ;; else
                  (aref new yy1 xx1 zz)
                  )))
          (when (not (= 0 bb6))
            (rgb-times
              (if (and (= 2 zz)
                       (or (= (logand yy1 #b111) (logand xx1 #b111))
                           (= 0 (logand yy1 #b111))
                           (= 0 (logand xx1 #b111))))
                  255
                  ;; else
                  (aref new yy1 xx1 zz)
                  ))))))
    new))

;; launcher

(defun save-packflag-for-test (filepath par-packflag par-packline y-size x-size channels bit-depth &optional img)
  (let ((new (re-conv-8x8 par-packflag par-packline y-size x-size channels bit-depth img)))
    (with-open-file (output filepath :element-type '(unsigned-byte 8)
                                     :direction :output :if-exists :supersede)
      (png:encode new output))))


;; (let* ((img
;;          (with-open-file
;;              ;; (input "x-snapshot-true-color-telegram.png" :element-type '(unsigned-byte 8))
;;              (input "antalya.png" :element-type '(unsigned-byte 8))
;;            (png:decode input))))
;;   (multiple-value-bind (packflag packline height width channels bit-depth)
;;       (conv-8x8 img)
;;     ;; (save-packflag-for-test
;;     ;; "copy2.png" packflag packline height width channels bit-depth img)))
;;     (let ((new (re-conv-8x8 packflag packline height width channels bit-depth
;;                             img
;;                             )))
;;       (with-open-file
;;           (output "copy3.png"
;;                   :element-type '(unsigned-byte 8)
;;                   :direction :output :if-exists :supersede)
;;         (png:encode new output)))
;;     ))


(defstruct pnt
  (y 0 :type fixnum)
  (x 0 :type fixnum))

(deftype direction ()
  ;; to-right to-up to-left to-down
  '(member :tr :tu :tl :td))

(defstruct edge-pnt
  (y 0 :type fixnum)
  (x 0 :type fixnum)
  (d :tr :type direction))

(defmethod print-object ((obj edge-pnt) stream)
  (format stream "#S(EDGE-PNT :Y ~A :X ~A :D :~A)"
          (edge-pnt-y obj)
          (edge-pnt-x obj)
          (edge-pnt-d obj)))

(defun rotate-left (direction)
  (cond ((equal :tr direction) :tu)
        ((equal :tu direction) :tl)
        ((equal :tl direction) :td)
        ((equal :td direction) :tr)))

(defun rotate-right (direction)
  (cond ((equal :tr direction) :td)
        ((equal :tu direction) :tr)
        ((equal :tl direction) :tu)
        ((equal :td direction) :tl)))

;; Y-координата идет в CUR-PNT первой
;; -1 means to low
;; -2 means to high
;; TODO:inilne
(defun forward-side (cur-pnt direction height width)
  (let ((yy (pnt-y cur-pnt))
        (xx (pnt-x cur-pnt)))
    (destructuring-bind (yyy . xxx)
        (ecase direction
          (:tr (cons yy (+ xx 1)))
          (:tu (cons (- yy 1) xx))
          (:tl (cons yy (- xx 1)))
          (:td (cons (+ yy 1) xx)))
      (when (< yyy 0)       (setf yyy -1))
      (when (< xxx 0)       (setf xxx -1))
      (when (> yyy height)  (setf yyy -2))
      (when (> xxx width)   (setf xxx -2))
      (make-pnt :y yyy :x xxx))))

;; (forward-side-pnt-coords (cons 202 3) :td 200 300)

(defun fg-p (cur-pnt packflag height width)
  (let ((yy (pnt-y cur-pnt))
        (xx (pnt-x cur-pnt)))
    (and (>= yy 0)
         (>= xx 0)
         (< yy height)
         (< xx width)
         (not (= 0 (aref packflag yy xx))))))

(defun left-side (cur-pnt direction height width)
  (forward-side cur-pnt (rotate-left direction) height width))


(defun edge-8x8 (cur-pnt height width packflag)
  ;; packflag not mutable in this functuin
  (let ((entry-point cur-pnt)        ;; сохраняем начальную точку
        (stack '())                  ;; создаем пустой стек
        (direction :tr))       ;; задаем начальное направление
    (labels ((f ()
               (let ((out-edge-pnt
                       (make-edge-pnt
                        :y (pnt-y cur-pnt)
                        :x (pnt-x cur-pnt)
                        :d direction)))
                 (push out-edge-pnt stack))
               (setf cur-pnt (forward-side cur-pnt direction height width))
               (not (equalp cur-pnt entry-point))))
      ;; (format t "direction: ~A ~%" direction)
      (tagbody
       lab_A
         ;; (read-line)
         ;; (format t ":: cur-pnt: ~A ~%" cur-pnt)
         (let* ((left-crd (left-side cur-pnt direction height width))
                (left-val (fg-p left-crd packflag height width)))
           ;; (format t "left: ~A : ~A ~%" left-crd left-val)
           (if left-val
               (progn ;; then fg == left-val
                 ;; (format t ":: left fg detected => rotate-left ~%")
                 (setf direction (rotate-left direction))
                 ;; (format t "direction: ~A ~%" direction)
                 (when (f)
                   (go lab_A)))
               (progn ;; else bg == left-val
                 (let ((rotate-cnt 0))
                   (tagbody
                    lab_R
                      ;; (format t "rotate-cnt: ~A ~%" rotate-cnt)
                      (let* ((forward-crd (forward-side cur-pnt direction height width))
                             (forward-val (fg-p forward-crd packflag height width)))
                        ;; (format t "forward: ~A : ~A ~%" forward-crd forward-val)
                        (if forward-val
                            (when (f)
                              (go lab_A))
                            (progn ;; else
                              ;; (format t "=>: rotate-right ~%")
                              (setf direction (rotate-right direction))
                              ;; (format t "direction: ~A ~%" direction)
                              (incf rotate-cnt)
                              (when (< rotate-cnt 3)
                                (go lab_R)))))))))
           (progn
             ;; (format t ":: finish: ~A ~%"
                     ;; (if (equal cur-pnt entry-point)
                         ;; "entry-point achived"
                         ;; "over rotation"))
             (return-from edge-8x8 (reverse stack))))))))

;; Tests

(defun simple-test ()
  (let* ((packflag #2A((0 0 0 0 )
                       (0 1 1 0 )
                       (0 1 1 0 )
                       (0 0 0 0 )))
         (start-point (make-pnt :y 1 :x 1))
         (height 10)
         (width 10)
         (expected-result
           (list #S(EDGE-PNT :Y 1 :X 1 :D :TR)
                 #S(EDGE-PNT :Y 1 :X 2 :D :TD)
                 #S(EDGE-PNT :Y 2 :X 2 :D :TL)
                 #S(EDGE-PNT :Y 2 :X 1 :D :TU)))
         (result (edge-8x8 start-point height width packflag)))
    (if (equalp result expected-result)
        (progn
          (format t ":: Test passed.")
          t)
        (progn
          (format t "::= Test failed. Expected:~%~A~%, but got:~%~A"
                  expected-result result)
        nil))))

;; (simple-test)

(defun complex-test ()
  (let* ((packflag #2A((0 0 0 0 0 0 0)
                       (0 1 0 1 1 1 0)
                       (0 1 1 1 0 1 0)
                       (0 0 1 0 0 1 0)
                       (0 0 1 0 1 1 0)
                       (0 0 1 1 1 0 0)
                       (0 0 0 1 1 0 0)
                       (0 0 0 0 0 0 0)))
         (start-point (make-pnt :y 1 :x 1))
         (height 10)
         (width 10)
         (expected-result
           (list
            #S(EDGE-PNT :Y 1 :X 1 :D :TD) #S(EDGE-PNT :Y 2 :X 1 :D :TR)
            #S(EDGE-PNT :Y 2 :X 2 :D :TR) #S(EDGE-PNT :Y 2 :X 3 :D :TU)
            #S(EDGE-PNT :Y 1 :X 3 :D :TR) #S(EDGE-PNT :Y 1 :X 4 :D :TR)
            #S(EDGE-PNT :Y 1 :X 5 :D :TD) #S(EDGE-PNT :Y 2 :X 5 :D :TD)
            #S(EDGE-PNT :Y 3 :X 5 :D :TD) #S(EDGE-PNT :Y 4 :X 5 :D :TL)
            #S(EDGE-PNT :Y 4 :X 4 :D :TD) #S(EDGE-PNT :Y 5 :X 4 :D :TD)
            #S(EDGE-PNT :Y 6 :X 4 :D :TL) #S(EDGE-PNT :Y 6 :X 3 :D :TU)
            #S(EDGE-PNT :Y 5 :X 3 :D :TL) #S(EDGE-PNT :Y 5 :X 2 :D :TU)
            #S(EDGE-PNT :Y 4 :X 2 :D :TU) #S(EDGE-PNT :Y 3 :X 2 :D :TU)
            #S(EDGE-PNT :Y 2 :X 2 :D :TL) #S(EDGE-PNT :Y 2 :X 1 :D :TU)))
         (result (edge-8x8 start-point height width packflag)))
    (if (equalp result expected-result)
        (progn
          (format t ":: Test passed.")
          t)
        (progn
          (format t "::= Test failed. Expected:~%~A~%, but got:~%~A"
                  expected-result result)
        nil))))

;; (complex-test)

(defun border-test ()
  (let* ((packflag #2A((1 1 1 0)
                       (0 1 1 1)
                       (1 1 1 1)
                       (1 1 1 0)
                       (0 1 1 0)))
         (start-point (make-pnt :y 0 :x 0))
         (height 5)
         (width 4)
         (expected-result
           (list
            #S(EDGE-PNT :Y 0 :X 0 :D :TR) #S(EDGE-PNT :Y 0 :X 1 :D :TR)
            #S(EDGE-PNT :Y 0 :X 2 :D :TD) #S(EDGE-PNT :Y 1 :X 2 :D :TR)
            #S(EDGE-PNT :Y 1 :X 3 :D :TD) #S(EDGE-PNT :Y 2 :X 3 :D :TL)
            #S(EDGE-PNT :Y 2 :X 2 :D :TD) #S(EDGE-PNT :Y 3 :X 2 :D :TD)
            #S(EDGE-PNT :Y 4 :X 2 :D :TL) #S(EDGE-PNT :Y 4 :X 1 :D :TU)
            #S(EDGE-PNT :Y 3 :X 1 :D :TL) #S(EDGE-PNT :Y 3 :X 0 :D :TU)
            #S(EDGE-PNT :Y 2 :X 0 :D :TR) #S(EDGE-PNT :Y 2 :X 1 :D :TU)
            #S(EDGE-PNT :Y 1 :X 1 :D :TU) #S(EDGE-PNT :Y 0 :X 1 :D :TL)))
         (result (edge-8x8 start-point height width packflag)))
    (if (equalp result expected-result)
        (progn
          (format t ":: Test passed.")
          t)
        (progn
          (format t "::= Test failed. Expected:~%~A~%, but got:~%~A"
                  expected-result result)
          nil))))

;; (border-test)

;; (declaim (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# ))

(defun pnt-or-nil-p (thing)
  (or (pnt-p thing)
      (null thing)))

(deftype pnt-or-nil (&optional type)
  (declare (ignore type))
  `(satisfies pnt-or-nil-p))


(defun fill-8x8 (cur-pnt height width packflag fill-mask &optional ret-flag)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum width) (type fixnum height))
  (let ((stk (list (make-pnt :y (car cur-pnt) ;; Поместим первую точку в стек
                             :x (cdr cur-pnt))))
        (last-right nil)
        (res)) ;; Закрашенные точки возвращаются если ret-flag=t
    (declare (type pnt-or-nil last-right))
    (labels ((set-last-right (cur-y x-pnt)
               (setf last-right (make-pnt :y cur-y :x x-pnt)))
             (enq (elt)
               (push elt stk))
             (deq ()
               (pop stk))
             (fg-pp (yy xx)
               (declare (type fixnum yy) (type fixnum yy)
                        (ftype (function (fixnum fixnum) fixnum) fg-pp))
               (and (>= yy 0)
                    (>= xx 0)
                    (< yy height)
                    (< xx width)
                    (not (= 0 (aref packflag yy xx)))))
             (set-fill (cur-y x-pnt)
               (declare (type fixnum cur-y) (type fixnum x-pnt)
                        (ftype (function (fixnum fixnum)) set-fill))
               (setf (aref packflag cur-y x-pnt)
                     (logior fill-mask
                             (aref packflag cur-y x-pnt))))
             (fill-p (cur-y x-pnt)
               (declare (type fixnum cur-y) (type fixnum x-pnt)
                        (ftype (function (fixnum fixnum)) fill-p))
               (not (= 0 (logand fill-mask
                                 (aref packflag cur-y x-pnt))))))
      (declare (inline set-last-right) (inline enq) (inline deq) (inline fg-pp)
               (inline set-fill) (inline fill-p))
      (loop for cur-pnt = (deq) then (deq) always cur-pnt do
        (let ((cur-y (pnt-y cur-pnt))
              (cur-x (pnt-x cur-pnt)))
          (declare (type fixnum cur-y)  (type fixnum cur-x))
          (let ((x-left cur-x)
                (x-right cur-x))
            (declare (type fixnum x-left)  (type fixnum x-right))
            ;; (format t "(=*_*=) cur-pnt: ~A stk: ~A ~%" cur-pnt stk)
            ;; (format t "init :: x-left: ~A | x-right: ~A ~%" x-left x-right)
            ;;
            ;; Закрасить максимальное кол-во пикселей влево от затравки,
            ;; пока не попадется граничный пиксель. Это будет координата X-left
            ;; NB: cur-x изменяется по мере продвижения влево
            ;; (format t "идем по строке влево ~%")
            (loop always (fg-pp cur-y (- x-left 1)) do
              ;; закрасить текущую точку (закрасим позже в цикле по строке)
              ;; fg-тест пройден, теперь можно сдвинуть координату X-LEFT влево
              (decf x-left))
            ;; Теперь у нас есть x-left
            ;; (format t "new x-left: ~A ~%" x-left)
            ;;
            ;; Закрасить максимальное кол-во пикселей вправо от затравки,
            ;; пока не попадется граничный пиксель. Это будет координата X-right
            ;; (format t "идем по строке вправо ~%")
            (loop always (fg-pp cur-y (+ x-right 1)) do
              ;; закрасить текущую точку (закрасим позже в цикле по строке)
              ;; fg-тест пройден, теперь можно сдвинуть координату X-RIGHT вправо
              (incf x-right))
            ;; Теперь у нас есть x-right
            ;; (format t "new x-right: ~A ~%" x-right)
            ;;
            ;; Заливка от x-left до x-right
            ;; (format t "закрашиваем - y: ~A | -от- x-left: ~A -до- x-right: ~A ~%"
            ;;         cur-y x-left x-right)
            (if ret-flag
                (loop for x-pnt from x-left to x-right do
                  (set-fill cur-y x-pnt)
                  (push (cons cur-y x-pnt) res))
                ;; else
                (loop for x-pnt from x-left to x-right do
                  (set-fill cur-y x-pnt)))
            ;;
            ;; (format t "закрашены пиксели в строке ~A:" cur-y)
            ;; (print packflag)
            ;; (format t "~%")
            ;;
            ;; Проанализировать строку ниже закрашиваемой в пределах
            ;; от X-left и X-right и найти в ней крайние правые пиксели всех
            ;; незакрашенных последовательностей. Их координаты заносятся в стек
            (let ((x-left x-left) (x-right x-right))
              (setf last-right nil)
              ;; (format t ";;~%low init: last-right is nil [~A..~A] ~%" x-left x-right)
              (loop with cur-y = (+ cur-y 1)
                    for x-pnt from x-left to x-right
                    always (< cur-y height)
                    do (let* ((cur-fg (fg-pp cur-y x-pnt))
                              (prv-fg (fg-pp cur-y (- x-pnt 1)))
                              (cur-clr (when cur-fg (fill-p cur-y x-pnt))))
                         ;; (format t "low-str pnt (~A . ~A) ; fg: ~A ; clr: ~A ; prev: ~A~%"
                         ;;         cur-y x-pnt cur-fg cur-clr prv-fg)
                         (cond
                           ;; предыдущий пиксель был фоновым и этот - фоновой и не закрашен:
                           ;; ничего не делаем
                           ;; ...
                           ((and (not prv-fg) cur-fg (not cur-clr))
                            (progn
                              (set-last-right cur-y x-pnt)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был фоновым, "
                              ;;         "а этот - тон и не закрашен:"
                              ;;         "сохраняем текущий как last-right = "
                              ;;         last-right)
                              ))
                           ((and prv-fg cur-fg (not cur-clr))
                            (progn
                              (set-last-right cur-y x-pnt)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был тон, "
                              ;;         "и этот - тон и не закрашен:"
                              ;;         "сохраняем текущий как last-right = "
                              ;;         last-right)
                              ))
                           ((and prv-fg (not cur-fg) (not cur-clr))
                            (when last-right ;; если last-right не NIL
                              (enq last-right)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был тон, "
                              ;;         "а этот - фон и не закрашен:"
                              ;;         "добавляем last-right в очередь :: "
                              ;;         stk)
                              ))
                           ;; предыдущий пиксель был фоновым и этот - фоновой и закрашен:
                           ;; это невозможно, фоновые пиксели не могут быть закрашены
                           ;; ...
                           ;; предыдущий пиксель был фоновым, а этот - тон и закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           ;; предыдущий пиксель был тон и этот - тон и закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           ;; предыдущий пиксель был тон а этот - фоновой и не закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           (t (setf last-right nil)))))
              (when last-right
                (enq last-right)
                ;; (format t "~A~A~%~A~%~a~%" "остался last-right = "
                ;;         last-right
                ;;         "добавляем оставшийся last-right в очередь, теперь она ="
                ;;         stk)
                ))
            ;; То же самое проделывается для строки выше закрашиваемой
            (let ((x-left x-left) (x-right x-right))
              (setf last-right nil)
              ;; (format t ";;~%high init: last-right is nil [~A..~A] ~%" x-left x-right)
              (loop with cur-y = (- cur-y 1)
                    for x-pnt from x-left to x-right
                    always (> cur-y 0)
                    do (let* ((cur-fg (fg-pp cur-y x-pnt))
                              (prv-fg (fg-pp cur-y (- x-pnt 1)))
                              (cur-clr (when cur-fg (fill-p cur-y x-pnt))))
                         ;; (format t "high-str pnt (~A . ~A) ; fg: ~A ; clr: ~A ; prev: ~A~%"
                         ;;         cur-y x-pnt cur-fg cur-clr prv-fg)
                         (cond
                           ;; предыдущий пиксель был фоновым и этот - фоновой и не закрашен:
                           ;; ничего не делаем
                           ;; ...
                           ((and (not prv-fg) cur-fg (not cur-clr))
                            (progn
                              (set-last-right cur-y x-pnt)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был фоновым, "
                              ;;         "а этот - тон и не закрашен:"
                              ;;         "сохраняем текущий как last-right = "
                              ;;         last-right)
                              ))
                           ((and prv-fg cur-fg (not cur-clr))
                            (progn
                              (set-last-right cur-y x-pnt)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был тон, "
                              ;;         "и этот - тон и не закрашен:"
                              ;;         "сохраняем текущий как last-right = "
                              ;;         last-right)
                              ))
                           ((and prv-fg (not cur-fg) (not cur-clr))
                            (when last-right ;; если last-right не NIL
                              (enq last-right)
                              ;; (format t "~A~A~%~A ~a~%" "предыдущий пиксель был тон, "
                              ;;         "а этот - фон и не закрашен:"
                              ;;         "добавляем last-right в очередь :: "
                              ;;         stk)
                              ))
                           ;; предыдущий пиксель был фоновым и этот - фоновой и закрашен:
                           ;; это невозможно, фоновые пиксели не могут быть закрашены
                           ;; ...
                           ;; предыдущий пиксель был фоновым, а этот - тон и закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           ;; предыдущий пиксель был тон и этот - тон и закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           ;; предыдущий пиксель был тон а этот - фоновой и не закрашен:
                           ;; ничего не делаем, пропускаем
                           ;; ...
                           (t (setf last-right nil)))))
              (when last-right
                (enq last-right)
                ;; (format t "~A~A~%~A~%~a~%" "остался last-right = "
                ;;         last-right
                ;;         "добавляем оставшийся last-right в очередь, теперь она ="
                ;;         stk)
                ))
            )
          )
            )
      )
    (if ret-flag
        (values (reverse (remove-duplicates res)))
        ;; else
        (values))))

(defun not-so-simple-test ()
  (format t "~%~%")
  (let* ((packflag
           (alexandria:copy-array
            #2A((0 0 0 0 0 1 1 0 0 1 1)
                (0 1 1 1 0 0 0 0 0 1 1)
                (0 1 1 1 0 0 1 1 0 0 0)
                (0 1 0 1 0 0 1 1 1 1 0)
                (1 1 0 1 1 1 1 0 0 1 1)
                (0 1 1 1 0 0 0 0 0 1 1)
                (0 0 0 0 0 1 1 0 0 0 1)
                (1 0 0 0 0 1 1 1 1 1 1))))
         (start-point (cons 1 2))
         (height (array-dimension packflag 0))
         (width  (array-dimension packflag 1))
         (exp-res
           (alexandria:copy-array
            #2A((0   0   0   0   0   1   1   0   0   1   1)
                (0   129 129 129 0   0   0   0   0   1   1)
                (0   129 129 129 0   0   129 129 0   0   0)
                (0   129 0   129 0   0   129 129 129 129 0)
                (129 129 0   129 129 129 129 0   0   129 129)
                (0   129 129 129 0   0   0   0   0   129 129)
                (0   0   0   0   0   129 129 0   0   0   129)
                (1   0   0   0   0   129 129 129 129 129 129))))
         (points (fill-8x8 start-point height width packflag #b10000000 t))
         (result (if (equalp packflag exp-res)
                     (progn
                       (format t "::= Test passed.~%")
                       t)
                     (progn
                       (format t "::= Test failed. Expected:~%~A~%but got:~%~A~%"
                               exp-res packflag)
                       nil))))
    (print points)
    result))

;; (prog1
;;     (not-so-simple-test)
;;   (finish-output))

(defmacro dbgout-array (name)
  (let ((format-str (concatenate 'string "~%~%len " (symbol-name name) " : ~A")))
    `(progn
       (format t ,format-str (length ,name))
       (print ,name)
       (format t "~%"))))

;; (macroexpand-1 '(dbgout-array rest-of-candidates))


;; (defmacro plot-points (packflag list-of-points mask)
;;   `(loop for (yy . xx) in ,list-of-points do
;;     (setf (aref ,packflag yy xx)
;;           (logior ,mask
;;                   (aref ,packflag yy xx)))))

;; (macroexpand-1 '(plot-points packflag cur-edge #b01000000))


(defun save-packflag (filepath par-packflag par-packline y-size x-size channels bit-depth &optional img)
  (let ((new (re-conv-8x8 par-packflag par-packline y-size x-size channels bit-depth img)))
    (with-open-file (output filepath :element-type '(unsigned-byte 8)
                                     :direction :output :if-exists :supersede)
      (png:encode new output))))

(defun find-edges (packflag height width)
  (let ((pf-height (ash height -3))
        (pf-width  (ash width  -3))
        (edges))
    (labels ((find-candidates-8x8 (packflag height width)
               ;; Поиск затравок сверху вниз. Затравка - это точка с
               ;; цветом тона (1) которая находится ниже точки с цветом
               ;; фона (0)"
               (let ((ret-candidates))
                 (do* ((xx 0  (+ 1 xx)))
                      ((>= xx width))
                   (let ((prev 0))
                     (do* ((yy 0  (+ 1 yy)))
                          ((>= yy height))
                       (let ((flag (aref packflag yy xx)))
                         (if (= 0 flag)
                             (setf prev 0)    ;; then
                             (when (= 0 prev) ;; else
                               (let ((cand(make-pnt :y yy :x xx)))
                               (push cand ret-candidates)
                                 (setf prev flag))))))))
                 (reverse ret-candidates)))
             (remove-from-candidates (list-of-points rest-of-candidates)
               (loop for edge-pnt in list-of-points do
                 (setf rest-of-candidates ;; delete really faster them remove
                       (delete-if #'(lambda (cand-pnt)
                                      (and
                                       (equalp (edge-pnt-y edge-pnt)
                                               (pnt-y cand-pnt))
                                       (equalp (edge-pnt-x edge-pnt)
                                               (pnt-x cand-pnt))))
                                  rest-of-candidates)))
               rest-of-candidates)
             (rec-elim (candidates)
               ;; Поиск краев фигуры, начиная от затравки с последующим
               ;; удалением всех точек краев из остатка списка кандидатов
               ;; пока список кандидатов не закончится
               (if (null candidates)
                   (values)
                   ;; else
                   (let* ((candidate (car candidates))
                          (rest-of-candidates (cdr candidates))
                          (cur-edge (edge-8x8 candidate pf-height pf-width packflag)))
                     (push cur-edge edges)
                     ;; Удаляем из списка кандидатов все точки обнаруженного
                     ;; края фигуры (cur-edge). Так как по определению края ищутся
                     ;; начиная с каждой точки-кандидата было бы избыточно удалять
                     ;; из кандидатов все точки заливки (cur-full)
                     (setf rest-of-candidates
                           (remove-from-candidates cur-edge rest-of-candidates))
                     ;; (print (length rest-of-candidates))
                     ;; Хвостовая рекурсия
                     (rec-elim rest-of-candidates)))))
      (declare (inline find-candidates-8x8) (inline rec-elim))
      ;; Итерируемся по точкам-кандидатам, ища края и удаляя из
      ;; оставшихся кандидатов тех, что совпадают с найденными краями
      ;; пока кандидатов не останется. На выходе - edges
      (rec-elim (find-candidates-8x8 packflag pf-height pf-width))
      (reverse edges))))


(defun edge-visualization (edges new height width)
  (labels ((fence (it max)
             (when (< it  0)
               (setf it 0))
             (when (> it (- max 1))
               (setf it (- max 1)))
             it)
           (plot (yy xx rr gg bb)
             (setf (aref new (fence yy height) (fence xx width) 0) rr)
             (setf (aref new (fence yy height) (fence xx width) 1) gg)
             (setf (aref new (fence yy height) (fence xx width) 2) bb)))
    (loop for edge in edges do
      (loop for edge-pnt in edge do
        (loop for off from 0 to 7 do
          (let ((yy (ash (edge-pnt-y edge-pnt) 3))
                (xx (ash (edge-pnt-x edge-pnt) 3)))
            (ecase (edge-pnt-d edge-pnt)
              (:tr (progn
                     (loop for off from 0 to 7 do
                       (plot yy (+ xx off) 255 0 0))
                     (loop for off from 1 to 3 do
                       (plot (+ yy off) (- (+ xx 7) off) 255 0 0))))
              (:tl (progn
                     (loop for off from 0 to 7 do
                       (plot (+ yy 7) (+ xx off) 0 0 255))
                     (loop for off from 4 to 6 do
                       (plot (+ yy off) (- (+ xx 7) off) 0 0 255))))
              (:tu (progn
                     (loop for off from 0 to 7 do
                       (plot (+ yy off) xx 0 255 0))
                     (loop for off from 1 to 3 do
                       (plot (+ yy off) (+ xx off) 0 255 0))))
              (:td (progn
                     (loop for off from 0 to 7 do
                       (plot (+ yy off) (+ xx 7) 0 255 255))
                     (loop for off from 4 to 6 do
                       (plot (+ yy off) (+ xx off) 0 255 255))))))))))
  new)


(defun get-xy-pair (edge-pnt)
  (cons (edge-pnt-y edge-pnt)
        (edge-pnt-x edge-pnt)))


(defun get-edge-ht-keys (edge)
  (sort (remove-duplicates
         (mapcar #'get-xy-pair edge))
        #'(lambda (a b)
            (if (= (car a) (car b))
                (> (cdr a) (cdr b))
                (= (car a) (car b))))))


(defun zero-frame-factor-p (edge)
  (let ((ht (make-hash-table :test #'equalp)))
    (loop for edge-pnt in edge :do
      (let ((key (get-xy-pair edge-pnt)))
        (multiple-value-bind (value present)
            (gethash key ht)
          (if present
              (setf (gethash key ht)
                    (push (edge-pnt-d edge-pnt) value))
              ;; else
              (setf (gethash key ht)
                    (list (edge-pnt-d edge-pnt)))))))
    (let ((single 0)  (multi  0))
      (loop for key being the hash-keys of ht using (hash-value val) do
        (if (= 1 (length val))
            (incf single)
            (incf multi)))
      (unless (= 0 single)
        (= 0 (floor multi single))))))


(defun show-square (yy xx rr gg bb packflag packline)
  (setf (aref packflag yy xx) 63)
  (setf (aref packline yy xx 0) rr)
  (setf (aref packline yy xx 1) gg)
  (setf (aref packline yy xx 2) bb))


(defun match-near-edges (r-edge-pnt l-edge-pnt offset-y offset-x packflag packline)
  ;; match
  (when (and
         (= (+ offset-y (edge-pnt-y r-edge-pnt)) (edge-pnt-y l-edge-pnt))
         (= (+ offset-x (edge-pnt-x r-edge-pnt)) (edge-pnt-x l-edge-pnt)))
    (let ((rs))
      (loop for y-rep from 0 to offset-y do
        (loop for x-rep from 0 to offset-x do
          ;; retval
          (push (make-pnt :y (+ y-rep (edge-pnt-y r-edge-pnt))
                          :x (+ x-rep (edge-pnt-x r-edge-pnt)))
                rs)
          ;; light-blue
          ;; (show-square (+ y-rep (edge-pnt-y r-edge-pnt))
          ;;              (+ x-rep (edge-pnt-x r-edge-pnt))
          ;;              0 255 255 packflag packline)
              ))
      ;; ;; magenta right
      ;; (show-square (edge-pnt-y r-edge-pnt) (edge-pnt-x r-edge-pnt) 255 0 255 packflag packline)
      ;; ;; yelloy left
      ;; (show-square (edge-pnt-y l-edge-pnt) (edge-pnt-x l-edge-pnt) 255 255 0 packflag packline)
      ;; ret true
      (reverse rs))))

(defun find-junctions (edges packflag packline)
  ;; Функция получает на вход список периметров, найденный в изображении
  ;; (каждый периметр - это список структур EDGE-PNT)
  ;; и возвращает список пар смежности, где первые два элемента - это
  ;; порядковые номера смежных периметров в массиве edges,
  ;; а последний элемент - это список структур PNT смежных точек
  (let* ((l-edges ;; Для каждоого периметра здесь оставляем только левый край
           (mapcar #'(lambda (edge) ;; - это точки с направленем вверх
                       (remove-if-not #'(lambda (edge-pnt)
                                          (equal :tu (edge-pnt-d edge-pnt)))
                                      edge))
                   edges))
         (r-edges ;; Для каждой периметра здесь оставляем только правый край
           (mapcar #'(lambda (edge) ;;  - это точки с направлением вниз
                       (remove-if-not #'(lambda (edge-pnt)
                                          (equal :td (edge-pnt-d edge-pnt)))
                                      edge))
                   edges))
         (junctions))
    ;; Для каждого из ПРАВЫХ краев получаем этот край
    ;; (потому что будем искать точки близкого ему ЛЕВОГО края)
    (loop for r-edge in r-edges for r-idx from 0 do
      ;; Для каждого из ЛЕВЫХ краев получаем этот край
      (loop for l-edge in l-edges for l-idx from 0 do
        ;; Если мы найдем сопоставление, то среди точек пары
        ;; правый_край:левый_край больше искать нет смысла
        ;; поэтому будем выходить из этого блока
        (unless (= r-idx l-idx) ;; Незачем сопоставлять периметр с самим собой
          (block edge-match
            ;; сопоставляем поточечно, и если находим
            ;; хотя бы одну пару соответствующих точек справа и слева
            ;; то считаем что края совпали и дальше не проверяем
            (loop for r-edge-pnt :in r-edge for r-idx-pnt from 0 do
              (loop for l-edge-pnt :in l-edge for l-idx-pnt from 0 do
                ;; ;; magenta right
                ;; (show-square (edge-pnt-y r-edge-pnt) (edge-pnt-x r-edge-pnt) 255 0 255 packflag packline)
                ;; ;; yelloy left
                ;; (show-square (edge-pnt-y l-edge-pnt) (edge-pnt-x l-edge-pnt) 255 255 0 packflag packline)
                ;; match
                (awhen (or (match-near-edges r-edge-pnt l-edge-pnt 0 2 packflag packline)
                           (match-near-edges r-edge-pnt l-edge-pnt 0 3 packflag packline)
                           ;; (match-near-edges r-edge-pnt l-edge-pnt 1 2 packflag packline)
                           ;; (match-near-edges r-edge-pnt l-edge-pnt 1 3 packflag packline)
                           )
                  (push (list r-idx l-idx it) junctions)
                  ;; ret block
                  (return-from edge-match t))))
            ;; Здесь мы оказываемся, если для этих edges ничего не нашли
            ;; Тогда ищем дальше, для следующих точек в цикле
            ))))
    ;; (loop for junk in junctions do
    ;;   (print junk))
    junctions))


(defun connlists (graph)
  ;; построение списка списков вершин компонент связности
  ;; graph - граф, представленный структурой смежности в
  ;; виде ассоциативного списка
  (connlsts graph nil))

(defun connlsts (graph lists)
  (cond ((null graph) lists)
        (t (cond ((null (lmember (caar graph) lists))
                  (connlsts
                   (cdr graph)
                   (cons (depthfirst graph (caar graph))
                         lists)) )
                 (t (connlsts (cdr graph) lists))))))

(defun lmember (vertex lists)
  ;; функция рекурсивно проверяет, содержится ли вершина vertex в
  ;; каком-либо из списков, составляющих lists
  (and lists
       (or (member  vertex (car lists))
           (lmember vertex (cdr lists)))))

(defun depthfirst (graph root)
  ;; обход графа в глубину:
  ;; DEPTHFIRST вычисляет список вершин связной компоненты вершины ROOT
  ;; graph - граф, представленный структурой смежности в
  ;;         виде ассоциативного списка,
  ;; root  - вершина, с которой начинается обход графа,
  ;; результат: список вершин графа в порядке посещения в глубину
  (cond ((null graph) nil)
        (t (defi graph (list root) (list root)))))

(defun defi (graph visited path)
  ;; DeepFirstSearch - обход графа в глубину
  ;; graph   - граф, представленный структурой смежности в
  ;;           виде ассоциативного списка,
  ;; visited - список уже посещенных вершин,
  ;; path    - список вершин, определяющий путь посещения
  (cond ((null path) (reverse visited))
        (t (cond ((null (expnd graph visited (car path)))
                  (defi graph visited (cdr path)))
                 (t (defi graph
                        (cons (expnd graph visited (car path))
                              visited)
                      (cons (expnd graph visited (car path))
                            path)))))))

(defun expnd (graph visited vertex)
  ;; выбор в графе graph следующей еще не просмотренной
  ;; вершины, смежной с вершиной vertex
  (cond ((null (neighbour3 vertex graph)) nil)
        (t (firstnotvisited visited (neighbour3 vertex graph)))))

(defun firstnotvisited (visited vlist)
  ;; поиск первой непосещенной вершины в списке vlist
  ;; visited - список уже посещенных вершин
  (cond ((null vlist) nil )
        (t (cond ((null (member (car vlist) visited))
                  (car vlist))
                 (t (firstnotvisited visited (cdr vlist)))))))

(defun neighbour3 (x graph)
  ;; функция возвращает список вершин графа graph, смежных с
  ;; вершиной x
  (cond ((null (assoc x graph)) nil)
        (t (cdr (assoc x graph)))))

;; (connlists '((1 . (2 3)) (2 . (3)) (5 . ())))
;; ((5) (1 2 3))
;; (connlists '((1 . (2 3)) (2 . (3)) (5 . ()) (6 . ())))
;; ((6) (5) (1 2 3))
;; (connlists '((1 . (2 3)) (2 . (3)) (5 . (6))))
;; ((5 6) (1 2 3))
;; (connlists '((1 . (2 3 5)) (2 . (3)) (5 . (6))))
;; ((1 2 3 5 6))

(defun f2 (graph lst)
  ;; построение структур смежности всех компонент связности
  ;; graph - граф, представленный структурой смежности в
  ;;         виде ассоциативного списка,
  ;; lst   - список списков компонент связности
  (cond ((null lst) nil )
        (t  (cons (f1 graph (car lst))
                  (f2 graph (cdr lst))))))

(defun f1 (graph lst)
  (cond ((null graph) nil)
        ((member (caar graph) lst)
         (cons (car graph) (f1 (cdr graph) lst)))
        (t  (f1 (cdr graph) lst))))

;; (f2 '((1 . (2 3)) (4 . (5 6)) (5 . (4)) (3 . (2 1)))
;;       '((1 2 3) (4 5 6)))
;; (((1 2 3) (3 2 1)) ((4 5 6) (5 4)))

(defun adjacency-list (edges)
  (let ((ht (make-hash-table))
        (rs))
    (dolist (edge edges)
      (destructuring-bind (node1 . node2) edge
        (push node2 (gethash node1 ht))
        (unless (gethash node2 ht)
          (setf (gethash node2 ht) nil))))
    (maphash #'(lambda (key val)
                 (push (cons key val) rs))
             ht)
    rs))


;; launcher

(defun launcher (filename)
  (let* ((img (with-open-file (input filename :element-type '(unsigned-byte 8))
                (png:decode input)))
         (base-dir (ensure-directories-exist (format nil "./~A/" (symbol-name (gensym)))))
         ;; (edge-dir (ensure-directories-exist (format nil "~A/edges/" base-dir)))
         (fill-dir (ensure-directories-exist (format nil "~A/fills/" base-dir)))
         (chop-dir (ensure-directories-exist (format nil "~A/chops/" base-dir)))
         (tess-list))
    (multiple-value-bind (packflag packline height width channels bit-depth)
        (conv-8x8 img)
      (let* ((pf-height (ash height -3))
             (pf-width  (ash width  -3))
             (edges ;; Находим все границы фигур и удаляем найденные рамки из них
               ;; рамки - это фигуры, которые состоят почти только из каймы.
               ;; Это список списков структур EDGE-PNT
               (remove-if-not #'zero-frame-factor-p
                              (find-edges packflag height width)))
             ;; находим точки соединения смежных фигур
             ;; эта функция изменяет packflag!
             (junctions (find-junctions edges packflag packline))
             ;; и строим по ним пары индексов смежных периметров
             (adj-pairs (mapcar #'(lambda (jnk)
                                    (cons (car jnk) (cadr jnk)))
                                junctions))
             ;; очищаем их от дублирующихся и самосмежных пар
             (adj-pairs (remove-duplicates
                         (remove-if #'(lambda (elt)
                                        (= (car elt) (cdr elt)))
                                    adj-pairs)))
             ;; так как это ребра неориентированного графа, то нам
             ;; нужно для каждого ребра создать обратное ему
             (adj-pairs (let ((rs))
                          (mapcar #'(lambda (elt)
                                      (push elt rs)
                                      (push (cons (cdr elt) (car elt)) rs))
                                  adj-pairs)
                          rs))
             ;; чтобы получить из них список списков смежности
             (adj-list (adjacency-list adj-pairs))
             ;; и выделить компоненты связности
             (conn-list (connlists adj-list)))
        ;; у нас есть junctions - теперь мы можем расставить точки связи
        (loop for jun in junctions :do
          (loop for jp in (car (last jun)) :do
            (show-square (pnt-y jp) (pnt-x jp) 0 255 255 packflag packline)))
        ;; У нас есть компоненты связности - выполним слияние блоков
        (loop for conn-elt in conn-list do
          ;; Для каждого edge компонента связности кроме первого
          (loop for rest-elt in (cdr conn-elt) do
            ;; Перенесем все точки в первый edge
            (setf (nth (car conn-elt) edges)
                  (append (nth (car conn-elt) edges)
                          (nth rest-elt edges)))
            ;; И очистим список точек
            (setf (nth rest-elt edges) nil)))
        (loop for edge-idx from 0
              for cur-edge in edges do
                ;; (print (list edge-idx (nth edge-idx edges)))
                ;; (let* ((edge-idx 69) ;; dbg instead of loop
                ;;        (cur-edge (nth edge-idx edges)))
                (when cur-edge
                  (let* ((filename (format nil "~A" edge-idx))
                         ;; ;; Восстанавливаем изображение из packflag и packline
                         ;; (new (re-conv-8x8 packflag packline height width channels
                         ;;                   bit-depth img))
                         ;; ;; Добавляем визуализацию краев
                         ;; (new (edge-visualization (list cur-edge) new height width))
                         ;; ;; Сохраняем в файл визуализацию краев
                         ;; (stb (let ((filepath (format nil "~A/~A.png" edge-dir filename)))
                         ;;        (with-open-file (output
                         ;;                         filepath
                         ;;                         :element-type '(unsigned-byte 8)
                         ;;                         :direction :output :if-exists :supersede)
                         ;;          (png:encode new output))))
                         ;; Локальная копия packflag и packline
                         (loc-packflag (alexandria:copy-array packflag))
                         (loc-packline (alexandria:copy-array packline))
                         ;; Затравка для заливки
                         (candidate (get-xy-pair (car cur-edge)))
                         ;; Заливаем loc-packflag
                         (cur-fill ;; в процесе заливки сам расставляет признаки
                           (fill-8x8 candidate pf-height pf-width loc-packflag #b10000000 t))
                         (y-strt (mapcar #'edge-pnt-y cur-edge))
                         (x-strt (mapcar #'edge-pnt-x cur-edge))
                         (y-mini (reduce #'min y-strt))
                         (x-mini (reduce #'min x-strt))
                         (y-maxi (reduce #'max y-strt))
                         (x-maxi (reduce #'max x-strt))
                         (y-size (+ 1 (- y-maxi y-mini)))
                         (x-size (+ 1 (- x-maxi x-mini)))
                         ;; Создаем пустые chop-packflag и chop-packline
                         ;; чтобы потом вырезать из оригинальных контент
                         ;; и вставить в chop
                         (chop-packflag (make-array `(,y-size ,x-size)
                                                    :element-type 'fixnum :initial-element 0))
                         (chop-packline (make-array `(,y-size ,x-size ,channels)
                                                    :element-type '(unsigned-byte 8)
                                                    :initial-element 0))
                         ;; Создаем chop-img
                         (chop-img (png:make-image (ash y-size 3) (ash x-size 3)
                                                   channels bit-depth)))
                    (declare (ignore cur-fill))
                    (when (and
                           ;; Если edge не занимает больше 9/10 изображения
                           ;; по какой-то из сторон
                           ;; (< x-size (* 9 (floor (ash width -3) 10)))
                           ;; (< y-size (* 9 (floor (ash height -3) 10)))
                           ;; Или если x-size < 4
                           (> x-size 4))
                      ;; Вырезаем блок из packflag и packline и вставляем в chops
                      ;; При этом не переносим точки если packflag == 0
                      (do* ((yy 0  (+ 1 yy)))
                           ((>= yy y-size))
                        (declare (type fixnum yy))
                        (do* ((xx 0  (+ 1 xx)))
                             ((>= xx x-size))
                          (declare (type fixnum xx))
                          (let ((packflag-pnt (aref packflag (+ yy y-mini) (+ xx x-mini))))
                            (unless (= 0 packflag-pnt)
                              (setf (aref chop-packflag yy xx)
                                    packflag-pnt)
                              (dotimes (zz channels)
                                (setf (aref chop-packline yy xx zz)
                                      (aref packline (+ yy y-mini) (+ xx x-mini) zz)))))))
                      ;; делаем вырезку в chop-img, который нужен нам для re-conv
                      (let ((y-start (ash y-mini 3))
                            (x-start (ash x-mini 3)))
                        (do* ((yy 0  (+ 1 yy)))
                             ((>= yy (ash y-size 3)))
                          (declare (type fixnum yy))
                          (do* ((xx 0 (+ 1 xx)))
                               ((>= xx (ash x-size 3)))
                            (declare (type fixnum xx))
                            (dotimes (zz channels)
                              (let ((packflag-pnt (aref packflag
                                                        (+ (ash yy -3) y-mini)
                                                        (+ (ash xx -3) x-mini))))
                                (unless (= 0 packflag-pnt)
                                  (setf (aref chop-img yy xx zz)
                                        (aref img
                                              (+ yy y-start)
                                              (+ xx x-start)
                                              zz))))))))
                      ;; сохраняем chop-img
                      (let ((filepath (format nil "~A/~A.png" chop-dir filename)))
                        (save-packflag filepath
                                       chop-packflag chop-packline (ash y-size 3) (ash x-size 3)
                                       channels bit-depth chop-img)
                        (let* ((tes-ru (cl-tesseract:image-to-text filepath :lang "rus+eng"))
                               (tes-ru (cl-ppcre:regex-replace-all "^\\s+|\\s+$" tes-ru ""))
                               ;; (tes-en (cl-tesseract:image-to-text filepath :lang "eng"))
                               ;; (tes-en (cl-ppcre:regex-replace-all "^\\s+|\\s+$" tes-en ""))
                               )
                          (when (< 3 (length tes-ru))
                            (push (list :edge-idx edge-idx :y y-mini :x x-mini
                                        :tes-ru (bprint tes-ru) ;; :tes-en (bprint tes-en)
                                        )
                                  tess-list)
                            (format t "~%~A" tes-ru))))
                      ;; сохраняем img c локальной копией pack-ов
                      (let ((filepath (format nil "~A/~A.png" fill-dir filename)))
                        (save-packflag filepath
                                       loc-packflag loc-packline height width
                                       channels bit-depth img))
                      ;; сохраняем img c оригиналом pack-ов
                      ;; (let (filepath (format nil "~A/~A.png" orig-dir filename))
                      ;;   (save-packflag filepath
                      ;;                  packflag packline height width
                      ;;                  channels bit-depth img))
                      ))))
        ;; (save-packflag (format nil "~A/ALL-~A.png" base-dir (symbol-name (gensym)))
        ;;                packflag packline height width
        ;;                channels bit-depth img)
        ))
    ;; (loop for item in (sort tess-list #'(lambda (a b)
    ;;                                       (< (getf a :y) (getf b :y))))
    ;;       do
    ;;   (format t "~%~A:(~A:~A) ~A"
    ;;           (getf item :edge-idx)
    ;;           (getf item :y)
    ;;           (getf item :x)
    ;;           (getf item :tes-ru)
    ;;           ;; (getf item :tess-en)
    ;;           ))
    )
  nil)

;; (time
;;  (progn
;;    (init-defaults)
;;    (launcher "antalya.png")))

;; (print
;;  (cl-tesseract:image-to-text "demo/artem.png" :lang "rus+eng")
;;  nil)

;; (defun core-main ()
;;   (time
;;    (progn
;;      (init-defaults)
;;      (x-snapshot :path "out/snapshot.png")
;;      ;; (launcher "snapshot.png")
;;      )))


;; TODO:
;; - получить распознанные блоки и их координаты
;; - отправлять блоки в LLM

(defun x-move (x y)
  (if (and (integerp x) (integerp y))
      (with-default-display-force (d)
        (xlib/xtest:fake-motion-event d x y))
      (error "Integer only for position, (x: ~S, y: ~S)" x y)))

(defun mklist (obj)
  (if (and
       (listp obj)
       (not (null obj)))
      obj (list obj)))

(defmacro defun-with-actions (name params actions &body body)
  ;; "This macro defun a function which witch do mouse or keyboard actions,
  ;; body is called on each action."
  `(defun ,name ,params
     (mapcar
      #'(lambda (action)
          ,@body)
      (mklist ,actions))))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xlib/xtest:fake-button-event d button press?)))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xlib/xtest:fake-key-event d keycode press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name
                  (&key (button 1) x y)
                ,actions
                (funcall #'perform-mouse-action
                         action button :x x :y y))))
  (def x-mouse-down t)
  (def x-mouse-up nil)
  (def x-click '(t nil))
  (def x-dbclick '(t nil t nil)))

(defmacro with-scroll (pos neg clicks x y)
  `(let ((button (cond
                   ((= 0 ,clicks) nil)
                   ((> 0 ,clicks) ,pos)    ; scroll up/right
                   ((< 0 ,clicks) ,neg)))) ; scroll down/left
     (dotimes (_ (abs ,clicks))
       (x-click :button button :x ,x :y ,y))))

(defun x-vscroll (clicks &key x y)
  (with-scroll 4 5 clicks x y))

(defun x-scroll (clicks &key x y)
  (x-vscroll clicks :x x :y y))

(defun x-hscroll (clicks &key x y)
  (with-scroll 7 6 clicks x y))

;; test: vertiacal scroll for 5 clics in position X and Y
;; (progn
;;   (sleep .1)
;;   (x-vscroll -5 :x 1500 :y 300))

(macrolet ((def (name actions)
             `(defun-with-actions ,name (keycode)
                ,actions
                (funcall #'perform-key-action
                         action keycode))))
  (def x-key-down t)
  (def x-key-up nil)
  (def x-press '(t nil)))

;; (block perform-key-action-test
;;   (perform-key-action t 116)
;;   (sleep .1)
;;   (perform-key-action nil 116))

;; (defparameter *mouse-left* 1)

;; (defparameter *mouse-middle* 2)

;; (block perform-mouse-action-test
;;   (perform-mouse-action t *mouse-left* :x 100 :y 100)
;;   (sleep .1)
;;   (perform-mouse-action nil *mouse-left* :x 100 :y 100))
