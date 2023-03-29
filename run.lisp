(ql:quickload "clx")
(ql:quickload "zpng")

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

(ql:quickload "png")

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
  (let* ((new        (png:make-image height width channels bit-depth)))
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
              (rgb-times
                (if (or (not (= 0 (logand yy1 #b010)))
                        (not (= 0 (logand xx1 #b010))))
                    (aref packline yy3 xx3 zz)
                    ;; else
                    (logand 255 (+ 20 (aref packline yy3 xx3 zz)))))
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
                  (when old
                    (rgb-times
                      (logand 255
                              ;; we need only 1 bit for channel
                              (logior #b00011111
                                      (aref old yy1 xx1 zz)))))))
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

(ql:quickload "alexandria")
(ql:quickload "cl-tesseract")

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
      (= 0 (floor multi single)))))


;; launcher

(defun edge-mrg (r-idx l-idx)
  (format t "~%  ~A <==> ~A ~%" r-idx l-idx))

(defun edge-match (r-edge l-edge r-idx l-idx)

  nil)

(defun launcher (filename)
  (let ((img (with-open-file (input filename :element-type '(unsigned-byte 8))
               (png:decode input)))
        (dir (ensure-directories-exist (format nil "./~A/"  (symbol-name (gensym))))))
    (multiple-value-bind (packflag packline height width channels bit-depth)
        (conv-8x8 img)
      (let* ((edges ;; Находим все границы фигур и удаляем найденные рамки из них
               (remove-if-not #'zero-frame-factor-p
                              (find-edges packflag height width)))
             (l-edges
               (mapcar #'(lambda (edge)
                           (remove-if-not #'(lambda (edge-pnt)
                                              (equal :tu (edge-pnt-d edge-pnt)))
                                          edge))
                       edges))
             (r-edges
               (mapcar #'(lambda (edge)
                           (remove-if-not #'(lambda (edge-pnt)
                                              (equal :td (edge-pnt-d edge-pnt)))
                                          edge))
                       edges)))
        (loop for r-edge in l-edges for r-idx from 0 do
          (loop for l-edge in r-edges for l-idx from 0 do
            (when (block edge-match
                    (loop for r-edge-pnt :in r-edge for r-idx-pnt from 0 do
                      (loop for l-edge-pnt :in l-edge for l-idx-pnt from 0 do
                        (when (and (= (edge-pnt-y r-edge-pnt) (edge-pnt-y l-edge-pnt))
                                   (= (edge-pnt-x r-edge-pnt) (+ (edge-pnt-x l-edge-pnt) 2)))
                          (setf (aref packflag
                                      (edge-pnt-y r-edge-pnt)
                                      (edge-pnt-x r-edge-pnt))
                                0)
                          (setf (aref packline
                                      (edge-pnt-y r-edge-pnt)
                                      (edge-pnt-x r-edge-pnt)
                                      0)
                                255)
                          (setf (aref packflag
                                      (edge-pnt-y r-edge-pnt)
                                      (edge-pnt-x l-edge-pnt))
                                0)
                          (setf (aref packline
                                      (edge-pnt-y r-edge-pnt)
                                      (edge-pnt-x l-edge-pnt)
                                      1)
                                255)
                          (return-from edge-match t))))
                    nil)
              (format t "~%  ~A <==> ~A | ~A >--< ~A ~%"
                      r-idx l-idx (length r-edge) (length l-edge)))))
        (loop for edge in edges do
          (let ((new (re-conv-8x8 packflag packline height width channels bit-depth img)))
            (setf new (edge-visualization (list edge) new height width))
            ;; save
            (with-open-file (output
                             (format nil "~A/~A+.png" dir (symbol-name (gensym)))
                             :element-type '(unsigned-byte 8)
                             :direction :output :if-exists :supersede)
              (png:encode new output))))
        ;; (save-packflag (format nil "~A/~A+.png" dir (symbol-name (gensym)))
        ;;                packflag packline height width
        ;;                channels bit-depth img)
        ))))

;; (time
;;  (launcher "antalya.png"))


(defun core-main ()
  (time
   (progn
     (init-defaults)
     (x-snapshot :path "snapshot.png")
     ;; (launcher "snapshot.png")
     )))


;; (maximin (loop for cur-edge in edges
;;                collect (list
;;                         (reduce #'(lambda (max-pair pair)
;;                                     (if (> (cdr pair) (cdr max-pair))
;;                                         pair
;;                                         max-pair))
;;                                 cur-edge)
;;                         (reduce #'(lambda (min-pair pair)
;;                                     (if (< (cdr pair) (cdr min-pair))
;;                                         pair
;;                                         min-pair))
;;                                 cur-edge))))


;; (print maximin)
;; ;; maximin - это список. Каждый элемент этого списка - это
;; ;; список из 2-х элементов, первый из которых - это
;; ;; кордината самого правого края фигуры, а второй - это
;; ;; координата самого левого края фигуры
;; (loop for (cur-right-pair cur-left-pair) in maximin do
;;   (plot-points packflag (list cur-right-pair)  #b10000000)
;;   (plot-points packflag (list cur-left-pair)   #b01000000)
;;   (destructuring-bind (yy-cur-right . xx-cur-right)
;;       cur-right-pair
;;     (loop for (cand-right-pair cand-left-pair) in maximin do
;;       (destructuring-bind (yy-cand-left . xx-cand-left)
;;           cand-left-pair
;;         (when (and (= yy-cur-right yy-cand-left)
;;                    (< 0 (- xx-cand-left xx-cur-right))
;;                    (> 3 (- xx-cand-left xx-cur-right)))
;;           (plot-points packflag (list cur-right-pair)  #b10000000)
;;           (plot-points packflag (list cand-left-pair)  #b01000000)
;;           (print (- xx-cand-left xx-cur-right))
;;           )))))



;; (let ((img (with-open-file (input "antalya.png" :element-type '(unsigned-byte 8))
;;              (png:decode input)))
;;       (dir  (ensure-directories-exist (format nil "./~A/"  (symbol-name (gensym)))))
;;       ;; (dir_ (ensure-directories-exist (format nil "./~A_/" (symbol-name (gensym)))))
;;       )
;;   (multiple-value-bind (packflag packline height width channels bit-depth)
;;       (conv-8x8 img)
;;     (let ((pf-height (ash height -3))
;;           (pf-width  (ash width  -3)))
;;       (labels ((rec-elim (candidates)
;;                  (if (null candidates)
;;                      (values)
;;                      ;; else
;;                      (let* ((candidate (car candidates))
;;                             (rest-of-candidates (cdr candidates))
;;                             (cur-fill ;; в процесе заливки сам расставляет признаки
;;                               (fill-8x8 candidate pf-height pf-width packflag #b10000000 t))
;;                             (cur-edge (edge-8x8 candidate pf-height pf-width packflag))
;;                             (filename (symbol-name (gensym)))
;;                             (filepath (format nil "~A/~A.png" dir filename))
;;                             (ys (mapcar #'car cur-edge))
;;                             (xs (mapcar #'cdr cur-edge))
;;                             (y-min (reduce #'min ys))
;;                             (x-min (reduce #'min xs))
;;                             (y-max (reduce #'max ys))
;;                             (x-max (reduce #'max xs))
;;                             (y-size (+ 1 (- y-max y-min)))
;;                             (x-size (+ 1 (- x-max x-min)))
;;                             (new-packflag
;;                               (make-array `(,pf-height ,pf-width)
;;                                           :element-type 'fixnum :initial-element 0))
;;                             (new-packline
;;                               (make-array `(,pf-height ,pf-width ,channels)
;;                                           :element-type '(unsigned-byte 8)
;;                                           :initial-element 0))
;;                             (new-img  (png:make-image (ash y-size 3) (ash x-size 3)
;;                                                       channels bit-depth)))
;;                        (format t "y-size:~A x-size:~A ~%" y-size x-size)
;;                        ;; Окраска точек для визуализации
;;                        ;; (dbgout-array cur-fill)
;;                        ;; (plot-points packflag cur-fill #b10000000) ;; не надо, уже окрашено
;;                        (plot-points packflag cur-edge #b01000000)
;;                        ;; new-packflag, new-packline
;;                        (do* ((yy 0  (+ 1 yy)))
;;                             ((>= yy y-size))
;;                          (declare (type fixnum yy))
;;                          (do* ((xx 0  (+ 1 xx)))
;;                               ((>= xx x-size))
;;                            (declare (type fixnum xx))
;;                            (setf (aref new-packflag yy xx)
;;                                  (aref packflag (+ yy y-min) (+ xx x-min)))
;;                            (dotimes (zz channels)
;;                              (setf (aref new-packline yy xx zz)
;;                                    (aref packline (+ yy y-min) (+ xx x-min) zz)))))
;;                        ;; new-img
;;                        (let ((y-start (ash y-min 3))
;;                              (x-start (ash x-min 3)))
;;                          (do* ((yy 0  (+ 1 yy)))
;;                               ((>= yy (ash y-size 3)))
;;                            (declare (type fixnum yy))
;;                            (do* ((xx 0 (+ 1 xx)))
;;                                 ((>= xx (ash x-size 3)))
;;                              (declare (type fixnum xx))
;;                              (dotimes (zz channels)
;;                                (setf (aref new-img yy xx zz)
;;                                      (aref img
;;                                            (+ yy y-start)
;;                                            (+ xx x-start)
;;                                            zz))))))
;;                        ;; ;; save img
;;                        ;; (with-open-file
;;                        ;;     (output filepath :element-type '(unsigned-byte 8)
;;                        ;;                      :direction :output :if-exists :supersede)
;;                        ;;   (png:encode new-img output))
;;                        ;; save pic
;;                        (save-packflag filepath
;;                                       new-packflag new-packline (ash y-size 3) (ash x-size 3)
;;                                       channels bit-depth new-img)
;;                        ;; (save-packflag (format nil "~A/~A.png" dir_ filename)
;;                        ;;                packflag packline height width
;;                        ;;                channels bit-depth img)
;;                        ;; ocr
;;                        (print (cl-tesseract:image-to-text filepath :lang "rus+eng"))
;;                        ;; Удаляем из списка кандидатов все точки обнаруженного
;;                        ;; края фигуры (cur-edge). Так как по определению края ищутся
;;                        ;; начиная с каждой точки-кандидата было бы избыточно удалять
;;                        ;; из кандидатов все точки заливки (cur-full)
;;                        (remove-from-candidates cur-edge rest-of-candidates)
;;                        ;; (print (length rest-of-candidates))
;;                        ;; Хвостовая рекурсия
;;                        (rec-elim rest-of-candidates)))))
;;         ;; Итерируемся по точкам-кандидатам, ища края и удаляя из
;;         ;; оставшихся кандидатов тех, что совпадают с найденными краями
;;         ;; пока кандидатов не останется. На выходе - edges
;;         (rec-elim (start-fig-candidates-8x8 packflag pf-height pf-width)))
;;       ;; dbgout: Количество фигур
;;       ;; (format t "~%len edges : ~A ~%" (length edges))
;;       ;; Выводим картинки
;;       ;; (save-packflag (format nil "~A/~A+.png" dir (symbol-name (gensym)))
;;       ;;                packflag packline height width
;;       ;;                channels bit-depth img)
;;       ;;
;;       )))


;; TODO:
;; слить соседние фигуры
;;   - сделать алгоритм поиска соседних блоков
;;   - сделать алгоритм слияния
;; отправить в тессеракт
;; получить распознанные блоки и их координаты
