(require 'uiop)

(defvar *pos* '(1 1))

(defun reset-pos! (v) (setf *pos* v))

(defun read-moves (fp)
  (with-open-file (stream fp)
    (loop for line = (read-line stream nil) while line collect line)))

(defvar pad1 #2A((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(defvar pad2 #2A((NIL NIL 1 NIL NIL)
                 (NIL 2 3 4 NIL)
                 (5 6 7 8 9)
                 (NIL "A" "B" "C" NIL)
                 (NIL NIL "D" NIL NIL)))

(defun pad-at (p pos)
  (let ((d (nth 1 (array-dimensions p))))
    (when (and (< -1 (nth 0 pos) d) (< -1 (nth 1 pos) d))
      (aref p (nth 0 pos) (nth 1 pos)))))

(defun make-move (move pos)
  (let ((x (nth 0 pos)))
    (let ((y (nth 1 pos)))
      (cond ((string= move "U") (list (- x 1) y))
            ((string= move "D") (list (+ x 1) y))
            ((string= move "L") (list x (- y 1)))
            ((string= move "R") (list x (+ y 1)))))))

(defun pr (p) (if (stringp p) p (write-to-string p)))

(defun commit-move (move p) (setf *pos* move) (pr p))

(defun simulate-move (acc cur pad)
  (concatenate 'string acc
    (reduce (lambda (button m)
              (let ((uncommitted (make-move m *pos*)))
                (let ((p (pad-at pad uncommitted)))
                  (if p (commit-move uncommitted p) button))))
            cur :initial-value (pr (pad-at pad *pos*)))))

(defun solve-file (fp)
  (let ((moves (read-moves fp)))
    (defun solve (pad) (reduce (lambda (acc cur) (simulate-move acc cur pad)) moves :initial-value ""))
    (format t "Solving for file : ~A~%" fp)
    (format t "part 1: ~A~%" (solve pad1))
    (reset-pos! '(2 0))
    (format t "part 2: ~A~%" (solve pad2))))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (loop for fp in args
          do (solve-file fp))))

(main)
