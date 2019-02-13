;;; guess word game for emacs !!

(defconst VERSION "0.0.1")

(defgroup guess-word nil
  "Guess word for ESL "
  :version "0.0.1"
  :prefix "guess-word-")

(defcustom guess-word-mask
  "-"
  "guess word mask "
  :group 'guess-word
  :type 'string)

(defun random-word-map-string (fn str)
  (let ((index 0))
    (mapcar
     (lambda (ele)
       (setq index (1+ index))
       (funcall fn ele (1- index)))
     str)))

;;; random word by insert mask, pure function not random
(defun random-word (word)
  (s-join "" (random-word-map-string
   (lambda (char index)
     (cond
      ((oddp index) (string char))
      (t guess-word-mask)))
   word)))

(defun guess ()
  (with-current-buffer
      (get-buffer-create (format "*guess-word %s *" VERSION))
    (erase-buffer)
    (guess-word-mode)
    (switch-to-buffer-other-window (current-buffer))

    (save-excursion
      (insert (format "%s\n" (random-word "hello")))
      (insert "welcome word"))
    )
  )

;;; autoload
(define-derived-mode guess-word-mode nil "GSW"
  "The guss word game major mode"
  :group "guess-word")
