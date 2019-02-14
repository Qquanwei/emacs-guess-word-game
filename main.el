;;; guess word game for emacs !!

(require 's)

(defconst VERSION "0.0.1")

(defgroup guess-word nil
  "Guess word for ESL "
  :version "0.0.1"
  :prefix "guess-word-"
  :link '(url-link "https://github.com/Qquanwei/emacs-guess-word-game"))

(defcustom guess-word-mask
  "-"
  "guess word mask "
  :group 'guess-word
  :type 'string)

(defvar-local guess-word-mask-condition 'oddp)

;; (defface guess-word-headline
;;   '((t (:inherit bold)))
;;    :group 'guess-word)

;; (defface guess-word-definement
;;   '((t (:inherit italic)))
;;    :group 'guess-word)

(defun random-word-map-string (fn str)
  (let ((index 0))
    (mapcar
     (lambda (ele)
       (setq index (1+ index))
       (funcall fn ele (1- index)))
     str)))

(defun guess-word-success (word)
  (message "success"))

(defun guess-word-failed (word)
  (message "wrong"))

;;; submit your answer
(defun guess-word-submit ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((word (thing-at-point 'word)))
      (if (string= (car guess-word-current-context) word)
        (guess-word-success word)
        (guess-word-failed word)))))

;;; random word by insert mask, pure function not random
(defun random-word (word)
  (s-join "" (random-word-map-string
   (lambda (char index)
     (cond
      ((funcall guess-word-mask-condition index) (string char))
      (t guess-word-mask)))
   word)))

(defun guess-word-insert-word (word definement)
  (setq guess-word-current-context (list `(,@word)))
  (save-excursion
    (insert (format "%s\n\n" (random-word word)))
    (let ((begin (point)))
      (insert definement)
      (add-text-properties (- begin 2) (point) '(read-only t)))))

;;; autoload
(defun guess ()
  (with-current-buffer
      (get-buffer-create (format "*guess-word %s *" VERSION))
    (add-text-properties 1 (point-max) '(read-only nil))
    (erase-buffer)
    (switch-to-buffer-other-window (current-buffer))
    (guess-word-mode)
    (guess-word-insert-word "broad" "welcome word2")))

(defvar guess-word-mode-map (make-sparse-keymap)
  "Keymap for guess-word-mode")

(progn
  (define-key guess-word-mode-map (kbd "<return>") 'guess-word-submit))

(define-derived-mode guess-word-mode nil "GSW"
  "The guss word game major mode"
  :group "guess-word"
  (setq-local guess-word-current-context nil)
  (overwrite-mode)
  (use-local-map guess-word-mode-map))

(provide 'guess-word-mode)
