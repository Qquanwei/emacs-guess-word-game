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


(defcustom guess-word-dictionarys
  '("四级词汇.txt")
  "guess word dictionary paths"
  :group 'guess-word
  :type 'list)

(defvar-local guess-word-mask-condition 'oddp)

(defface guess-word-headline
  '((t (:inherit bold)))
   :group 'guess-word)

(defface guess-word-definement
  '((t (:inherit italic)))
   :group 'guess-word)

(defun random-word-map-string (fn str)
  (let ((index 0))
    (mapcar
     (lambda (ele)
       (setq index (1+ index))
       (funcall fn ele (1- index)))
     str)))

(defun guess-word-success (word)
  (setq guess-word-current-result t)
  (message "success"))

(defun guess-word-failed (word)
  (setq guess-word-current-result nil)
  (message "wrong"))

(defun guess-word-next ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((pair (guess-word-esl-line-to-pair (guess-word-extract-word))))
    (guess-word-insert-word (car pair) (cdr pair))))

(defun guess-word-next-maybe-wrong ()
  (interactive)
  (if guess-word-current-result
      (guess-word-next)
    (save-excursion
      (setq guess-word-current-result t)
      (goto-char (point-min))
      (delete-region (point-min) (line-end-position))
      (insert (car guess-word-current-context)))))

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
  (setq guess-word-current-result nil)
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
    (switch-to-buffer-other-window (current-buffer))
    (guess-word-mode)
    (guess-word-next)))


(defun guess-word-esl-line-to-pair (line)
  (let* (
         (index (s-index-of " " line))
         (prefix (s-left index line)))
    `(,prefix ,@(substring line index -1))))

(defun guess-word-extract-word ()
  (with-temp-buffer
    (setq-local guess-word-current-dictionary (car guess-word-dictionarys))
    (insert-file-contents guess-word-current-dictionary)
    (let ((line (random (line-number-at-pos (point-max)))))
      (forward-line line)
      (thing-at-point 'line t))))

(defvar guess-word-mode-map (make-sparse-keymap)
  "Keymap for guess-word-mode")

(progn
  (define-key guess-word-mode-map (kbd "C-<return>") 'guess-word-next-maybe-wrong)
  (define-key guess-word-mode-map (kbd "<return>") 'guess-word-submit))

(define-derived-mode guess-word-mode nil "GSW"
  "The guss word game major mode"
  :group "guess-word"
  (setq-local guess-word-current-result nil)
  (setq-local guess-word-current-context nil)
  (overwrite-mode)
  (use-local-map guess-word-mode-map))

(provide 'guess-word-mode)
