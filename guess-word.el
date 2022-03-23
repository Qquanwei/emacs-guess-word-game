;;; guess-word.el --- guess word game for emacs !

;; Author: quanwei9958@126.com
;; Version: 1.0.0
;; Package-Requires: ((s "1.12.0") (f "0.20.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Usage
;; M-x guess-word

;;; Code:
(require 's)
(require 'f)
(require 'subr-x)

(defconst VERSION "1.0.1")

(defconst DIRNAME (file-name-directory (f-this-file)))

(defgroup guess-word nil
  "Guess word for ESL "
  :group 'language
  :version VERSION
  :prefix "guess-word-"
  :link '(url-link "https://github.com/Qquanwei/emacs-guess-word-game"))

(defcustom guess-word-mask
  "-"
  "Guess word mask."
  :group 'guess-word
  :type 'string)

(defcustom guess-word-dictionarys
  '("CET4_edited.txt" "CET6_edited.txt" "TOEFL.txt")
  "Guess word dictionary paths."
  :group 'guess-word
  :type 'list)

(defcustom guess-word-show-pronunciation
  nil
  "Show pronunciation of word if it exists in dictionary when non-nil."
  :group 'guess-word
  :type 'boolean)

(defvar-local guess-word-mask-condition 'cl-oddp)
(defvar-local guess-word-total 0)
(defvar-local guess-word-score 0)

(defface guess-word-headline
  '((t (:inherit bold)))
  "Guess word headline face"
  :group 'guess-word)

(defface guess-word-definement
  '((t (:inherit italic)))
  "Guess word definement face"
  :group 'guess-word)

(defun random-word-map-string (fn str)
  "Map STR with index to FN."
  (let ((index 0))
    (mapcar
     (lambda (ele)
       (setq index (1+ index))
       (funcall fn ele (1- index)))
     str)))

(defun guess-word-success ()
  "The input is success"
  (unless (member
           "result"
           (hash-table-keys guess-word-current-context))
    (puthash "result" t guess-word-current-context))
  (guess-word-next)
  (setq-local guess-word-curans-error-times 0)
  (message "success"))

(defun guess-word-failed (word)
  (if (or (not word) (= guess-word-curans-error-times 1))
      (progn
        (guess-word-fill-the-answer)
        (puthash "result" nil guess-word-current-context))
    (message "wrong")
    (setq-local guess-word-curans-error-times (1+ guess-word-curans-error-times))
    (guess-word-refresh-header-line)
    ))

(defun guess-word-next ()
  (save-excursion
    (when (gethash "result" guess-word-current-context)
      (setq  guess-word-score (1+ guess-word-score)))
    (setq  guess-word-total (1+ guess-word-total))
    (setq guess-word-curans-error-times 0)
    (guess-word-refresh-header-line)
    (clrhash guess-word-current-context)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (erase-buffer)
      (let ((pair (guess-word-esl-line-to-pair (guess-word-extract-word))))
        (guess-word-insert-word (car pair) (cdr pair))))))

(defun guess-word-switch-dictionary ()
  "切换词库"
  (interactive)
  (setq
   guess-word-dictionarys
   (append (cdr guess-word-dictionarys) (list (car guess-word-dictionarys))))
  (guess-word-refresh-header-line)
  (message (format "switch to %s!" (car guess-word-dictionarys))))

(defun guess-word-fill-the-answer ()
  "填充正确答案."
  (save-excursion
    (goto-char (point-min))
    (delete-region (point-min) (line-end-position))
    (insert (gethash "word" guess-word-current-context))))

(defun guess-word-submit ()
  "submit input answer"
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((word (thing-at-point 'word)))
      (if (string= (gethash "word" guess-word-current-context) word)
          (guess-word-success)
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
  (puthash "word" word  guess-word-current-context)

  (save-excursion
    (insert (format "%s\n\n" (random-word word)))
    (let ((begin (point)))
      (insert definement)
      (add-text-properties (- begin 2) (point) '(read-only t)))))

;;;###autoload
(defun guess-word ()
  (interactive)
  (let ((buffer-name (format "*guess-word %s*" VERSION)))
    (when (not (get-buffer buffer-name))
      (with-current-buffer
          (get-buffer-create buffer-name)
        (add-text-properties 1 (point-max) '(read-only nil))
        (guess-word-mode)
        (guess-word-next)))
    (switch-to-buffer-other-window (get-buffer buffer-name))))

(defun guess-word-esl-line-p (line)
  (not (or (s-matches? "^ +$" line)
           (s-matches? "^[a-zA-Z]+\\." line))))

(defun guess-word-esl-line-to-pair (line)
  (let* (
         (index (s-index-of " " line))
         (prefix (s-left index line)))
    `(,prefix ,@(substring line index -1))))

(defun guess-word-extract-word ()
  (with-temp-buffer
    (setq-local
     guess-word-current-dictionary
     (if (f-absolute-p (car guess-word-dictionarys))
         (car guess-word-dictionarys)
       (expand-file-name (car guess-word-dictionarys) DIRNAME) ))

    (insert-file-contents guess-word-current-dictionary)
    (let ((line (random (line-number-at-pos (point-max)))))
      (forward-line line)
      (if (guess-word-esl-line-p (thing-at-point 'line t))
          (let ((line (thing-at-point 'line t)))
            (if guess-word-show-pronunciation
                line
              (replace-regexp-in-string "\\[.*?\] " "" line 'fixedcase nil)))
        (guess-word-extract-word)))))

(defvar guess-word-mode-map (make-sparse-keymap)
  "Keymap for guess-word-mode")

(progn
  (define-key guess-word-mode-map (kbd "C-r") 'guess-word-switch-dictionary)
  (define-key guess-word-mode-map (kbd "<return>") 'guess-word-submit))

(setq guess-word-mode-font-lock
      '(("^[a-zA-Z]+$" . guess-word-headline)
        ("^ ." . guess-word-definement)))

(defun guess-word-refresh-header-line ()
  (setq-local
   header-line-format
   (substitute-command-keys
    (if (eq guess-word-curans-error-times 0)
        (format
         "[%s][%s/%s] 检查 `\\[guess-word-submit]' 切换词库`\\[guess-word-switch-dictionary]'"
         (car guess-word-dictionarys) guess-word-score guess-word-total)
      (format
       "[%s][%s/%s] 错误,查看答案 `\\[guess-word-submit]'"
       (car guess-word-dictionarys) guess-word-score guess-word-total)
      )
    )))

(define-derived-mode guess-word-mode nil "GSW"
  "The guss word game major mode"
  :group 'guess-word
  (setq-local guess-word-current-result nil)
  ;; 错误次数，如果连续按下submit错误次数为2次，则自动展示正确答案
  (setq-local guess-word-curans-error-times 0)
  (setq-local
   guess-word-current-context
   (make-hash-table :test 'equal))
  (setq-local guess-word-total 0)
  (setq-local guess-word-score 0)
  (setq font-lock-defaults '(guess-word-mode-font-lock))
  (guess-word-refresh-header-line)
  (overwrite-mode)
  (use-local-map guess-word-mode-map)
  ;; 这个hook可以监听普通按键的按下,对于<return><backward>这类特殊按键不会触发
  (add-hook 'post-self-insert-hook
            '(lambda ()
               (setq guess-word-curans-error-times 0)
               (guess-word-refresh-header-line))
            0 t)
  )

(defun guess-word-add-dictionary-path (pName)
  "Added local dictionary to guess-word search list"
  (interactive "fAdded Path: ")
  (unless (member pName guess-word-dictionarys)
    (add-to-list 'guess-word-dictionarys pName ))
  (message (format "%s Added!" pName)))

(defun guess-word-delete-dictionary-path ()
  "Delete dictionary from guess-word search list"
  (interactive)
  (let ((item (completing-read "" guess-word-dictionarys)))
    (setq guess-word-dictionarys (remove item guess-word-dictionarys ) )))

(provide 'guess-word)

;;; guess-word.el ends here
