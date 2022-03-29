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
(require 'guess-word-db)
(require 'guess-word-resemble)
(require 'guess-word-paragraph)
(require 'guess-word-fn)

;; 版本号
(defconst guess-word-VERSION "1.0.2")

;; 文件夹名字
(defconst DIRNAME (file-name-directory (f-this-file)))

(defgroup guess-word nil
  "Guess word for ESL "
  :group 'language
  :version guess-word-VERSION
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

(defun guess-word-next ()
  (let ((word (plist-get guess-word-current-context ':word)))
    (save-excursion
      (if (plist-get guess-word-current-context ':result)
          (progn
            (guess-word-plist-map ':score guess-word-current-context '1+)
            (guess-word-db-increase-word word))
        (guess-word-db-decrease-word word))
      (guess-word-plist-map ':total guess-word-current-context '1+)
      (guess-word-refresh-header-line)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (erase-buffer)
        (let ((pair (guess-word-esl-line-to-pair (guess-word-extract-word))))
          (guess-word-insert-word (car pair) (cdr pair)))))))

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
    (insert (plist-get guess-word-current-context ':word))))

;; 提交按键穷举所有状态。guess-word-current-context.submit
;; 0. 正常待用户输入
;; 1. 显示正确结果，单词区域只读。跳转到例句输入。 用户按下回车后，检查结果正确, 此时让单词变成只读, 跳转到例句输入(如果有), 如果没有，切换下一个单词，并回到状态0
;; 2. 第一次错误。用户按下回车后，检查结果错误, 并且为第一次
;; 3. 错误后展示正确答案。用户在错误后没有修改任何东西的情况下第二次按下回车键，此时应该填充正确答案 （如果用户此时进行了修改，例如退格，回到状态 0）, 并回到状态5
;; 4. 展示正确例句。 用户输入例句按下回车了，变成状态4， 此时展示正确的例句, 如果在状态4按下回车，切换到下一个单词，并重置状态0.
;; 5. 在单词显示正确答案的情况下输入例句。5和1大致相同功能，只是来源不同.
;; 0 -回车-> 1 -> 0
;; 0 -回车-> 1 -> 4 -> 0
;; 0 -回车-> 2 -> 3 -> 5 -> 4 -> 0
;; 0 -回车-> 2 -> 0
(defun guess-word-submit ()
  "submit input answer"
  (interactive)
  (save-excursion
    (goto-char 0)
    (let* ((status (plist-get guess-word-current-context ':submit))
           (word (thing-at-point 'word))
           (correct (string= (plist-get guess-word-current-context ':word) word)))
      (cond
       ;; 当前处于状态0
       ((equal status 0) (plist-put guess-word-current-context ':submit (if correct 1 2)))
       ((or (equal status 1) (equal status 5)) (plist-put guess-word-current-context ':submit (if (guess-word-paragraph-query word) 4 0)))
       ;; 2 -> 0 在 guess-word-keyboard-hook 内处理
       ;; 这里只处理 2 -> 3
       ((equal status 2) (plist-put guess-word-current-context ':submit 3))
       ((equal status 3) (plist-put guess-word-current-context ':submit 5))
       ((equal status 4) (plist-put guess-word-current-context ':submit 0))
       )))

  ;; 根据当前状态进行展示
  (let ((status (plist-get guess-word-current-context ':submit)))
    (cond
     ((equal status 0) (guess-word-next))
     ((or (equal status 1) (equal status 5)) (progn (freezen-word) (goto-paragraph) (guess-word-show-resemble)))
     ((equal status 3) (progn (guess-word-fill-the-answer) (guess-word-show-resemble)))
     ((equal status 4) (fill-the-paragraph-answer))
     )
    (guess-word-refresh-header-line)))

(defun freezen-word ()
  (goto-char 0)
  (put-text-property (line-beginning-position) (line-end-position) ':read-only t))

;; 跳转到输入例句的地方
(defun goto-paragraph ()
  (search-forward-regexp "翻译:"))

(defun fill-the-paragraph-answer ()
  (let* ((word (plist-get guess-word-current-context ':word))
         (ans (plist-get (guess-word-paragraph-query word) ':en)))
    (save-excursion
      (search-forward-regexp "例句对照:")
      (insert (propertize ans 'read-only t)))
    ))

;;; random word by insert mask, pure function not random
(defun random-word (word)
  (s-join "" (random-word-map-string
              (lambda (char index)
                (cond
                 ((funcall guess-word-mask-condition index) (string char))
                 (t guess-word-mask)))
              word)))

(defun guess-word-show-resemble ()
  (let ((word (plist-get guess-word-current-context ':word)))
    (save-excursion
      (goto-char (buffer-end 1))
      (when (guess-word-resemble-query word)
        (insert (propertize (format "\n\n辨析%s" (guess-word-resemble-query word)) 'read-only t))))))

(defun guess-word-insert-word (word definement)
  (guess-word-plist-map ':result guess-word-current-context 'ignore)
  (plist-put guess-word-current-context ':word word)

  (save-excursion
    (insert (propertize (format "%s\n\n" (random-word word) ) 'font-lock-face 'guess-word-headline))
    (insert (propertize definement 'read-only t 'font-lock-face 'guess-word-definement))
    (when (guess-word-paragraph-query word)
      (insert (propertize
               (format "\n\n\n例句: %s" (plist-get (guess-word-paragraph-query word) ':zh))
               'read-only t
               'font-lock-face 'guess-word-headline
               ))
      (insert (propertize
               (format "\n翻译: ")
               'font-lock-face 'guess-word-headline
               ))
      (insert (propertize
               (format "\n例句对照:")))
      )
    ))

(defun guess-word ()
  (interactive)
  (let ((buffer-name (format "*guess-word %s*" guess-word-VERSION)))
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
  (define-key guess-word-mode-map (kbd "C-s") 'guess-word-save)
  (define-key guess-word-mode-map (kbd "<return>") 'guess-word-submit))

(setq guess-word-mode-font-lock
      '(("^[a-zA-Z]+$" . guess-word-headline)
        ("^ ." . guess-word-definement)))

(defun guess-word-save ()
  (interactive)
  (guess-word-db-update-context guess-word-current-context)
  (guess-word-db-memory-to-file)
  (message "保存成功"))

(defun guess-word-refresh-header-line ()
  (let* ((status (plist-get guess-word-current-context ':submit))
         (word (plist-get guess-word-current-context ':word))
         (hasparagraph (guess-word-paragraph-query word))
         (text
          (cond
           ((equal status 0) "检查")
           ((equal status 1) (if hasparagraph "正确，输入例句" "正确"))
           ((equal status 2) "错误, 回车查看结果")
           ((equal status 3) "回车输入例句")
           ((equal status 4) "查看下一个单词")
           ((equal status 5) "输入例句")
           )))
    (setq-local
     header-line-format
     (substitute-command-keys
      (format
       "[%s][%s/%s] %s `\\[guess-word-submit]' 切换词库`\\[guess-word-switch-dictionary]' 保存进度 \\[guess-word-save]"
       (car guess-word-dictionarys)
       (plist-get guess-word-current-context ':score)
       (plist-get guess-word-current-context ':total)
       text
       )))))

(define-derived-mode guess-word-mode nil "GSW"
  "The guss word game major mode"
  :group 'guess-word
  (guess-word-db-init)
  (guess-word-resemble-init)
  (guess-word-paragraph-init)
  (let ((context (guess-word-db-query-word 'context)))
    (if context
        (setq-local guess-word-current-context context)
      (setq-local
       guess-word-current-context
       (list :total 0 :score 0 :result nil :word nil :submit 0))))
  (plist-put guess-word-current-context ':submit 0)
  (setq font-lock-defaults '(guess-word-mode-font-lock))
  (guess-word-refresh-header-line)
  (overwrite-mode)
  (use-local-map guess-word-mode-map)
  ;; 这个hook监听文件变更
  (add-hook 'after-change-functions
            'guess-word-keyboard-hook
            0 t))

(defun guess-word-keyboard-hook (&rest char)
  (when (equal 2 (plist-get guess-word-current-context ':submit))
    (plist-put guess-word-current-context ':submit 0)
    (guess-word-refresh-header-line)))

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
