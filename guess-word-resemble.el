(require 's)
(require 'f)

;; 内存hash
(defvar-local guess-word-resemble-map-in-memory
  (make-hash-table :test 'equal))

(defvar-local guess-word-resemble-filepath (s-concat (file-name-directory (f-this-file)) "resemble.txt"))
;; 将词典读取到内存中
(defun guess-word-resemble-init ()
  (let (
        (start 0)
        (end 0)
        (first-line-end 0)
        )

    (with-temp-buffer
      (insert-file-contents guess-word-resemble-filepath)
      (while (search-forward-regexp "%[^%]*\n" nil t)
        (setq start (match-beginning 0))
        (setq end (point))
        (save-excursion
          ;; 首字母为 % 跳过
          (goto-char (1+ start))
          (setq first-line-end (line-end-position))
          (setq content (buffer-substring-no-properties start end))
          (mapcar
           #'(lambda (word)
               (puthash (s-trim word) content guess-word-resemble-map-in-memory))
           (s-split "," (buffer-substring-no-properties (point) first-line-end))))))))


(defun guess-word-resemble-query (word)
  (gethash word guess-word-resemble-map-in-memory))

(provide 'guess-word-resemble)

;;; guess-word-resemble ends here
