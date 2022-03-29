(require 's)
(require 'f)

;; 文件名
(defvar-local guess-word-paragraph-filename
  (s-concat (file-name-directory (f-this-file)) "paragraph.txt"))

;; map对象
(defvar-local guess-word-paragraph-map-in-memory
  (make-hash-table :test 'equal))

(defun guess-word-paragraph-init ()
  (with-temp-buffer
    (insert-file-contents guess-word-paragraph-filename)
    (while (not (eobp))
      (let* ((line (thing-at-point 'line))
             (tup (s-split "|" line))
             (word (s-trim (nth 0 tup)))
             (en (s-trim (nth 1 tup)))
             (zh (s-trim (nth 2 tup))))
        (puthash word (list :word word :en en :zh zh) guess-word-paragraph-map-in-memory)
        (forward-line 1))
      )))

(defun guess-word-paragraph-query (word)
  (gethash word guess-word-paragraph-map-in-memory))

(provide 'guess-word-paragraph)

;; guess-word-paragraph ends here
