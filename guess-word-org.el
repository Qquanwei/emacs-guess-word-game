(require 'org)
(require 'outline)

(defcustom guess-word-org-file
  (f-expand "./guess-word.org" (file-name-directory (f-this-file)))
  "guess-word output org file"
  :group 'guess-word
  :type 'file
  )

(defun guess-word-current-buffer-to-org (word)
  "Save current word to org file"
  (let (
        (content (buffer-substring-no-properties (point-min) (point-max)))
        (headlines (make-hash-table :test 'equal))
        )
    (with-current-buffer
        (find-file-noselect guess-word-org-file nil nil)
      (org-mode)

      (let ((ast (org-element-parse-buffer))
            (done nil))
        (org-element-map ast 'headline
          (lambda (ele)
            (let ((headlinetext (car (plist-get (plist-get ele 'headline) ':title)))
                  (content-region (plist-get ele 'headline))
                  (hasset nil))
              (when (equal word headlinetext)
                (save-excursion
                  (narrow-to-region (plist-get content-region ':begin) (plist-get content-region ':end))
                  (org-element-set-contents ele content)
                  (org-element-set-element ele (org-element-put-property ele :todo-keyboard "DONE"))
                  (delete-region (point-min) (point-max))
                  (goto-char (point-min))
                  (insert (org-element-interpret-data ele))
                  (widen))
                (setq done t)
                ))))
        (unless done
          (goto-char (point-max))
          (insert (format "\n* TODO %s \n %s" word content)))
        (save-buffer)
        (goto-char (point-max))
        (current-buffer)))))

(provide 'guess-word-org)
;;; guess-word-org ends here . xx
