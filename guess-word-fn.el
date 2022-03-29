;; 修改一个plist记录
(defun guess-word-plist-map (key plist fn)
  (plist-put plist key (funcall fn (plist-get plist key))))

;; 修改一个hash记录
(defun guess-word-hash-map (key hash fn)
  (when (gethash key hash)
    (let ((v (gethash key hash)))
      (puthash key (funcall fn v) hash))))

(provide 'guess-word-fn)
;;; guess-word-fn ends here
