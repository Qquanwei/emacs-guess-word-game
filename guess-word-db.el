(require 'f)
(require 'guess-word-fn)

;; 数据库文件
;; 字段设计：eng, text, times(正确次数), addtime, lasttime
(defcustom guess-word-db-file
  (s-concat (file-name-directory (f-this-file)) "db.txt")
  "Database file path"
  :group 'guess-word
  :type 'string)

;; 定义内存map
(defvar-local guess-word-db-map-in-memory nil)

;; 初始化，确保文件存在, 如果存在读取db到内存中
(defun guess-word-db-init ()
  "init guess word db"
  (unless (f-exists-p guess-word-db-file)
    (f-touch guess-word-db-file)
    (f-write (prin1-to-string (make-hash-table :test 'equal)) 'utf-8 guess-word-db-file)
    )
  (guess-word-db-read-file-to-memory)
  )

(defun guess-word-db-read-file-to-memory ()
  (setq guess-word-db-map-in-memory
        (read (f-read-text guess-word-db-file 'utf-8)))
  (if (hash-table-p guess-word-db-map-in-memory)
      (message "加载DB成功")
    (message "DB文件损坏")
    ))

(defun guess-word-db-memory-to-file ()
  (f-write (prin1-to-string guess-word-db-map-in-memory)
           'utf-8
           guess-word-db-file))

(defun guess-word-db-query-word (word)
  "query word return record"
  (gethash word guess-word-db-map-in-memory))

;; 增加一次错误计数或插入
(defun guess-word-db-increase-word (word)
  (let (
        (current (gethash word guess-word-db-map-in-memory))
        (record "")
        )
    (if current
        (progn
          (plist-put current ':times
                     (1+ (plist-get current ':times)))
          (plist-put current ':lasttime
                     (current-time))
          (setq record current))
      (setq record (list :word word :times 1 :addtime (current-time) :lasttime (current-time)))
      )
    (puthash word record guess-word-db-map-in-memory)))

;; 减少一次错误计数
(defun guess-word-db-decrease-word (word)
  (let (
        (current (gethash word guess-word-db-map-in-memory))
        (record "")
        )
    (when current
      (unless (eq (plist-get current ':times 0))
        (progn
          (plist-put current ':times
                     (1- (plist-get current ':times)))
          (plist-put current ':lasttime
                     (current-time))
          (puthash word current guess-word-db-map-in-memory)
          )))))

(defun guess-word-db-update-context (context)
  (puthash 'context context guess-word-db-map-in-memory))

;; 记录用户输入的例句
(defun guess-word-db-update-paragraph (word paragraph)
  (guess-word-hash-map
   word
   guess-word-db-map-in-memory
   #'(lambda (plist) (plist-put plist ':paragraph paragraph))))


(provide 'guess-word-db)

;;; guess-word-db.el ends here
