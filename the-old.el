(provide 'the-old)

(require 'json)
(require 'url)
(require 'shr)
(require 'pp)
(require 'cl)
;(require 'browse-url)


(defvar the-old-api-url "https://theoldreader.com/reader/api/0/")
(defvar the-old-api-login-url "https://theoldreader.com/accounts/ClientLogin?output=json")
(defvar the-old-api-login nil) 
(defvar the-old-api-passwd nil)
(defvar the-old-api-params "?output=json")
(defvar the-old-api-cmd
  '((:userinfo      . "user-info")
    (:token         . "token")
    (:login         . "accounts/ClientLogin")
    (:status        . "status")
    (:preferences   . "preference/list")
    (:folders       . "tag/list")
    (:subscriptions . "subscription/list")
    (:unread-count  . "unread-count")
    (:item-ids      . "stream/items/ids")
    (:items         . "stream/items/contents")
    (:stream        . "stream/contents")
    ))

(defvar folder-fields
  '((:id            . id)
    (:sortid        . sortid)))
(defvar subscription-fields
  '((:icon-url      . iconUrl)
    (:html-url      . htmlUrl)
    (:url           . url)
    (:timestamp     . firstitemmsec)
    (:sortid        . sortid)
    (:categories    . categories)
    (:title         . title)
    (:id            . id)))
(defvar unread-fields
  '((:timestamp     . newestItemTimestampUsec)
    (:count         . count)
    (:id            . id)))
(defvar article-fields
  '((:crawl-time    . crawlTimeMsec)
    (:timestamp     . timestampUsec)
    (:id            . id)
    (:categories    . categories)
    (:title         . title)
    (:published     . published)
    (:updated       . updated)
    (:canonical     . canonical)
    (:alternate     . alternate)
    (:summary       . (summary
		       (:direction . direction)
		       (:content . content)))
    (:author        . author)
    (:annotations   . annotations)
    (:likingUsers   . likingUsers)
    (:likingUsersCount . likingUsersCount)
    (:comments      . comments)    
    (:origin        . (origin
		       (:origin-url . htmlUrl)
		       (:origin-title . title)
		       (:stream-id . streamId)))
    )
  )
  
;; google security token
(defvar token ())
(defvar the-old-folders ())
(defvar the-old-subscriptions ())
(defvar the-old-articles ())
(defvar the-old-articles-continuation ())
(defvar the-old-unread ())

;; buffer names
(defvar the-old-buffer "the-old-reader")

(defvar the-old-menu-mode-map nil
  "Keymap for get-messages-menu-mode")

(defvar the-old-menu-sort-key nil
  "sort packages by key")

;; =================================================================================================
;; menu 
;; =================================================================================================

(defvar get-messages-menu-mode-hook nil
  "Hooks to run after get-messages menu init.")

(defvar get-messages-menu-mode-map nil
  "Keymap for get-messages-menu-mode")

(defvar get-messages-menu-sort-key nil
  "sort packages by key")

;; folders view columns start positions
(defconst menu-folder-columns
  '(("Date"         . 1)
    ("Name"         . 24)
    ("Unread"       . 100))
  "An alist of (NAME . COLUMN) entries.")
(defconst menu-subscription-columns
  '(("Date"         . 1)
    ("Title"        . 32)
    ("Unread"       . 100))
  "An alist of (NAME . COLUMN) entries.")
(defconst menu-article-columns
  '(("Date"         . 1)
    ("Subscription" . 8)
    ("Title"        . 32))
  "An alist of (NAME . COLUMN) entries.")


;; =================================================================================================
;; api post / get commands
;; =================================================================================================

;; get api rest url by command name (see the-old-api-cmd alist for details)
(defun get-cmd-url (cmd &optional params)
  "Construct api url from command symbol and additional parameters"
  (let ((the-old-api-cmd-part (cdr (assoc cmd the-old-api-cmd))))
    (concat
     ; base url
     the-old-api-url
     ; particular command part
     the-old-api-cmd-part
     ; common query parameters
     the-old-api-params
     ; additional parameters for particular query
     params
     )))

(defun api-get (addr)
  (with-current-buffer
      (let ((url-request-method "GET")
	    (url-request-extra-headers
	     `(
	       ("Content-Type" . "application/x-www-form-urlencoded")
	       ("Authorization" . ,(concat "GoogleLogin auth=" token)))))
	(url-retrieve-synchronously addr))
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (decode-coding-string (buffer-string) 'utf-8)))

(defun test (addr)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/x-www-form-urlencoded")
	   ("Authorization" . ,(concat "GoogleLogin auth=" token)))))
    (url-retrieve-synchronously addr)))

(defun api-post (addr args)
  (with-current-buffer
      (let ((url-request-method "POST")
	    (url-request-extra-headers
	     '(("Content-Type" . "application/x-www-form-urlencoded")))
	    (url-request-data
	     (mapconcat (lambda (arg)
			  (concat (url-hexify-string (car arg))
				  "="
				  (url-hexify-string (cdr arg))))
			args
			"&")))
        (url-retrieve-synchronously addr))
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (buffer-string)))

(defun api-query (cmd &optional params)
  "generic api query"
  (json-read-from-string
   (api-get
    (get-cmd-url cmd params))))

(defun api-get-token ()
  (let ((client-login
	 (api-post the-old-api-login-url `(("client" . "the-old-emacs")
					   ("accountType" . "HOSTED_OR_GOOGLE")
					   ("service" . "reader")
					   ("Email" . ,the-old-api-login)
					   ("Passwd" . ,the-old-api-passwd)))))    
    (cdr (assoc 'Auth (json-read-from-string client-login)))))

;; =================================================================================================
;; helpers
;; =================================================================================================
(defun filter (f col)
  "classic filter function"
  (remove-if-not f col))

(defun alist-get (lst key)
  "plist-get for alist"
  (cdr (assoc key lst)))

(defun alist-set (lst key item)
  (if (null (assoc key lst))
      (append lst (list (cons :id key) (cons :item (list item))))))

(defun vec-to-list (vec)
  "convert vector to list"
  (append vec nil))

(defun date-diff (sec)
  (cond
   ((< sec 3600) (concat (number-to-string (/ sec 60)) " m"))
   ((< sec (* 24 3600)) (concat (number-to-string (/ sec 3600)) " h"))
   ((< sec (* 365 24 3600)) (concat (number-to-string (/ sec (* 24 3600))) " d"))
   (t (concat (number-to-string (/ sec (* 365 24 3600))) " y"))))


;; =================================================================================================
;; retrieve subscriptions structure
;; =================================================================================================
(defun refresh-structure ()
  "refresh folders and subscriptions structure"
  (let ((folders (vec-to-list (alist-get (api-query :folders) 'tags)))
	(subscriptions (vec-to-list (alist-get (api-query :subscriptions) 'subscriptions)))
	(unread (vec-to-list (alist-get (api-query :unread-count) 'unreadcounts))))
    (setq the-old-folders folders)
    (setq the-old-subscriptions subscriptions)
    (setq the-old-unread unread)
    ))

(defun refresh-container-items (cont)
  "refresh stream"
  (let ((art (api-query :stream (concat "&s=" (alist-get cont 'id) "&n=256"))))
    (setq the-old-articles (vec-to-list(alist-get art 'items)))
    (setq the-old-articles-continuation (alist-get art 'continuation))))


;; =================================================================================================
;; api objects parsing
;; =================================================================================================

(defun get-obj-by-id (id a-list)
  "get alist from a list of alists by 'id field"
  (let ((res
	 (filter
	  (lambda (f) (equal id (alist-get f 'id)))
	  a-list)))
    (if (null res)
	res
      (car res))))

(defun get-folder (id)
  "get folder by id"
  (get-obj-by-id id the-old-folders))
     
(defun get-subscription (id)
  "get subscription by id"
  (get-obj-by-id id the-old-subscriptions))

(defun get-article (id)
  "get article by id"
  (get-obj-by-id id the-old-articles))

(defun get-subscriptions-by-folder (folder)
  "get subscriptions by folder"
  (let ((f-id (folder-param :id folder)))
    (filter (lambda (s)
	      (not (null
		    (get-obj-by-id
		     f-id
		     (vec-to-list (subscription-param :categories s))))))
	    the-old-subscriptions)))

(defun get-unread-by-container (cont)
  "get unread count for particular folder or subscription"
  (let ((f-id (folder-param :id cont)))
    (get-obj-by-id f-id the-old-unread)))
  
;(defun get-value-by-param (param a-list param-dict)
;  "get value of alist by parameter name (it decoded by param-dict, see subscription-fields for example)"
;  (let ((field (cdr (assoc param param-dict))))
;    (cdr (assoc field a-list))))

(defun get-value-by-param (param a-list param-dict)
  "get value of alist by parameter name recursively (it decoded by param-dict, see subscription-fields for example)"
  (if-let ((p (caar param-dict))
	   (p-val (cdar param-dict)))
      (cond
       ((null param-dict) nil)
       ((listp p-val)
	(let ((ret (get-value-by-param param (cdr (assoc (car p-val) a-list)) (cdr p-val))))
	  (if (null ret)
	      (get-value-by-param param a-list (cdr param-dict))
	    ret)))
       ((equal p param) (cdr (assoc p-val a-list)))
       (t (get-value-by-param param a-list (cdr param-dict)))
       )
    )
  )

;(get-value-by-param- :id (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339") article-fields)
;(alist-get (alist-get (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339") 'summary) 'content)
;(cdr (assoc 'summary (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339")))


(defun folder-param (param folder)
  "get folder attribute by parameter name"
  (get-value-by-param param folder folder-fields))
  
(defun subscription-param (param subscription)
  "get subscription attribute by parameter name"
  (get-value-by-param param subscription subscription-fields))

(defun unread-param (param unread)
  "get unread count attribute by parameter name"
  (get-value-by-param param unread unread-fields))

(defun article-param (param article)
  "get article attribute by parameter name"
  (get-value-by-param param article article-fields))

(defun article-unread? (article)
  "is article unread?"
  (let ((categories (vec-to-list (article-param :categories article))))
    (null (filter (lambda (x) (string= x "user/-/state/com.google/read")) categories))))

;;
;; Keybindings
;;
(unless the-old-menu-mode-map
  (setq the-old-menu-mode-map (make-keymap))
  (suppress-keymap the-old-menu-mode-map)
  ;;
  ;; key bindings
  ;;
  (define-key the-old-menu-mode-map "n" 'next-line)
  (define-key the-old-menu-mode-map "p" 'previous-line)
  (define-key the-old-menu-mode-map "i" '(lambda () (interactive) (message (get-row-id))))
  (define-key the-old-menu-mode-map "W" '(lambda () (interactive)
					   (let ((addr (cdar
							(elt
							 (article-param :canonical (get-article (get-row-id)))
							 0))))
					     (kill-new addr)
					     (message addr))))
  (define-key the-old-menu-mode-map "w" '(lambda () (interactive)
					   (let ((addr (cdar
							(elt
							 (article-param :canonical (get-article (get-row-id)))
							 0))))
					     (browse-url addr))))
					     
  ;; sort columns
  ;(define-key the-old-menu-mode-map "1" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 0)))
  ;(define-key the-old-menu-mode-map "2" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 1)))
  ;(define-key the-old-menu-mode-map "3" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 2)))
  ;(define-key the-old-menu-mode-map "4" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 3)))
  ;(define-key the-old-menu-mode-map "5" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 4)))
  ;(define-key the-old-menu-mode-map "6" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 5)))
  ;(define-key the-old-menu-mode-map "7" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 6)))
  ;; toggle bHold
  ;(define-key the-old-menu-mode-map (kbd "SPC") 'menu-read-toggle)
  ;; show record info
  (define-key the-old-menu-mode-map (kbd "RET") '(lambda () (interactive) 
						 (let ((str (article-param :content (get-article (get-row-id)))))
						   (with-temp-buffer
						     (insert str)
						     (shr-render-buffer (current-buffer))))
						 (other-window 1)))
  (define-key the-old-menu-mode-map (kbd "e") '(lambda () (interactive) 
						 (let ((addr (cdar
							      (elt
							       (article-param :canonical (get-article (get-row-id)))
							       0))))
						   (with-temp-buffer
						     (insert (api-get addr))
						     (shr-render-buffer (current-buffer)))
						   (other-window 1))))
  
  ;; help
  ;(define-key the-old-menu-mode-map "h" 'menu-quick-help)
  ;; quit
  (define-key the-old-menu-mode-map "q" 'quit-window))


;(defun get-items-by-folder

;; folders example

; [((sortid . "ffffffffffffffffffffffff") (id . "user/-/state/com.google/starred")) ((sortid . "52e61550fea0e760b30000ae") (id . "user/-/label/Новая папка")) ((sortid . "573c0bcefea0e7b66e00004f") (id . "user/-/label/Clojure"))]

;; subscriptions example

; ((iconUrl . "//s.theoldreader.com/system/uploads/feed/picture/50e2/ea12/e721/ec9f/ff00/icon_1bf7.ico") (htmlUrl . "http://www.opennet.ru/opennews/") (url . "http://opennet.ru/opennews/opennews_all.rss") (firstitemmsec . "1464468506000") (sortid . "51d55f89d1716ce9550000fd") (categories . []) (title . "OpenNews.opennet.ru: Основная лента") (id . "feed/51d55f89d1716ce9550000fd"))
; ((iconUrl . "//s.theoldreader.com/system/uploads/feed/picture/50e2/ea0b/bd92/79f1/0300/icon_0383.ico") (htmlUrl . "http://avvakoum.livejournal.com/") (url . "http://avvakoum.livejournal.com/data/rss") (firstitemmsec . "1464518384000") (sortid . "51d55facd1716ca33a000047") (categories . []) (title . "Личный Опыт") (id . "feed/51d55facd1716ca33a000047"))

;; unread-count example

; ((newestItemTimestampUsec . "1464544031000000") (count . 1114) (id . "user/-/state/com.google/reading-list"))
; ((newestItemTimestampUsec . "1464518378000000") (count . 12) (id . "user/-/label/Новая папка"))
; ((newestItemTimestampUsec . "1464341733000000") (count . 4) (id . "feed/54be0848fea0e78a0c00040a"))

;;
;; Here are functions to parse rows on the screen in order to get their IDs, for instance
;;
(defun get-row-id ()
  "Get ID of active row in the table"
  (save-excursion
    (beginning-of-line)
    (propertize (if (looking-at " \\([^ \t]*\\)")
		    (match-string 1)) 'invisible nil)))



;; =================================================================================================
;; interface
;; =================================================================================================

;;
;; Part of code that print rows at main table
;;
(defun menu-folder-row (folder)
  "Prepare folder to show at main table"
  (let* ((id (folder-param :id folder))
	 (name (folder-param :id folder))
	 (unread (get-unread-by-container folder))
	 (count (if (null unread) 0 (unread-param :count unread)))
	 (timestamp (if (null unread) 0 (/ (string-to-number (unread-param :timestamp unread)) 1000000)))
	 (date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
    (let ((face (cond
		 ((> count 0) 'font-lock-comment-face)
		 (t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize id 'invisible t))
      ; date
      (indent-to (alist-get menu-folder-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; name
      (indent-to (alist-get menu-folder-columns "Title") 2)
      (insert (propertize name 'font-lock-face face))
      ; unread count
      (indent-to (alist-get menu-folder-columns "Unread") 2)
      (insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      )))

(defun menu-subscription-row (subscription)
  "Prepare subscription to show at main table"
  (let* ((id (subscription-param :id subscription))
	 (title (subscription-param :title subscription))
	 (unread (get-unread-by-container subscription))
	 (count (if (null unread) 0 (unread-param :count unread)))
	 ;(timestamp (/ (string-to-number (subscription-param :timestamp subscription)) 1000000))
	 ;(date (format-time-string "%F %T" (seconds-to-time timestamp)))
	 (timestamp (if (null unread) 0 (/ (string-to-number (unread-param :timestamp unread)) 1000000)))
	 (date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
    (let ((face (cond
		((= count 0) 'font-lock-comment-face)
		(t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize id 'invisible t))
      ; date
      (indent-to (alist-get menu-subscription-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; name
      (indent-to (alist-get menu-subscription-columns "Title") 2)
      (insert (propertize title 'font-lock-face face))
      ; unread count
      (indent-to (alist-get menu-subscription-columns "Unread") 2)
      (insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      )))

(defun menu-article-row (article)
  "Prepare article to show at main table"
  (let* ((id (article-param :id article))
	 (title (article-param :title article))
	 (origin-title
	  (let ((ot (article-param :origin-title article)))
	    (substring ot 0
		       (min
			(length ot)
			(- (alist-get menu-article-columns "Title") (alist-get menu-article-columns "Subscription") 2)))))
	 (timestamp (article-param :published article))
	 ;(date (format-time-string "%F %T" (seconds-to-time timestamp)))
	 (date (date-diff (- (floor (time-to-seconds (current-time))) timestamp)))
	 (face (cond
		((not (article-unread? article)) 'font-lock-comment-face)
		(t 'default)))
	 )
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize id 'invisible t))
      ; date
      (indent-to (alist-get menu-article-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; subscription title
      (indent-to (alist-get menu-article-columns "Subscription") 2)
      (insert (propertize origin-title 'font-lock-face face))      
      ; name
      (indent-to (alist-get menu-article-columns "Title") 2)
      (insert (propertize title 'font-lock-face face))
      ; unread count
      ;(indent-to (alist-get menu-article-columns "Unread") 2)
      ;(insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      ))

(defun get-list-folders ()
  "Get folders and print them into table"
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((items the-old-folders))
      (let ((selector (cond	       
		       ((string= get-messages-menu-sort-key "Date")
			#'(lambda (item) 
			    (folder-param :timestamp item)))
		       ((string= get-messages-menu-sort-key "Title")
			#'(lambda (item) 
			    (folder-param :id item)))
		       ((string= get-messages-menu-sort-key "Unread")
			#'(lambda (item)
			    0)))))
;			    (let* ((unread (get-unread-by-container item))
;				   (count (if (null unread) 0 (unread-param :count unread))))
;			      count))))))
	(setq items
	      (sort items
		    (lambda (left right)
		      (let ((vleft (funcall selector left))
			    (vright (funcall selector right)))
			(string< vleft vright)))))
	(mapc (lambda (item)
		(menu-folder-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))

(defun get-list-subscriptions ()
  "Get subscriptions and print them into table"
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((items the-old-subscriptions))
      (let ((selector (cond	       
		       ((string= get-messages-menu-sort-key "Date")
			#'(lambda (item) 0))
	;		    (folder-param :timestamp item)))
		       ((string= get-messages-menu-sort-key "Title")
			#'(lambda (item) 0))
	;		    (folder-param :id item)))
		       ((string= get-messages-menu-sort-key "Unread")
			#'(lambda (item)
			    0)))))
;			    (let* ((unread (get-unread-by-container item))
;				   (count (if (null unread) 0 (unread-param :count unread))))
;			      count))))))
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
	(mapc (lambda (item)
		(menu-subscription-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))

(defun get-list-articles ()
  "Get subscriptions and print them into table"
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((items the-old-articles))
      (let ((selector (cond	       
		       ((string= get-messages-menu-sort-key "Date")
			#'(lambda (item) 0))
		       ((string= get-messages-menu-sort-key "Subscription")
			#'(lambda (item) 0))		       
		       ((string= get-messages-menu-sort-key "Title")
			#'(lambda (item) 0)))))
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
	(mapc (lambda (item)
		(menu-article-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))


;;
;; Sorting
;;
(defun get-messages-menu-sort-by-column (&optional e)
  "Sort the messages menu by the last column clicked on."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'column-name (car obj))
                (get-text-property (posn-point pos) 'column-name))))
    (setq get-messages-menu-sort-key col)
    (get-folders-menu)))

(defun get-messages-menu-sort-by-column-interactively (column-number)
  "Change column sorting by column number"
  (interactive)
  (setq get-messages-menu-sort-key (car(nth column-number get-column-alist)))
  (get-folders-menu))

(defvar get-messages-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'get-messages-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for messages menu sort buttons.")


(defun get-messages-menu-mode ()
  "Major mode for browsing a news"
  (kill-all-local-variables)
  (use-local-map the-old-menu-mode-map)
  ;(setq get-messages-menu-buffer (current-buffer))
  (setq major-mode 'the-old-mode)
  (setq mode-name "the-old-mode")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;(if (fboundp 'run-mode-hooks)
  ;    (run-mode-hooks 'get-messages-menu-mode-hook)
  ;  (run-hooks 'get-messages-menu-mode-hook))
  )


;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun get-folders-menu ()
  "Shows menu"
  (with-current-buffer (get-list-articles)
    (get-messages-menu-mode)
    (setq header-line-format
          (mapconcat
           (lambda (pair)
             (let ((name (car pair))
                   (column (cdr pair)))
               (concat
                ;; Insert a space that aligns the button properly.
                (propertize " " 'display (list 'space :align-to column)
                            'face 'fixed-pitch)
                ;; Set up the column button.
                (propertize name
                            'column-name name
                            'help-echo "mouse-1: sort by column"
                            'mouse-face 'highlight
                            'keymap get-messages-menu-sort-button-map
			    ))))
           menu-article-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Main function, run it to start the mode
;;
(defun the-old ()
  "Display a list of folders."
  (interactive)
  ;(unless filter-preset-current (filter-preset-parse-and-set 0)) ;; set filter for first time
  (setq token (api-get-token))
  (refresh-structure)
  (refresh-container-items (get-subscription "s=user/-/state/com.google/reading-list")) ;;"feed/573c0b8dc70bc2551d0004c1"))
  (let ((p (point)))
    (get-folders-menu)
    (goto-char p)))




;;(switch-to-buffer (test "https://theoldreader.com/reader/api/0/stream/contents?output=json"))

;;(api-update 'login)

;(setq token (api-get-token))
;(refresh-structure)
;(refresh-container-items (get-subscription "feed/573c0b8dc70bc2551d0004c1"))
;(the-old)
;(api-query 'userinfo)
;(api-query 'status)
;(cdr (assoc 'tags (api-query 'folders)))
;(aref (cdr (assoc 'unreadcounts (api-query 'unread-count))) 6)
;(assoc 'categories (aref (cdr (assoc 'subscriptions (api-query 'subscriptions))) 25))
;(test (get-cmd-url 'userinfo))
;the-old-unread
;(alist-get (api-query :folders) 'tags)
;(alist-get (api-query :unread-count) 'unreadcounts)
;(get-unread-by-container (get-folder "user/-/label/Clojure"))
;(get-unread-by-container (get-subscription "feed/57498699c70bc27dea0007df"))
;(unread-param :count (get-obj-by-id "user/-/label/Новая папка" the-old-unread));

;(get-unread-by-container (get-folder "user/-/label/Clojure"))

;(filter (lambda (x) (string= x "user/-/state/com.google/read"))
;	(vec-to-list (article-param :categories (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339"))))

;(article-unread? (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339"))
;(article-param :summary (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339"))

;(rest '("user/-/state/com.google/reading-list" "user/-/state/com.google/read"))
;((lambda (x) (string= x "user/-/state/com.google/read")) (car '("user/-/state/com.google/reading-list" "user/-/state/com.google/read")))
;((lambda (x) (string= x "user/-/state/com.google/read")) (car '("user/-/state/com.google/read")))

 
;(get-subscriptions-by-folder (get-folder "user/-/label/Clojure"))
;(menu-folder-row (get-folder "user/-/label/Clojure"))

;(map menu-folder-row the-old-folders)

;(mapc #'menu-folder-row the-old-folders)
;(mapc #'menu-subscription-row the-old-subscriptions)
;(mapc #'menu-article-row (vec-to-list (alist-get the-old-articles 'items)))

;(get-cmd-url :stream "&s=user/-/label/Новая%20папка")
;(mapc (lambda (x) (alist-get x 'id)) (vec-to-list (refresh-container-items (get-folder "user/-/label/Новая папка"))))

;(set-language-environment "UTF-8")
;(set-default-coding-systems 'utf-8)

;(mapc #'menu-article-row (vec-to-list (alist-get the-old-articles 'items))) 


;(cdar (elt (article-param :canonical (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339")) 0))
;(article-param :alternate (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339"))


(article-param :origin-title (get-article "tag:google.com,2005:reader/item/5753ddee5f45b74522000339"))
