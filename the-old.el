(provide 'the-old)

(require 'json)
(require 'url)
(require 'shr)
(require 'pp)

(defvar the-old-api-url "https://theoldreader.com/reader/api/0/")
(defvar the-old-api-login-url "https://theoldreader.com/accounts/ClientLogin?output=json")
;; the old reader login
(defvar the-old-api-login nil)
;; the old reader password
(defvar the-old-api-passwd nil)
;; the old reader security token
(defvar the-old-api-token nil)
;; get answers in json format
(defvar the-old-api-params "?output=json")
;; lookup table to convert local commands to the old reader API commands
;; if API commands will be changed, we'll need to edit second column
(defvar the-old-api-cmd
  '((:userinfo          . "user-info")
    (:token             . "token")
    (:login             . "accounts/ClientLogin")
    (:status            . "status")
    (:preferences       . "preference/list")
    (:folders           . "tag/list")
    (:subscriptions     . "subscription/list")
    (:unread-count      . "unread-count")
    (:item-ids          . "stream/items/ids")
    (:items             . "stream/items/contents")
    (:stream            . "stream/contents")
    (:update-item       . "edit-tag")
    (:add-subscription  . "subscription/quickadd")
    (:edit-subscription . "subscription/edit")
    ))
;; lookup table to convert json fields to local names
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
(defvar add-subscription-response
  '((:query       . query)
    (:num-results . numResults)
    (:stream-id   . streamId)
    (:error       . error))
  )
  
(defvar item-parameters
  '((:read     . "user/-/state/com.google/read")
    (:starred  . "user/-/state/com.google/starred")
    (:shared   . "user/-/state/com.google/broadcast")
    (:liked    . "user/-/state/com.google/like")))

;; data lists
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

;; global filters
(defvar the-old-filter-folder ())        ; current selected folder id
(defvar the-old-filter-subscription ())  ; current selected subscription id

;; current mode (folders / subscriptions / articles)
(defvar the-old-current-mode nil
  "current mode (folders / subscriptions / articles)")

;; function to show list of current entities
(defvar the-old-current-list-function
  'get-subscriptions-menu)

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

(defun api-ask (query-type addr &optional args)
  "query-type = POST/GET, addr = web address, args = arguments (body of POST)"
  (with-current-buffer
      (let ((url-request-method query-type)
	    (url-request-extra-headers
	     (list '("Content-Type" . "application/x-www-form-urlencoded")
		   (when the-old-api-token `("Authorization" . ,(concat "GoogleLogin auth=" the-old-api-token)))))
	    (url-request-data
	     (mapconcat (lambda (arg)
			  (concat (url-hexify-string (car arg))
				  "="
				  (url-hexify-string (cdr arg))))
			args
			"&"))
	    )
	(url-retrieve-synchronously addr))
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (decode-coding-string (buffer-string) 'utf-8)))


(defun api-get (addr)
  "GET from address"
  (api-ask "GET" addr))

(defun api-post (addr args)
  "POST args to address"
  (api-ask "POST" addr args))

(defun api-query (cmd &optional params)
  "generic api query"
  (json-read-from-string
   (api-get
    (get-cmd-url cmd
		 params))))

(defun api-get-token ()
  "get security token by usern ame and pasword"
  (let ((client-login
	 (api-post the-old-api-login-url `(("client" . "the-old-emacs")
					   ("accountType" . "HOSTED_OR_GOOGLE")
					   ("service" . "reader")
					   ("Email" . ,the-old-api-login)
					   ("Passwd" . ,the-old-api-passwd)))))
    (cdr (assoc 'Auth (json-read-from-string client-login)))))

(defun api-add-subscription (address)
  "add subscription"
  (let* ((subscription-result  (json-read-from-string (api-post (get-cmd-url :add-subscription (concat "&quickadd=" address)) nil)))
	(error-msg (add-subscription-param :error subscription-result)))
    (if error-msg error-msg "Added")))
    
(defun api-delete-subscription (subscription)
  "remove subscription"
  (let* ((subscription-id (subscription-param :id subscription)))
    (api-post (get-cmd-url :edit-subscription)
	      `(("ac" . "unsubscribe")
		("s"  . ,subscription-id)))))

(defun api-mark-article-parameter (article action parameter)
  "set/unset article parameter, action: a - mark / r - remove mark"
  (let* ((article-id (article-param :id article))
	 (param (alist-get item-parameters parameter)))
    (api-post (get-cmd-url :update-item)
	      `(("i"     . ,article-id)
		(,action . ,param)))))

(defun api-mark-article (article parameter)
  "mark article with parameter"
  (api-mark-article-parameter article "a" parameter))

(defun api-unmark-article (article parameter)
  "remove article mark"
  (api-mark-article-parameter article "r" parameter))

(defun api-toggle-article-parameter (article parameter)
  "toggle article parameter"
  (funcall (if (article-paramater-set? article parameter) 'api-unmark-article 'api-mark-article) article parameter))

(defun api-set-article-read (article)
  "set article read"
  (when (article-unread? article) (api-mark-article article :read)))

(defun api-set-article-unread (article)
  "set article unread"
  (unless (article-unread? article) (api-unmark-article article :read)))

(defun api-set-article-starred (article)
  "set article starred"
  (when (article-starred? article) (api-mark-article article :starred)))

(defun api-set-article-unstarred (article)
  "set article unstarred"
  (unless (article-starred? article) (api-unmark-article article :starred)))

;; =================================================================================================
;; helpers
;; =================================================================================================
(defun filter (f col)
  "classical filter function"
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
  "Convert date diference in seconds to human readable format"
  (cond
   ((< sec 0) "never")
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
  (let ((art (api-query :stream (concat "&s=" (alist-get cont 'id)
					"&n=1000"
					))))
    (setq the-old-articles (vec-to-list(alist-get art 'items)))
    (setq the-old-articles-continuation (alist-get art 'continuation))))


;; =================================================================================================
;; api objects parsing
;; =================================================================================================

(defun get-obj-by-id (id a-list)
  "get alist from a list of alists by 'id field"
  (let ((res
	 (filter
	  (lambda (f)
	    (string= id (alist-get f 'id)))
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
  (let ((folder-id (folder-param :id folder)))
    (get-subscriptions-by-folder-id folder-id)))

(defun get-subscriptions-by-folder-id (folder-id)
  "get subscriptions by folder id"
  (filter (lambda (s)
	    (not (null
		  (get-obj-by-id
		   folder-id
		   (vec-to-list (subscription-param :categories s))))))
	  the-old-subscriptions))

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

(defun add-subscription-param (param subscription-result)
  "get adding subscription result attribute by parameter name"
  (get-value-by-param param subscription-result add-subscription-response))

(defun unread-param (param unread)
  "get unread count attribute by parameter name"
  (get-value-by-param param unread unread-fields))

(defun article-param (param article)
  "get article attribute by parameter name"
  (get-value-by-param param article article-fields))

(defun article-paramater-set? (article parameter)
  "is article parameter set?"
  (let ((categories (vec-to-list (article-param :categories article))))
    (not (null (filter (lambda (x) (string= x (alist-get item-parameters parameter))) categories)))))

(defun article-unread? (article)
  "is article unread?"
  (not (article-paramater-set? article :read)))

(defun article-starred? (article)
  "is article unread?"
  (article-paramater-set? article :starred))

;;
;; Menu functions
;;

(defun menu-remove-subscription (yes)
  (interactive
   (list (y-or-n-p (format "Remove subscription '%s', continue? " (subscription-param :title (get-subscription (get-row-id)))))))
  (if yes
    (message (api-delete-subscription (get-subscription (get-row-id))))))

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
  (define-key the-old-menu-mode-map "a" '(lambda (web-addr) (interactive "sAdd new subscription. Enter the address: ") (message (api-add-subscription web-addr))))
  (define-key the-old-menu-mode-map (kbd "DEL") 'menu-remove-subscription)
  (define-key the-old-menu-mode-map (kbd "TAB") '(lambda () (interactive) (message "%s" (get-article (get-row-id)))))
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
					     (browse-url addr)
					     (api-set-article-read (get-article (get-row-id)))
					     )))
  (define-key the-old-menu-mode-map "m" '(lambda () (interactive)
					   (setq the-old-current-list-function
						 (cond
						  ((eq the-old-current-mode :folders) 'get-subscriptions-menu)
						  ((eq the-old-current-mode :subscriptions) 'get-articles-menu)
						  ((eq the-old-current-mode :articles) 'get-folders-menu)))
					   (the-old-redraw)
					   ))
  ;; refresh (get all data)
  (define-key the-old-menu-mode-map "R" '(lambda () (interactive)
					   (the-old)))

  (define-key the-old-menu-mode-map (kbd "M-a") '(lambda () (interactive)
					     (progn
					       (setq the-old-filter-folder nil)
					       (setq the-old-filter-subscription nil)
					       (message "Filters cleared")
					       (the-old-redraw))))
					     
  ;; sort columns
  ;(define-key the-old-menu-mode-map "1" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 0)))
  ;(define-key the-old-menu-mode-map "2" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 1)))
  ;(define-key the-old-menu-mode-map "3" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 2)))
  ;(define-key the-old-menu-mode-map "4" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 3)))
  ;(define-key the-old-menu-mode-map "5" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 4)))
  ;(define-key the-old-menu-mode-map "6" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 5)))
  ;(define-key the-old-menu-mode-map "7" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 6)))
  ;; toggle read/unread
  (define-key the-old-menu-mode-map (kbd "SPC") (lambda () (interactive)
						  (api-toggle-article-parameter (get-article (get-row-id)) :read)))
  ;; set item read
  (define-key the-old-menu-mode-map (kbd "r") (lambda () (interactive)
						  (api-set-article-read (get-article (get-row-id)))))
  ;; set item unread
  (define-key the-old-menu-mode-map (kbd "u") (lambda () (interactive)
						  (api-set-article-unread (get-article (get-row-id)))))
  ;; toggle star
  (define-key the-old-menu-mode-map (kbd "s") (lambda () (interactive)
;						  (api-set-article-starred (get-article (get-row-id)))))
						(api-toggle-article-parameter (get-article (get-row-id)) :starred)))
  ;; show article / open folder / open subscription
  (define-key the-old-menu-mode-map (kbd "RET")
    (lambda () (interactive)
      (let ((id (get-row-id)))
	(cond
	 ((eq the-old-current-mode :folders) (progn
					       (setq the-old-current-list-function 'get-subscriptions-menu)
					       (setq the-old-filter-folder id)
					       (the-old-redraw)))					       
	 ((eq the-old-current-mode :subscriptions) (progn
						     (setq the-old-current-list-function 'get-articles-menu)
						     (setq the-old-filter-subscription id)
						     (the-old-redraw)))
	 (t
	  (letrec ((article (get-article (get-row-id)))
		   (str (article-param :content article)))
	    (with-temp-buffer
	      (insert str)
	      (shr-render-buffer (current-buffer)))
	    (other-window 1)
	    (run-at-time "0 sec" nil 'api-set-article-read article)))))))
  
  (define-key the-old-menu-mode-map (kbd "e") '(lambda () (interactive) 
						 (let ((addr (cdar
							      (elt
							       (article-param :canonical (get-article (get-row-id)))
							       0))))
						   (with-temp-buffer
						     (insert (api-get addr))
						     (shr-render-buffer (current-buffer))
						     (read-only-mode 1))
						   (other-window 1))))
  
  ;; help
  ;(define-key the-old-menu-mode-map "h" 'menu-quick-help)
  ;; quit
  (define-key the-old-menu-mode-map "q" 'quit-window))

;;
;; Here are functions to parse rows on the screen in order to get their IDs, for instance
;;
(defun get-row-id ()
  "Get ID of active row in the table"
  (save-excursion
    (beginning-of-line)
    (propertize (if (looking-at " \\([^\t]*\\)")
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
	 (timestamp (if (null unread) (floor (+ 100 (time-to-seconds (current-time)))) (/ (string-to-number (unread-param :timestamp unread)) 1000000)))
	 ;(date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
	 (date (date-diff (- (floor (time-to-seconds (current-time))) timestamp))))
    (let ((face (cond
		 ((> count 0) 'font-lock-comment-face)
		 (t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize (concat id "\t") 'invisible t))
      ; date
      (indent-to (alist-get menu-folder-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; name
      (indent-to (alist-get menu-folder-columns "Name") 2)
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
	 (timestamp (if (null unread) (floor (+ 100 (time-to-seconds (current-time)))) (/ (string-to-number (unread-param :timestamp unread)) 1000000)))
	 ;(date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
    	 (date (date-diff (- (floor (time-to-seconds (current-time))) timestamp))))
    (let ((face (cond
		((= count 0) 'font-lock-comment-face)
		(t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize (concat id "\t") 'invisible t))
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
      (insert (propertize (concat id "\t") 'invisible t))
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
  (setq the-old-current-mode :folders)
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
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
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
  (setq the-old-current-mode :subscriptions)
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let*
	;((items the-old-subscriptions))
	((items
	  (if (null the-old-filter-folder)
	      the-old-subscriptions
	    (get-subscriptions-by-folder (get-folder the-old-filter-folder)))))
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
  (setq the-old-current-mode :articles)
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let*
	((items
	  (if (null the-old-filter-subscription)
	      the-old-articles
	    (filter
	     (lambda (a)
	       (string= the-old-filter-subscription (article-param :stream-id a)))
	     the-old-articles))))
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
  (with-current-buffer (get-list-folders)
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
           menu-folder-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun get-subscriptions-menu ()
  "Shows subscriptions secreen"
  (with-current-buffer (get-list-subscriptions)
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
           menu-subscription-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun get-articles-menu ()
  "Shows articles secreen"
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
  (unless the-old-api-token (setq the-old-api-token (api-get-token)))
  (refresh-structure)
  (refresh-container-items (get-subscription "s=user/-/state/com.google/reading-list")) ;;"feed/573c0b8dc70bc2551d0004c1"))
  (let ((p (point)))
    ;(get-subscriptions-menu)
    (funcall the-old-current-list-function)
    (goto-char p)))

(defun the-old-redraw ()
  "Display a list of folders."
  (interactive)
  (let ((p (point)))
    (funcall the-old-current-list-function)
    (goto-char p)))
