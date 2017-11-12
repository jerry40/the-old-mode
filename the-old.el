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
    (:mark-as-read      . "mark-all-as-read")
    ))
;; lookup table to convert json fields to local names
(defvar the-old-folder-fields
  '((:id            . id)
    (:sortid        . sortid)))
(defvar the-old-subscription-fields
  '((:icon-url      . iconUrl)
    (:html-url      . htmlUrl)
    (:url           . url)
    (:timestamp     . firstitemmsec)
    (:sortid        . sortid)
    (:categories    . categories)
    (:title         . title)
    (:id            . id)))
(defvar the-old-unread-fields
  '((:timestamp     . newestItemTimestampUsec)
    (:count         . count)
    (:id            . id)))
(defvar the-old-article-fields
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
(defvar the-old-add-subscription-response
  '((:query       . query)
    (:num-results . numResults)
    (:stream-id   . streamId)
    (:error       . error))
  )
  
(defvar the-old-item-parameters
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
(defvar the-old-filter-read ())          ; filter by read/unreaad
(defvar the-old-filter-starred ())       ; filter by starred
(defvar the-old-filter-name ())          ; filter by name (regular expression)
(defvar the-old-filter-content ())       ; filter by content (regular expression)
(defvar the-old-filter-days ())          ; filter by days 

;; current mode (folders / subscriptions / articles)
(defvar the-old-current-mode nil
  "current mode (folders / subscriptions / articles)")

;; function to show list of current entities
(defvar the-old-current-list-function
  'the-old-get-subscriptions-menu)

;; =================================================================================================
;; menu 
;; =================================================================================================

(defvar the-old-get-messages-menu-mode-hook nil
  "Hooks to run after get-messages menu init.")

(defvar the-old-get-messages-menu-mode-map nil
  "Keymap for get-messages-menu-mode")

(defvar the-old-get-messages-menu-sort-key nil
  "sort packages by key")

;; folders view columns start positions
(defconst the-old-menu-folder-columns
  '(("Date"         . 1)
    ("Name"         . 24)
    ("Unread"       . 100))
  "An alist of (NAME . COLUMN) entries.")
(defconst the-old-menu-subscription-columns
  '(("Date"         . 1)
    ("Title"        . 32)
    ("Unread"       . 100))
  "An alist of (NAME . COLUMN) entries.")
(defconst the-old-menu-article-columns
  '(("Date"         . 1)
    ("Subscription" . 8)
    ("Title"        . 32))
  "An alist of (NAME . COLUMN) entries.")

;; =================================================================================================
;; interface
;; =================================================================================================

;; borrowed from elfeed-show.el (under The Unlicense)
(defvar the-old-show-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)	
      (define-key map "q" '(lambda () (interactive)
			     (kill-buffer (current-buffer))
			     (delete-window)))
      (define-key map (kbd "SPC") 'scroll-up-command)
      (define-key map (kbd "DEL") 'scroll-down-command)
      (define-key map "\t" 'shr-next-link)
      (define-key map [tab] 'shr-next-link)
      (define-key map "\e\t" 'shr-previous-link)
      (define-key map [backtab] 'shr-previous-link)
      (define-key map [mouse-2] 'shr-browse-url)))
"Keymap for showing the-old articles")


;; =================================================================================================
;; api post / get commands
;; =================================================================================================

;; get api rest url by command name (see the-old-api-cmd alist for details)
(defun the-old-get-cmd-url (cmd &optional params)
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

(defun the-old-api-ask (query-type addr &optional args)
  "query-type = POST/GET, addr = web address, args = arguments (body of POST)"
  (message "Contacting host")
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
	(url-retrieve-synchronously addr t))
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (message "Done")
    (decode-coding-string (buffer-string) 'utf-8))
  )


(defun the-old-api-get (addr)
  "GET from address"
  (the-old-api-ask "GET" addr))

(defun the-old-api-post (addr args)
  "POST args to address"
  (the-old-api-ask "POST" addr args))

(defun the-old-api-query (cmd &optional params)
  "generic api query"
  (json-read-from-string
   (the-old-api-get
    (the-old-get-cmd-url cmd
		 params))))

;; get last line of the-old-get-cmd-url result
(defun the-old-api-answer (url-answer)
  "Try to get an api answer (ignored all lines except the last)"
  (string-trim (car (last (split-string url-answer "\n")))))

(defun the-old-api-get-token ()
  "get security token by usern ame and pasword"
  (let ((client-login
	 (the-old-api-post the-old-api-login-url `(("client" . "the-old-emacs")
					   ("accountType" . "HOSTED_OR_GOOGLE")
					   ("service" . "reader")
					   ("Email" . ,the-old-api-login)
					   ("Passwd" . ,the-old-api-passwd)))))
    (cdr (assoc 'Auth (json-read-from-string client-login)))))

(defun the-old-refresh-container-items (cont)
  "refresh stream"
  (let ((articles (the-old-api-query :stream (concat "&s=" (the-old-alist-get cont 'id)
						     "&n=1000"
						     ;;"&xt=user/-/state/com.google/read"
						     ))))
    (setq the-old-articles (the-old-vec-to-list (the-old-alist-get articles 'items)))
    (setq the-old-articles-continuation (the-old-alist-get articles 'continuation))))

(defun the-old-api-get-article (article-id)
  "Get an article by id (id example: feed/00157a17b192950b65be3791)"
  (let ((article (json-read-from-string (the-old-api-post (the-old-get-cmd-url :items) `(("i" . ,article-id))))))
    (car (the-old-vec-to-list (the-old-alist-get article 'items)))))

(defun the-old-api-add-subscription (address)
  "add subscription"
  (let* ((subscription-result  (json-read-from-string (the-old-api-post (the-old-get-cmd-url :add-subscription (concat "&quickadd=" address)) nil)))
	(error-msg (the-old-add-subscription-param :error subscription-result)))
    (if error-msg error-msg "Added")))
    
(defun the-old-api-delete-subscription (subscription)
  "remove subscription"
  (let* ((subscription-id (the-old-subscription-param :id subscription)))
    (the-old-api-post (the-old-get-cmd-url :edit-subscription)
	      `(("ac" . "unsubscribe")
		("s"  . ,subscription-id)))))

(defun the-old-api-mark-subscription-read (subscription)
  "mark all items in the subscription as read"
  (let* ((subscription-id (the-old-subscription-param :id subscription)))
    (the-old-api-post (the-old-get-cmd-url :mark-as-read)
	      `(("s"  . ,subscription-id)))))

(defun the-old-api-mark-article-parameter (article action parameter &optional silent)
  "set/unset article parameter, action: a - mark / r - remove mark"
  (letrec ((article-id (the-old-article-param :id article))
	   (param (the-old-alist-get the-old-item-parameters parameter))
	   (result (the-old-api-post (the-old-get-cmd-url :update-item)
				     `(("i"     . ,article-id)
				       (,action . ,param))))
	   (answer (the-old-api-answer result)))
    (if (string= answer "OK")
	(progn
	  ;; refresh edited line
	  (let ((new-article (the-old-api-get-article (the-old-article-param :id article))))
	    (setq the-old-articles (the-old-replace-obj-by-id new-article the-old-articles))
	    (let ((cur-pos (current-column)))
	      (setq buffer-read-only nil)
	      (forward-line 0)
	      (the-old-menu-article-row new-article)
	      (delete-region (- (line-beginning-position) 1) (line-end-position))
	      (move-to-column cur-pos)
	      (setq buffer-read-only t)
	      ))	  
	  (unless silent
	    (message (format "The article %smarked as '%s'." (if (string= action "a") "" "un") parameter))))
      (message answer))))

(defun the-old-api-mark-article (article parameter &optional silent)
  "mark article with parameter"
  (the-old-api-mark-article-parameter article "a" parameter silent))

(defun the-old-api-unmark-article (article parameter &optional silent)
  "remove article mark"
  (the-old-api-mark-article-parameter article "r" parameter silent))

(defun the-old-api-toggle-article-parameter (article parameter &optional silent)
  "toggle article parameter"
  (funcall (if (the-old-article-paramater-set? article parameter) 'the-old-api-unmark-article 'the-old-api-mark-article) article parameter silent))

(defun the-old-api-set-article-read (article &optional silent)
  "set article read"
   (when (the-old-article-unread? article) (the-old-api-mark-article article :read silent)))

(defun the-old-api-set-article-unread (article &optional silent)
  "set article unread"
  (unless (the-old-article-unread? article) (the-old-api-unmark-article article :read silent)))

(defun the-old-api-set-article-starred (article &optional silent)
  "set article starred"
  (when (the-old-article-starred? article) (the-old-api-mark-article article :starred silent)))

(defun the-old-api-set-article-unstarred (article &optional silent)
  "set article unstarred"
  (unless (the-old-article-starred? article) (the-old-api-unmark-article article :starred silent)))

;; =================================================================================================
;; helpers
;; =================================================================================================
(defun the-old-filter (f col)
  "classical filter function"
  (remove-if-not f col))

(defun the-old-alist-get (lst key)
  "plist-get for alist"
  (cdr (assoc key lst)))

(defun the-old-alist-set (lst key item)
  (if (null (assoc key lst))
      (append lst (list (cons :id key) (cons :item (list item))))))

(defun the-old-vec-to-list (vec)
  "convert vector to list"
  (append vec nil))

(defun the-old-date-diff (sec)
  "Convert date difference in seconds to human readable format"
  (cond
   ((< sec 0) "never")
   ((< sec 3600) (concat (number-to-string (/ sec 60)) " m"))
   ((< sec (* 24 3600)) (concat (number-to-string (/ sec 3600)) " h"))
   ((< sec (* 365 24 3600)) (concat (number-to-string (/ sec (* 24 3600))) " d"))
   (t (concat (number-to-string (/ sec (* 365 24 3600))) " y"))))


;; =================================================================================================
;; retrieve subscriptions structure
;; =================================================================================================
(defun the-old-refresh-structure ()
  "refresh folders and subscriptions structure"
  (let ((folders (the-old-vec-to-list (the-old-alist-get (the-old-api-query :folders) 'tags)))
	(subscriptions (the-old-vec-to-list (the-old-alist-get (the-old-api-query :subscriptions) 'subscriptions)))
	(unread (the-old-vec-to-list (the-old-alist-get (the-old-api-query :unread-count) 'unreadcounts))))
    (setq the-old-folders folders)
    (setq the-old-subscriptions subscriptions)
    (setq the-old-unread unread)
    ))

;; =================================================================================================
;; api objects parsing
;; =================================================================================================

(defun the-old-get-obj-by-id (id a-list)
  "get alist from a list of alists by 'id field"
  (let ((res
	 (the-old-filter
	  (lambda (f)
	    (string= id (the-old-alist-get f 'id)))
	  a-list)))
    (if (null res)
	res
      (car res))))

(defun the-old-replace-obj-by-id (new-item a-list &optional lst)
  "Replace an element in list"
  (let ((obj-id (the-old-alist-get new-item 'id)))
    (mapcar
     (lambda (item)
       (if (string= obj-id (the-old-alist-get item 'id))
	   new-item
	 item))
     a-list)))

(defun the-old-get-folder (id)
  "get folder by id"
  (the-old-get-obj-by-id id the-old-folders))
     
(defun the-old-get-subscription (id)
  "get subscription by id"
  (the-old-get-obj-by-id id the-old-subscriptions))

(defun the-old-get-article (id)
  "get article by id"
  (the-old-get-obj-by-id id the-old-articles))

(defun the-old-get-subscriptions-by-folder (folder)
  "get subscriptions by folder"
  (let ((folder-id (the-old-folder-param :id folder)))
    (the-old-get-subscriptions-by-folder-id folder-id)))

(defun the-old-get-subscriptions-by-folder-id (folder-id)
  "get subscriptions by folder id"
  (the-old-filter (lambda (s)
		    (not (null
			  (the-old-get-obj-by-id
			   folder-id
			   (the-old-vec-to-list (the-old-subscription-param :categories s))))))
		  the-old-subscriptions))

(defun the-old-get-unread-by-container (cont)
  "get unread count for particular folder or subscription"
  (let ((f-id (the-old-folder-param :id cont)))
    (the-old-get-obj-by-id f-id the-old-unread)))
  
(defun the-old-get-value-by-param (param a-list param-dict)
  "get value of alist by parameter name recursively (it decoded by param-dict, see the-old-subscription-fields for example)"
  (if-let ((p (caar param-dict))
	   (p-val (cdar param-dict)))
      (cond
       ((null param-dict) nil)
       ((listp p-val)
	(let ((ret (the-old-get-value-by-param param (cdr (assoc (car p-val) a-list)) (cdr p-val))))
	  (if (null ret)
	      (the-old-get-value-by-param param a-list (cdr param-dict))
	    ret)))
       ((equal p param) (cdr (assoc p-val a-list)))
       (t (the-old-get-value-by-param param a-list (cdr param-dict)))
       )
    )
  )

(defun the-old-folder-param (param folder)
  "get folder attribute by parameter name"
  (the-old-get-value-by-param param folder the-old-folder-fields))
  
(defun the-old-subscription-param (param subscription)
  "get subscription attribute by parameter name"
  (the-old-get-value-by-param param subscription the-old-subscription-fields))

(defun the-old-add-subscription-param (param subscription-result)
  "get adding subscription result attribute by parameter name"
  (the-old-get-value-by-param param subscription-result the-old-add-subscription-response))

(defun the-old-unread-param (param unread)
  "get unread count attribute by parameter name"
  (the-old-get-value-by-param param unread the-old-unread-fields))

(defun the-old-article-param (param article)
  "get article attribute by parameter name"
  (the-old-get-value-by-param param article the-old-article-fields))

(defun the-old-article-paramater-set? (article parameter)
  "is article parameter set?"
  (let ((categories (the-old-vec-to-list (the-old-article-param :categories article))))
    (not (null (the-old-filter (lambda (x) (string= x (the-old-alist-get the-old-item-parameters parameter))) categories)))))

(defun the-old-article-unread? (article)
  "is article unread?"
  (not (the-old-article-paramater-set? article :read)))

(defun the-old-article-starred? (article)
  "is article starred?"
  (the-old-article-paramater-set? article :starred))

;;
;; Menu functions
;;

(defun the-old-menu-remove-subscription (yes)
  (interactive
   (list (y-or-n-p (format "Remove subscription '%s', continue? " (the-old-subscription-param :title (the-old-get-subscription (the-old-get-row-id)))))))
  (if yes
    (message (the-old-api-delete-subscription (the-old-get-subscription (the-old-get-row-id))))))

(defun the-old-menu-quick-help ()
  "Menu bar - cheatsheet"
  (interactive)
  (message "n-ext, p-revious, m-mode, a-add new subscription, d-remove subscription, v-show article, RET-browse article(SHR), w-open in browser, c-kill article address, g-reload data, l-clear filters/redraw, t-toggle read, r-set read, u-set unread, s-toggle star, *-mark all read, \\-show item, o-show row id, h-help, q-quit"))

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
  (define-key the-old-menu-mode-map "o" '(lambda () (interactive) (message (the-old-get-row-id))))
  (define-key the-old-menu-mode-map "a" '(lambda (web-addr) (interactive "sAdd new subscription. Enter the address: ") (message (the-old-api-add-subscription web-addr))))
  (define-key the-old-menu-mode-map "d" 'the-old-menu-remove-subscription)
  (define-key the-old-menu-mode-map "\\" '(lambda () (interactive) (message "%s" (the-old-get-article (the-old-get-row-id)))))
  (define-key the-old-menu-mode-map "c" '(lambda () (interactive)
					   (let ((addr (cdar
							(elt
							 (the-old-article-param :canonical (the-old-get-article (the-old-get-row-id)))
							 0))))
					     (kill-new addr)
					     (message addr))))
  (define-key the-old-menu-mode-map "w" '(lambda () (interactive)
					   (let ((addr (cdar
							(elt
							 (the-old-article-param :canonical (the-old-get-article (the-old-get-row-id)))
							 0))))
					     (browse-url addr)
					     (the-old-api-set-article-read (the-old-get-article (the-old-get-row-id)))
					     )))
  (define-key the-old-menu-mode-map "m" '(lambda () (interactive)
					   (setq the-old-current-list-function
						 (cond
						  ((eq the-old-current-mode :folders) 'the-old-get-subscriptions-menu)
						  ((eq the-old-current-mode :subscriptions) 'the-old-get-articles-menu)
						  ((eq the-old-current-mode :articles) 'the-old-get-folders-menu)))
					   (the-old-redraw)
					   ))
  ;; refresh (get all data)
  (define-key the-old-menu-mode-map "g" '(lambda () (interactive)
					   (the-old)))

  (define-key the-old-menu-mode-map "l" '(lambda () (interactive)
					     (progn
					       (setq the-old-filter-folder nil)
					       (setq the-old-filter-subscription nil)
					       (setq the-old-filter-read nil)
					       (setq the-old-filter-starred nil)
					       (setq the-old-filter-name nil)
					       (setq the-old-filter-content nil)
					       (message "filters cleared")
					       (the-old-redraw))))

  ;; mark all items in subscription read
  (define-key the-old-menu-mode-map "*"
    '(lambda () (interactive)
       (message
	(the-old-api-mark-subscription-read
	 (the-old-get-subscription (the-old-get-row-id))
	 ))))
  
  ;; sort columns
  ;(define-key the-old-menu-mode-map "1" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 0)))
  ;(define-key the-old-menu-mode-map "2" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 1)))
  ;(define-key the-old-menu-mode-map "3" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 2)))
  ;(define-key the-old-menu-mode-map "4" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 3)))
  ;(define-key the-old-menu-mode-map "5" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 4)))
  ;(define-key the-old-menu-mode-map "6" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 5)))
  ;(define-key the-old-menu-mode-map "7" '(lambda () (interactive) (get-messages-menu-sort-by-column-interactively 6)))

  ;; filters
  (define-key the-old-menu-mode-map "1" '(lambda (c)
					   (interactive "cEnter filter value (1-unread, 2-read, 3-both)")
					   (cond
					    ((member c `(?1 ?2))
					     (progn
					       (setq the-old-filter-read c)
					       (the-old-redraw)))
					    ((= c ?3)
					     (setq the-old-filter-read nil)
					     (the-old-redraw))
					    (t
					     (message "Wrong option, ignore.")))))

  (define-key the-old-menu-mode-map "2" '(lambda (c)
					   (interactive "cEnter filter value (1-starred, 2-unstarred, 3-all)")
					   (cond
					    ((member c `(?1 ?2))
					     (progn
					       (setq the-old-filter-starred c)
					       (the-old-redraw)))
					    ((= c ?3)
					     (setq the-old-filter-starred nil)
					     (the-old-redraw))
					    (t
					     (message "Wrong option, ignore.")))))
  (define-key the-old-menu-mode-map "3" '(lambda (s)
					   (interactive "sEnter article title filter value (regular expression):")
					   (setq the-old-filter-name (if (= (length s) 0) nil s))
					   (the-old-redraw)))
  (define-key the-old-menu-mode-map "4" '(lambda (s)
					   (interactive "sEnter article title and content filter value (regular expression):")
					   (setq the-old-filter-content (if (= (length s) 0) nil s))
					   (the-old-redraw)))
  
  ;; toggle read/unread
  (define-key the-old-menu-mode-map "t" (lambda () (interactive)
						  (the-old-api-toggle-article-parameter (the-old-get-article (the-old-get-row-id)) :read)))
  ;; set item read
  (define-key the-old-menu-mode-map (kbd "r") (lambda () (interactive)
						  (the-old-api-set-article-read (the-old-get-article (the-old-get-row-id)))))
  ;; set item unread
  (define-key the-old-menu-mode-map (kbd "u") (lambda () (interactive)
						  (the-old-api-set-article-unread (the-old-get-article (the-old-get-row-id)))))
  ;; toggle star
  (define-key the-old-menu-mode-map (kbd "s") (lambda () (interactive)
						(the-old-api-toggle-article-parameter (the-old-get-article (the-old-get-row-id)) :starred)))
  ;; show article / open folder / open subscription
  (define-key the-old-menu-mode-map "v"
    (lambda () (interactive)
      (let ((id (the-old-get-row-id)))
	(cond
	 ((eq the-old-current-mode :folders) (progn
					       (setq the-old-current-list-function 'the-old-get-subscriptions-menu)
					       (setq the-old-filter-folder id)
					       (the-old-redraw)))					       
	 ((eq the-old-current-mode :subscriptions) (progn
						     (setq the-old-current-list-function 'the-old-get-articles-menu)
						     (setq the-old-filter-subscription id)
						     (the-old-redraw)))
	 (t
	  (letrec ((article (the-old-get-article (the-old-get-row-id)))
		   (str (the-old-article-param :content article)))
	    (with-temp-buffer
	      (insert str)
	      (shr-render-buffer (current-buffer)))
	    (other-window 1)
	    (run-at-time "0 sec" nil 'the-old-api-set-article-read article)))))))
  
  (define-key the-old-menu-mode-map (kbd "RET") '(lambda () (interactive) 
						 (let ((addr (cdar
							      (elt
							       (the-old-article-param :canonical (the-old-get-article (the-old-get-row-id)))
							       0))))
						   (with-temp-buffer
						     (insert (the-old-api-get addr))
						     (shr-render-buffer (current-buffer))
						     (read-only-mode 1)
						     (use-local-map the-old-show-map)))))

  ;; help
  (define-key the-old-menu-mode-map "h" 'the-old-menu-quick-help)
  ;; quit
  (define-key the-old-menu-mode-map "q" 'quit-window))

;;
;; Here are functions to parse rows on the screen in order to get their IDs, for instance
;;
(defun the-old-get-row-id ()
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
(defun the-old-menu-folder-row (folder)
  "Prepare folder to show at main table"
  (let* ((id (the-old-folder-param :id folder))
	 (name (the-old-folder-param :id folder))
	 (unread (the-old-get-unread-by-container folder))
	 (count (if (null unread) 0 (the-old-unread-param :count unread)))
	 (timestamp (if (null unread) (floor (+ 100 (time-to-seconds (current-time)))) (/ (string-to-number (the-old-unread-param :timestamp unread)) 1000000)))
	 ;(date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
	 (date (the-old-date-diff (- (floor (time-to-seconds (current-time))) timestamp))))
    (let ((face (cond
		 ((> count 0) 'font-lock-comment-face)
		 (t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize (concat id "\t") 'invisible t))
      ; date
      (indent-to (the-old-alist-get the-old-menu-folder-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; name
      (indent-to (the-old-alist-get the-old-menu-folder-columns "Name") 2)
      (insert (propertize name 'font-lock-face face))
      ; unread count
      (indent-to (the-old-alist-get the-old-menu-folder-columns "Unread") 2)
      (insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      )))

(defun the-old-menu-subscription-row (subscription)
  "Prepare subscription to show at main table"
  (let* ((id (the-old-subscription-param :id subscription))
	 (title (the-old-subscription-param :title subscription))
	 (unread (the-old-get-unread-by-container subscription))
	 (count (if (null unread) 0 (the-old-unread-param :count unread)))
	 ;(timestamp (/ (string-to-number (the-old-subscription-param :timestamp subscription)) 1000000))
	 ;(date (format-time-string "%F %T" (seconds-to-time timestamp)))
	 (timestamp (if (null unread) (floor (+ 100 (time-to-seconds (current-time)))) (/ (string-to-number (the-old-unread-param :timestamp unread)) 1000000)))
	 ;(date (if (null unread) "" (format-time-string "%F %T" (seconds-to-time timestamp)))))
    	 (date (the-old-date-diff (- (floor (time-to-seconds (current-time))) timestamp))))
    (let ((face (cond
		((= count 0) 'font-lock-comment-face)
		(t 'default))))
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize (concat id "\t") 'invisible t))
      ; date
      (indent-to (the-old-alist-get the-old-menu-subscription-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; name
      (indent-to (the-old-alist-get the-old-menu-subscription-columns "Title") 2)
      (insert (propertize title 'font-lock-face face))
      ; unread count
      (indent-to (the-old-alist-get the-old-menu-subscription-columns "Unread") 2)
      (insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      )))

(defun the-old-menu-article-row (article)
  "Prepare article to show at main table"
  (let* ((id (the-old-article-param :id article))
	 (title (the-old-article-param :title article))
	 (starred (the-old-article-starred? article))
	 (origin-title
	  (let ((ot (the-old-article-param :origin-title article)))
	    (substring ot 0
		       (min
			(length ot)
			(- (the-old-alist-get the-old-menu-article-columns "Title") (the-old-alist-get the-old-menu-article-columns "Subscription") 2)))))
	 (timestamp (the-old-article-param :published article))
	 ;(date (format-time-string "%F %T" (seconds-to-time timestamp)))
	 (date (the-old-date-diff (- (floor (time-to-seconds (current-time))) timestamp)))
	 (face (cond
		((not (the-old-article-unread? article)) 'font-lock-comment-face)
		(t 'default)))
	 )
      ; id (hidden)
      (indent-to 1 1)
      (insert (propertize (concat id "\t") 'invisible t))
      ; date
      (indent-to (the-old-alist-get the-old-menu-article-columns "Date") 2)
      (insert (propertize date 'font-lock-face face))
      ; subscription title
      (indent-to (the-old-alist-get the-old-menu-article-columns "Subscription") 2)
      (insert (propertize origin-title 'font-lock-face face))      
      ; name
      (indent-to (the-old-alist-get the-old-menu-article-columns "Title") 2)
      (insert (propertize (concat (when starred "* ") title) 'font-lock-face face))
      ; unread count
      ;(indent-to (the-old-alist-get the-old-menu-article-columns "Unread") 2)
      ;(insert (propertize (format "%9s" count) 'font-lock-face face))
      (insert "\n")
      ))

(defun the-old-get-list-folders ()
  "Get folders and print them into table"
  (setq the-old-current-mode :folders)
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((items the-old-folders))
      (let ((selector (cond	       
		       ((string= the-old-get-messages-menu-sort-key "Date")
			#'(lambda (item) 
			    (the-old-folder-param :timestamp item)))
		       ((string= the-old-get-messages-menu-sort-key "Title")
			#'(lambda (item) 
			    (the-old-folder-param :id item)))
		       ((string= the-old-get-messages-menu-sort-key "Unread")
			#'(lambda (item)
			    0)))))
;			    (let* ((unread (the-old-get-unread-by-container item))
;				   (count (if (null unread) 0 (the-old-unread-param :count unread))))
;			      count))))))
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
	(mapc (lambda (item)
		(the-old-menu-folder-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))

(defun the-old-get-list-subscriptions ()
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
	    (the-old-get-subscriptions-by-folder (the-old-get-folder the-old-filter-folder)))))
      (let ((selector (cond	       
		       ((string= the-old-get-messages-menu-sort-key "Date")
			#'(lambda (item) 0))
	;		    (the-old-folder-param :timestamp item)))
		       ((string= the-old-get-messages-menu-sort-key "Title")
			#'(lambda (item) 0))
	;		    (the-old-folder-param :id item)))
		       ((string= the-old-get-messages-menu-sort-key "Unread")
			#'(lambda (item)
			    0)))))
;			    (let* ((unread (the-old-get-unread-by-container item))
;				   (count (if (null unread) 0 (the-old-unread-param :count unread))))
;			      count))))))
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
	(mapc (lambda (item)
		(the-old-menu-subscription-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))

(defun the-old-get-list-articles ()
  "Get subscriptions and print them into table"
  (setq the-old-current-mode :articles)
  (with-current-buffer (get-buffer-create the-old-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let
	((items
	  ;(if (null the-old-filter-subscription)
	  ;    the-old-articles
	  ;  (the-old-filter
	  ;   (lambda (a)
	  ;     (string= the-old-filter-subscription (the-old-article-param :stream-id a)))
					;   the-old-articles))))
	  (the-old-filter 'the-old-get-list-articles-filter the-old-articles)))
      (let ((selector (cond	       
		       ((string= the-old-get-messages-menu-sort-key "Date")
			#'(lambda (item) 0))
		       ((string= the-old-get-messages-menu-sort-key "Subscription")
			#'(lambda (item) 0))		       
		       ((string= the-old-get-messages-menu-sort-key "Title")
			#'(lambda (item) 0)))))
;	(setq items
;	      (sort items
;		    (lambda (left right)
;		      (let ((vleft (funcall selector left))
;			    (vright (funcall selector right)))
;			(string< vleft vright)))))
	(mapc (lambda (item)
		(the-old-menu-article-row item))
	      items)))
    ;; remove empty line at the end
    (let ((beg (point)))
      (forward-line 1)
      (forward-char -1)
      (delete-region beg (point)))
    (goto-char (point-min))
    (current-buffer)))

(defun the-old-get-list-articles-filter (article)
  "Filter articles"
  (and
   ;; (or (null the-old-filter-folder) (string= the-old-filter-folder (the-old-article-param :stream-id article)))
   (or (null the-old-filter-subscription) (string= the-old-filter-subscription (the-old-article-param :stream-id article)))
   (or (null the-old-filter-read) (and (= the-old-filter-read (if (the-old-article-unread? article) ?1 ?2))))
   (or (null the-old-filter-starred) (and (= the-old-filter-starred (if (the-old-article-starred? article) ?1 ?2))))
   (or (null the-old-filter-name) (string-match-p the-old-filter-name (the-old-article-param :title article)))
   (or (null the-old-filter-content) (string-match-p the-old-filter-content (the-old-article-param :content article)) (string-match-p the-old-filter-content (the-old-article-param :title article)))
   ;(or (null the-old-filter-days) (> (* 3600 24 the-old-filter-days) (/ (string-to-number (the-old-unread-param :published article)) 1000000))))
  ))

;;
;; Sorting
;;
(defun the-old-get-messages-menu-sort-by-column (&optional e)
  "Sort the messages menu by the last column clicked on."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'column-name (car obj))
                (get-text-property (posn-point pos) 'column-name))))
    (setq the-old-get-messages-menu-sort-key col)
    (the-old-get-folders-menu)))

(defun the-old-get-messages-menu-sort-by-column-interactively (column-number)
  "Change column sorting by column number"
  (interactive)
  (setq the-old-get-messages-menu-sort-key (car(nth column-number get-column-alist)))
  (the-old-get-folders-menu))

(defvar the-old-get-messages-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'the-old-get-messages-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for messages menu sort buttons.")


(defun the-old-get-messages-menu-mode ()
  "Major mode for browsing a news"
  (kill-all-local-variables)
  (use-local-map the-old-menu-mode-map)
  ;(setq get-messages-menu-buffer (current-buffer))
  (setq major-mode 'the-old-mode)
  (setq mode-name "the-old-mode")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;(if (fboundp 'run-mode-hooks)
  ;    (run-mode-hooks 'the-old-get-messages-menu-mode-hook)
  ;  (run-hooks 'the-old-get-messages-menu-mode-hook))
  )


;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun the-old-get-folders-menu ()
  "Shows menu"
  (with-current-buffer (the-old-get-list-folders)
    (the-old-get-messages-menu-mode)
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
                            'keymap the-old-get-messages-menu-sort-button-map
			    ))))
           the-old-menu-folder-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun the-old-get-subscriptions-menu ()
  "Shows subscriptions secreen"
  (with-current-buffer (the-old-get-list-subscriptions)
    (the-old-get-messages-menu-mode)
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
                            'keymap the-old-get-messages-menu-sort-button-map
			    ))))
           the-old-menu-subscription-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Paints the user menu at top of the buffer and pulls functions to populate the main screen
;;
(defun the-old-get-articles-menu ()
  "Shows articles secreen"
  (with-current-buffer (the-old-get-list-articles)
    (the-old-get-messages-menu-mode)
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
                            'keymap the-old-get-messages-menu-sort-button-map
			    ))))
           the-old-menu-article-columns ""))
    (switch-to-buffer (current-buffer) nil t)
    )
  )

;;
;; Main function, run it to start the mode
;;
(defun the-old ()
  "Display a list of folders."
  (interactive)
  ;(unless the-old-filter-preset-current (the-old-filter-preset-parse-and-set 0)) ;; set the-old-filter for first time
  (unless the-old-api-token
    (progn
      (unless the-old-api-passwd (setq the-old-api-passwd (read-passwd "Password:")))
      (setq the-old-api-token (the-old-api-get-token))
      (kill-new the-old-api-token)
      ;;(message the-old-api-token)  
      ))
  (the-old-refresh-structure)
  (the-old-refresh-container-items (the-old-get-subscription "s=user/-/state/com.google/reading-list")) ;;"feed/573c0b8dc70bc2551d0004c1"))
  (let ((p (point)))
    ;(the-old-get-subscriptions-menu)
    (funcall the-old-current-list-function)
    (goto-char p))
  )

(defun the-old-redraw ()
  "Display a list of folders."
  (interactive)
  (let ((p (point)))
    (funcall the-old-current-list-function)
    (goto-char p)))
