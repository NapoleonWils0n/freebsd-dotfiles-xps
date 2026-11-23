;;; -*- lexical-binding: t; -*-
;; ----------------------------------------------------------------------------------
;; phony-speech-commands.el
;; https://github.com/sachac/phony.el
;; ----------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------
;; require phony-parser
;; ----------------------------------------------------------------------------------

(require 'phony-parser)
(require 'phony-stdlib)
(define-error 'sacha-phony-not-parsed "Sentence not parsed." 'error)


;; ----------------------------------------------------------------------------------
;; sacha-phony-parser--sanitize
;; ----------------------------------------------------------------------------------

;; updated regex
(defun sacha-phony-parser--sanitize (s)
  "Lowercase, remove leading quotes/punctuation, and trim whitespace."
  (let* ((cleaned (replace-regexp-in-string "^[[:space:]\"',.!?]+" "" s))
         (lowered (downcase cleaned)))
    (string-trim (replace-regexp-in-string "[,\\.!?:]" "" lowered))))


;; ----------------------------------------------------------------------------------
;; sacha-phony-compare-literal
;; ----------------------------------------------------------------------------------

;;;###autoload
(defun sacha-phony-compare-literal (a b)
  "Ignore case and ending punctuation."
  (string= (sacha-phony-parser--sanitize a)
           (sacha-phony-parser--sanitize b)))


;; ----------------------------------------------------------------------------------
;; sacha-phony-evaluate
;; ----------------------------------------------------------------------------------

;;;###autoload
;; updated code
(defun sacha-phony-evaluate (sentence)
  "Evaluate SENTENCE using phony.el definitions after sanitizing.
Throw 'not-parsed if not parsed."
  (let* ((clean-sentence (sacha-phony-parser--sanitize sentence))
         (parses (phony-parser--parse-utterance clean-sentence)))
    (unless parses
      (signal 'sacha-phony-not-parsed sentence))
    (phony--evaluate-ast (caar parses))))

;; ----------------------------------------------------------------------------------
;; sacha-whisper-phony
;; ----------------------------------------------------------------------------------

;;;###autoload
(defun sacha-whisper-phony (text)
  "Evaluate TEXT using phony.el definitions.
Lower-case the first letter of TEXT to match command definitions.
Useful in `speech-input-commands'."
  (sacha-phony-evaluate
	 (concat (downcase (substring text 0 1))
					 (if (> (length text) 1) (substring text 1) "")))
  "")


;; ----------------------------------------------------------------------------------
;; speech-input-phony-process
;; ----------------------------------------------------------------------------------

(defun speech-input-phony-process (text)
  "Evaluate TEXT via phony. Pass cleaned text through if not parsed,
unless the buffer is read-only."
  (let* ((processed-text 
          ;; Dynamically convert any numeric digits to words
          (replace-regexp-in-string 
           "\\b[0-9]+\\b" 
           (lambda (match)
             (sacha-number-to-words (string-to-number match)))
           text)))
    (condition-case err
        (progn
          ;; Lowercase the first letter to match command definitions
          (sacha-phony-evaluate
           (concat (downcase (substring processed-text 0 1))
                   (if (> (length processed-text) 1) (substring processed-text 1) "")))
          ;; Return empty string if successfully parsed & executed
          "")
      (sacha-phony-not-parsed
       (if buffer-read-only
           (progn
             (message "Buffer is read-only; suppressing text insertion.")
             "")
         (string-trim text "[ \t\n\r]+" nil)))
      (error
       (signal (car err) (cdr err))))))


;; ----------------------------------------------------------------------------------
;; phony numbers
;; ----------------------------------------------------------------------------------

(defun sacha-number-to-words (n)
  "Convert integer N to English words for phony-stdlib parsing."
  (cond
   ((< n 10)
    (nth n '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
   ((< n 20)
    (nth (- n 10) '("ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")))
   ((< n 100)
    (let ((tens (floor n 10))
          (rem (% n 10))
          (tens-words '("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")))
      (if (= rem 0)
          (nth tens tens-words)
        (concat (nth tens tens-words) " " (sacha-number-to-words rem)))))
   ((< n 1000)
    (let ((hundreds (floor n 100))
          (rem (% n 100)))
      (if (= rem 0)
          (concat (sacha-number-to-words hundreds) " hundred")
        (concat (sacha-number-to-words hundreds) " hundred and " (sacha-number-to-words rem)))))
   (t (number-to-string n))))


;; ----------------------------------------------------------------------------------
;; phony-define-open-rule
;; ----------------------------------------------------------------------------------

;; open rule target
(phony-define-open-rule target)

;; region
(phony-defun target-region "region"
	:contributes-to target
	(when (region-active-p)
	  (cons (region-beginning) (region-end))))

;; sentence
(phony-defun target-sentence "sentence"
	:contributes-to target
	(bounds-of-thing-at-point 'sentence))

;; buffer
(phony-defun target-buffer "buffer"
	:contributes-to target
	(bounds-of-thing-at-point 'buffer))

;; current line
(phony-defun current-line "line"
  :contributes-to target
  (bounds-of-thing-at-point 'line))


;; ----------------------------------------------------------------------------------
;; command definitions
;; ----------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------
;; dired commands
;; ----------------------------------------------------------------------------------

;; dired open file
(phony-defun sacha-phony-open-file ("open file")
  "Open file under cursor"
  (dired-find-file))

;; ----------------------------------------------------------------------------------
;; go to line number
;; ----------------------------------------------------------------------------------

;; go to line N
(phony-defun sacha-phony-goto-line ("go to line" (n number))
  "Go to line N"
  (goto-line n))


;; ----------------------------------------------------------------------------------
;; buffer commands
;; ----------------------------------------------------------------------------------

;; save buffer
(phony-defun sacha-phony-save-buffer ("save")
  "Save current buffer"
  (save-buffer))

;; kill buffer
(phony-defun sacha-phony-kill-buffer ("close")
  "Close current buffer"
  (kill-current-buffer))

;; mark buffer
(phony-defun sacha-phony-mark-buffer ("mark buffer")
  "Select full buffer"
  (mark-whole-buffer))

;; list buffers
(phony-defun sacha-phony-list-buffers ("list buffers")
  "List open buffers"
  (list-buffers))

;; switch buffer
(phony-defun sacha-phony-switch-buffer ("switch buffer")
  "Switch active buffer"
  (consult-buffer))

;; messages buffer
(phony-defun sacha-phony-messages ("messages")
  "Message buffer."
  (switch-to-buffer "*Messages*"))


;; ----------------------------------------------------------------------------------
;; window commands
;; ----------------------------------------------------------------------------------

;; switch to other window
(phony-defun sacha-phony-other-window ("other window")
  "Switch window"
  (other-window 1))

;; delete window
(phony-defun sacha-phony-delete-window ("delete window")
  "Close window"
  (delete-window))

;; delete other window
(phony-defun sacha-phony-delete-other-window ("delete other window")
  "Close other window"
  (delete-other-windows))

;; toggle side window
(phony-defun sacha-phony-toggle-side-window ("toggle side window")
  "Toggle side window"
  (window-toggle-side-windows))

;; split-window-below
(phony-defun sacha-phony-split-window-below ("split below")
	"Split window below."
	(split-window-below))

;; split-window-right
(phony-defun sacha-phony-split-window-right ("split right")
	"Split window right."
	(split-window-right))

;; window-layout-transpose
(phony-defun sacha-phony-window-layout-transpose ("window transpose")
  "Window layout transpose."
  (window-layout-transpose))

;; window-layout-flip-leftright
(phony-defun sacha-phony-window-layout-flip-leftright ("window left right")
  "Window flip left right."
  (window-layout-flip-leftright))

;; window-layout-flip-leftright
(phony-defun sacha-phony-window-layout-flip-topdown ("window top down")
  "Window flip top down."
  (window-layout-flip-topdown))

;; window-layout-rotate-clockwise
(phony-defun sacha-phony-window-layout-rotate-clockwise ("window clockwise")
  "Window rotate clockwise."
  (window-layout-rotate-clockwise))

;; window-layout-rotate-anticlockwise
(phony-defun sacha-phony-window-layout-rotate-anticlockwise ("window anti-clockwise")
  "Window rotate anticlockwise."
  (window-layout-rotate-anticlockwise))


;; ----------------------------------------------------------------------------------
;; delete commands
;; ----------------------------------------------------------------------------------

;; delete N number of lines 
(phony-defun sacha-phony-delete-lines ("delete" (n number) "lines")
  "Delete N lines"
  (when (numberp n)
    (dotimes (i n)
      (delete-line))))

;; delete target
(phony-defun sacha-phony-delete-current ("delete" (bounds target))
  "Delete target bounds"
  (delete-region (car bounds) (cdr bounds)))

;; delete N number of words
(phony-defun sacha-phony-delete-words ("delete" (n number) "words")
  "Delete N words"
  (let ((start (point)))
    (forward-word n)
    (delete-region start (point))))

;; delete a single word
(phony-defun sacha-phony-delete-single-word ("delete" "word")
  "Delete single word"
  (let ((start (point)))
    (forward-word 1)
    (delete-region start (point))))


;; ----------------------------------------------------------------------------------
;; undo + redo
;; ----------------------------------------------------------------------------------

;; undo
(phony-defun sacha-phony-undo ("undo")
  "Undo last action"
  (undo))

;; redo
(phony-defun sacha-phony-redo ("redo")
  "Redo action"
  (undo-redo))

;; undo tree
(phony-defun sacha-phony-undo-tree ("undo tree")
  "Show undo tree"
  (undo-tree-visualize))


;; ----------------------------------------------------------------------------------
;; copy + paste
;; ----------------------------------------------------------------------------------

;; Copy current line or active region
(phony-defun sacha-phony-yank ("yank")
  "Yank line or region"
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

;; Copy line specifically
(phony-defun sacha-phony-yank-line ("yank line")
  "Yank current line"
  (kill-ring-save (line-beginning-position)
                  (line-end-position)))

;; Paste
(phony-defun sacha-phony-paste ("paste")
  "Paste from kill ring"
  (yank))


;; ----------------------------------------------------------------------------------
;; control c control c
;; ----------------------------------------------------------------------------------

;; control c control c
(phony-defun sacha-phony-ctrl-c-ctrl-c ("execute")
  "Run org-ctrl-c-ctrl-c"
  (call-interactively #'org-ctrl-c-ctrl-c))


;; ----------------------------------------------------------------------------------
;; paragraph commands
;; ----------------------------------------------------------------------------------

;; forward paragraph
(phony-defun sacha-phony-forward-paragraph ("next paragraph")
  "Go to next paragraph"
  (forward-paragraph))

;; forward paragraph
(phony-defun sacha-phony-backwards-paragraph ("previous paragraph")
  "Go to prev paragraph"
  (backward-paragraph))


;; ----------------------------------------------------------------------------------
;; text commands
;; ----------------------------------------------------------------------------------

;; new line below function
(defun my/new-line-below (&optional count)
  "Insert COUNT (default 1) indented line(s) below current line without splitting text."
  (interactive "p")
  (let ((n (or count 1)))
    (dotimes (_ n)
      (end-of-line)
      (newline-and-indent))))

;; Single new line
(phony-defun sacha-phony-new-line ("new line")
  "Insert a new indented line below."
  (my/new-line-below 1))

;; N new lines
(phony-defun sacha-phony-new-lines ((n number) "new lines")
  "Insert N new indented lines below."
  (when (numberp n)
    (my/new-line-below n)))

;; insert space
(phony-defun sacha-phony-insert-space ("space")
  "Insert space."
  (call-interactively #'just-one-space))


;; ----------------------------------------------------------------------------------
;; scroll commands
;; ----------------------------------------------------------------------------------

;; scroll up
(phony-defun sacha-phony-scroll-up ("scroll up")
 "Scroll page up"
 (scroll-up-command))

;; scroll down
(phony-defun sacha-phony-scroll-down ("scroll down")
  "Scroll page down"
  (scroll-down-command))

;; scroll to top of buffer
(phony-defun sacha-phony-scroll-top ("top")
  "Go to the first line of the buffer"
  (beginning-of-buffer))

;; scroll to bottom of buffer
(phony-defun sacha-phony-scroll-bottom ("bottom")
  "Go to the last line of the buffer"
  (end-of-buffer))


;; ----------------------------------------------------------------------------------
;; cursor commands
;; ----------------------------------------------------------------------------------

;; cursor move N number of lines up
(phony-defun sacha-phony-cursor-up-lines ("cursor" "up" (n number))
  "Cursor up N lines"
  (dotimes (i n)
    (evil-previous-line)))

;; cursor move N number of lines down
(phony-defun sacha-phony-cursor-down-lines ("cursor" "down" (n number))
  "Cursor down N lines"
  (dotimes (i n)
    (evil-next-line)))


;; ----------------------------------------------------------------------------------
;; check spelling
;; ----------------------------------------------------------------------------------

;; spell check word
(phony-defun sacha-phony-ispell-word ("spell check word")
  "Spell check word"
  (ispell-word))

;; spell check buffer
(phony-defun sacha-phony-ispell-buffer ("spell check buffer")
  "Spell check buffer"
  (ispell-buffer))


;; ----------------------------------------------------------------------------------
;; dictionary look up definition of current word
;; ----------------------------------------------------------------------------------

(phony-defun sacha-phony-dictionary ("dictionary")
  "Look up definition"
  (dictionary-lookup-definition))


;; ----------------------------------------------------------------------------------
;; org mode
;; ----------------------------------------------------------------------------------

;; org-insert-heading
(phony-defun sacha-phony-insert-heading ("insert heading")
  "Insert new heading"
  (org-insert-heading))

;; org-insert-subheading
(phony-defun sacha-phony-insert-subheading ("insert subheading")
  "Insert subheading"
  (call-interactively #'org-insert-subheading))

;; org-next-visible-heading
(phony-defun sacha-phony-next-heading ("next heading")
  "Jump to next heading"
  (org-next-visible-heading))

;; org-previous-visible-heading
(phony-defun sacha-phony-previous-heading ("previous heading")
  "Jump to prev heading"
  (org-previous-visible-heading))

;; Expand / Collapse current block or heading (toggles like TAB)
(phony-defun sacha-phony-org-toggle ("toggle element")
  "Toggle Org block or heading."
  (when (derived-mode-p 'org-mode)
    (org-cycle)))

;; Cycle global visibility across all Org headings
(phony-defun sacha-phony-org-cycle-global ("cycle headings")
  "Cycle global visibility across OVERVIEW, CONTENTS, and SHOW ALL."
  (when (derived-mode-p 'org-mode)
    (pcase org-cycle-global-status
      ('overview
       (org-content)
       (setq org-cycle-global-status 'contents)
       (message "CONTENTS"))
      ('contents
       (org-fold-show-all)
       (setq org-cycle-global-status 'all)
       (message "SHOW ALL"))
      (_
       (org-overview)
       (setq org-cycle-global-status 'overview)
       (message "OVERVIEW")))))


;; ----------------------------------------------------------------------------------
;; narrow + widen commands
;; ----------------------------------------------------------------------------------

;; narrow org tree
(phony-defun sacha-phony-narrow ("narrow")
  "Narrow subtree"
  (org-narrow-to-subtree))

;; widen org tree
(phony-defun sacha-phony-widen ("widen")
  "Widen subtree"
  (widen))


;; ----------------------------------------------------------------------------------
;; org mode source blocks
;; ----------------------------------------------------------------------------------

;; org edit source block
(phony-defun sacha-phony-edit-block ("edit block")
  "Edit source block"
  (org-edit-special))

;; org exit source block
(phony-defun sacha-phony-exit-block ("exit block")
  "Exit source block"
  (org-edit-src-exit))

;; org next source block
(phony-defun sacha-phony-next-src-block ("next block")
  "Jump to next block"
  (org-babel-next-src-block))

;; org previous source block
(phony-defun sacha-phony-previous-src-block ("previous block")
  "Jump to prev block"
  (org-babel-previous-src-block))


;; ----------------------------------------------------------------------------------
;; org babel tangle
;; ----------------------------------------------------------------------------------

;; org babel tangle block
(phony-defun sacha-phony-tangle-block ("tangle block")
  "Tangle babel block"
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-babel-tangle)))

;; org babel tangle file
(phony-defun sacha-phony-tangle-file ("tangle file")
  "Tangle current file"
  (when buffer-file-name
    (org-babel-tangle-file buffer-file-name)))


;; ----------------------------------------------------------------------------------
;; org todo state
;; ----------------------------------------------------------------------------------

;; org to do state
(phony-defun sacha-phony-org-todo-state ("to do state")
  "Change TODO state of an item."
  (org-todo))


;; ----------------------------------------------------------------------------------
;; tab commands
;; ----------------------------------------------------------------------------------

;; new tab
(phony-defun sacha-phony-new-tab ("new tab")
  "Open new tab"
  (tab-bar-new-tab))

;; close tab
(phony-defun sacha-phony-close-tab ("close tab")
  "Close current tab"
  (tab-bar-close-tab))

;; move tab forwards
(phony-defun sacha-phony-move-tab-forwards ("move tab forwards")
  "Move tab right"
  (tab-bar-move-tab 1))

;; move tab backwards
(phony-defun sacha-phony-move-tab-backwards ("move tab backwards")
  "Move tab left"
  (tab-bar-move-tab-backward))

;; next tab
(phony-defun sacha-phony-next-tab ("next tab")
  "Switch to next tab"
  (tab-bar-switch-to-next-tab))

;; previous tab
(phony-defun sacha-phony-previous-tab ("previous tab")
  "Switch to prev tab"
  (tab-bar-switch-to-prev-tab))

;; first tab
(phony-defun sacha-phony-first-tab ("first tab")
  "Switch to first tab"
  (tab-bar-select-tab 1))

;; last tab
(phony-defun sacha-phony-last-tab ("last tab")
  "Switch to last tab"
  (tab-bar-switch-to-last-tab))

;; switch to N tab
(phony-defun sacha-phony-select-tab ("switch to tab" (n number))
  "Switch to tab N"
  (when (and (numberp n) (> n 0))
    (tab-bar-select-tab n)))

;; move tab to position N
(phony-defun sacha-phony-move-tab-to ("move tab to" (n number))
  "Move current tab to position N"
  (when (numberp n)
    (tab-bar-move-tab-to n)))


;; ----------------------------------------------------------------------------------
;; display-line-numbers-mode
;; ----------------------------------------------------------------------------------

;; toggle display line numbers
(phony-defun sacha-phony-line-number ("line numbers")
  "Toggle line numbers"
  (display-line-numbers-mode 'toggle))


;; ----------------------------------------------------------------------------------
;; magit stage and commit
;; ----------------------------------------------------------------------------------

;; magit stage and commit
(defun sacha-phony-magit-stage-and-commit ()
  "Stage all modified and untracked changes silently, then open the commit message buffer."
  (interactive)
  (magit-status)
    (magit-with-toplevel
      (call-process "git" nil nil nil "add" "-A"))
    (magit-refresh)
    (magit-commit-create))

(phony-defun sacha-phony-git-commit ("git" "commit")
  "Magit stage & commit"
  (sacha-phony-magit-stage-and-commit))


;; ----------------------------------------------------------------------------------
;; treesitter commands
;; ----------------------------------------------------------------------------------

;; treesitter explore mode
(phony-defun sacha-phony-treesitter ("tree sitter")
  "Explore tree-sitter"
  (treesit-explore-mode))


;; ----------------------------------------------------------------------------------
;; selection commands
;; ----------------------------------------------------------------------------------

;; select paragraph
(phony-defun sacha-phony-select-paragraph ("select" "paragraph")
  "Select paragraph"
  (mark-paragraph))

;; select subtree
(phony-defun sacha-phony-select-subtree ("select" "subtree")
  "select subtree"
  (org-mark-subtree))

(phony-defun sacha-phony-select-sub-tree ("select" "sub tree")
  "Select subtree"
  (org-mark-subtree))

;; select line
(phony-defun sacha-phony-select-line ("select" "line")
  "Select line"
	(goto-char (line-beginning-position))
	(push-mark)
	(goto-char (line-end-position))
	(activate-mark))

;; select x number of lines
(phony-defun sacha-phony-select-lines ("select" (n number) "lines")
  "Select N number of lines"
  (goto-char (line-beginning-position))
  (push-mark)
  (forward-line (1- n))
  (goto-char (line-end-position))
  (activate-mark))

;; select contents
(phony-defun sacha-phony-select-contents ("select" "contents")
  "Select contents"
	(org-back-to-heading)
	(org-end-of-meta-data)
	(push-mark)
	(org-end-of-subtree)
	(activate-mark))

;; select element
(phony-defun sacha-phony-select-element ("select" "element")
  "Select element"
  (org-mark-element))

;; select element contents
(phony-defun sacha-phony-select-element-contents ("select" "element" "contents")
  "Select element contents"
	(let ((elem (org-element-context)))
		(goto-char (or (org-element-contents-begin elem)
		 (org-element-begin elem)))
		(push-mark)
		(goto-char (or (org-element-contents-end elem)
		 (org-element-end elem)))
		(activate-mark)))


;; toggle text to a list
(defun sacha-mark-list ()
	"Mark the current list."
	(let ((list (org-list-struct)))
		(goto-char (caar list))
		(push-mark)
		(goto-char (car (last (car list))))
		(activate-mark)))

(phony-defun pick-list ("pick" "list")
	(sacha-mark-list))

(phony-defun toggle-list ("toggle" "list")
	(unless (region-active-p)
		(if (org-list-struct)
				(sacha-mark-list)
			(mark-paragraph)))
	(org-toggle-item nil))


;; ----------------------------------------------------------------------------------
;; help buffer
;; ----------------------------------------------------------------------------------

;; help buffer
(phony-defun sacha-phony-help-buffer ("help")
  "Open Emacs help"
  (help-for-help))


;; ----------------------------------------------------------------------------------
;; bookmarks voice commands
;; ----------------------------------------------------------------------------------

(bookmark-maybe-load-default-file)

(dolist (bm bookmark-alist)
  (let* ((bm-name (car bm))
         ;; Convert the bookmark name into a valid symbol name for the function identifier
         (sym-name (intern (format "bookmark-%s" (string-replace " " "-" (downcase bm-name)))))
         (docstring (format "Jump to bookmark %s." bm-name)))
    (eval `(phony-defun ,sym-name (,bm-name)
             (bookmark-jump ,bm-name)))))


;; ----------------------------------------------------------------------------------
;; mangowm
;; ----------------------------------------------------------------------------------

(phony-define-dictionary sacha-mangowm-layout
  "MangoWM layout names mapped to `mmsg dispatch setlayout' arguments."
  '(("tile" . "tile")
    ("scroller" . "scroller")
    ("monocle" . "monocle")
    ("grid" . "grid")
    ("deck" . "deck")
    ("center tile" . "center_tile")
    ("vertical tile" . "vertical_tile")
    ("right tile" . "right_tile")
    ("vertical scroller" . "vertical_scroller")
    ("vertical grid" . "vertical_grid")
    ("vertical deck" . "vertical_deck")
    ("dwindle" . "dwindle")
    ("fair" . "fair")
    ("vertical fair" . "vertical_fair")))

(phony-defun sacha-phony-mangowm-layout ("layout" (layout sacha-mangowm-layout))
  "Switch MangoWM layout using `mmsg dispatch setlayout'."
  (call-process "mmsg" nil 0 nil "dispatch" "setlayout," layout))

;; ----------------------------------------------------------------------------------
;; MangoWM workspace / tag commands (1..9)
;; ----------------------------------------------------------------------------------

(phony-defun sacha-phony-mangowm-workspace ("workspace" (n number))
  "Switch MangoWM tag (workspace) N for N in 1..9."
  (when (and (integerp n) (<= 1 n 9))
    (call-process "mmsg" nil 0 nil "dispatch" (format "view,%d" n))))


;; ----------------------------------------------------------------------------------
;; MangoWM windows to monitor
;; ----------------------------------------------------------------------------------

;; move window to the left monitor
(phony-defun sacha-phony-mango-monitor-left ("monitor left")
  "Move window to the left monitor"
  (call-process "mmsg" nil 0 nil "dispatch" "tagmon," "left"))

;; move window to the right monitor
(phony-defun sacha-phony-mango-monitor-right ("monitor right")
  "Move window to the right monitor"
  (call-process "mmsg" nil 0 nil "dispatch" "tagmon," "right"))


;; ----------------------------------------------------------------------------------
;; MangoWM move cursor to monitor
;; ----------------------------------------------------------------------------------

;; move cursor to the left monitor
(phony-defun sacha-phony-mango-cursor-left ("cursor left")
  "Move cursor to the left monitor"
  (call-process "mmsg" nil 0 nil "dispatch" "focusmon," "left"))

;; move cursor to the right monitor
(phony-defun sacha-phony-mango-cursor-right ("cursor right")
  "Move cursor to the right monitor"
  (call-process "mmsg" nil 0 nil "dispatch" "focusmon," "right"))


;; ----------------------------------------------------------------------------------
;; MangoWM overview
;; ----------------------------------------------------------------------------------

(phony-defun sacha-phony-mango-overview ("jump mode")
  "Jump."
  (call-process "mmsg" nil 0 nil "dispatch" "togglejump"))


;; ----------------------------------------------------------------------------------
;; MangoWM focus windows
;; ----------------------------------------------------------------------------------

(phony-define-dictionary sacha-mangowm-focus-direction
  "MangoWM window focus directions mapped to dispatcher and target."
  '(("next"  . ("focusstack" . "next"))
    ("left"  . ("focusdir"   . "left"))
    ("right" . ("focusdir"   . "right"))
    ("up"    . ("focusdir"   . "up"))
    ("down"  . ("focusdir"   . "down"))))

(phony-defun sacha-phony-mangowm-focus ("focus" (dir sacha-mangowm-focus-direction))
  "Switch MangoWM window focus in direction DIR."
  (pcase-let ((`(,dispatcher . ,arg) dir))
    (call-process "mmsg" nil 0 nil "dispatch" (format "%s,%s" dispatcher arg))))


;; ----------------------------------------------------------------------------------
;; MangoWM move window
;; ----------------------------------------------------------------------------------

(phony-define-dictionary sacha-mangowm-move-direction
  "MangoWM window movement directions mapped to `mmsg dispatch move_client' arguments."
  '(("up"    . "up")
    ("down"  . "down")
    ("left"  . "left")
    ("right" . "right")))

(phony-defun sacha-phony-mangowm-move ("move" (dir sacha-mangowm-move-direction))
  "Move MangoWM window in direction DIR."
  (call-process "mmsg" nil 0 nil "dispatch" (format "move_client,%s" dir)))


;; ----------------------------------------------------------------------------------
;; fix common voice errors
;; ----------------------------------------------------------------------------------

;; fix miss spoken commands
(defvar my-common-edits
  '(
    ("previous tab" "previous time")
    ("next tab" "next time")
    ("new tab" "new time")
    ("new tab" "new tap")
    ("move tab" "move tap")
    ("last tab" "last time")
    ("first tab" "first time")
    ("save buffer" "safe buffer")
    ("element" "elements")
    ("widen" "wyden")
    ("widen" "biden")
    ("edit block" "edit lock")
    ("exit block" "edit lock")
    ("exit block" "exit lock")
    ("git commit" "get commit")
    ("dotfiles" "dot files")
    ("changelog" "change log")
    )
  "Commonly-misrecognized words or words that need special capitalization.")


(defun my-find-next-fix-point ()
  (when (re-search-forward
         (format "\\_<\\(%s\\)\\_>"
                 (downcase
                  (string-join
                   (seq-mapcat
                    (lambda (o)
                      (if (listp o)
                          (if (string= (car o) "") (cdr o) o)
                        (list o)))
                    my-common-edits)
                   "\\|")))
         nil t)
    (goto-char (match-beginning 0))
    (seq-find
     (lambda (o)
       (if (listp o)
           (seq-find (lambda (s)
                       (save-match-data
                         (string-match
                          (format "\\_<%s\\>" (downcase s))
                          (downcase (match-string 0))))) o)
         (string= (downcase o) (downcase (match-string 0)))))
     my-common-edits)))

(defun my-fix-common-errors ()
  (interactive)
  (let (done entry correction)
    (while (and
            (not done)
            (setq entry (my-find-next-fix-point)))
      (setq correction (if (listp entry) (car entry) entry))
      (if (let ((case-fold-search nil))
            (looking-at (regexp-quote correction)))
          (forward-word 1)
        (if (called-interactively-p 'any)
            (let* ((c (read-char (format "%s (yn.): " correction))))
              (cond
               ((= c ?y) (replace-match correction t t))
               ((= c ?n) (goto-char (match-end 0)))
               ((= c ?.) (setq done t))))
          (replace-match correction t t))))))


;; ----------------------------------------------------------------------------------
;; fix common errors
;; ----------------------------------------------------------------------------------

(with-eval-after-load 'whisper
  (add-hook 'whisper-after-transcription-hook #'my-fix-common-errors -100))


;; ----------------------------------------------------------------------------------
;; voice command help buffer
;; ----------------------------------------------------------------------------------

;; phony speech commands help
(defun phony-speech-commands--get-help-content ()
  "Parse source buffer and return the formatted help text string."
  (let* ((source-buf (or (get-buffer "phony-speech-commands.el")
                         (find-buffer-visiting (locate-library "phony-speech-commands"))))
         (source-content (if source-buf
                             (with-current-buffer source-buf (buffer-string))
                           (let ((file-path (or (locate-library "phony-speech-commands")
                                                (expand-file-name "phony-speech-commands.el" default-directory))))
                             (if (file-exists-p file-path)
                                 (with-temp-buffer
                                   (insert-file-contents file-path)
                                   (buffer-string))
                               ""))))
         results)
    (with-temp-buffer
      (insert source-content)
      (goto-char (point-min))
      (when (search-forward ";; command definitions" nil t)
        (while (< (point) (point-max))
          (condition-case nil
              (let* ((form (read (current-buffer)))
                     (op (car-safe form)))
                (when (eq op 'phony-defun)
                  (let* ((pattern (nth 2 form))
                         (body (nthcdr 3 form))
                         (body (if (keywordp (car body)) (nthcdr 2 body) body))
                         (docstring (when (stringp (car body)) (car body)))
                         (pattern-str
                          (cond
                           ((stringp pattern) pattern)
                           ((listp pattern)
                            (mapconcat (lambda (item)
                                         (cond
                                          ((stringp item) item)
                                          ((listp item) (format "<%s>" (car item)))
                                          ((symbolp item) (symbol-name item))
                                          (t "")))
                                       pattern
                                       " "))
                           (t ""))))
                    (when (and pattern-str (not (string-empty-p pattern-str)) docstring)
                      (push (format "%s\n  %s\n\n" pattern-str docstring) results)))))
            (end-of-file (goto-char (point-max)))
            (error (forward-char 1))))))
    (with-temp-buffer
      (insert "Voice Commands Help\n\n")
      (dolist (item (nreverse results))
        (insert item))
      (buffer-string))))

;; phony speech commands help side window
(defun phony-speech-commands-display-side-window (buf)
  "Display BUF in a side window taking 15% on the right."
  (display-buffer
   buf
   '(display-buffer-in-side-window
     (side . right)
     (window-width . 0.20))))

;; phony speech command help
(defun phony-speech-commands-help ()
  "Create a side window listing voice commands and their docstrings."
  (interactive)
  (let ((content (phony-speech-commands--get-help-content))
        (buf (get-buffer-create "*Voice Commands Help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min)))
      ;; 1. Make the buffer read-only so text cannot be entered
      (setq buffer-read-only t)
      ;; 2. Bind 'q' to close the window (works in standard and Evil states)
      (local-set-key (kbd "q") (lambda () (interactive) (quit-window t)))
      (when (bound-and-true-p evil-mode)
        (evil-local-set-key 'normal (kbd "q") (lambda () (interactive) (quit-window t)))))
    (phony-speech-commands-display-side-window buf)))

;; toggle voice command help side window
(phony-defun sacha-phony-toggle-help-window ("voice")
  "Toggle help side window"
  (let ((win (get-buffer-window "*Voice Commands Help*" t)))
    (if win
        (delete-window win)
      (phony-speech-commands-help))))


;; ----------------------------------------------------------------------------------
;; provide phony-speech-commands
;; ----------------------------------------------------------------------------------

(provide 'phony-speech-commands)
