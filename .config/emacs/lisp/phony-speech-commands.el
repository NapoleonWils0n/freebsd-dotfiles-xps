;;; -*- lexical-binding: t; -*-
;; ----------------------------------------------------------------------------------
;; phony-speech-commands.el
;; https://github.com/sachac/phony.el
;; ----------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------
;; require phony-parser
;; ----------------------------------------------------------------------------------

(require 'phony-parser)
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
  "Evaluate TEXT via phony. Pass cleaned text through if not parsed.
Add this to `whisper-insert-text-at-point'."
  (message "Phony received raw text: %S" text)
  (condition-case nil
      (progn
        ;; Lowercase the first letter to match command definitions
        (sacha-phony-evaluate
         (concat (downcase (substring text 0 1))
                 (if (> (length text) 1) (substring text 1) "")))
        ;; Return empty string if successfully parsed & executed (stops further processing)
        "")
    (error
     ;; If not parsed, trim only leading whitespace using built-in string-trim
     (string-trim text "[ \t\n\r]+" nil))))


;; ----------------------------------------------------------------------------------
;; phony-define-dictionary - numbers
;; ----------------------------------------------------------------------------------

;; define numbers
(phony-define-dictionary number
  '(("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)
    ("ten" . 10)))


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
;; delete commands
;; ----------------------------------------------------------------------------------

;; delete x number of lines
(phony-defun delete-lines ("delete" (n number) "lines")
  (dotimes (i n)
    (delete-line)))

;; delete target
(phony-defun delete-current ("delete" (bounds target))
  (delete-region (car bounds) (cdr bounds)))

;; delete x number of words
(phony-defun delete-words ("delete" (n number) "words")
  (let ((start (point)))
    (forward-word n)
    (delete-region start (point))))

;; delete a single word
(phony-defun delete-single-word ("delete" "word")
  (let ((start (point)))
    (forward-word 1)
    (delete-region start (point))))


;; ----------------------------------------------------------------------------------
;; undo + redo
;; ----------------------------------------------------------------------------------

;; undo
(phony-defun undo ("undo")
  (undo))

;; redo
(phony-defun redo ("redo")
  (undo-redo))

;; undo tree
(phony-defun undo ("undo tree")
  (undo-tree-visualize))


;; ----------------------------------------------------------------------------------
;; copy + paste
;; ----------------------------------------------------------------------------------

;; Copy current line or active region
(phony-defun yank ("yank")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

;; Copy line specifically
(phony-defun yank-line ("yank line")
  (kill-ring-save (line-beginning-position)
                  (line-end-position)))

;; Paste
(phony-defun paste ("paste")
  (yank))


;; ----------------------------------------------------------------------------------
;; check spelling
;; ----------------------------------------------------------------------------------

;; spell check word
(phony-defun ispell-word ("spell check word")
  (ispell-word))

;; spell check buffer
(phony-defun ispell-buffer ("spell check buffer")
  (ispell-buffer))


;; ----------------------------------------------------------------------------------
;; open dictionary
;; ----------------------------------------------------------------------------------

(phony-defun dictionary ("dictionary")
  (dictionary-lookup-definition))


;; ----------------------------------------------------------------------------------
;; scroll commands
;; ----------------------------------------------------------------------------------

;; scroll up
(phony-defun scroll-up ("scroll up")
  (scroll-up-command))

;; scroll down
(phony-defun scroll-down ("scroll down")
  (scroll-down-command))


;; ----------------------------------------------------------------------------------
;; buffer commands
;; ----------------------------------------------------------------------------------

;; mark buffer
(phony-defun mark-buffer ("mark buffer")
  (mark-whole-buffer))

;; save buffer
(phony-defun save-buffer ("save buffer")
  (save-buffer))

;; kill buffer
(phony-defun kill-buffer ("kill buffer")
  (kill-current-buffer))

;; switch buffer
(phony-defun switch-buffer ("switch buffer")
  (consult-buffer))


;; ----------------------------------------------------------------------------------
;; tab commands
;; ----------------------------------------------------------------------------------

;; new tab
(phony-defun new-tab ("new tab")
  (tab-bar-new-tab))

;; close tab
(phony-defun close-tab ("close tab")
  (tab-bar-close-tab))

;; move tab forwards
(phony-defun move-tab-forwards ("move tab forwards")
  (tab-bar-move-tab 1))

;; move tab backwards
(phony-defun move-tab-backwards ("move tab backwards")
  (tab-bar-move-tab-backward))

;; next tab
(phony-defun next-tab ("next tab")
  (tab-bar-switch-to-next-tab))

;; previous tab
(phony-defun previous-tab ("previous tab")
  (tab-bar-switch-to-prev-tab))

;; first tab
(phony-defun first-tab ("first tab")
  (tab-bar-select-tab 1))

;; last tab
(phony-defun last-tab ("last tab")
  (tab-bar-switch-to-last-tab))


;; ----------------------------------------------------------------------------------
;; window commands
;; ----------------------------------------------------------------------------------

;; switch to other window
(phony-defun other-window ("other window")
  (other-window))

;; delete window
(phony-defun delete-window ("delete window")
  (delete-window))

;; delete other window
(phony-defun delete-other-window ("delete other window")
  (delete-other-windows))

;; toggle side window
(phony-defun toggle-side-window ("toggle side window")
  (window-toggle-side-windows))


;; ----------------------------------------------------------------------------------
;; narrow + widen commands
;; ----------------------------------------------------------------------------------

;; narrow org tree
(phony-defun narrow ("narrow")
  (org-narrow-to-subtree))

;; widen org tree
(phony-defun widen ("widen")
  (widen))


;; ----------------------------------------------------------------------------------
;; treesitter commands
;; ----------------------------------------------------------------------------------

;; treesitter explore mode
(phony-defun treesitter ("tree sitter")
  (treesit-explore-mode))


;; ----------------------------------------------------------------------------------
;; org mode headings
;; ----------------------------------------------------------------------------------

;; org-next-visible-heading
(phony-defun next-heading ("next heading")
  (org-next-visible-heading))

;; org-previous-visible-heading
(phony-defun previous-heading ("previous heading")
  (org-previous-visible-heading))

;; org-insert-heading
(phony-defun insert-heading ("insert heading")
  (org-insert-heading))

;; org-insert-subheading
(phony-defun insert-subheading ("insert subheading")
  (org-insert-subheading))


;; ----------------------------------------------------------------------------------
;; org mode source blocks
;; ----------------------------------------------------------------------------------

;; org edit source block
(phony-defun edit-block ("edit block")
  (org-edit-special))

;; org source block exit
(phony-defun exit-block ("exit block")
  (org-edit-src-exit))

;; org next source block
(phony-defun next-src-block ("next source block")
  (org-babel-next-src-block))

;; org previous source block
(phony-defun previous-src-block ("previous source block")
  (org-babel-previous-src-block))


;; ----------------------------------------------------------------------------------
;; org babel tangle
;; ----------------------------------------------------------------------------------

;; org babel tangle block
(phony-defun tangle-block ("tangle block")
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-babel-tangle)))

;; org babel tangle file
(phony-defun tangle-file ("tangle file")
  (when buffer-file-name
    (org-babel-tangle-file buffer-file-name)))


;; ----------------------------------------------------------------------------------
;; magit stage and commit
;; ----------------------------------------------------------------------------------

(defun magit-stage-and-commit ()
  "Stage all modified and untracked changes silently, then open the commit message buffer."
  (interactive)
  (magit-status)
    (magit-with-toplevel
      (call-process "git" nil nil nil "add" "-A"))
    (magit-refresh)
    (magit-commit-create))

(phony-defun git-commit ("git" "commit")
  (magit-stage-and-commit))



;; ----------------------------------------------------------------------------------
;; display-line-numbers-mode
;; ----------------------------------------------------------------------------------

;; toggle display line numbers
(phony-defun line-number ("line numbers")
  (display-line-numbers-mode 'toggle))


;; ----------------------------------------------------------------------------------
;; help buffer
;; ----------------------------------------------------------------------------------

;; help buffer
(phony-defun help-buffer ("help")
  (help-for-help))


;; ----------------------------------------------------------------------------------
;; selection commands
;; ----------------------------------------------------------------------------------

;; select paragraph
(phony-defun select-paragraph ("select" "paragraph")
  (mark-paragraph))

;; select subtree
(phony-defun select-subtree ("select" "subtree")
  (org-mark-subtree))

(phony-defun select-sub-tree ("select" "sub tree")
  (org-mark-subtree))

;; select line
(phony-defun select-line ("select" "line")
	(goto-char (line-beginning-position))
	(push-mark)
	(goto-char (line-end-position))
	(activate-mark))

;; select x number of lines
(phony-defun select-lines ("select" (n number) "lines")
  (goto-char (line-beginning-position))
  (push-mark)
  (forward-line (1- n))
  (goto-char (line-end-position))
  (activate-mark))

;; select contents
(phony-defun select-contents ("select" "contents")
	(org-back-to-heading)
	(org-end-of-meta-data)
	(push-mark)
	(org-end-of-subtree)
	(activate-mark))

;; select element
(phony-defun select-element ("select" "element")
  (org-mark-element))

;; select element contents
(phony-defun select-element-contents ("select" "element" "contents")
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
;; send command to tmux buffer
;; ----------------------------------------------------------------------------------

(phony-defun tmux-send ("send command")
  (my/emamux-smart-send))


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
    ("edit block" "edit lock")
    ("exit block" "edit lock")
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
;; provide phony-speech-commands
;; ----------------------------------------------------------------------------------

(provide 'phony-speech-commands)
