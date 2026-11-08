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
  "Evaluate TEXT via phony. Pass TEXT through if not parsed.
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
     ;; If not parsed or an error occurs, pass text through
     text)))


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

(phony-defun undo ("undo") (undo))
(phony-defun redo ("redo") (undo-redo))

;; undo tree
(phony-defun undo ("undo tree") (undo-tree-visualize))


;; ----------------------------------------------------------------------------------
;; copy + paste
;; ----------------------------------------------------------------------------------

;; evil yank (copy)
(phony-defun copy ("copy") (evil-yank))

;; evil yank line (copy)
(phony-defun copy-line ("copy line") (evil-yank-line))

;; evil paste
(phony-defun paste ("paste") (evil-paste-after))


;; ----------------------------------------------------------------------------------
;; check spelling
;; ----------------------------------------------------------------------------------

(phony-defun ispell ("spell check") (ispell-buffer))


;; ----------------------------------------------------------------------------------
;; open dictionary
;; ----------------------------------------------------------------------------------

(phony-defun dictionary ("dictionary") (dictionary-lookup-definition))


;; ----------------------------------------------------------------------------------
;; send command to tmux buffer
;; ----------------------------------------------------------------------------------

(phony-defun tmux-send ("send command") (my/emamux-smart-send))


;; ----------------------------------------------------------------------------------
;; scroll commands
;; ----------------------------------------------------------------------------------

(phony-defun scroll-up ("scroll up") (scroll-up-command))
(phony-defun scroll-down ("scroll down") (scroll-down-command))


;; ----------------------------------------------------------------------------------
;; buffer commands
;; ----------------------------------------------------------------------------------

(phony-defun mark-buffer ("mark buffer") (mark-whole-buffer))
(phony-defun save-buffer ("save buffer") (save-buffer))
(phony-defun kill-buffer ("kill buffer") (kill-current-buffer))
(phony-defun switch-buffer ("switch buffer") (consult-buffer))


;; ----------------------------------------------------------------------------------
;; tab commands
;; ----------------------------------------------------------------------------------

(phony-defun new-tab ("new tab") (tab-bar-new-tab))
(phony-defun close-tab ("close tab") (tab-bar-close-tab))
(phony-defun move-tab-forwards ("move tab forwards") (tab-bar-move-tab))
(phony-defun move-tab-backwards ("move tab backwards") (tab-bar-move-tab-backward))
(phony-defun next-tab ("next tab") (tab-bar-switch-to-next-tab))
(phony-defun previous-tab ("previous tab") (tab-bar-switch-to-prev-tab))
(phony-defun first-tab ("first tab") (tab-bar-select-tab 1))
(phony-defun last-tab ("last tab") (tab-bar-switch-to-last-tab))


;; ----------------------------------------------------------------------------------
;; window commands
;; ----------------------------------------------------------------------------------

(phony-defun other-window ("other window") (other-window))
(phony-defun delete-window ("delete window") (delete-window))
(phony-defun delete-other-window ("delete other window") (delete-other-windows))
(phony-defun toggle-side-window ("toggle side window") (window-toggle-side-windows))


;; ----------------------------------------------------------------------------------
;; narrow + widen commands
;; ----------------------------------------------------------------------------------

(phony-defun narrow ("narrow") (org-narrow-to-subtree))
(phony-defun widen ("widen") (widen))


;; ----------------------------------------------------------------------------------
;; treesitter commands
;; ----------------------------------------------------------------------------------

(phony-defun treesitter ("tree sitter") (treesit-explore-mode))


;; ----------------------------------------------------------------------------------
;; org mode headings
;; ----------------------------------------------------------------------------------

;; org-next-visible-heading
(phony-defun next-heading ("next heading") (org-next-visible-heading))

;; org-previous-visible-heading
(phony-defun previous-heading ("previous heading") (org-previous-visible-heading))

;; org-insert-heading
(phony-defun insert-heading ("insert heading") (org-insert-heading))

;; org-insert-subheading
(phony-defun insert-subheading ("insert sub heading") (org-insert-subheading))


;; ----------------------------------------------------------------------------------
;; org mode source blocks
;; ----------------------------------------------------------------------------------

;; org edit source block
(phony-defun edit-block ("edit block") (org-edit-special))

;; org source block exit
(phony-defun exit-block ("block exit") (org-edit-src-exit))

;; org next source block
(phony-defun next-src-block ("next source block") (org-babel-next-src-block))

;; org previous source block
(phony-defun previous-src-block ("previous source block") (org-babel-previous-src-block))


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
;; selection commands
;; ----------------------------------------------------------------------------------

;; select paragraph
(phony-defun select-paragraph ("select" "paragraph") (mark-paragraph))

;; select subtree
(phony-defun select-subtree ("select" "subtree") (org-mark-subtree))
(phony-defun select-sub-tree ("select" "sub tree") (org-mark-subtree))

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
(phony-defun select-element ("select" "element") (org-mark-element))

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
