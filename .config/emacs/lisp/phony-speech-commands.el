;;; -*- lexical-binding: t; -*-
;; ----------------------------------------------------------------------------------
;; phony-speech-commands.el
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
;; phony-define-dictionary
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
;; phony defun commands
;; ----------------------------------------------------------------------------------

;; region
(phony-define-open-rule region)

(phony-defun current-line "line"
  :contributes-to region
  (bounds-of-thing-at-point 'line))

(phony-defun current-sentence "sentence"
  :contributes-to region
  (bounds-of-thing-at-point 'sentence))

(phony-defun delete-current ("delete" (bounds region))
  (delete-region (car bounds) (cdr bounds)))


;; ----------------------------------------------------------------------------------
;; undo - redo
;; ----------------------------------------------------------------------------------

(phony-defun undo ("undo") (undo))
(phony-defun redo ("redo") (undo-redo))


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
;; paragraph commands
;; ----------------------------------------------------------------------------------

(phony-defun pick-paragraph ("pick" "paragraph") (mark-paragraph))
(phony-defun pick-subtree ("pick" "subtree") (org-mark-subtree))
(phony-defun pick-sub-tree ("pick" "sub tree") (org-mark-subtree))
(phony-defun pick-function ("pick" "function")
	(mark-defun)
	(activate-mark))
(phony-defun pick-contents ("pick" "contents")
	(org-back-to-heading)
	(org-end-of-meta-data)
	(push-mark)
	(org-end-of-subtree)
	(activate-mark))
(phony-defun pick-element ("pick" "element") (org-mark-element))
(phony-defun pick-line ("pick" "line")
	(goto-char (line-beginning-position))
	(push-mark)
	(goto-char (line-end-position))
	(activate-mark))

(defun sacha-mark-list ()
	"Mark the current list."
	(let ((list (org-list-struct)))
		(goto-char (caar list))
		(push-mark)
		(goto-char (car (last (car list))))
		(activate-mark)))

(phony-defun pick-list ("pick" "list")
	(sacha-mark-list))

(phony-defun pick-element-contents ("pick" "element" "contents")
	(let ((elem (org-element-context)))
		(goto-char (or (org-element-contents-begin elem)
									 (org-element-begin elem)))
		(push-mark)
		(goto-char (or (org-element-contents-end elem)
									 (org-element-end elem)))
		(activate-mark)))

(phony-defun toggle-list ("toggle" "list")
	(unless (region-active-p)
		(if (org-list-struct)
				(sacha-mark-list)
			(mark-paragraph)))
	(org-toggle-item nil))


(phony-define-open-rule target)
(phony-defun region "region"
	:contributes-to target
	(car (region-beginning)
			 (region-end)))
(phony-defun function "function"
	:contributes-to target
	(bounds-of-thing-at-point 'defun))
(phony-defun function "sentence"
	:contributes-to target
	(bounds-of-thing-at-point 'sentence))
(phony-defun function "buffer"
	:contributes-to target
	(bounds-of-thing-at-point 'buffer))


;; ----------------------------------------------------------------------------------
;; bookmarks
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

(defvar my-common-edits
  '(
    ("dotfiles" "dot files")
    ("changelog" "change log")
    ("widen" "wyden")
    ("previous tab" "previous time")
    ("next tab" "next time")
    ("new tab" "new time")
    ("new tab" "new tap")
    ("move tab" "move tap")
    ("last tab" "last time")
    ("first tab" "first time")
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
