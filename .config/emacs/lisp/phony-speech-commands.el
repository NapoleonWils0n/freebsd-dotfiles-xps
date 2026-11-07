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
;; phony defun commands
;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;; undo - redo
;; ----------------------------------------------------------------------------------

(phony-defun undo ("undo") (undo))
(phony-defun redo ("redo") (undo-redo))


;; ----------------------------------------------------------------------------------
;; check spelling
;; ----------------------------------------------------------------------------------

(phony-defun ispell ("spelling") (ispell))


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

;; first tab
(phony-defun first-tab ("first tab") (tab-bar-select-tab 1))
(phony-defun first-tab-tap ("first tap") (tab-bar-select-tab 1))
(phony-defun first-tab-time ("first time") (tab-bar-select-tab 1))

;; last tab
(phony-defun last-tab ("last tab") (tab-bar-switch-to-last-tab))
(phony-defun last-tab-tap ("last tap") (tab-bar-switch-to-last-tab))
(phony-defun last-tab-time ("last time") (tab-bar-switch-to-last-tab))


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
;; provide sacha-phony
;; ----------------------------------------------------------------------------------

(provide 'phony-speech-commands)
