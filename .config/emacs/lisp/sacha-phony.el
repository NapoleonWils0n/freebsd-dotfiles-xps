;;; -*- lexical-binding: t; -*-
;; ----------------------------------------------------------------------------------
;; sacha-phony
;; ----------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------
;; require phony-parser
;; ----------------------------------------------------------------------------------

(require 'phony-parser)
(define-error 'sacha-phony-not-parsed "Sentence not parsed." 'error)


;; ----------------------------------------------------------------------------------
;; sacha-phony-parser--sanitize
;; ----------------------------------------------------------------------------------

(defun sacha-phony-parser--sanitize (s)
  "Lowercase and remove punctuation."
  (string-trim (replace-regexp-in-string "[,\\.!?:]" "" (downcase s))))


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
(defun sacha-phony-evaluate (sentence)
  "Evaluate SENTENCE using phony.el definitions.
Throw 'not-parsed if not parsed."
  (let ((parses (phony-parser--parse-utterance sentence)))
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
;; phony defun
;; ----------------------------------------------------------------------------------

;; undo - redo
(phony-defun undo ("undo") (undo))
(phony-defun redo ("redo") (undo-redo))
(phony-defun undo-tree-visualize ("undo tree") (undo-tree-visualize))

;; dictionary
(phony-defun dictionary-lookup-definition ("dictionary") (dictionary-lookup-definition))

;; ispell
(phony-defun ispell ("spelling") (ispell))


;; ----------------------------------------------------------------------------------
;; provide sacha-phony
;; ----------------------------------------------------------------------------------

(provide 'sacha-phony)
