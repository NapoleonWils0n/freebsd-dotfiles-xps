;;; sacha-speech-input.el ---  -*- lexical-binding: t -*-

(defcustom sacha-whisper-mode 'all
  "Type of recognition."
  :type '(choice
          (const :tag "Commands and dictation" 'all)
          (const :tag "Commands only, no insertion" 'commands)
          (const :tag "Only dictation, ignore most commands" 'dictation-focus)
          (const :tag "Sleep" 'sleep))
  :group 'sacha)

(defvar sacha-whisper-last-annotation nil "Last annotation so we can skip duplicates.")
(defvar sacha-whisper-skip-annotation nil)
(defvar sacha-whisper-target-markers nil "List of markers to send text to.")
(defvar sacha-whisper-one-sentence-per-line t "Non-nil means split text onto separate lines.")

(defun sacha-whisper-process-commands (text)
  "Process commands unless set otherwise."
  (if (member sacha-whisper-mode '(dictation-focus sleep))
      text
    (and text (speech-input-commands-process (speech-input-phony-process text)))))

(defvar sacha-whisper-mode-indicator "" "Indicates `sacha-whisper-mode' or `sacha-whisper-sleep'.")

(defvar sacha-whisper-mode-indicators
  '((all . "✓")
    (commands . "📋")
    (dictation . "👂")
    (dictation-focus . "💬")
    (kebab . "-"))
  "Indicators.")

(defvar sacha-whisper-sleep nil "Non-nil means ignore most of the input.")

(defun sacha-whisper-filter-mode (text)
  "Discard, process, or modify TEXT based on `sacha-whisper-mode'."
  (when text
    (pcase (sacha-phony-parser--sanitize text)
      ("continuous"
       (speech-input-vad-start)
       (setq text nil))
      ("manual"
       (speech-input-vad-stop)
       (setq text nil))
      ((or "wake up" "dictation")
       (setq sacha-whisper-mode 'all text nil
             sacha-whisper-sleep nil))
      ((or "commands" "drive")
       (sacha-whisper-audio-feedback "command mode")
       (setq sacha-whisper-mode 'commands text nil
             sacha-whisper-sleep nil))
      ((or "only dictation" "dictation focus")
       (sacha-whisper-audio-feedback "dictation focus")
       (setq sacha-whisper-mode 'dictation-focus text nil sacha-whisper-sleep nil))
      ((or "kebab" "skewer")
       (sacha-whisper-audio-feedback "kebab mode")
       (setq sacha-whisper-mode 'kebab
             sacha-whisper-sleep nil
             text nil))
      ("panic"
       (sacha-obs-panic))
      ("over"
       (setq sacha-whisper-sleep (not sacha-whisper-sleep) text nil)
       (if sacha-whisper-sleep
           (sacha-whisper-audio-feedback "bye")
         (sacha-whisper-audio-feedback "hi")))
      ((or "sleep" "all done")
       (setq sacha-whisper-sleep t text nil
             sacha-whisper-mode-indicator "💤"))
      (_
       (cond
        ((string-match "\\`\\(so\\|anyway\\)?[\\.?! ]*chat\\>" text)
         (setq text nil))
        ((string-match "\\<\\(scratch that\\)[\\.?! ]*$" text)
         (setq text nil))
        ((string-match "\\<\\(over\\)[\\.?! ]*$" text)
         (setq sacha-whisper-sleep (not sacha-whisper-sleep)
               text
               (if sacha-whisper-sleep
                   nil
                 (replace-match "" nil nil text 1)))
         (if sacha-whisper-sleep
             (sacha-whisper-audio-feedback "bye")
           (sacha-whisper-audio-feedback "hi")))
        (sacha-whisper-sleep
         (setq text nil))))))
  (setq sacha-whisper-mode-indicator
        (if sacha-whisper-sleep
            "💤"
          (alist-get sacha-whisper-mode sacha-whisper-mode-indicators)))
  (when (eq sacha-whisper-mode 'commands)
    (unless (and text
                 (or (phony-parser--parse-utterance text)
                     (speech-input-commands-find text)))
      (setq text nil)))
  text)


;;;###autoload
(defun sacha-whisper-insert (text)
  "Insert TEXT at point, or at `sacha-whisper-target-markers'."
  (let ((markers
         (cond
          ((null sacha-whisper-target-markers)
           (list whisper--marker))
          ((listp sacha-whisper-target-markers)
           sacha-whisper-target-markers)
          ((markerp sacha-whisper-target-markers)
           (list sacha-whisper-target-markers))))
        (orig-point (point))
        (orig-buffer (current-buffer)))
    (when text
      (when (eq sacha-whisper-mode 'kebab)
        (setq text
              (replace-regexp-in-string
               "-$" ""
               (downcase (replace-regexp-in-string "[,\\. ]+" "-" text)))))
      (mapcar (lambda (marker)
                (with-current-buffer (or (and marker (marker-buffer marker)) (current-buffer))
                  (save-restriction
                    (widen)
                    (when (and marker
                               (marker-position marker)
                               (marker-buffer marker))
                      (goto-char marker))
                    (whisper--insert-text
                     (concat
                      (if (looking-back "[ \t\n]\\|^")
                          ""
                        " ")
                      text))
                    (when (and
                           marker
                           (marker-position marker)
                           (marker-buffer marker))
                      (move-marker marker (point))))))
              markers)
      (when sacha-whisper-target-markers
        (goto-char orig-point))
      nil)))

;; provide
(provide 'sacha-speech-input)
