;;; -*- lexical-binding: t; -*-
;;; silero-vad.el

;; ----------------------------------------------------------------------------------
;; VAD Handlers
;; ----------------------------------------------------------------------------------

(defun my-sacha-vad-start-handler ()
  "Automatically start whisper recording when speech begins, unless already busy."
  (unless (or (whisper-recording-p) 
              (whisper-transcribing-p)
              (get-process "whisper-recording"))
    (when (fboundp 'my-whisper-set-temp-filename)
      (my-whisper-set-temp-filename))
    (setq sacha-whisper-current-mode 'command)
    (whisper-run)))

(defun my-sacha-vad-end-handler ()
  "Automatically queue and continue when speech pauses."
  (when (whisper-recording-p)
    (my-whisper-continue)))

(with-eval-after-load 'speech-input-vad
  (add-hook 'speech-input-vad-on-start-functions #'my-sacha-vad-start-handler)
  (add-hook 'speech-input-vad-on-end-functions #'my-sacha-vad-end-handler))


;;(speech-input-vad-start)

;; ----------------------------------------------------------------------------------
;; Queue Core & Controls
;; ----------------------------------------------------------------------------------

(defvar my-whisper--queue nil)
(defvar-local my-whisper--queue-item nil)

(defun my-whisper-continue (&optional arg)
  "Send what we've got so far for transcription and then continue recording."
  (interactive "P")
  (require 'whisper)
  (if arg
      (my-whisper-done)
    (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
    (when (process-live-p whisper--recording-process)
      (unless (string= (plist-get (car (last my-whisper--queue)) :file)
                       whisper--temp-file)
        (add-to-list 'my-whisper--queue
                     (list :file whisper--temp-file
                           :buffer (format "*result: %s*" (file-name-base whisper--temp-file)))
                     t))
      (set-process-sentinel whisper--recording-process
                            (lambda (_process _event)
                              (my-whisper-process-queue)))
      (interrupt-process whisper--recording-process))
    (run-hooks 'whisper-before-transcription-hook)
    (whisper--setup-mode-line :show 'recording)
    (whisper--record-audio)))

(defun my-whisper-done ()
  (interactive)
  (when (process-live-p whisper--recording-process)
    (add-to-list 'my-whisper--queue
                 (list :file whisper--temp-file
                       :buffer (format "*result: %s*" (file-name-base whisper--temp-file)))
                 t)
    (set-process-sentinel whisper--recording-process
                          (lambda (_process _event)
                            (my-whisper-process-queue)))
    (whisper--setup-mode-line :hide 'recording)
    (interrupt-process whisper--recording-process)))

(defun my-whisper-discard ()
  "Ignore the previous recording."
  (interactive)
  (when (process-live-p whisper--recording-process)
    (set-process-sentinel whisper--recording-process
                          (lambda (_process _event)
                            (when (file-exists-p whisper--temp-file)
                              (delete-file whisper--temp-file))
                            (my-whisper-process-queue)))
    (interrupt-process whisper--recording-process)))

(defun my-whisper-discard-and-continue ()
  "Ignore the previous recording and continue."
  (interactive)
  (if (process-live-p whisper--recording-process)
      (progn
        (set-process-sentinel whisper--recording-process
                              (lambda (_process _event)
                                (my-whisper-process-queue)
                                (my-whisper-continue)))
        (interrupt-process whisper--recording-process))
    (my-whisper-continue)))


;; ----------------------------------------------------------------------------------
;; Queue Processing (Sacha Chua's Buffer-Native Pattern)
;; ----------------------------------------------------------------------------------

(defun my-whisper-process-queue-result ()
  "Process items in queue that have completed results."
  (while (and my-whisper--queue (plist-get (car my-whisper--queue) :results))
    (let* ((o (pop my-whisper--queue))
           (text (plist-get o :results)))
      (unless (bound-and-true-p my-whisper-target-markers)
        (setq whisper--marker (point-marker)
              whisper--point-buffer (current-buffer)))
      (when (get-buffer (plist-get o :buffer))
        (with-current-buffer (plist-get o :buffer)
          (erase-buffer)
          (insert text)))
      ;; Dispatch text to Phony / Dictation
      (when (fboundp 'sacha-whisper-dispatcher)
        (sacha-whisper-dispatcher text)))))

(defun my-whisper-process-queue ()
  "Process pending items in `my-whisper--queue` using curl."
  (let (o)
    (while (setq o (seq-find (lambda (item) 
                               (and (plist-get item :file)
                                    (not (plist-get item :process))
                                    (not (plist-get item :results))))
                             my-whisper--queue))
      (let* ((headers (list "Content-Type: multipart/form-data"))
             (params (list (concat "file=@" (plist-get o :file))
                           "temperature=0.0"
                           "temperature_inc=0.2"
                           "response_format=json"
                           (concat "model=" whisper-model)
                           (concat "language=" whisper-language)))
             (url my-whisper-url-format)
             (command `("curl" "-s" ,url
                        ,@(mapcan (lambda (h) (list "-H" h)) headers)
                        ,@(mapcan (lambda (p) (list "-F" p)) params))))
        
        ;; Prepare process output buffer
        (with-current-buffer (get-buffer-create (plist-get o :buffer))
          (erase-buffer))
        
        ;; Start curl process
        (plist-put
         o :process
         (make-process
          :name "whisper-curl"
          :command command
          :buffer (plist-get o :buffer)
          :coding 'utf-8
          :sentinel
          (lambda (process event)
            (when (string-equal "finished\n" event)
              (with-current-buffer (process-buffer process)
                (let ((current my-whisper--queue-item))
                  (when (and current (get-buffer (plist-get current :buffer)))
                    (goto-char (point-min))
                    (let ((parsed (condition-case nil
                                      (json-parse-buffer)
                                    (error nil))))
                      (plist-put current :results
                                 (or (and (hash-table-p parsed) (gethash "text" parsed))
                                     (string-trim (buffer-string))
                                     "(error)")))))))
            (my-whisper-process-queue-result))))
        
        (plist-put o :command (string-join command " "))
        
        ;; Set buffer-local item so sentinel finds it in (process-buffer process)
        (with-current-buffer (process-buffer (plist-get o :process))
          (setq-local my-whisper--queue-item o))))))


;; ----------------------------------------------------------------------------------
;; Queue Management & Maintenance
;; ----------------------------------------------------------------------------------

(defun my-whisper-reprocess-queue ()
  (interactive)
  (setq whisper--marker (point-marker) whisper--point-buffer (current-buffer))
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (my-whisper-process-queue))

(defun my-whisper-clear-queue ()
  (interactive)
  (mapc (lambda (o)
          (when (process-live-p (plist-get o :process))
            (kill-process (plist-get o :process)))
          (when (get-buffer (plist-get o :buffer))
            (kill-buffer (plist-get o :buffer)))
          (plist-put o :process nil)
          (plist-put o :results nil))
        my-whisper--queue)
  (setq my-whisper--queue nil))

(provide 'silero-vad)
