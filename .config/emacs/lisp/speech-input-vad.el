;;; speech-input-vad.el --- Silero voice activity detection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar speech-input-vad-events-process nil)
(defvar speech-input-vad-events-dir (file-name-directory (or load-file-name (buffer-file-name))))
(defvar speech-input-vad-events-command
  `("python3"
    ,(expand-file-name "vad-events.py" speech-input-vad-events-dir)))

(defvar speech-input-vad-on-end-functions nil)
(defvar speech-input-vad-on-start-functions nil)
(defvar speech-input-vad-debug nil "Non-nil means display a message on start and end.")

(defun speech-input-vad-toggle-debug ()
  "Toggle `speech-input-vad-debug'."
  (interactive)
  (setq speech-input-vad-debug (not speech-input-vad-debug))
  (message (if speech-input-vad-debug
               "Debugging VAD."
             "Stopped debugging VAD.")))

(defun speech-input-vad-events-filter (proc string)
  (cond
   ((string-match "^START" string)
    (when speech-input-vad-debug (message "speech start"))
    (run-hooks 'speech-input-vad-on-start-functions))
   ((string-match "^END" string)
    (when speech-input-vad-debug (message "speech stop"))
    (run-hooks 'speech-input-vad-on-end-functions))))

;;;###autoload
(defun speech-input-vad-start ()
  "Start the process if it's not already running."
  (interactive)
  (unless (process-live-p speech-input-vad-events-process)
    (let ((process-environment
           (cons "AUDIODEV=/dev/dsp3" process-environment)))
      (setq speech-input-vad-events-process
            (make-process
             :name "vad-events"
             :command speech-input-vad-events-command
             :buffer (get-buffer-create "*vad-events*")
             :stderr (get-buffer-create "*vad-events-err*")
             :filter #'speech-input-vad-events-filter)))))

(defun speech-input-vad-stop ()
  (interactive)
  (when (process-live-p speech-input-vad-events-process)
    (kill-process speech-input-vad-events-process)))

(defun speech-input-vad-stop-recording-once ()
  (remove-hook 'speech-input-vad-on-end-functions 'speech-input-vad-stop-recording-once)
  (speech-input-stop-recording))

;;;###autoload
(defun speech-input-vad-record-one-turn (callback &optional filename)
  "Call CALLBACK with the filename of the recording."
  (speech-input-vad-start)
  (add-hook 'speech-input-vad-on-end-functions 'speech-input-vad-stop-recording-once)
  (speech-input-start-recording callback filename))


(provide 'speech-input-vad)
;;; speech-input-vad.el ends here
