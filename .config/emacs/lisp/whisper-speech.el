;;; -*- lexical-binding: t; -*-
;; ----------------------------------------------------------------------------------
;; whisper-speech
;; ----------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------
;; set whisper output directory
;; ----------------------------------------------------------------------------------

(with-eval-after-load 'whisper
  (add-hook 'whisper-before-transcription-hook #'my-whisper-set-temp-filename))


;; ----------------------------------------------------------------------------------
;; run commands or insert text
;; ----------------------------------------------------------------------------------

(with-eval-after-load 'whisper
  (setq whisper-insert-text-at-point
        '(my-whisper-handle-commands
          speech-input-phony-process
          insert)))


;; ----------------------------------------------------------------------------------
;; Bind C-z globally to whisper-run, overriding Evil mode's default binding
;; ----------------------------------------------------------------------------------

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-z") 'whisper-run)
  (define-key evil-insert-state-map (kbd "C-z") 'whisper-run)
  (define-key evil-visual-state-map (kbd "C-z") 'whisper-run)
  (define-key evil-motion-state-map (kbd "C-z") 'whisper-run))


;; ----------------------------------------------------------------------------------
;; handle whisper commands with whisper-server
;; ----------------------------------------------------------------------------------

(defun my-whisper-handle-commands (text)
  (interactive (list (if (region-active-p)
                         (buffer-substring (region-beginning) (region-end))
                       (buffer-substring (line-beginning-position) (line-end-position)))))
  ;; Clean up leading spaces, quotes, or punctuation returned by the server
  (setq text (replace-regexp-in-string "^[[:space:]\"',.!?]+" "" text))
  (let (done entry func num-args)
    (while (and text (not (string= text "")) (not done))
      (setq entry
            (seq-find
             (lambda (row)
               (string-match (format "^\\(?:%s\\)\\>" (car row)) (downcase text)))
             my-whisper-commands))
      (if (null entry)
          (setq done t)
        (setq func (cdr entry))
        (setq num-args (and func (func-arity func)))
        (cond
         ((commandp func)
          (setq text (replace-match "" nil nil text))
          (call-interactively func))
         ((and (functionp func) (>= (cdr num-args) 1))
          (unless (match-string 1 text)
            (setq text (replace-match "" nil nil text)))
          (setq text (funcall func (or (match-string 1 text) text))))
         ((functionp func)
          (setq text (replace-match "" nil nil text))
          (funcall func))))
      (when (string-match "^[[:space:]\\.?,]+\\(?:and \\)?" text)
        (setq text (replace-match "" nil nil text))))
    (when (string= text "") (setq text nil))
    (or text "")))


;; ----------------------------------------------------------------------------------
;; Load default bookmarks so bookmark-alist is populated
;; ----------------------------------------------------------------------------------

(bookmark-maybe-load-default-file)


;; ----------------------------------------------------------------------------------
;; my whisper commands
;; ----------------------------------------------------------------------------------

(defvar my-whisper-commands
  `(("voice commands" . commands-sidebar)
    ("scroll up" . scroll-down-command)
    ("scroll down" . scroll-up-command)
    ("mark buffer" . mark-whole-buffer)
    ("save buffer" . save-buffer)
    ("kill buffer" . kill-current-buffer)
    ("switch buffer" . consult-buffer)
    ("new tab" . tab-bar-new-tab)
    ("close tab" . tab-bar-close-tab)
    ("move tab forwards" . tab-bar-move-tab)
    ("move tab backwards" . tab-bar-move-tab-backward)
    ("next tab" . tab-bar-switch-to-next-tab)
    ("previous tab" . tab-bar-switch-to-prev-tab)
    ("first tab" . (lambda () (tab-bar-select-tab 1)))
    ("last tab" . tab-bar-switch-to-last-tab)
    ("other window" . other-window)
    ("toggle side window" . window-toggle-side-windows)
    ("mark paragraph" . mark-paragraph)
    ("dictionary" . dictionary-lookup-definition)
    ("spelling" . ispell)
    ("narrow" . org-narrow-to-subtree)
    ("widen" . widen)))


;; ----------------------------------------------------------------------------------
;; Dynamically inject all bookmarks from ~/.config/emacs/bookmarks into whisper commands
;; ----------------------------------------------------------------------------------

(dolist (bm bookmark-alist)
  (let ((bm-name (car bm)))
    (add-to-list 'my-whisper-commands
                 (cons bm-name (lambda () (interactive) (bookmark-jump bm-name))))))


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
;; display recording icons in the buffer
;; ----------------------------------------------------------------------------------

(defvar whisper--current-overlay nil
  "Overlay to show whisper status at point.")

(advice-add 'whisper--setup-mode-line :override
  (lambda (command phase)
    (when whisper-show-progress-in-mode-line
      (with-current-buffer (or whisper--point-buffer (current-buffer))
        (if (eq command :show)
            (let ((pos (point)))
              (when (and whisper--current-overlay
                         (overlay-buffer whisper--current-overlay))
                (delete-overlay whisper--current-overlay))
              (setq whisper--current-overlay (make-overlay pos pos))
              (let* ((ind (whisper--mode-line-indicator phase))
                     (ind-string (cond
                                   ((stringp ind) ind)
                                   ((eq (car-safe ind) :eval) (eval (nth 1 ind)))
                                   (t ""))))
                ;; Removed the leading space inside the concat
                (overlay-put whisper--current-overlay 'after-string
                             (concat ind-string " "))))
          (when whisper--current-overlay
            (delete-overlay whisper--current-overlay)
            (setq whisper--current-overlay nil)))))))


;; ----------------------------------------------------------------------------------
;; open side window and list voice commands
;; ----------------------------------------------------------------------------------

(defun commands-sidebar ()
  "Display available Whisper voice commands in a simple side window on the right."
  (interactive)
  (let ((buf (get-buffer-create "*Whisper Commands*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Voice Commands\n")
        (insert "--------------\n")
        
        ;; Sort commands so bookmarks appear at the bottom
        (let ((sorted-commands
               (sort (copy-sequence my-whisper-commands)
                     (lambda (a b)
                       (let ((a-bm (assoc (car a) bookmark-alist))
                             (b-bm (assoc (car b) bookmark-alist)))
                         (and (not a-bm) b-bm))))))
          
          (dolist (cmd sorted-commands)
            (insert (car cmd) "\n")))
        
        (goto-char (point-min))
        (special-mode)))
      
      ;; Open via display-buffer so display-buffer-alist handles the 20% width rule
      (display-buffer buf)))


;; Whisper Commands side window (20% width)
(add-to-list 'display-buffer-alist
             '("^*Whisper Commands*" display-buffer-in-side-window
               (side . right)
               (window-width . 0.20)))


;; ----------------------------------------------------------------------------------
;; provide whisper speech
;; ----------------------------------------------------------------------------------

(provide 'whisper-speech)
