;;; lazarus.el -- Open recently closed files
;;; Copyright 2011 by Andrew J Vargo <ajvargo@computer.org>

;; Lazarus is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Lazarus is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Lazarus.  If not, see <http://www.gnu.org/licenses/>.

;; Version 1.0.0

;;; Commentary:
;; Lazarus is a minor mode for opening files that have been closed.
;; The idea is to provide a 'web browser' sort of functionality to
;; recover from accidental kills. It provides several convenience
;; functions to this end. It also allows the conjuring of an interactive
;; buffer to manage or jump to the closed files.
;;
;; Lazarus will save a list of closed files when you close Emacs, and
;; read them back in when you start the mode.
;;
;; It will save the last 20 files that have been closed.
;;
;; You can customize the file path and number of files saved
;; by M-x customize-group lazarus
;;
;; recentf.el was leaned on for writing this.
;; http://stackoverflow.com/questions/2227401/how-to-get-a-list-of-last-closed-files-in-emacs
;; was also used for inspiration
;;
;;; Usage:
;;
;; 1. Add this file to your load path.
;; 2. (lazarus-mode t) ; should get you running
;;
;; Functions of note:
;;  lazarus-find-last-closed:
;;    Opens the last file that was closed.
;;    Providing a numerical prefix opens the nth file.
;;    C-u 4 M-x lazarus-find-last-closed would open the 4 most
;;    recently closed file.
;;
;;  lazarus-ido-find-file:
;;    Works with ido-mode to server closed file list through
;;    Ido's interface.
;;
;;  lazarus-flush-list:
;;    Empties the list of closed files.
;;
;;  lazarus-load-list:
;;    Loads a list of saved closed files
;;
;;  lazarus-save-list:
;;    Saves closed files list to disk.
;;    Default file is ~/.lazarus
;;
;;  lazarus-closed-files:
;;    Opens a buffer for interacting with the closed files list
;;    g     - refreshes the buffer
;;    [RET] - opens the file on the line of point
;;    q     - closes buffer window
;;    d     - removes file on the line of point from the list
;;    0-9   - convenience for opening the first 10 files in the list
;;


;;; CODE
;;
(defvar lazarus-list nil
  "List of closed files")

;;; Customizations
(defgroup lazarus nil
  "Track a list of closed files."
  :version "1.0.0"
  :group 'files)

(defcustom lazarus-max-saved-items 20
  "Maximum number of items the closed list that will be saved.
A nil value means no limit."
  :group 'lazarus
  :type 'integer)

(defcustom lazarus-save-file (convert-standard-filename "~/.lazarus")
  "File to save the recent list into."
  :group 'lazarus
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                lazarus-mode
                (lazarus-load-list)))))

(defcustom lazarus-initialize-file-name-history t
  "Non-nil means to initialize `file-name-history' with the recent list.
If `file-name-history' is not empty, do nothing."
  :group 'lazarus
  :type  'boolean)

;;; Temporary buffer for interactive selection
;;

(defun lazarus-closed-file-setup-buffer-text ()
  "Insert instructions and closed files into buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Lazarus Closed File List:

\tRET\tselect at point\t\t\t0-9\tselect from first 10
\td\tdelete item at point\t\tg\trefresh buffer
\tq\tquit
")
  (let ((c 0))
    (dolist (f lazarus-list)
      (insert (format "\n [%d]  %s" c f))
      (setq c (+ 1 c))))
    (setq buffer-read-only t))

(defun lazarus-closed-file-get-file-from-line ()
  "Returns file name on the current line."
  (save-excursion
    (save-match-data
      (let ((line (thing-at-point 'line)))
        (string-match "\\(/.*$\\)" line)
        (match-string 1 line)))))

(defun lazarus-closed-file-quit ()
  "Closes the window with the Lazarus Closed Files"
  (interactive)
  (delete-window)
  t)

(defun lazarus-closed-file-refresh-buffer ()
  "Refresh Lazarus closed list buffer with current list."
  (interactive)
  (lazarus-closed-file-setup-buffer-text))

(defun lazarus-closed-file-delete-at-point ()
  "Removes file at point from Lazarus list and updates buffer."
  (interactive)
  (let ((file (lazarus-closed-file-get-file-from-line)))
    (if file
        (progn
          (lazarus-remove-file file)
          (lazarus-closed-file-refresh-buffer))
      t)))

(defun lazarus-closed-file-select-at-point ()
  "Opens file on current line."
  (interactive)
  (let ((file (lazarus-closed-file-get-file-from-line)))
    (if file
        (progn
          (lazarus-closed-file-quit)
          (lazarus-find-file file))
      t)))

(defun lazarus-closed-file-select-by-digit ()
  "Opens file corresponding to digit in buffer."
  (interactive)
  (let* ((key (char-to-string (aref (this-command-keys) 0)))
         (digit (string-to-number key)))
    (if (>= (length lazarus-list) digit)
        (progn
          (lazarus-closed-file-quit)
          (lazarus-find-nth-file digit))
      nil)))

(defun lazarus-closed-files ()
  "Create a temporary buffer displayiing list of closed files and manage them."
  (interactive)
  (let ((buffer (get-buffer-create "*Lazarus Closed Files *")))
    (set-buffer buffer)
    (lazarus-closed-files-mode)
    (lazarus-closed-file-setup-buffer-text)
    (pop-to-buffer buffer)))

(defvar lazarus-closed-files-mode-map nil
  "Keymap for Lazarus Closed Files mode.")

(defvar lazarus-closed-files-list nil
  "Mode copy of lazarus closed files")

(if lazarus-closed-files-mode-map
    nil
  (setq lazarus-closed-files-mode-map (make-sparse-keymap))
  (define-key lazarus-closed-files-mode-map "\r" 'lazarus-closed-file-select-at-point)
  (define-key lazarus-closed-files-mode-map "g" 'lazarus-closed-file-refresh-buffer)
  (define-key lazarus-closed-files-mode-map "q" 'lazarus-closed-file-quit)
  (define-key lazarus-closed-files-mode-map "d" 'lazarus-closed-file-delete-at-point)
  (define-key lazarus-closed-files-mode-map "0" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "1" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "2" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "3" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "4" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "5" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "6" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "7" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "8" 'lazarus-closed-file-select-by-digit)
  (define-key lazarus-closed-files-mode-map "9" 'lazarus-closed-file-select-by-digit))

(defun lazarus-closed-files-mode nil
  "Major mode for managing Lazarus Closed Files lists."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lazarus-closed-files-mode)
  (setq mode-name "Lazarus Closed Files")
  (use-local-map lazarus-closed-files-mode-map)
  (run-hooks 'lazarus-closed-files-mode-hook)
)

;;; The meat of things
;;
(defconst lazarus-save-file-header
  ";;; Automatically generated by `lazarus' on %s.\n"
  "Header to be written into the `lazarus-save-file'.")

(defconst lazarus-save-file-coding-system
  (if (coding-system-p 'utf-8-emacs)
      'utf-8-emacs
    'emacs-mule)
  "Coding system of the file `lazarus-save-file'.")

(defun lazarus-save-list ()
  "Save the recent list.
Write data into the file specified by `lazarus-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system lazarus-save-file-coding-system)
        (insert (format lazarus-save-file-header (current-time-string)))
        (insert (format "\n(setq %S\n      '(" 'lazarus-list ))
        (dolist (e lazarus-list)
          (insert (format "\n        %S" e)))
        (insert "\n        ))\n")
        (insert "\n\n;; Local Variables:\n"
                (format ";; coding: %s\n" lazarus-save-file-coding-system)
                ";; End:\n")
        (write-file (expand-file-name lazarus-save-file))
        nil)
    (error
     (warn "lazarus mode: %s" (error-message-string error)))))

(defun lazarus-load-list ()
  "Load a previously saved closed list.
Read data from the file specified by `lazarus-save-file'.
When `lazarus-initialize-file-name-history' is non-nil, initialize an
empty `file-name-history' with the recent list."
  (interactive)
  (let ((file (expand-file-name lazarus-save-file)))
    (when (file-readable-p file)
      (load-file file)
      (and lazarus-initialize-file-name-history
           (not file-name-history)
           (setq file-name-history (mapcar 'abbreviate-file-name
                                           lazarus-list))))))

(defun lazarus-flush-list()
  "Empty closed file list."
  (interactive)
  (setq lazarus-list nil))

(defun lazarus-remove-file (file)
  "Removes file from list of closed files. Returns file."
  (setq lazarus-list (delete file lazarus-list))
  file)

(defun lazarus-ido-find-file ()
  "Find a closed file with the help of ido."
  (interactive)
  (lazarus-find-file (lazarus-remove-file (ido-completing-read "Last closed: " lazarus-list))))


(defun lazarus-add-file(file)
  "Adds file to closed file list and assures list proper length."
  (setq lazarus-list (remove nil (subseq
                      (add-to-list 'lazarus-list (lazarus-remove-file buffer-file-name))
                      0
                      lazarus-max-saved-items))))

(defun lazarus-track-closed-file ()
  "If buffer closed, add buffer file name to front of list of closed files.
Used as a hook function for Lazarus Minor Mode."
  (and buffer-file-name
       (message buffer-file-name)
       (lazarus-add-file buffer-file-name)))

(defun lazarus-find-file (file)
  "Open 'file' if it exists."
  (if file
      (progn
        (if (file-exists-p file)
            (progn
              (find-file file)
              t)
          (progn
            (message "Could not find file '%s'" file)
            nil)))
    (progn
      (message "Closed file list empty or too small.")
      nil)))

(defun lazarus-find-nth-file (n)
  "Open the nth file in the closed file list."
  (interactive "nFind Nth closed file: ")
  (let ((file (nth n lazarus-list)))
    (lazarus-find-file (lazarus-remove-file file))))

(defun lazarus-find-last-closed (&optional n)
  "Open the Nth most recently closed file.
Optional argument N must be a valid digit number.  It defaults to 1.
1 opens the most recent file, 2 the second most recent one, etc..
0 opens the tenth most recent file."
  (interactive "p")
  (if (< n 1)
      (setq n 1))
  (lazarus-find-nth-file (1- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lazarus-mode-map (make-sparse-keymap)
  "Keymap to use in lazarus minor mode.")

(defun lazarus-remove-found-file ()
  (and buffer-file-name
       (lazarus-remove-file buffer-file-name)))

(defun lazarus-start ()
  "Set up for starting lazarus mode. Hooks and loading the list."
  (add-hook 'kill-buffer-hook 'lazarus-track-closed-file)
  (add-hook 'kill-emacs-hook 'lazarus-save-list)
  (add-hook 'find-file-hook 'lazarus-remove-found-file)
  (lazarus-load-list))

(defun lazarus-stop ()
  "Tear down for stopping lazarus mode. Hooks and saving the list."
  (lazarus-save-list)
  (remove-hook 'kill-buffer-hook 'lazarus-track-closed-file)
  (remove-hook 'find-file-hook 'lazarus-remove-found-file)
  (remove-hook 'kill-emacs-hook 'lazarus-save-list))

(define-minor-mode lazarus-mode
  "Toggle lazarus mode.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

When lazarus minor mode is enabled, it maintains a list of files
that were close, which you can then re-open."
  :global t
  :group 'lazarus
  :keymap lazarus-mode-map
  :lighter " Lazarus"
  (if lazarus-mode
      (lazarus-start)
    (lazarus-stop))
  (when (called-interactively-p 'interactive)
    (message "Lazarus mode %sabled" (if lazarus-mode "en" "dis"))))

(provide 'lazarus)
;;; lazarus.el ends here