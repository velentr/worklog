;;; worklog.el --- Track work across a number of tasks

;; Copyright (C) 2021 Brian Kubisiak <brian@kubisiak.com>
;;
;; Author: Brian Kubisiak <brian@kubisiak.com>

;;; Commentary:

;; worklog.el is a method for tracking planned/completed/in-progress work.
;;
;; The data directory is structured as follow:
;;
;; $XDG_DATA_HOME/worklog/
;;   data/
;;     <id>.org
;;     ...
;;   todo/
;;     <symlink to data>.org
;;   doing/
;;     <symlink to data>.org
;;   done/
;;     <symlink to data>.org
;;
;; The data/ directory contains all the org files composing the individual "work
;; logs" for all your projects.  Each file is named with a id composed of a
;; 40-bit timestamp and 16-bit random number; mandatory fields are title and
;; tags.  The title is for display purposes and the tags are for sorting and
;; searching.
;;
;; The todo/, doing/, and done/ directories are equivalent to the standard
;; kanban boards.  todo/ contains symlinks to future planned work, doing/
;; contains links for in-progress work, and done/ contains links for completed
;; work.  The symlinks point at <id>.org files in the data/ directory.

;;; Code:

(require 'org)
(require 'seq)

(defvar worklog-data-directory
  (let ((xdg-data-home (getenv "XDG_DATA_HOME")))
    (cond
     ((and xdg-data-home (not (equal xdg-data-home "")))
      (concat (file-name-as-directory xdg-data-home) "worklog"))
     (t "~/.local/worklog")))
  "Path to the directory to use for storing worklog data.

Respects the XDG_DATA_HOME variable, defaulting to ~/.local/worklog if the
environment is not set.  Should be expanded before use.")

(defun worklog--path (name)
  "Get the absolute path to the NAME component of worklog storage."
  (expand-file-name
   (concat
    (file-name-as-directory worklog-data-directory)
    name)))

(defun worklog--storage-path (id)
  "Get the absolute path to the data for ID."
  (let ((dir (worklog--path "data")))
    (make-directory dir t)
    (concat (file-name-as-directory dir) id ".org")))

(defun worklog--board-path (board)
  "Get the absolute path to the storage for the given kanban BOARD."
  (let ((path (worklog--path board)))
    (make-directory path t)
    path))

(defun worklog--tags-path ()
  "Get the absolute path to the storage directory for tags."
  (worklog--path "tags"))

(defun worklog--tag-path (tag)
  "Get the absolute path to the storage directory for TAG."
  (concat
   (file-name-as-directory (worklog--tags-path))
   tag))

(defun worklog--list (board)
  "List all worklogs on the given kanban BOARD."
  (let ((board-dir (worklog--board-path board)))
    (seq-map
     (lambda (filename)
       ;; trim off the .org suffix
       (substring filename 0 -4))
     (seq-filter
      (lambda (filename)
        (string-suffix-p ".org" filename))
      (directory-files board-dir)))))

(defvar worklog-mode-font-lock-defaults
  '((("^* todo$" . font-lock-function-name-face)
     ("^* doing$" . font-lock-function-name-face)
     ("^* done$" . font-lock-function-name-face)
     ("^[0-9a-f]\\{4\\}-[0-9a-f]\\{10\\}" . font-lock-comment-face)))
  "Font lock settings for the worklog dashboard.")

(define-derived-mode worklog-mode special-mode "worklog"
  "Major mode for the worklog dashboard."
  (setq-local
   font-lock-defaults
   worklog-mode-font-lock-defaults))

(defun worklog--get-title (id)
  "Get the title of worklog ID."
  (let ((path (worklog--storage-path id)))
    (with-current-buffer (find-file-noselect path)
      (cadr (car (org-collect-keywords '("TITLE")))))))

(defun worklog--insert-summary (id)
  "Insert the summary for worklog ID into the current buffer."
  (let ((title (worklog--get-title id)))
    (insert id " " title "\n")))

(defun worklog-refresh-dashboard ()
  "Refresh the summary of worklogs appearing on the dashboard."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((todo (worklog--list "todo"))
          (doing (worklog--list "doing"))
          (done (worklog--list "done")))
      (insert "* todo\n")
      (seq-do 'worklog--insert-summary todo)
      (insert "\n* doing\n")
      (seq-do 'worklog--insert-summary doing)
      (insert "\n* done\n")
      (seq-do 'worklog--insert-summary done))
    (goto-char (point-min))))

(defun worklog-dashboard ()
  "Open the buffer containing all existing worklogs."
  (interactive)
  (let ((buffer (get-buffer-create "*worklogs*")))
    (switch-to-buffer buffer))
  (worklog-refresh-dashboard)
  (worklog-mode))

(defun worklog--new-id ()
  "Get a new pseudo-unique id for a worklog."
  (let ((rand (random (ash 1 16)))
        (timestamp (time-convert (current-time) 'integer)))
    (format "%04x-%010x" rand timestamp)))

(defun worklog--read-id ()
  "Read the worklog id from the current line."
  (let ((line (thing-at-point 'line)))
    (car (split-string line " "))))

(defun worklog-open (&optional id)
  "Open the worklog for ID, creating it if it does not exist."
  (interactive)
  (let* ((real-id
          (cond (id id)
                (t (worklog--read-id))))
         (orgfile (worklog--storage-path real-id)))
    (find-file orgfile)))

(defun worklog--add-to-board (id board)
  "Add worklog ID to kanban BOARD."
  (let* ((orgfile (concat id ".org"))
         (symlink (concat
                   (file-name-as-directory
                    (concat (file-name-as-directory "..") ;; FIXME?
                            "data"))
                   orgfile))
         (link-path
          (concat (file-name-as-directory (worklog--board-path board))
                  orgfile)))
    (make-symbolic-link symlink link-path)))

(defun worklog--remove-from-board (id)
  "Remove worklog ID from its current board."
  (let ((existing-link
         (locate-file
          (concat id ".org")
          (seq-map
           'worklog--board-path
           '("todo" "doing" "done"))
          nil
          ;; double-check this is a symlink so we don't delete the data file
          'file-symlink-p)))
    (cond
     (existing-link (delete-file existing-link)))))

(defun worklog-move-to-board (board &optional id)
  "Move worklog ID to the kanban BOARD."
  (interactive
   (list
    (completing-read
     "board: "
     '("todo" "doing" "done"))))
  (if (worklog-board-p board)
      (let ((real-id (if id id
                       (worklog--read-id))))
        (cond ((worklog-p real-id)
               (worklog--remove-from-board real-id)
               (worklog--add-to-board real-id board)
               (if (worklog-dashboard-p)
                   (worklog-refresh-dashboard)))))
    (message "invalid board: %s" board)))

(defun worklog-p (id)
  "Test if ID is a valid worklog."
  (file-regular-p (worklog--storage-path id)))

(defun worklog-board-p (board)
  "Test if BOARD is a valid kanban board."
  (seq-contains-p '("todo" "doing" "done") board))

(defun worklog-new ()
  "Create a new empty worklog."
  (interactive)
  (let* ((id (worklog--new-id)))
    (worklog-open id)
    (worklog--add-to-board id "todo")))

(defun worklog-dashboard-p ()
  "Test if the current buffer is the dashboard."
  (equal (buffer-name) "*worklogs*"))

(provide 'worklog)

;;; worklog.el ends here
