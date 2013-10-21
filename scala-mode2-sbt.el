;;; scala-mode-lib.el - Major mode for editing scala, common functions
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'comint)

(defcustom scala-sbt:sbt-command "sbt"
  "The name of the sbt script to run. This must be either a
command on path or in the sbt root."
  :type 'string
  :group 'scala)

(defcustom scala-sbt:default-command "test:compile"
  "The default command to run with scala-sbt:command."
  :type 'string
  :group 'scala)

(defvar scala-sbt:previous-command scala-sbt:default-command)

(defun scala-sbt:find-root-impl (name-or-pred &optional dir best-root)
  (when (null dir) (setq dir default-directory))
  (let ((parent (if (string-match locate-dominating-stop-dir-regexp dir) nil
                  (file-name-directory (directory-file-name dir)))))
    (cond ((or (null parent)
               (equal dir parent))
           best-root)
          ((if (stringp name-or-pred)
               (file-exists-p (expand-file-name name-or-pred dir))
             (funcall name-or-pred dir))
           (scala-sbt:find-root-impl name-or-pred parent dir))
          ('t
           (scala-sbt:find-root-impl name-or-pred parent best-root)))))

(defun scala-sbt:find-root ()
  "Starting from the current default-directory, find the top-most
parent directory that is an sbt root. An sbt root directory is
identified by the following rules:

  - a directory containing a 'project/build.properties' in it.

  - a directory that contains a file matching one of the patterns
    '*.sbt' or 'project/*.scala' file in it.

The first rule is applied first and the second is used only if it
fails to find the sbt root."
  (or
   (scala-sbt:find-root-impl "project/build.properties")
   (scala-sbt:find-root-impl 
    (lambda (dir) 
      (or (directory-files dir nil ".+\\.sbt$")
          (and (file-exists-p (concat dir "project"))
               (directory-files (concat dir "project") nil ".+\\.scala$")))))))

(defun scala-sbt:start () 
  "Start sbt in a buffer called *sbt*, stops any existing sbt
running in the same buffer."
  (interactive)
  (let ((project-root (scala-sbt:find-root))
        (compilation-buffer-name-function (lambda (m) "*sbt*")))
    (when (null project-root)
      (error "Could not find project root, type `C-h f scala-sbt:find-root` for help."))

    (when (not (or (executable-find scala-sbt:sbt-command)
                   (file-executable-p (concat project-root scala-sbt:sbt-command))))
      (error "Could not find %s in %s or on PATH" scala-sbt:sbt-command project-root))
                    
    ;; kill existing sbt
    (when (get-buffer "*sbt*") (kill-buffer "*sbt*"))

    ;; start new sbt
    (with-current-buffer (get-buffer-create "*sbt*")
      (display-buffer (current-buffer))
      (buffer-disable-undo)
      (cd project-root)
      (comint-mode)
      (compilation-shell-minor-mode)
      (setq compilation-error-regexp-alist            
            `((,(rx line-start
                    ?[ (or (group "error") (group "warn") (group "info")) ?]
                    " " (group (1+ (not (any ": "))))
                    ?: (group (1+ digit)) ?:)
               4 5 nil (2 . 3))))
                    
      (comint-exec (current-buffer) "sbt" scala-sbt:sbt-command nil nil))))

(defun scala-sbt:command (&optional command)
  "Send a command to the sbt running in the '*sbt*'
buffer. Prompts for the command to send when in interactive
mode.

This command does the following:
  - displays the buffer without moving focus to it
  - erases the buffer
  - forgets about compilation errors

The command is most usefull for running a compilation command
that outputs errors."
  (interactive)

  (when (not (get-buffer "*sbt*"))
    (message "Running scala-sbt:start ...")
    (scala-sbt:start)
    (sit-for 5))
  
  (when (null command)
    (setq command (read-string (format "Command to run (default %s): " 
                                       scala-sbt:previous-command) 
                               nil nil scala-sbt:previous-command)))

  (save-some-buffers)
  (let ((buffer (get-buffer "*sbt*")))
    (with-current-buffer buffer
      (display-buffer (current-buffer))
      (compilation-forget-errors)
      (erase-buffer)
      (comint-send-string buffer (concat "\n" command "\n")))
    (setq scala-sbt:previous-command command)))

(provide 'scala-mode2-sbt)
