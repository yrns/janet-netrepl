
;;; janet-netrepl.el --- a janet netrepl client -*- lexical-binding: t -*-

(require 'bindat)

(defvar janet-netrepl-name "janet-netrepl")
(defvar janet-netrepl-host "localhost")
(defvar janet-netrepl-port 9365)

(defvar janet-netrepl-bindat-spec (bindat-type (length uintr 32) (data str length)))

(defvar janet-netrepl-stream nil)
(defvar janet-netrepl-data nil)

(defun janet-netrepl-filter (p out)
  (let* ((data (bindat-unpack janet-netrepl-bindat-spec out))
         (res (bindat-get-field data 'data)))
    (setq janet-netrepl-data res)))

(defun janet-netrepl-sentinel (p event)
  (message "janet-netrepl event: %s" (string-trim event)))

(defun janet-netrepl-open ()
  (let ((p (open-network-stream janet-netrepl-name nil janet-netrepl-host janet-netrepl-port)))
    (set-process-filter-multibyte p nil)
    ;; (set-process-coding-system p 'utf-8 'utf-8)
    (set-process-filter p #'janet-netrepl-filter)
    (set-process-sentinel p #'janet-netrepl-sentinel)
    ;; the server expects a client name, use process name
    (janet-netrepl-send p nil (process-name p))
    ;; ignore initial prompt
    (accept-process-output p)
    p))

(defun janet-netrepl-close ()
  (process-send-eof janet-netrepl-stream))

(defun janet-netrepl-check ()
  (if (or (not janet-netrepl-stream)
          (eq (process-status janet-netrepl-stream) 'closed))
      (setq janet-netrepl-stream (janet-netrepl-open))
    janet-netrepl-stream))

(defun janet-netrepl-send (p prefix s)
  (let* ((msg (if prefix (format "%c%s" prefix s) s))
         (packed (bindat-pack janet-netrepl-bindat-spec `((length . ,(length msg)) (data . ,msg)))))
    (process-send-string p packed)))

(defun janet-netrepl-eval (s)
  (let ((p (janet-netrepl-check)))
    ;; the xFF prefix tells the server to just send the result back w/o any other output or prompts
    (janet-netrepl-send p #xFF s)
    (accept-process-output p)
    ;; handle errors? strip true on success?
    janet-netrepl-data))

(provide 'janet-netrepl)
