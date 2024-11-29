(in-package #:neomacs)

(sera:export-always
    '(*adblock-hostlist-path* *adblock-hostlist-url*
      *adblock-hostlist-update-interval* get-hostlist-maybe
      install-adblocker))

(defvar *adblock-hostlist-path* nil
  "File path to save downloaded and parsed blocked hostlist.")

(defvar *adblock-hostlist-update-interval* (* 60 60 24)
  "Automatically redownload hostlist from `*adblock-hostlist-url*' after
this amount of time.")

(defvar *adblock-hostlist-url*
  "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
  "URL to get blocked hosts from.")

(defun get-hostlist (url)
  "Download and parse hostlist from URL."
  (let ((stream (dex:get url :want-stream t)) urls)
    (unwind-protect
         (iter (for line = (read-line stream nil nil))
           (while line)
           (when (sera:string-prefix-p "0.0.0.0" line)
             (push (cadr (str:split #\space line)) urls)))
      (close stream))
    urls))

(defun get-hostlist-maybe ()
  "Download or return cached hostlist from `*adblock-hostlist-url*'."
  (unless *adblock-hostlist-path*
    (setq *adblock-hostlist-path* (uiop:xdg-data-home "neomacs" "web-adblock-hostlist")))
  (if (handler-case
          (< (local-time:timestamp-to-unix (local-time:now))
             (+ (osicat-posix:stat-mtime
                 (osicat-posix:stat *adblock-hostlist-path*))
                *adblock-hostlist-update-interval*))
        (osicat-posix:enoent ()))
      (uiop:read-file-lines *adblock-hostlist-path*)
      (progn
        (message "web-adblock: updateing hostlist from ~a"
                 *adblock-hostlist-url*)
        (lret ((hostlist (get-hostlist *adblock-hostlist-url*)))
          (with-open-file (s *adblock-hostlist-path*
                             :direction :output
                             :if-exists :supersede)
            (iter (for url in hostlist)
              (write-line url s)))
          (message "web-adblock: hostlist updated")))))

(define-command install-adblocker (&optional (hostlist (get-hostlist-maybe)))
  "Block HOSTLIST for current Web session.

By default, gets the blocked host list by calling `get-hostlist-maybe'."
  (evaluate-javascript
   (format nil "electron.session.fromPartition(\"\").webRequest.onBeforeRequest({urls:[~{'*://*.~a/*'~^,~}]},(details,cb)=>{
    cb({cancel:true})})"
           hostlist)
   :global))

(define-command uninstall-adblocker ()
  "Stop ad-blocking for current Web session."
  (evaluate-javascript
   "electron.session.fromPartition(\"\").webRequest.onBeforeRequest(null)"
   :global))
