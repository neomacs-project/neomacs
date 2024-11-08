(in-package #:neomacs)

(sera:eval-always
  '(key key-p
    key-ctrl key-meta key-super key-super key-shift key-sym
    parse-keyspec key-description
    lookup-keybind find-keybind collect-command-keybindings
    set-key define-keys))

;; Initially adapted from lem

(defstruct (key (:constructor %make-key))
  (ctrl nil :type boolean)
  (meta nil :type boolean)
  (super nil :type boolean)
  (hypher nil :type boolean)
  (shift nil :type boolean)
  (sym (alex:required-argument :sym) :type string))

(defvar *key-constructor-cache* (make-hash-table :test 'equal))

(defun make-key (&rest args &key ctrl meta super hypher shift sym)
  (let ((hashkey (list ctrl meta super hypher shift sym)))
    (or (gethash hashkey *key-constructor-cache*)
        (setf (gethash hashkey *key-constructor-cache*)
              (apply #'%make-key args)))))

(defvar *keymap-table* (make-hash-table))

(deftype key-sequence ()
  '(trivial-types:proper-list key))

(defstruct (keymap (:constructor %make-keymap))
  undef-hook
  (table (make-hash-table :test 'eq))
  (function-table (make-hash-table :test 'eq)))

(defun prefix-command-p (command)
  (hash-table-p command))

;; TODO: handle undefine key sequence (with prefix key),
;; i.e. cleanup empty prefix hash-table

(defun set-key (keymap keyspec command)
  "Bind COMMAND to a KEYSPEC in a KEYMAP.

If KEYSPEC argument is a `string', valid prefixes are:
H (Hyper), s (Super), M (Meta), C (Ctrl), S (Shift)

Example: (set-key *global-keymap* \"C-x b\" 'switch-to-buffer)"
  (check-type keyspec (or symbol string))
  (check-type command (or symbol function keymap))
  (typecase keyspec
    (symbol
     (setf (gethash keyspec (keymap-function-table keymap))
           command))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (define-key-internal keymap keys command))))
  (values))

(defmacro define-keys (mode-name &body bindings)
  "Define key BINDINGS for MODE-NAME.

If MODE-NAME is `global', define global key bindings instead.

Example: (define-keys global
    \"C-x b\" 'switch-to-buffer
    \"C-x k\" 'delete-buffer)"
  `(progn
     ,@ (iter (for (k v) on bindings by #'cddr)
          (collect `(set-key (keymap ',mode-name) ,k ,v)))))

(defun make-keymap (&rest bindings)
  (lret ((keymap (%make-keymap)))
    (iter (for (k v) on bindings by #'cddr)
      (set-key keymap k v))))

(defun define-key-internal (keymap keys symbol)
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (setf (gethash k table) symbol))
                  (t
                   (let ((next (gethash k table)))
                     (if (and next (prefix-command-p next))
                         (setf table next)
                         (let ((new-table (make-hash-table :test 'eq)))
                           (setf (gethash k table) new-table)
                           (setf table new-table))))))))

(defun string-to-camel-case (str)
  (with-output-to-string (s)
    (iter (with upcase = t)
      (for c in-string str)
      (if (eql c #\-)
          (setq upcase t)
          (if upcase
              (progn
                (write-char (char-upcase c) s)
                (setq upcase nil))
              (write-char c s))))))

(defun string-from-camel-case (str)
  (with-output-to-string (s)
    (iter (for c in-string str)
      (if (upper-case-p c)
          (progn
            (unless (first-iteration-p)
              (write-char #\- s))
            (write-char (char-downcase c) s))
          (write-char c s)))))

(defvar *char-to-event* (make-hash-table) "Map character to (event-code . shift-p).")
(defvar *event-to-char* (make-hash-table :test 'equal) "Map (event-code . shift-p) to character.")
;; Add character event translations
(labels ((add (char sym shift-p)
           (setq sym (string-to-camel-case sym))
           (setf (gethash char *char-to-event*) (cons sym shift-p)
                 (gethash (cons sym shift-p) *event-to-char*) char)))
  (iter (for i from (char-code #\a) to (char-code #\z))
    (for c = (code-char i))
    (add c (sera:concat "key-" (string c)) nil))
  (iter (for i from (char-code #\A) to (char-code #\Z))
    (for c = (code-char i))
    (add c (sera:concat "key-" (string c)) t))
  (iter (for i from (char-code #\0) to (char-code #\9))
    (for c = (code-char i))
    (add c (sera:concat "digit-" (string c)) nil))
  (iter (for c in '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\())
    (for i from 0)
    (add c (format nil "digit-~A" i) t))
  (add #\; "semicolon" nil)
  (add #\= "equal" nil)
  (add #\, "comma" nil)
  (add #\- "minus" nil)
  (add #\. "period" nil)
  (add #\/ "slash" nil)
  (add #\` "backquote" nil)
  (add #\\ "backslash" nil)
  (add #\[ "bracket-left" nil)
  (add #\] "bracket-right" nil)
  (add #\' "quote" nil)
  (add #\: "semicolon" t)
  (add #\+ "equal" t)
  (add #\< "comma" t)
  (add #\_ "minus" t)
  (add #\> "period" t)
  (add #\? "slash" t)
  (add #\~ "backquote" t)
  (add #\| "backslash" t)
  (add #\{ "bracket-left" t)
  (add #\} "bracket-right" t)
  (add #\" "quote" t))

(defun parse-keyspec (string)
  (labels ((fail ()
             (error "parse error: ~A" string))
           (parse (str)
             (iter (with ctrl) (with meta) (with super) (with hypher) (with shift)
               (cond
                 ((ppcre:scan "^[cmshCMSH]-" str)
                  (ecase (char str 0)
                    ((#\c #\C) (setf ctrl t))
                    ((#\m #\M) (setf meta t))
                    ((#\s) (setf super t))
                    ((#\S) (setf shift t))
                    ((#\h #\H) (setf hypher t)))
                  (setf str (subseq str 2)))
                 ((string= str "")
                  (fail))
                 (t
                  (if (= (length str) 1)
                      (if-let (translation (gethash (only-elt str) *char-to-event*))
                        (setf str (car translation)
                              shift (cdr translation))
                        (fail))
                      (setq str (string-to-camel-case str)))
                  (return (make-key :ctrl ctrl
                                    :meta meta
                                    :super super
                                    :hypher hypher
                                    :shift shift
                                    :sym str)))))))
    (mapcar #'parse (uiop:split-string string :separator " "))))

(defun key-description (key &optional stream)
  (ematch key
    ((key ctrl meta super hypher shift sym)
     (if-let (translation (gethash (cons sym shift) *event-to-char*))
       (setf shift nil sym (string translation))
       (setq sym (string-from-camel-case sym)))
     (format stream "~:[~;C-~]~:[~;M-~]~:[~;s-~]~:[~;H-~]~:[~;S-~]~A"
             ctrl meta super hypher shift sym))
    ((list* keys) (sera:mapconcat #'key-description keys " "))))

(defun traverse-keymap (keymap fun)
  (labels ((f (table prefix)
             (maphash (lambda (k v)
                        (cond ((prefix-command-p v)
                               (f v (cons k prefix)))
                              ((keymap-p v)
                               (f (keymap-table v) (cons k prefix)))
                              (t (funcall fun (reverse (cons k prefix)) v))))
                      table)))
    (f (keymap-table keymap) nil)))

(defgeneric keymap-find-keybind (keymap key cmd)
  (:method ((keymap t) key cmd)
    (let ((table (keymap-table keymap)))
      (labels ((f (k)
                 (let ((cmd (gethash k table)))
                   (cond ((prefix-command-p cmd)
                          (setf table cmd))
                         ((keymap-p cmd)
                          (setf table (keymap-table cmd)))
                         (t cmd)))))
        (or (etypecase key
              (key
               (f key))
              (list
               (let (cmd)
                 (dolist (k key)
                   (unless (setf cmd (f k))
                     (return)))
                 cmd)))
            (gethash cmd (keymap-function-table keymap))
            (keymap-undef-hook keymap)
            cmd)))))

(defun lookup-keybind (key &optional (keymaps (keymaps (current-buffer))))
  (let (cmd)
    (loop :for keymap :in (reverse keymaps)
          :do (setf cmd (keymap-find-keybind keymap key cmd)))
    cmd))

(defun find-keybind (key)
  (let ((cmd (lookup-keybind key)))
    (when (symbolp cmd)
      cmd)))

(defun collect-command-keybindings (command keymap)
  ;; TODO: handle function-table rebinds
  (let ((bindings '()))
    (traverse-keymap keymap
                     (lambda (kseq cmd)
                       (when (eq cmd command)
                         (push kseq bindings))))
    (nreverse bindings)))

(defvar *global-keymap* (make-keymap))

(defmethod keymap ((name (eql 'global)))
  *global-keymap*)

(define-keys global
  "s-u" 'revert-buffer
  "s-k" 'delete-this-buffer)
