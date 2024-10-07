(in-package :neomacs)

;; Adapted from Nyxt

(defun print-arglist (arglist package)
  (if arglist
      (let ((*package* package))
        (princ-to-string arglist))
      "()"))

(defun resolve-backtick-quote-links (string package)
  (declare (ignore package))
  (if string string ""))

(spinneret:deftag :nxref (body attrs &rest keys &key slot mode class-name function macro command (command-key-p t) variable package (target "_self") &allow-other-keys)
  (let* ((keys keys)
         (first (first body))
         (symbol (or package variable function macro command slot class-name mode
                     (when (symbolp first) first)))
         (printable (or (when (and (symbolp first) (eq first symbol))
                          (second body))
                        first package variable function macro command slot class-name mode))
         (type (cond
                 (package :package)
                 (variable :variable)
                 (macro :macro)
                 (command :command)
                 (function :function)
                 ((and slot class-name) :slot)
                 (mode :mode)
                 (class-name :class))))
    (declare (ignorable keys type))
    `(:code (princ-to-string ,printable))))

(spinneret:deftag :nxdoc (body attrs &rest keys &key slot mode class-name function macro command (command-key-p t) variable package (target "_self") &allow-other-keys)
  "Create an inline description for BODY symbol.

Relies on the type keywords (SLOT, MODE, CLASS-NAME, FUNCTION, MACRO, COMMAND,
VARIABLE, PACKAGE, TARGET), always provide those.

CLASS-NAME, if present, should be the symbol designating a class. It's not
called CLASS because Spinneret has special behavior for CLASS pre-defined and
non-overridable."
  (let* ((type (cond
                 (package :package)
                 (variable :variable)
                 (macro :macro)
                 (command :command)
                 (function :function)
                 ((and slot class-name) :slot)
                 (mode :mode)
                 (class-name :class))))
    (assert (null body))
    (ecase type
      ((:function)
       `(:li "Function: " (:nxref ,@keys) " "
             (:code (print-arglist (swank-backend:arglist ,function)
                                   (symbol-package ,function)))
             (:p (:raw (resolve-backtick-quote-links
                        (documentation ,function 'function)
                        (symbol-package ,function))))))
      ((:command)
       `(:li "Command: " (:nxref ,@keys) " "
             (:code (print-arglist (swank-backend:arglist ,command)
                                   (symbol-package ,command)))
             (:p (:raw (resolve-backtick-quote-links
                        (documentation ,command 'function)
                        (symbol-package ,command))))))
      ((:variable)
       `(:li "Variable: " (:nxref ,@keys)
             (:p (:raw (resolve-backtick-quote-links
                        (documentation ,variable 'variable)
                        (symbol-package ,variable)))))))))

(spinneret:deftag :nsection (body attrs &rest keys
                        &key (title (alexandria:required-argument 'title))
                        level
                        (anchor-p t)
                        (open-p t)
                        (id (if (stringp title)
                                (str:remove-punctuation (str:downcase title) :replacement "-")
                                (alexandria:required-argument 'id)))
                        &allow-other-keys)
  "Collapsible and reference-able <section> with a neader.
TITLE should be a human-readable title for a section, or the form producing one.
LEVEL (if provided), is the level of heading for the section. If it's 2, the
heading is <h2>, if it's 3, then <h3> etc. If not provided, uses <h*> Spinneret
tag to intelligently guess the current heading level.
ID is the string identifier with which to reference the section elsewhere. Is
auto-generated from title by replacing all the punctuation and spaces with
hyphens, if not provided AND if the TITLE is a string.
OPEN-P mandates whether the section is collapsed or not. True (= not collapsed)
by default."
  (check-type level (or null (integer 2 6)))
  (let ((keys keys))
    (declare (ignorable keys))
    (alex:with-gensyms (id-var)
      `(let ((spinneret::*html-path*
               ;; Push as many :section tags into the path, as necessary to imply
               ;; LEVEL for the sections inside this one. A trick on Spinneret to
               ;; make it think it's deeply nested already.
               (append
                spinneret::*html-path*
                (make-list ,(if level
                                `(1- (- ,level (spinneret::heading-depth)))
                                0)
                           :initial-element :section)))
             (,id-var ,id))
         (:section.section
          :id ,id-var
          (:details
           :open ,open-p
           (:summary
            :class "nsection-summary"
            (:header
             :style "display: inline"
             (:h* :style "display: inline"
               ,@attrs ,title)
             " " (when ,anchor-p
                   (:a :class "link nsection-anchor" :href (uiop:strcat "#" ,id-var) "#"))))
           ,@body))))))

(defun render-manual-section (src-file output-file)
  (with-open-file (spinneret:*html*
                   output-file
                   :direction :output :if-exists :supersede)
    (spinneret:with-html
      (:doctype)
      (:head
       (:link :rel "stylesheet" :href "styles.css"))
      (:html
       (:body
        (progn
          (load src-file)
          nil))))))

(defun render-manual-style (styles output-file)
  (with-open-file (s output-file
                   :direction :output :if-exists :supersede)
    (dolist (style (reverse styles))
      (write-string (cell-ref (css-cell style)) s))))

(defun build-manual ()
  (ensure-directories-exist (asdf:system-relative-pathname "neomacs" "doc/html/"))
  (render-manual-style
   '(buffer)
   (asdf:system-relative-pathname "neomacs" "doc/html/styles.css"))
  (dolist (path (uiop:directory-files
                 (asdf:system-relative-pathname "neomacs" "doc")))
    (when (equal (pathname-type path) "lisp")
      (render-manual-section
       path
       (uiop:merge-pathnames*
        (make-pathname :name (pathname-name path) :type "html")
        (asdf:system-relative-pathname "neomacs" "doc/html/"))))))

#+nil (define-internal-page-command-global neomacs-manual ()
    (buffer "*Neomacs Manual*" 'nyxt/mode/help:help-mode)
  "Display Neomacs manual."
  (spinneret:with-html-string
    (:nstyle '(body :max-width "80ch"))
    (:style "li p{margin-top: 0}")
    (:raw (neomacs-manual-content))))
