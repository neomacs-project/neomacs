(in-package #:neomacs)

(sera:export-always '(*themes* apply-theme define-theme))

(defvar *themes* nil "List of all known themes")

(defmethod documentation (symbol (type (eql 'theme)))
  (get symbol 'theme-doc))

(defmethod (setf documentation) (new-val symbol (type (eql 'theme)))
  (setf (get symbol 'theme-doc) new-val))

(defmacro define-theme (name doc &body bindings)
  "Define a theme with NAME.

BINDINGS are passed as arguments to `set-style' to apply the theme,
except the following special kinds of bindings:

:inherit PARENT: PARENT is a symbol which names another theme. Styles
not defined in this theme are inherited from PARENT."
  `(progn
     (setf (get ',name 'theme) (list ,@bindings))
     (pushnew ',name *themes*)
     (setf (documentation ',name 'theme) ,doc)
     (sera:export-always ',name)
     ',name))

(define-mode theme-list-mode (list-mode) ())

(defmethod generate-rows ((buffer theme-list-mode))
  (iter (for theme in *themes*)
    (collecting
      (attach-presentation
       (dom `(:tr
              (:td ,(string-downcase (symbol-name theme)))
              (:td ,@(when-let (doc (documentation theme 'theme))
                       (list doc)))))
       theme))))

(define-command apply-theme
  :interactive
  (lambda ()
    (list (completing-read "Apply theme: " 'theme-list-mode)))
  (theme)
  "Apply THEME."
  (let ((bindings (make-hash-table)))
    (iter (with th = theme)
      (for plist = (get th 'theme))
      (iter (for (k v) on plist by #'cddr)
        (unless (nth-value 1 (gethash k bindings))
          (setf (gethash k bindings) v)))
      (setf th (gethash :inherit bindings))
      (setf (gethash :inherit bindings) nil)
      (while th))
    (iter (for symbol in (cons :native-theme *styles*))
      (set-style symbol (gethash symbol bindings
                                 (get symbol 'standard-style))))))

;;; Built-in themes

(define-theme milk
    "The default warm light theme with proportional programming fonts.")

(define-theme matrix
    "A dark theme featuring monospace fonts."
  'frame-body
  `(:background "rgb(0,9,12)"
    :padding "8px"
    :inherit default
    :background-image "radial-gradient(rgb(120,160,190,0.2) 1px,transparent 0)"
    :background-size "20px 20px")
  'frame-buffer
  `(:flex "1 0 1em"
    :min-width "1em"
    :min-height "1em"
    :display "flex" :flex-flow "column"
    :outline "solid 1px rgb(120,160,190,0.2)")
  'frame-minibuffer
  `(:flex "0 0 2em"
    :display "flex" :flex-flow "column"
    :background-image (:url "data:image/svg+xml,%3csvg width='100%25' height='100%25' xmlns='http://www.w3.org/2000/svg'%3e%3cline x1='0' y1='0' x2='100%25' y2='0' stroke='rgba(120,160,190,0.3)' stroke-width='1' stroke-dasharray='7%2c 7' stroke-dashoffset='0' stroke-linecap='square'/%3e%3c/svg%3e"))
  'frame-buffer-focus
  `(:outline "solid 1px rgb(120,160,190,0.4)")
  :native-theme "dark"
  'default
  `(:font-family "Space Mono"
    :font-variant-ligatures "none"
    :color "rgba(210,239,240,0.5)")
  'keyword
  `(:color "rgb(120,160,190)")
  'comment
  `(:color "rgba(120,160,190,0.6)")
  'focus
  `(:outline "solid 1px rgba(120,160,190,0.5)")
  'selection
  `(:background-color "rgba(120,160,190,0.5)"
    :color "rgba(210,239,240,0.8)")
  'monospace
  `(:inherit default
    :inherit comment)

  ;; Frame
  'header
  `(:padding "8px"
    :display "flex" :flex-flow "row"
    :margin-bottom "8px"
    :inherit comment)
  'header-focus
  nil
  'header-buffer-name
  `(:flex "0 1 auto"
    :padding-right "1em"
    :max-width "100%"
    :white-space "nowrap"
    :overflow "hidden"
    :text-overflow "ellipsis"
    :inherit default)
  'completion-menu
  `(:white-space "nowrap"
    :overflow-x "hidden"
    :overflow-y "scroll"
    :background "rgb(0,9,12)"
    :position "absolute"
    :max-height "300px"
    :padding "0 8px"
    :display "block"
    :border-collapse "collapse"
    :outline "solid 1px rgba(120,160,190,0.4)")

  ;; Lisp
  'comment-node-1
  `(:position "sticky"
    :left "20em"
    :border-left "0.3rem solid rgba(120,160,190,0.4)"
    :padding-left "0.3rem"
    :inherit comment)
  'comment-node-2
  `(:border-left "0.3rem solid rgba(120,160,190,0.4)"
    :padding-left "0.3rem"
    :inherit comment)
  'comment-node-3
  `(((:append "::before")
     :content ""
     :display "list-item"
     :list-style-type "square"
     :list-style-position "inside"
     :font-size "1.2rem"
     :margin-left "-0.6rem"
     :position "absolute")
    :padding-left "0.6rem"
    :font-size "1.2em"
    :inherit comment)
  'comment-node-4
  `(((:append "::before")
     :content ""
     :display "list-item"
     :list-style-type "square"
     :list-style-position "inside"
     :font-size "1.2rem"
     :margin-left "-0.6rem"
     :position "absolute")
    :padding-left "0.6rem"
    :font-size "1.1em"
    :inherit comment)
  'compiler-note
  `(:color "#fff")
  'compiler-style-warning
  `(:color "#f70")
  'compiler-warning
  `(:color "#f00")
  'compiler-error
  `(:color "#f00")

  ;; Lists
  'list-mode
  `(("body" :margin 0)
    ("table" :white-space "pre" :width "100%"
             :border-collapse "collapse")
    ("tbody:empty::after"
     :content "<No Item>"
     :display "inline")
    ("td" :padding-right "1em")
    ("td:first-child" :padding-left "0.5rem")
    (".header" :inherit bold))

  ;; Web
  'web-hints
  `((".neomacs-hint"
     :color "#fff"
     :background-color "rgb(0,9,12)"
     :padding "0px 0.3em"
     :border-radius "2px"
     :border-width "2px"
     :border-style "solid"
     :z-index #.(1- (expt 2 31))))

  ;; Term
  ;; Adapted from Gogh schemes Github dark
  ;; https://gogh-co.github.io/Gogh/
  'ansi-black '(:color "#000")
  'ansi-red '(:color "#f78166")
  'ansi-green '(:color "#56d364")
  'ansi-yellow '(:color "#e3b341")
  'ansi-blue '(:color "#6ca4f8")
  'ansi-magenta '(:color "#db61a2")
  'ansi-cyan '(:color "#2b7489")
  'ansi-white '(:color "#ffffff")
  'ansi-bright-black '(:color "#4d4d4d")
  'ansi-bright-red '(:color "#f78166")
  'ansi-bright-green '(:color "#56d364")
  'ansi-bright-yellow '(:color "#e3b341")
  'ansi-bright-blue '(:color "#6ca4f8")
  'ansi-bright-magenta '(:color "#db61a2")
  'ansi-bright-cyan '(:color "#2b7489")
  'ansi-bright-white '(:color "#ffffff"))

(define-theme anti-matrix
    "Variant of `matrix', a mostly-monochrome light theme."
  :inherit 'matrix
  'frame-body
  `(:background "#fff"
    :padding "8px"
    :inherit default
    :background-image "radial-gradient(#ccc 1px,transparent 0)"
    :background-size "20px 20px")
  'frame-buffer
  `(:flex "1 0 1em"
    :min-width "1em"
    :min-height "1em"
    :display "flex" :flex-flow "column"
    :outline "solid 1px #ccc")
  'frame-minibuffer
  `(:flex "0 0 2em"
    :display "flex" :flex-flow "column"
    :background-image (:url "data:image/svg+xml,%3csvg width='100%25' height='100%25' xmlns='http://www.w3.org/2000/svg'%3e%3cline x1='0' y1='0' x2='100%25' y2='0' stroke='%23777' stroke-width='1' stroke-dasharray='7%2c 7' stroke-dashoffset='0' stroke-linecap='square'/%3e%3c/svg%3e"))
  'frame-buffer-focus
  `(:outline "solid 1px #777")
  :native-theme "light"
  'default
  `(:font-family "Space Mono"
    :font-variant-ligatures "none"
    :color "#000")
  'keyword
  `(:color "#000" :inherit bold)
  'comment
  `(:color "#777")
  'focus
  `(:outline "solid 1px #ccc")
  'selection
  `(:background-color "#ccc")

  ;; Frame
  'completion-menu
  `(:white-space "nowrap"
    :overflow-x "hidden"
    :overflow-y "scroll"
    :background "#fff"
    :position "absolute"
    :max-height "300px"
    :padding "0 8px"
    :display "block"
    :border-collapse "collapse"
    :outline "solid 1px #777")

  ;; Lisp
  'comment-node-1
  `(:position "sticky"
    :left "20em"
    :border-left "0.3rem solid #777"
    :padding-left "0.3rem"
    :inherit comment)
  'comment-node-2
  `(:border-left "0.3rem solid #777"
    :padding-left "0.3rem"
    :inherit comment)
  'compiler-note
  `(:color "#07c")

  ;; Web
  'web-hints
  `((".neomacs-hint"
     :background-color "#fff"
     :color "#000"
     :padding "0px 0.3em"
     :border-radius "2px"
     :border-width "2px"
     :border-style "solid"
     :z-index #.(1- (expt 2 31))))

  ;; Term
  ;; Adapted from Gogh schemes Github light
  ;; https://gogh-co.github.io/Gogh/
  'ansi-white '(:color "#000")
  'ansi-red '(:color "#cf222e")
  'ansi-green '(:color "#1a7f37")
  'ansi-yellow '(:color "#9a6700")
  'ansi-blue '(:color "#0969da")
  'ansi-magenta '(:color "#8250df")
  'ansi-cyan '(:color "#1b7c83")
  'ansi-black '(:color "#6e7781")
  'ansi-bright-white '(:color "#57606a")
  'ansi-bright-red '(:color "#a40e26")
  'ansi-bright-green '(:color "#2da44e")
  'ansi-bright-yellow '(:color "#bf8700")
  'ansi-bright-blue '(:color "#218bff")
  'ansi-bright-magenta '(:color "#a475f9")
  'ansi-bright-cyan '(:color "#3192aa")
  'ansi-bright-black '(:color "#8c959f"))

(define-theme light-blue
  "A light and blue theme."
  'frame-body
  `(:background "white"
    :padding "8px"
    :inherit default
    :background-size "10px 10px")
  'frame-buffer
  `(:flex "1 0 1em"
    :min-width "1em"
    :min-height "1em"
    :display "flex" :flex-flow "column"
    :outline "solid 1px #ccc")
  'frame-minibuffer
  `(:flex "0 0 2em"
    :display "flex" :flex-flow "column")
  'frame-buffer-focus
  `(:outline "solid 1px darkblue")
  'default
  `(:font-family "DejaVu Sans Mono"
    :font-variant-ligatures "none"
    :color "#2E3436"
    :font-size "14px")
  'keyword
  `(:color "#204A87")
  'comment
  `(:color "#777")
  'focus
  `(:outline "solid 1px #ccc")
  'selection
  `(:background-color "#ccc")
  'monospace
  `(:inherit default
    :inherit comment)

  ;;        Frame
  'header
  `(:padding "8px"
    :display "flex" :flex-flow "row"
    :margin-bottom "8px"
    :border-bottom "1px solid lightgray"
    ;; :background-color "whitesmoke"
    :inherit comment)
  'header-focus
  '(:inherit header :background-color "lavender")
  'header-buffer-name
  `(:flex "0 1 auto"
    :padding-right "1em"
    :max-width "100%"
    :white-space "nowrap"
    :overflow "hidden"
    :text-overflow "ellipsis"
    :inherit default)
  'completion-menu
  `(:white-space "nowrap"
    :font-size "0.8em"
    :overflow-x "hidden"
    :overflow-y "scroll"
    :background "#fff"
    :position "absolute"
    :max-height "300px"
    :padding "0 8px"
    :display "block"
    :border-collapse "collapse"
    :outline "solid 1px #777")
  'completion-selection
  `(:background-color "lavender")

  ;;        Lisp
  'string-node
  '(((:append "::before") :content "\"")
    ((:append "::after") :content "\"")
    ((:append ":not(:last-child)")
     :margin-right "0.4em")
    ((:append ".focus::before")
     :inherit selection)
    ((:append ".focus-tail::after")
     :content "\""
     :width "auto"
     :inherit selection)
    :inherit (sexp-node string)
    :color "green")
  'comment-node-1
  `(:position "sticky"
    :left "20em"
    :border-left "0.3rem solid #777"
    :padding-left "0.3rem"
    :inherit comment)
  'comment-node-2
  `(:border-left "0.3rem solid #777"
    :padding-left "0.3rem"
    :inherit comment)
  'comment-node-3
  `(((:append "::before")
     :content ""
     :display "list-item"
     :list-style-type "square"
     :list-style-position "inside"
     :font-size "1.2rem"
     :margin-left "-0.6rem"
     :position "absolute")
    :padding-left "0.6rem"
    :font-size "1.2em"
    :inherit comment)
  'comment-node-4
  `(((:append "::before")
     :content ""
     :display "list-item"
     :list-style-type "square"
     :list-style-position "inside"
     :font-size "1.2rem"
     :margin-left "-0.6rem"
     :position "absolute")
    :padding-left "0.6rem"
    :font-size "1.1em"
    :inherit comment)
  'compiler-note
  `(:color "blue")
  'compiler-style-warning
  `(:color "lightorange")
  'compiler-warning
  `(:color "orange")
  'compiler-error
  `(:color "red")

  ;;        Lists
  'list-mode
  `(("body" :margin 0)
    ("table" :white-space "pre" :width "100%"
             :border-collapse "collapse")
    ("tbody:empty::after"
     :content "<No Item>"
     :display "inline")
    ("td" :padding-right "1em")
    ("td:first-child" :padding-left "0.5rem")
    (".focus" :background-color "lavender")
    (".header" :inherit bold))

  ;;        Term
  ;; Adapted from Gogh schemes Github light
  ;; https://gogh-co.github.io/Gogh/
  'ansi-white '(:color "#000")
  'ansi-red '(:color "#cf222e")
  'ansi-green '(:color "#1a7f37")
  'ansi-yellow '(:color "#9a6700")
  'ansi-blue '(:color "#0969da")
  'ansi-magenta '(:color "#8250df")
  'ansi-cyan '(:color "#1b7c83")
  'ansi-black '(:color "#6e7781")
  'ansi-bright-white '(:color "#57606a")
  'ansi-bright-red '(:color "#a40e26")
  'ansi-bright-green '(:color "#2da44e")
  'ansi-bright-yellow '(:color "#bf8700")
  'ansi-bright-blue '(:color "#218bff")
  'ansi-bright-magenta '(:color "#a475f9")
  'ansi-bright-cyan '(:color "#3192aa")
  'ansi-bright-black '(:color "#8c959f"))
