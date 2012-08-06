;; Rudimentary project support for Distel so that we can more easily communicate
;; with more than one erlang node at a time.

;; TODO
;; - Keep tabs on modules in projects and auto-reload ones that change
;; - Close down (internal) node when last buffer in a project dies
;;   (erl-unload-hook)
;; - Fix bug with "buffer has a running process, kill it?"
;; - Don't start thousands of processes (related to previous point?).
;; - Fix edb-bug
;; - Fix assertion-bug in epmd.el
;; - Mark set when saving(?)
;; - Fix no erlang-extended-mode for modules outside of projects.
;; - Fix crashing hook for non-project modules.
;; - Run distel nodeup-hook when inferior node is started.

(defgroup erl-project '()
  "Distel and erlang-extended-mode development tools."
  :group 'tools)

(defcustom erl-project-projects nil
  "The list of projects."
  :group 'erl-project)

(defcustom erl-project-auto-start-node t
  "If non-nil, automagically start an erlang node whenever erlang-mode is
activated for the first file that is located inside a project."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erl-project-nodes nil
  "The list of nodes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-project-init ()
  "Initializes `erl-project'."
  (make-variable-buffer-local 'erl-nodename-cache)
  (add-hook 'erlang-mode-hook 'erl-project-erlang-load-hook))

(defun erl-project-erlang-load-hook ()
  "Buffer specific (not necessarily buffer-local) setup."
  (let* ((buffer  (current-buffer))
         (project (erl-project-buffer-project buffer)))
    (when (and project erl-project-auto-start-node)
      (erl-project-ensure-buffer-node-started buffer))))

(defun erl-project-ensure-buffer-node-started (buffer)
  "Start a buffer's project's node if it is not already started."
  (erl-project-ensure-node-started (erl-project-buffer-project buffer)))

(defun erl-project-ensure-node-started (project)
  "Start a buffer's project's node if it is not already started."
  (if (erl-project-node-started-p (erl-project-node-name project))
      (erl-project-check-backend project)
      (erl-project-start-node project)))

(defun erl-project-check-backend (project)
  "Ensure that distel modules are available on the node used by `project'"
  (erl-check-backend
   (erl-project-make-node-name (erl-project-buffer-node-name)) nil))

(defun erl-project-set-node-name-cache (str)
  "Sets the distel node-name cache to `str'@`system-name'. Return the newly set
node-name"
  (setq erl-nodename-cache (erl-project-make-node-name str)))

(defun erl-project-make-node-name (str)
  "Converts `str' to a valid erlang short node name of the form
`str'@`system-name'"
  (make-symbol (concat str "\@" system-name)))

(defun erl-project-buffer-start-node (&optional buffer)
  "Starts a new erlang node for the project that `buffer' belongs to."
  (unless buffer (setq buffer (current-buffer)))
  (erl-project-start-node (erl-project-buffer-project buffer)))

(defun erl-project-start-node (&optional project)
  "Starts a new erlang node for the project that `buffer' belongs to."
  (let* ((node-name    (erl-project-node-name project))
         (project-name (erl-project-name project))
         (code-path    (cons distel-ebin-directory
                            (erl-project-code-path-expand project)))
         (buffer-name (concat "*" project-name "*")))
    (erl-project-ensure-node-not-started node-name)
    (cd (expand-file-name (erl-project-root project)))
    (erl-project-make-comint-buffer-node-name buffer-name code-path)
    (erl-project-add-node node-name buffer-name)
    (erl-project-set-node-name-cache node-name)))

(defun erl-project-make-comint-buffer (node-name buffer-name path)
  "In a comint-mode buffer Starts a node with `name' in `buf-name' adding
`path' to the node's code-path using the -pa flag."
  (let ((args (append (list "-sname" node-name "-pa") path)))
    (get-buffer-create buffer-name)
    (apply #'make-comint-in-buffer node-name buffer-name "erl" nil args)))

(defun erl-project-add-node (node-name buffer-name)
  "Add a new node to `erl-project-nodes'."
  (add-to-list 'erl-project-nodes
               (list
                (cons 'node-name   node-name)
                (cons 'buffer-name buffer-name))))

(defun erl-project-buffer-node-started-p (&optional buffer)
  "Returns non-nil if there is an erl-project erlang node started that
corresponds to 'buffer'."
  (erl-project-node-started-p (erl-project-buffer-node-name buffer)))

(defun erl-project-node-started-p (node-name)
  "Returns non-nil if there is an erl-project erlang node with name  `node-name'
running on localhost."
  (member node-name (epmd-fetch-names-from-host-sync "localhost")))

(defun erl-project-ensure-node-not-started (node-name)
  "Signals an error if a node of name `node-name' is running on localhost."
  (when (erl-project-node-started-p node-name)
    (error "-- Node already up. --")))

(defun erl-project-name (project)
  "Returns the name of the erl-project `project'. No default value, come on you
have to do *something* yourself!"
  (erl-project-property 'name project))

(defun erl-project-root (project)
  "Returns the root directory of the erl-project `project'."
  (erl-project-property 'root project))

(defun erl-project-lib-dirs (project)
  "Returns the erl-project `project's lib-dirs, defaults to lib"
  (or (erl-project-property 'lib-dirs project) '("lib")))

(defun erl-project-node-name (project)
  "Returns the erl-project `project's erlang node-name. Currently only short
names are supported. Defaults to project-name"
  (or (erl-project-property 'node-sname project) (erl-project-name project)))

(defun erl-project-property (prop project)
  "Returns the value of the property of name prop from project."
  (cdr (assoc prop project)))

(defun erl-project-code-path-expand (project)
  "Expands `project's listed lib dirs to a full set of ebin directories,
treating every subdirectory of each lib dir a an OTP application."
  (let ((root     (erl-project-root project))
        (lib-dirs (erl-project-lib-dirs project)))
    (apply #'append
           (mapcar #'(lambda (dir)
                       (erl-project-path-expand root dir)) lib-dirs))))

(defun erl-project-path-expand (root dir)
  "Returns a list of all existing ebin directories in any folder directly
beneath `root'/`dir'."
  (setq root (erl-project-normalize-path root))
  (file-expand-wildcards (format "%s/%s/*/ebin" root dir)))

(defun erl-project-buffer-node-name (&optional buffer)
  "Returns the erlang node-name of `buffer''s erl-project node. `buffer
defaults to current-buffer."
  (unless buffer (setq buffer (current-buffer)))
  (erl-project-node-name (erl-project-buffer-project buffer)))

(defun erl-project-buffer-project (&optional buffer)
  "Returns the erl-project that `buffer' is part of, if any,
otherwise nil. If buffer is omitted, it defaults to the current buffer."
  (unless buffer (setq buffer (current-buffer)))
  (erl-project-file-project (buffer-file-name buffer)))

(defun erl-project-file-project (&optional file-name)
  "Returns the erl-project that the file with `file-name' is part of, if any,
otherwise nil. If `file-name' is omitted, it defaults to the file-name of the
current buffer."
  (unless file-name (setq file-name (buffer-file-name)))
  (find-if  #'(lambda (p) (erl-project-file-in-project-p p file-name))
            erl-project-projects))


(defun erl-project-file-in-project-p (project file-name)
  "Returns non-nil if the fully qualified `file-name' is located inside the
erl-project `project'."
  (file-under-path-p (erl-project-root project) file-name))

(defun file-under-path-p (path file-name)
  "Returns non-nil if the fully qualified `file-name' is located underneath
`path'."
  (or
   (string-prefix-p (erl-project-normalize-path path)
                    (expand-file-name file-name))
   (string-prefix-p                (erl-project-normalize-path path)
                    (file-truename (expand-file-name file-name)))
   (string-prefix-p (file-truename (erl-project-normalize-path path))
                                   (expand-file-name file-name))
   (string-prefix-p (file-truename (erl-project-normalize-path path))
                    (file-truename (expand-file-name file-name)))))

(defun erl-project-normalize-path (path-str)
  "Bad name. Only replaces duplicate /'s in path-str and make sure it ends
with a /."
  (replace-regexp-in-string "//+" "/"
                            (concat (expand-file-name path-str) "/")))

(provide 'erl-project)
