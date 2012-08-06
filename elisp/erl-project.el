;; Rudimentary project support for Distel so that we can more easily communicate
;; with more than one erlang node at a time.

;; TODO
;; - Keep tabs on modules in projects and auto-reload ones that change
;; - Close down (internal) node when last buffer in a project dies
;;   (erl-unload-hook)
;; - Fix bug with "buffer has a running process, kill it?"
;; - Fix edb-bug

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
  (make-local-variable erl-nodename-cache)
  (add-hook 'erlang-mode-hook 'erl-project-buffer-setup))

(defun erl-project-buffer-setup ()
  "Buffer specific (not necessarily buffer-local) setup."
  (when erl-project-auto-start-node
    (erl-project-ensure-buffer-node-started (current-buffer))
    ))

(defun erl-project-ensure-buffer-node-started (buffer)
  "Start a buffer's project's node if it is not already started."
  (if (erl-project-buffer-node-started-p buffer)
      (erl-project-buffer-check-backend buffer)
      (erl-project-buffer-start-node buffer)))

(defun erl-project-buffer-check-backend (buffer)
  "Ensure that distel modules are available on the node used by `buffers''s
project"
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
  (let* ((proj        (erl-project-buffer-project buffer))
         (node-name   (erl-project-node-name proj))
         (proj-name   (erl-project-name proj))
         (code-path   (cons distel-ebin-directory
                            (erl-project-code-path-expand proj)))
         (buffer-name (concat "*" proj-name "*")))
    (cd (expand-file-name (erl-project-root proj)))
    (erl-project-start-node node-name buffer-name code-path)
    (erl-project-set-node-name-cache node-name)))

(defun erl-project-start-node (node-name buffer-name path)
  "Starts a node with `name' in `buf-name' adding `path' to the
node's code-path using the -pa flag."
  (erl-project-ensure-node-not-started node-name)
  (let ((args (append (list "-sname" node-name "-pa") path)))
    (get-buffer-create buffer-name)
    (apply #'make-comint-in-buffer node-name buffer-name "erl" nil args)
    (erl-project-add-node node-name buffer-name)))

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

(defun erl-project-name (proj)
  "Returns the name of the erl-project `proj'. No default value, come on you
have to do *something* yourself!"
  (erl-project-property 'name proj))

(defun erl-project-root (proj)
  "Returns the root directory of the erl-project `proj'."
  (erl-project-property 'root proj))

(defun erl-project-lib-dirs (proj)
  "Returns the erl-project `proj's lib-dirs, defaults to lib"
  (or (erl-project-property 'lib-dirs proj) '("lib")))

(defun erl-project-node-name (proj)
  "Returns the erl-project `proj's erlang node-name. Currently only short names
are supported. Defaults to project-name"
  (or (erl-project-property 'node-sname proj) (erl-project-name proj)))

(defun erl-project-property (prop proj)
  "Returns the value of the property of name prop from proj."
  (cdr (assoc prop proj)))

(defun erl-project-code-path-expand (proj)
  "Expands `proj's listed lib dirs to a full set of ebin directories, treating
every subdirectory of each lib dir a an OTP application."
  (let ((root     (erl-project-root proj))
        (lib-dirs (erl-project-lib-dirs proj)))
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


(defun erl-project-file-in-project-p (proj file-name)
  "Returns non-nil if the fully qualified `file-name' is located inside the
erl-project `proj'."
  (file-under-path-p (erl-project-root proj) file-name))

(defun file-under-path-p (path file-name)
  "Returns non-nil if the fully qualified `file-name' is located underneath
`path'."
  (string-prefix-p (erl-project-normalize-path path)
                   (expand-file-name file-name)))

(defun erl-project-normalize-path (path-str)
  "Bad name. Only replaces duplicate /'s in path-str and make sure it ends
with a /."
  (replace-regexp-in-string "//+" "/"
                            (concat (expand-file-name path-str) "/")))

(provide 'erl-project)
