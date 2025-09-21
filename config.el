;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;; Enable fragtog mode
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; org-babel
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter-python . t)
     (lean4 . t)))
  (dolist (entry '(("lean4" . lean4)
                   ("lean" . lean4)))
    (setf (alist-get (car entry) org-src-lang-modes nil nil #'equal)
          (cdr entry)))
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
  
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python")
  
  ;; Ensure pyenv works with Emacs
  (setenv "PATH" (concat (getenv "HOME") "/.pyenv/shims:" (getenv "PATH")))
  (setq exec-path (cons (concat (getenv "HOME") "/.pyenv/shims") exec-path))
  
  ;; Force reload ob-python
  (require 'ob-python))

(after! ob-lean4
  (require 'cl-lib)
  (require 'subr-x)
  (require 'lean4-mode nil t)
  (defvar +org-babel-lean4-command "lean"
    "Shell command used by default to execute Lean 4 source blocks.")

  (defvar org-babel-lean4-latex-translations
    '(("to" . "→")
      ("mapsto" . "↦")
      ("mapsfrom" . "↤")
      ("Rightarrow" . "⇒")
      ("Leftarrow" . "⇐")
      ("leftrightarrow" . "↔")
      ("implies" . "→")
      ("hookrightarrow" . "↪")
      ("hookleftarrow" . "↩")
      ("times" . "×")
      ("otimes" . "⊗")
      ("cdot" . "⋅")
      ("star" . "⋆")
      ("circ" . "∘")
      ("le" . "≤")
      ("ge" . "≥")
      ("neq" . "≠")
      ("ne" . "≠")
      ("in" . "∈")
      ("notin" . "∉")
      ("subset" . "⊂")
      ("subseteq" . "⊆")
      ("supset" . "⊃")
      ("supseteq" . "⊇")
      ("setminus" . "∖")
      ("cup" . "∪")
      ("cap" . "∩")
      ("Union" . "⋃")
      ("Inter" . "⋂")
      ("lor" . "∨")
      ("vee" . "∨")
      ("land" . "∧")
      ("wedge" . "∧")
      ("bot" . "⊥")
      ("top" . "⊤")
      ("vdash" . "⊢")
      ("dashv" . "⊣")
      ("lnot" . "¬")
      ("neg" . "¬")
      ("forall" . "∀")
      ("exists" . "∃")
      ("lambda" . "λ")
      ("fun" . "λ")
      ("Nat" . "ℕ")
      ("nat" . "ℕ")
      ("Int" . "ℤ")
      ("int" . "ℤ")
      ("Rat" . "ℚ")
      ("rat" . "ℚ")
      ("Real" . "ℝ")
      ("real" . "ℝ")
      ("Complex" . "ℂ")
      ("complex" . "ℂ")
      ("bbN" . "ℕ")
      ("bbZ" . "ℤ")
      ("bbQ" . "ℚ")
      ("bbR" . "ℝ")
      ("bbC" . "ℂ")
      ("alpha" . "α")
      ("beta" . "β")
      ("gamma" . "γ")
      ("delta" . "δ")
      ("epsilon" . "ε")
      ("zeta" . "ζ")
      ("eta" . "η")
      ("theta" . "θ")
      ("iota" . "ι")
      ("kappa" . "κ")
      ("mu" . "μ")
      ("nu" . "ν")
      ("xi" . "ξ")
      ("pi" . "π")
      ("rho" . "ρ")
      ("sigma" . "σ")
      ("tau" . "τ")
      ("upsilon" . "υ")
      ("phi" . "φ")
      ("chi" . "χ")
      ("psi" . "ψ")
      ("omega" . "ω")
      ("Gamma" . "Γ")
      ("Delta" . "Δ")
      ("Theta" . "Θ")
      ("Lambda" . "Λ")
      ("Xi" . "Ξ")
      ("Pi" . "Π")
      ("Sigma" . "Σ")
      ("Upsilon" . "Υ")
      ("Phi" . "Φ")
      ("Psi" . "Ψ")
      ("Omega" . "Ω"))
    "Mapping of Lean input shortcuts to their Unicode counterparts.")

  (defun org-babel-lean4--latex-to-unicode (text)
    "Convert Lean LaTeX-style shortcuts in TEXT to Unicode characters."
    (replace-regexp-in-string
     "\\\\[A-Za-z]+"
     (lambda (match)
       (let* ((token (substring match 1))
              (replacement (or (cdr (assoc token org-babel-lean4-latex-translations))
                               (cdr (assoc (downcase token) org-babel-lean4-latex-translations)))))
         (or replacement match)))
     text t t))

  (defun org-babel-lean4--persistable-definitions (text)
    "Return the subset of TEXT that should persist across Lean sessions."
    (let* ((lines (split-string text "\n"))
           (defs (cl-remove-if (lambda (line)
                                 (or (string-match-p "^\\s*#" line)
                                     (string-empty-p (string-trim line))))
                               lines))
           (joined (string-join defs "\n")))
      (string-trim joined)))

  (defvar org-babel-lean4--session-code (make-hash-table :test 'equal)
    "Hash map of Lean 4 session names to accumulated source code.")

  (defvar org-babel-lean4--session-output (make-hash-table :test 'equal)
    "Hash map of Lean 4 session names to the last raw Lean output string.")

  ;; Provide sensible defaults for Lean 4 blocks (evaluate for results + output).
  (setq org-babel-default-header-args:lean4
        '((:results . "output verbatim replace")
          (:exports . "both")
          (:session . "lean4")))

  (defun org-babel-lean4-clear-session (&optional session)
    "Clear stored Lean 4 SESSION state.
If SESSION is nil, clear all sessions."
    (if (or (null session) (equal session "all"))
        (progn
          (clrhash org-babel-lean4--session-code)
          (clrhash org-babel-lean4--session-output))
      (remhash session org-babel-lean4--session-code)
      (remhash session org-babel-lean4--session-output)))

  (defun org-babel-lean4--session-output-delta (session new-output)
    "Return only the NEW-OUTPUT generated since the last SESSION run.
Falls back to returning NEW-OUTPUT in full if it does not extend the
previous output prefix."
    (let ((previous (gethash session org-babel-lean4--session-output)))
      (if (and previous (string-prefix-p previous new-output))
          (substring new-output (length previous))
        new-output)))

  (defun org-babel-lean4--normalize-session (session)
    "Return a canonical session name STRING or nil."
    (pcase session
      ((or `nil 'none "none") nil)
      ((or `t "*lean4*") "lean4")
      ((pred stringp) session)
      (_ (format "%s" session))))

  ;; Re-implement the executor to support custom commands and extra args.
  (defun org-babel-execute:lean4 (body params)
    "Execute a Lean 4 source block via org-babel.
Supports optional header arguments :cmd (string) to override the shell
command used to run Lean, and :args (list/string) for additional CLI args."
    (let* ((cmd-header (or (cdr (assq :cmd params))
                           (cdr (assq :command params))
                           +org-babel-lean4-command))
           (cmd (if (stringp cmd-header)
                    cmd-header
                  +org-babel-lean4-command))
           (extra-args (cdr (assq :args params)))
           (session (org-babel-lean4--normalize-session (cdr (assq :session params))))
           (reset-flag (cdr (assq :reset params)))
           (prologue (or (cdr (assq :prologue params)) ""))
           (epilogue (or (cdr (assq :epilogue params)) ""))
           (stored-code (if session
                            (progn
                              (when reset-flag
                                (org-babel-lean4-clear-session session))
                              (gethash session org-babel-lean4--session-code ""))
                          ""))
           (unicode-prologue (org-babel-lean4--latex-to-unicode (string-trim prologue)))
           (unicode-body (org-babel-lean4--latex-to-unicode (string-trim-right body)))
           (unicode-epilogue (org-babel-lean4--latex-to-unicode (string-trim epilogue)))
           (segments (delq nil
                           (list (unless (string-empty-p stored-code) stored-code)
                                 (unless (string-empty-p unicode-prologue) unicode-prologue)
                                 (unless (string-empty-p unicode-body) unicode-body)
                                 (unless (string-empty-p unicode-epilogue) unicode-epilogue))))
           (source (if segments (string-join segments "\n\n") ""))
           (args-list (append (list cmd)
                              (cl-typecase extra-args
                                (null nil)
                                (string (list extra-args))
                                (list extra-args)
                                (t (list (format "%s" extra-args))))))
           (lean-file (org-babel-temp-file "lean4-" ".lean")))
      (with-temp-file lean-file
        (insert source)
        (unless (string-suffix-p "\n" source)
          (insert "\n")))
      (let* ((raw-result (org-babel-eval
                          (mapconcat #'identity
                                     (append args-list
                                             (list (org-babel-process-file-name lean-file)))
                                     " ")
                          ""))
             (display-result (if session
                                 (org-babel-lean4--session-output-delta session raw-result)
                               raw-result)))
        (when session
          (let* ((persist (org-babel-lean4--persistable-definitions unicode-body))
                 (current (or stored-code "")))
            (when (and persist (not (string-empty-p persist))
                       (not (string-match-p (regexp-quote persist) current)))
              (setq current (if (string-empty-p (string-trim current))
                                persist
                              (concat (string-trim-right current) "\n\n" persist)))
              (puthash session current org-babel-lean4--session-code))
            (puthash session raw-result org-babel-lean4--session-output)))
        display-result)))

  (defun +lean4--enable-lsp-in-org-src ()
    "Start LSP when editing Lean 4 src blocks via org-edit-special."
    (when (and (derived-mode-p 'lean4-mode)
               (fboundp 'lsp!))
      (lsp!)))
  (add-hook 'org-src-mode-hook #'+lean4--enable-lsp-in-org-src))

;; EIN polymode fix
(defun pm--visible-buffer-name (&optional buffer)
  "Return visible name for BUFFER."
  (buffer-name (or buffer (current-buffer))))


;; Image orgmode
(after! org-download
        (setq org-download-method 'directory)
        (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
        (setq org-download-image-org-width 600)
        (setq org-download-link-format "[[file:%s]]\n"
          org-download-abbreviate-filename-function #'file-relative-name)
        (setq org-download-link-format-function #'org-download-link-format-function-default))


;; Spell-check: use Hunspell with US English + Mexican Spanish simultaneously
;; Requires hunspell and dictionaries installed (e.g., en_US, es_MX).
;; On Debian/Ubuntu:   sudo apt install hunspell hunspell-en-us hunspell-es-mx
;; On Arch:            sudo pacman -S hunspell hunspell-en_us hunspell-es_mx
;; On macOS (Homebrew): brew install hunspell && brew install hunspell-en-us hunspell-es
;; If es_MX isn't available on macOS, install the Spanish pack that includes it.
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US,es_MX")

(after! ispell
  (let ((multi-dict '("en_US,es_MX"
                      "[[:alpha:]]" "[^[:alpha:]]" "[']" t
                      ("-d" "en_US,es_MX") nil utf-8)))
    (setq ispell-hunspell-dictionary-alist
          (cons multi-dict
                (assoc-delete-all "en_US,es_MX" ispell-hunspell-dictionary-alist)))
    (setq ispell-local-dictionary-alist
          (cons multi-dict
                (assoc-delete-all "en_US,es_MX" ispell-local-dictionary-alist)))
    (setq ispell-dictionary-alist
          (cons multi-dict
                (assoc-delete-all "en_US,es_MX" ispell-dictionary-alist))))
  ;; Ensure hunspell is always asked for UTF-8 so iconv never receives NIL
  (setq ispell-cmd-args (delete-dups (append '("-i" "utf-8") ispell-cmd-args)))
  (when (fboundp 'ispell-set-spellchecker-params)
    (ispell-set-spellchecker-params)))

;; Ensure Flyspell switches to the multi-dictionary on enable
(defun +my-flyspell-use-multi-dict ()
  (when (and (boundp 'ispell-program-name)
             (stringp ispell-program-name)
             (string-match-p "hunspell" ispell-program-name))
    (ignore-errors (ispell-change-dictionary "en_US,es_MX" t))))
(add-hook 'flyspell-mode-hook #'+my-flyspell-use-multi-dict)

;; Lean 4 via lean4-mode + LSP (Lean 3 module disabled in init.el)
(use-package! lean4-mode
  :mode "\\.lean\\'"
  :hook (lean4-mode . lsp!)
  :config
  ;; `lsp!` sets up lean-language-server when :tools lsp is enabled.
  (setq lean4-rootdir nil) ; use Lean from PATH (e.g., `lean` or `lake env`)
  (set-company-backend! 'lean4-mode '(company-capf))
  (map! :localleader
        :map lean4-mode-map
        "=" #'lsp-format-buffer
        "l" #'lean4-switch-to-info-buffer
        "h" #'lean4-toggle-hole-state
        "r" #'lean4-refresh-file-dependencies))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
