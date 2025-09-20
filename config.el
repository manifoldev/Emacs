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
     (jupyter-python . t)))
  
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python")
  
  ;; Ensure pyenv works with Emacs
  (setenv "PATH" (concat (getenv "HOME") "/.pyenv/shims:" (getenv "PATH")))
  (setq exec-path (cons (concat (getenv "HOME") "/.pyenv/shims") exec-path))
  
  ;; Force reload ob-python
  (require 'ob-python))

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
