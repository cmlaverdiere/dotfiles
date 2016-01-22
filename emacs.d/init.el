;;;; Chris Laverdiere's Emacs config ;;;;

;; TODO
;;  - Write a fn to load all included header files into buffers.
;;  - Try paradox
;;  - Add 'make test' generic leader for 't'
;;  - Leader help keys

;; FIXME
;;  - g[hjkl] window commands should shadow everything.
;;  - Highlight persisting (anzu?)
;;  - company eshell
;;  - company irony c headers


;;; Package management ;;;

;; Package repositories
(require 'package)
(setq package-archives '(
  ("melpa" . "https://melpa.milkbox.net/packages/")
  ("gnu" . "https://elpa.gnu.org/packages/")
))

(setq package-enable-at-startup nil)
(package-initialize)

;; Package list
;; TODO make use-package blocks for all of these, and remove this list.
(defvar package-list '(
  anzu
  ace-jump-mode
  ace-window
  auctex
  bison-mode
  color-theme-sanityinc-tomorrow
  company
  company-c-headers
  company-ghc
  company-ghci
  company-irony
  company-jedi
  company-math
  company-quickhelp
  dash
  diminish
  epl
  eshell-autojump
  evil
  evil-anzu
  evil-args
  evil-escape
  evil-exchange
  evil-jumper
  evil-leader
  evil-magit
  evil-matchit
  evil-numbers
  evil-surround
  exec-path-from-shell
  flycheck
  flycheck-rust
  ggtags
  glsl-mode
  gnuplot
  gnuplot-mode
  golden-ratio
  google-this
  goto-chg
  guide-key
  haskell-mode
  helm
  helm-ag
  helm-gtags
  helm-projectile
  highlight-symbol
  hl-todo
  irony
  jedi
  key-chord
  ledger-mode
  linum-off
  magit
  markdown-mode
  multi-term
  ox-reveal
  pandoc-mode
  paredit
  pkg-info
  pony-mode
  popup
  projectile
  racer
  rainbow-delimiters
  rust-mode
  solarized-theme
  tao-theme
  undo-tree
  use-package
  visual-fill-column
  warm-night-theme
  web-mode
  wgrep
  wgrep-ag
  writeroom-mode
  xcscope
  yasnippet
  zenburn-theme
  zeal-at-point
))

(setq package-archive-contents nil)

;; Install missing packages.
;; (package-refresh-contents)
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; Use-package setup.
(require 'use-package)
(setq-default use-package-always-ensure t)
(setq use-package-verbose t)


;;; Vanilla Emacs Behavior ;;;

;; Disable garbage collector on init (restored at end of file).
(defvar temp-gc gc-cons-threshold)
(setq gc-cons-threshold 100000000)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq-default make-backup-files nil)
;; (setq-default backup-inhibited t)
;; (setq-default auto-save-default nil)

;; Color theme

;; Tomorrow theme specific.
(require 'sanityinc-tomorrow-night-theme)
(set-face-attribute 'fringe nil :background (face-background 'default))
(set-face-attribute 'mode-line nil :background (face-background 'default))
(set-face-attribute 'mode-line-inactive nil :background (face-background 'default))

; (setq-default solarized-scale-org-headlines nil)
; (load-theme 'solarized-dark t)
; (load-theme 'solarized-light t)
; (load-theme 'warm-night t)
; (load-theme 'tao-yin t)
; (load-theme 'zenburn t)


;; Config file location.
(defvar conf-file "~/.emacs.d/init.el")

;; Blinking cursor
(blink-cursor-mode t)

;; Emacs source location.
(setq-default source-directory (format "/usr/local/src/emacs-%d.%d"
  emacs-major-version emacs-minor-version))

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Font settings.
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'regular
                    :width 'normal)

;; GUI settings. This disables all the toolbar / extra GUI crap.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode)

;; Highlight all search matches line.
(require 'highlight-symbol)
(highlight-symbol-mode)
(setq highlight-symbol-idle-delay 0.5)

;; History settings.
(savehist-mode 1)

;; Auto reload buffers when changed on disk.
(global-auto-revert-mode t)

;; Disable confirmation for following symlinks.
(setq-default vc-follow-symlinks t)

;; Sentence definition should be one space after a period.
(setf sentence-end-double-space nil)

;; Shell settings.
(setenv "SHELL" "/usr/bin/zsh")

;; Show paren matching
(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; Time in modeline.
(setq-default display-time-format "%a %b %d, %l:%M %p")

;; No-confirm for buffer kill.
(define-key global-map (kbd "C-x k") 'kill-this-buffer)

;; Indentation settings.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)

;; Find mappings.
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Find at point mappings.
(define-prefix-command 'help-at-point-map)
(global-set-key (kbd "C-h C-p") 'help-at-point-map)
(define-key 'help-at-point-map (kbd "f") 'find-function-at-point)
(define-key 'help-at-point-map (kbd "v") 'find-variable-at-point)

(key-chord-mode 1)

;; Time in mode-line.
(defvar display-time-format "%I:%M %p")
(display-time-mode 1)

(defun ins-date ()
  "Insert date into current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%m:%S")))

;; Wrap settings
(setq-default fill-column 80)


;;; Utility functions ;;;

(defun quick-compile-and-run ()
  (interactive)
  (let* ((fn (buffer-name))
        (base (file-name-base fn)))
    (compile (format "gcc -o %s %s && ./%s" base fn base))))

(defun open-conf ()
  "Opens the emacs config file."
  (interactive)
  (find-file conf-file))

(defun open-scratch ()
  "Opens the emacs scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-last-buffer ()
  "Toggle between last buffer open."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun transparency-on ()
  "Set to 90% opacity."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 90)))

;; Transparency disable.
(defun transparency-off ()
  "Set to 100% opacity."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

(defun read-lines (fp)
  "Read lines of file fp into a list"
  (with-temp-buffer
    (insert-file-contents fp)
    (split-string (buffer-string) "\n" t)))

(defun do-in-split (fun)
  "Calls function in a split window"
  (interactive)
  (if (< 1 (length (window-list)))
      (progn
        (other-window 1)
        (funcall fun))
    (progn
      (split-window-right)
      (other-window 1)
      (funcall fun))))

(defun send-selection (start end buffer-fn region-fn)
  "Send either the selected region or the entire buffer to the process that
    buffer-fn and region-fn send to."
  (if (evil-visual-state-p)
      (funcall region-fn start end)
    (funcall buffer-fn)))

(defun kill-and-quit-buffer ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defun split-term () (interactive) (do-in-split 'multi-term))


;;; Package Configuration ;;;

(use-package ace-window)
(use-package ace-jump-mode
  :init
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  (setq ace-jump-mode-scope 'frame))


(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode +1))


(use-package asm-mode
  :bind (:unbind ";"
         :unbind ":")

  :config
  (add-hook 'asm-mode-hook
    (lambda ()
      (local-unset-key (vector asm-comment-char)))))


(use-package bison-mode
  :mode "\\.yy\\'")


;;; C/C++ ;;;

(require 'company)
(add-hook 'cc-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-c-headers)))
  ;; (flycheck-select-checker 'c/c++-gcc)


;;; Comint ;;;

(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)


;;; Company mode ;;;

(require 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.2.0/") ; FIXME

(company-quickhelp-mode 1)
(setq company-minimum-prefix-length 1)

;; Rebind moving down company suggestion list.
(define-key company-active-map (kbd "M-n") 'nil)
(define-key company-active-map (kbd "M-p") 'nil)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

(setq-default company-idle-delay 0.25)
(setq-default company-echo-delay 0)

(defun enable-company ()
  (company-mode 1))

;; Let yas play nicely with company completion.
(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

(add-hook 'company-mode-hook (lambda ()
  (substitute-key-definition 'company-complete-common
                             'company-yasnippet-or-completion
                              company-active-map)))


(use-package compile
  :config
  (setq-default compilation-scroll-output 'first-error))


(use-package doc-view
  :config
  (setf doc-view-continuous t)

  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "g") nil)
  (define-key doc-view-mode-map (kbd "h") nil)
  (define-key doc-view-mode-map (kbd "/") 'doc-view-search)
  (define-key doc-view-mode-map (kbd "?") 'doc-view-search-backward)
  (define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)

  (key-chord-define doc-view-mode-map "gg" 'doc-view-first-page)
  (key-chord-define doc-view-mode-map "gh" 'windmove-left)
  (key-chord-define doc-view-mode-map "gj" 'windmove-down)
  (key-chord-define doc-view-mode-map "gk" 'windmove-up)
  (key-chord-define doc-view-mode-map "gl" 'windmove-right))


(use-package eshell
  :defer 5
  :init

  (defun eshell-new ()
    "Create a new eshell instance."
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'eshell)))

  (defun split-eshell ()
    "Create an eshell split"
    (interactive)
    (do-in-split 'eshell)
    (evil-goto-line nil)
    (evil-append-line 0)))

  :config
  (require 'esh-mode)
  (require 'eshell-autojump)

  (setq-default eshell-save-history-on-exit t)
  (setq-default eshell-history-size 1000000)

  ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete) TODO

  (require 'em-term)
  (add-to-list 'eshell-visual-commands "sl")
  (add-to-list 'eshell-visual-commands "git")

  ;; Set path to shell path.
  (exec-path-from-shell-initialize)


(use-package erc
  :defer 5
  :init
  (setq-default erc-prompt-for-nickserv-password nil)

  (defvar erc-hide-list '("JOIN" "PART" "QUIT"))

  (let ((f (read-lines "~/.erc-login")))
    (defvar erc-nick (car f))
    (defvar erc-password (nth 1 f)))

  (setq-default erc-nickserv-passwords
    `((freenode ((erc-nick . ,erc-password)))
       (mozilla  ((erc-nick . ,erc-password)))))

  (erc-services-mode 1))


(use-package evil
  :init
  ;; When all else fails...
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Override the universal argument for scrolling.
  (setq-default evil-want-C-u-scroll t)

  ;; Use evil's search instead of isearch.
  (setq-default evil-ex-search-persistent-highlight nil)
  (setq-default evil-search-module 'evil-search)

  ;; Evil shift.
  (setq-default evil-shift-width 4)

  ;; Make * and # search for symbols rather than words.
  (setq-default evil-symbol-word-search t)

  ;; Use global regexes by default.
  (setq-default evil-ex-substitute-global t)

  ;; Use man for man pages instead of woman.
  (setq evil-lookup-func
    (lambda ()
      (interactive)
      (man (current-word))))

  :config
  (use-package evil-anzu)

  (use-package evil-args
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

  (use-package evil-escape
    :diminish evil-escape-mode
    :config
    (evil-escape-mode)

    (setq-default evil-escape-delay 0.10)
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-inhibit-functions '(evil-visual-state-p))

    (setq-default evil-escape-excluded-major-modes
      '(magit-mode magit-log-mode magit-cherry-mode
         magit-diff-mode magit-log-mode magit-log-select-mode
         magit-process-mode magit-reflog-mode magit-refs-mode
         magit-revision-mode magit-stash-mode magit-stashes-mode
         magit-status-mode Man-mode-map)))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-jumper
    :config
    ;; On ace jump, add to the jump list.
    (defadvice ace-jump-mode
      (before ace-jump-mode-before activate) (evil-jumper--set-jump))

    ;; On multi-line evil jump, add to the jump list.
    (defadvice evil-next-visual-line
      (before evil-next-visual-line-before activate)
      (unless (eq (ad-get-arg 0) nil)
        (evil-jumper--set-jump)))

    (defadvice evil-previous-visual-line
      (before evil-previous-visual-line-before activate)
      (unless (eq (ad-get-arg 0) nil)
        (evil-jumper--set-jump)))

    (global-evil-jumper-mode))

  (use-package evil-leader
    :config
    (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

    ;; Global evil leaders.
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "." 'repeat
      "/" 'helm-projectile-ag
      ";" 'helm-M-x
      "a" 'ace-window
      "A" 'align
      "b" 'switch-to-last-buffer
      "c" 'recompile
      "C" 'compile
      "D" 'dired
      "d" 'helm-projectile-find-dir
      "e" 'eval-last-sexp
      "E" 'helm-calcul-expression
      "g" 'magit-status
      "G" 'google-this
      "i" 'open-conf
      "I" 'helm-imenu
      "l" 'flycheck-list-errors
      "L" 'browse-url
      "f" 'helm-for-files
      "j" 'winner-undo
      "k" 'winner-redo
      "K" 'kill-compilation
      "m" 'helm-man-woman
      "n" 'flycheck-next-error
      "N" 'flycheck-previous-error
      "o" 'helm-occur
      "O" 'projectile-find-other-file
      "p" 'helm-projectile-switch-project
      "R" 'revert-buffer
      "q" 'evil-quit
      "Q" 'kill-this-buffer
      "s" 'split-eshell
      "S" 'sort-lines
      "T" 'eshell-new
      "u" 'universal-argument
      "U" 'undo-tree-visualize
      "v" 'evil-window-vsplit
      "V" 'evil-window-split
      "w" 'save-buffer
      "W" 'delete-other-windows
      "z" 'open-scratch
      "Z" 'zeal-at-point)

    (global-evil-leader-mode))

  (use-package evil-magit
    :defer 5
    :init
    (setq-default evil-magit-use-y-for-yank t)

    :config
    (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
    (evil-define-key evil-magit-state magit-mode-map "gh" 'windmove-left)
    (evil-define-key evil-magit-state magit-mode-map "gj" 'windmove-down)
    (evil-define-key evil-magit-state magit-mode-map "gk" 'windmove-up)
    (evil-define-key evil-magit-state magit-mode-map "gl" 'windmove-right))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-numbers)

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  ;; Autoadd curly brackets.
  (defun auto-add-curly ()
    (interactive)
    (insert "{")
    (newline-and-indent)
    (insert "}")
    (evil-shift-left-line 1)
    (evil-open-above 0))

  ;; Some modes aren't for text editing and thus don't need the full range of evil
  ;; bindings. We still want movement to work smoothly across all modes though, so
  ;; these are the base movement bindings.
  (defun bind-essential-evil (map mode)
    (evil-define-key mode map "h" 'evil-backward-char)
    (evil-define-key mode map "j" 'evil-next-visual-line)
    (evil-define-key mode map "k" 'evil-previous-visual-line)
    (evil-define-key mode map "l" 'evil-forward-char)
    (evil-define-key mode map "/" 'evil-search-forward)
    (evil-define-key mode map "?" 'evil-search-backward)
    (evil-define-key mode map ":" 'evil-ex)
    (evil-define-key mode map "n" 'evil-search-next)
    (evil-define-key mode map "N" 'evil-search-previous)
    (evil-define-key mode map "v" 'evil-visual-char)
    (evil-define-key mode map "V" 'evil-visual-line)
    (evil-define-key mode map "y" 'evil-yank)
    (evil-define-key mode map "s" 'ace-jump-mode)
    (evil-define-key mode map "gg" 'evil-goto-first-line)
    (evil-define-key mode map (kbd "C-j") 'evil-scroll-down)
    (evil-define-key mode map (kbd "C-k") 'evil-scroll-up)
    (evil-define-key mode map (kbd "C-<SPC>") 'helm-M-x)
    (bind-window-movements map mode))

  (defun bind-window-movements (map mode)
    (evil-define-key mode map "gh" 'windmove-left)
    (evil-define-key mode map "gj" 'windmove-down)
    (evil-define-key mode map "gk" 'windmove-up)
    (evil-define-key mode map "gl" 'windmove-right))

  (require 'man)
  (bind-essential-evil Man-mode-map 'motion)
  (setenv "MANWIDTH" "80")

  (require 'compile)
  (bind-essential-evil compilation-mode-map 'motion)

  (require 'help-mode)
  (bind-essential-evil help-mode-map 'motion)

  (bind-essential-evil dired-mode-map 'normal)

  ;; Mode specific evil init modes.
  (evil-set-initial-state 'org-capture-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)

  (evil-set-initial-state 'occur-mode 'normal)
  (evil-define-key 'normal occur-mode-map (kbd "RET")
    'occur-mode-goto-occurrence)

  ;; Mode specific evil mappings.
  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)

  ;; Auto-correct the last word with flyspell in normal mode.
  (define-key evil-normal-state-map (kbd "C-.") 'flyspell-auto-correct-previous-word)
  (define-key evil-insert-state-map (kbd "C-.") 'flyspell-auto-correct-previous-word)

  ;; Yank the whole buffer.
  (define-key evil-normal-state-map (kbd "gy") (kbd "gg y G C-o"))

  ;; Company manual complete.
  (define-key evil-insert-state-map (kbd "C-;") 'company-complete)

  ;; Curly bracket insertion
  (define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)

  ;; Remove digraph key (useless, interferes with company)
  (define-key evil-insert-state-map (kbd "C-k") nil)

  ;; Evil window movement.
  (define-key evil-normal-state-map "gh" 'windmove-left)
  (define-key evil-normal-state-map "gj" 'windmove-down)
  (define-key evil-normal-state-map "gk" 'windmove-up)
  (define-key evil-normal-state-map "gl" 'windmove-right)

  ;; Can't break my games.
  (add-to-list 'evil-emacs-state-modes 'snake-mode)

  ;; Line completion
  (define-key evil-insert-state-map (kbd "<backtab>") 'evil-complete-next-line)

  ;; Visual line information
  (define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)

  ;; Visual repeat command
  (define-key evil-visual-state-map (kbd ".")
    (lambda () (interactive) (execute-kbd-macro ":norm .")))

  ;; Evil window scrolling.
  (define-key evil-normal-state-map (kbd "C-S-j") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "C-S-k") 'scroll-other-window-down)

  ;; Transpose arguments
  (define-key evil-normal-state-map "g>" 'transpose-words)
  (define-key evil-normal-state-map "g<" (lambda () (interactive) (transpose-words -1)))

  ;; Fix wrapped line movement.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Comment a region like tcomment from vim.
  (define-key evil-visual-state-map "gc" 'comment-dwim)

  ;; Quick buffer closing from insert mode.
  (define-key evil-insert-state-map (kbd "C-q") 'evil-quit)

  ;; Evil ace-jump
  (define-key evil-normal-state-map "s" 'ace-jump-mode)

  ;; Evil increment.
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

  ;; Evil scrolling.
  (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

  ;; Use gtags instead of etags for tag lookup.
  (define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-dwim)

  ;; Eshell history
  (define-key evil-insert-state-map (kbd "C-l") 'helm-eshell-history)

  (evil-mode 1))


(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (add-hook 'c++-mode-hook
    (lambda ()
      (setq flycheck-gcc-language-standard "c++11")
      (setq flycheck-clang-language-standard "c++11"))))


(use-package ggtags
  :defer t
  :init
  (add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode)
        ;; (ggtags-mode 1)
        (cscope-setup)
        (cscope-minor-mode))))

  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer t
    :init
    (defvar helm-gtags-ignore-case t)
    (defvar helm-gtags-auto-update t)
    (defvar helm-gtags-use-input-at-cursor t)

    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)))


(use-package golden-ratio
  :defer 5
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)

  :config
  (setq-default window-combination-resize t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-exclude-buffer-names " *guide-key*") ;; FIXME
  (add-to-list 'golden-ratio-inhibit-functions 'helm-active))


(use-package google-this
  :defer 10
  :diminish google-this-mode
  :init
  (google-this-mode 1))


(use-package guide-key
  :defer 10
  :diminish guide-key-mode
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.3)
  (setq guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))


(use-package haskell-mode
  :mode "\\.hs\\'"

  ;; TODO setup hasktags
  ;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

  :init
  (add-hook 'haskell-mode-hook
    (lambda ()
      ;; (add-to-list 'company-backends 'company-ghc)
      (turn-on-haskell-indent)))

  (add-hook 'interactive-haskell-mode-hook
    (lambda ()
      (add-to-list 'company-backends 'company-ghci)))

  :config
  (defun haskell-run-other-window ()
    (interactive)
    (let ((current-win (selected-window)))
      (inferior-haskell-load-and-run "main")
      (select-window current-win)
      (golden-ratio)))

  (evil-leader/set-key-for-mode 'haskell-mode
    "r" 'haskell-run-other-window
    "xd" 'inferior-haskell-send-decl
    "xi" 'run-haskell))

(use-package helm
  :diminish helm-mode

  :init
  (require 'helm-ag)
  (require 'helm-imenu)
  (require 'helm-projectile)
  (require 'grep)

  (helm-mode 1)
  (helm-projectile-on)

  (defvar helm-M-x-fuzzy-match t)
  (setq-default helm-ff-skip-boring-files t)
  (setq-default helm-buffers-fuzzy-matching t)
  (setq-default helm-imenu-fuzzy-match t)
  (setq-default helm-recentf-fuzzy-match t)

  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-o") 'helm-select-action)
  (define-key helm-map (kbd "C-;") 'helm-toggle-all-marks)

  (defun helm-active ()
    (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

  (setq-default helm-ag-insert-at-point 'symbol))


(use-package hideshow
  :diminish hs-minor-mode)


(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'cc-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  ;; Replace completion functions.
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))


(use-package magit
  :defer t
  :config
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (setq-default magit-push-always-verify nil))


(use-package markdown-mode
  :mode ("\\.md\\'" "\\.txt\\'")

  :init
  (add-hook 'text-mode-hook
    (lambda () (turn-on-auto-fill) (set-fill-column 80)))

  (add-hook 'markdown-mode-hook
    (lambda () (turn-on-auto-fill) (set-fill-column 80)))

  :config
  (evil-leader/set-key-for-mode 'markdown-mode
    "P" 'pandoc-convert-to-pdf))


; Clean out old buffers at midnight.
(use-package midnight
  :defer 5)


(use-package octave
  :mode "\\.m\\'"

  :config
  (evil-leader/set-key-for-mode 'octave-mode
    "r" 'octave-send-buffer
    "xi" 'run-octave
    "xr" 'octave-send-region)

  (add-hook 'octave-mode-hook
    (lambda ()
      (setq-local evil-shift-width 2))))


(use-package org
  :defer t

  :init
  (defvar org-log-done t)

  (setq org-agenda-files '("~/org/tracking"))
  (setq org-default-notes-file "~/org/notes.org")

  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-cl" 'org-store-link)

  ;; Capture templates
  (defvar org-capture-templates
    '(("d" "Dreams" entry
        (file+headline "~/org/dream.org" "Dreams")
        "*** %t\n")))

  :config
  (evil-define-key 'normal org-mode-map "t" 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "<left>") 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map (kbd "<right>") 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "<up>") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "<down>") 'org-metadown)

  (evil-leader/set-key-for-mode 'org-mode
    "A" 'org-agenda
    "D" 'org-archive-done
    "L" 'org-preview-latex-fragment
    "P" 'org-export-latex-no-preamble
    "r" 'org-latex-export-to-pdf
    ">" 'org-metaright
    "<" 'org-metaleft
    "+" (lambda () (interactive) (org-table-sort-lines nil ?a)))

  ;; Properly indent src blocks.
  (setq org-src-tab-acts-natively t)

  (defun org-archive-done ()
    "Removes all DONE entries and places them into an archive file."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (defun org-export-latex-no-preamble ()
    "Exports to latex without any preamble."
    (interactive)
    (org-latex-export-to-latex nil nil nil t nil))

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((C . t)
       (haskell . t)
       (python . t)
       (sh . t)))

  ;; Do not prompt for babel code execution
  (setq-default org-confirm-babel-evaluate nil)

  ;; Correct fonts for code blocks.
  (setq-default org-src-fontify-natively t)

  (add-hook 'org-mode-hook
    (lambda ()
      (turn-on-flyspell)
      (enable-company-math)
      (setq-local company-math-allow-latex-symbols-in-faces t)
      (enable-company)
      (setq-local company-minimum-prefix-length 100))))


(use-package ox-reveal
  :defer 5
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))


(use-package pandoc-mode
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))


(use-package paredit
  :config
  (evil-define-key 'normal scheme-mode-map
    (kbd ")") 'paredit-forward-slurp-sexp
    (kbd "(") 'paredit-backward-slurp-sexp
    (kbd "C-0") 'paredit-forward-barf-sexp
    (kbd "C-9") 'paredit-backward-barf-sexp))


;;; Prog-mode ;;;

(add-hook 'prog-mode-hook
  (lambda ()
    (define-key global-map (kbd "RET") 'newline-and-indent)
    (define-key global-map (kbd "<C-return>") 'indent-new-comment-line)
    (enable-company)
    (hs-minor-mode)
    (hl-todo-mode 1)
    (rainbow-delimiters-mode)))


(use-package projectile
  :defer 5
  :diminish projectile-mode

  :init
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-<SPC>") 'helm-M-x)
  (define-key evil-insert-state-map (kbd "C-<SPC>") 'helm-M-x)

  :config
  (projectile-global-mode)

  ;; Use project root as cscope database.
  (defadvice helm-projectile-switch-project
    (after helm-projectile-switch-project-after activate)
    (cscope-set-initial-directory (projectile-project-root))))


(use-package python
  :init
  (setq python-shell-enable-font-lock nil)

  :config
  (evil-leader/set-key-for-mode 'python-mode
    "r" 'python-shell-send-selection
    "xi" 'python-shell-switch-to-shell)

  (when (executable-find "ipython")
    (setq-default python-shell-interpreter "ipython"))

  (defun python-shell-send-selection (start end)
    (interactive "r")
    (send-selection
      start end 'python-shell-send-buffer 'python-shell-send-region))

  (add-hook 'python-mode-hook
    (lambda ()
      (add-to-list 'company-backends 'company-jedi)
      (setq tab-width 4)
      (setq evil-shift-width 4)
      (defvar python-indent 4)))

  (use-package pony-mode))


(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")

  :config
  (evil-leader/set-key-for-mode 'rust-mode
    "r" (lambda () (interactive) (cargo-cmd "run"))
    "t" (lambda () (interactive) (cargo-cmd "test")))

  (defun cargo-cmd (cmd)
    (interactive)
    (compile (format "cargo %s" cmd)))

  (add-hook 'rust-mode-hook
    (lambda ()
      (racer-mode)
      (setq tab-width 4)
      (setq rust-indent-offset 4)
      (setq evil-shift-width 4))))

  ;; FIXME
  ;; (use-package flycheck-rust
  ;;   :config
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


(use-package scheme
  :init
  ;; Setup interpreter to use settings compatable with sicp code.
  (setq-default scheme-program-name "racket -i -p neil/sicp")

  :config
  (defun scheme-send-buffer ()
    (interactive)
    (comint-send-region (scheme-proc) (point-min) (point-max))
    (comint-send-string (scheme-proc) "\n"))

  (defun scheme-send-selection (start end)
    (interactive "r")
    (send-selection
      start end 'scheme-send-buffer 'scheme-send-region))

  (evil-leader/set-key-for-mode 'scheme-mode
    "d" 'scheme-send-definition
    "e" 'scheme-send-last-sexp
    "r" 'scheme-send-selection))


(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook
    (lambda () (yas-minor-mode -1))))


(use-package tex-mode
  :mode "\\.tex\\."

  :init
  ;; TODO move this to company subsection.
  (require 'company-math)
  (defun enable-company-math ()
    (add-to-list 'company-backends 'company-math-symbols-latex)
    (setq company-tooltip-align-annotations t))

  (defun latex-compile ()
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file -1))

  :config
  (evil-leader/set-key-for-mode 'latex-mode
    "r" 'latex-compile)

  (add-hook 'TeX-mode-hook
    (lambda ()
      (enable-company-math)
      (enable-company)
      (setq-local company-minimum-prefix-length 100))))


;;; Emacs Lisp ;;;

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-offset 2)
            (prettify-symbols-mode 1)))


(use-package undo-tree
  :defer 10
  :diminish undo-tree-mode

  :config
  (setq undo-tree-auto-save-history t))


(use-package web-mode
  :mode ("\\.php\\'" "\\.erb\\'" "\\.scss\\'" )

  :config
  (add-hook 'web-mode-hook
    (lambda ()
      (setq web-mode-markup-indent-offset 4)
      (setq web-mode-css-indent-offset 4)
      (setq web-mode-code-indent-offset 4)
      (setq web-mode-enable-auto-closing nil)
      (setq web-mode-enable-auto-expanding nil)
      (setq web-mode-enable-auto-indentation nil)
      (setq web-mode-enable-auto-opening nil)
      (setq web-mode-enable-auto-pairing nil)
      (setq web-mode-enable-auto-quoting nil)
      (yas-activate-extra-mode 'html-mode))))

(use-package which-func
  :defer 5
  :config
  (which-function-mode 1))


(use-package winner
  :defer 5
  :config
  (winner-mode 1))


(use-package xcscope
  :config
  (defvar cscope-program "gtags-cscope"))


(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode

  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  (setq yas-triggers-in-field t)

  :config
  (yas-global-mode 1))


;;; Diminish ;;;

;; This must be done after everything is loaded.
(diminish 'abbrev-mode)
(diminish 'company-mode)

;; Restore gc threshhold.
(setq gc-cons-threshold temp-gc)
