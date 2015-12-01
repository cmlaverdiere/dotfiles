;;;; Chris Laverdiere's Emacs config ;;;;

;; TODO
;;  - Write a fn to load all included header files into buffers.
;;  - Try paradox
;;  - Try use-package
;;  - Add 'make test' generic leader for 't'
;;  - Leader help keys

;; FIXME
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


;; Package list
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
  expand-region
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
  linum-relative
  magit
  markdown-mode
  multi-term
  pandoc-mode
  pkg-info
  popup
  projectile
  racer
  rainbow-delimiters
  rust-mode
  solarized-theme
  tao-theme
  undo-tree
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

(package-initialize)

;; Fetch list of packages available.
(setq package-archive-contents nil)
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;; Vanilla Emacs Behavior ;;;

;; Garbage collector max
(setq gc-cons-threshold 100000000)

;; Backup settings
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

(setq-default solarized-scale-org-headlines nil)

;; Color theme

;; Tomorrow theme specific.
(require 'sanityinc-tomorrow-night-theme)
(set-face-attribute 'fringe nil :background (face-background 'default))
(set-face-attribute 'mode-line nil :background (face-background 'default))
(set-face-attribute 'mode-line-inactive nil :background (face-background 'default))

; (load-theme 'solarized-dark t)
; (load-theme 'solarized-light t)
; (load-theme 'warm-night t)
; (load-theme 'tao-yin t)
; (load-theme 'zenburn t)


;; Config file location.
(defvar conf-file "~/.emacs.d/init.el")

;; Emacs source location.
(setq-default source-directory (format "/usr/local/src/emacs-%d.%d"
  emacs-major-version emacs-minor-version))

;; Delete trailing whitespace on save
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

;; Highlight current line
(global-hl-line-mode)

;; Highlight all search matches line
(require 'highlight-symbol)
(highlight-symbol-mode)
(setq highlight-symbol-idle-delay 0.5)

;; History settings
(savehist-mode 1)

;; Auto reload buffers when changed on disk.
(global-auto-revert-mode t)

;; Sentence definition should be one space after a period.
(setf sentence-end-double-space nil)

;; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)

;; Find mappings
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Find at point mappings
(define-prefix-command 'help-at-point-map)
(global-set-key (kbd "C-h C-p") 'help-at-point-map)
(define-key 'help-at-point-map (kbd "f") 'find-function-at-point)
(define-key 'help-at-point-map (kbd "v") 'find-variable-at-point)

(key-chord-mode 1)

;; Time in mode-line
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

(defun kill-and-quit-buffer ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defun split-term () (interactive) (do-in-split 'multi-term))


;;; Ace jump ;;;

(require 'ace-window)
(require 'ace-jump-mode)
(ace-window-display-mode)
(setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
(setq ace-jump-mode-scope 'frame)


;;; Anzu

(global-anzu-mode +1)


;;; Asm ;;;

(require 'asm-mode)
(define-key asm-mode-map (kbd ";") nil)
(define-key asm-mode-map (kbd ":") nil)
(add-hook 'asm-mode-hook (lambda ()
    (local-unset-key (vector asm-comment-char))))


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


;;; Compilation mode ;;;

(setq-default compilation-scroll-output 'first-error)


;;; Cscope ;;;

(defvar cscope-program "gtags-cscope")
(require 'xcscope)


;;; DocView ;;;

(require 'doc-view)
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
(key-chord-define doc-view-mode-map "gl" 'windmove-right)


;;; Eshell ;;;

(require 'eshell)
(require 'esh-mode)
(require 'eshell-autojump)

(setq-default eshell-save-history-on-exit t)

;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete) TODO

(require 'em-term)
(add-to-list 'eshell-visual-commands "sl")
(add-to-list 'eshell-visual-commands "git")


;; Set path to shell path.
(exec-path-from-shell-initialize)

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
  (evil-append-line 0))


;;; ERC ;;;

;; Hide joins / parts / quits.
(defvar erc-hide-list '("JOIN" "PART" "QUIT"))

;; Account login info
(let ((f (read-lines "~/.erc-login")))
  (defvar erc-nick (car f))
  (defvar erc-password (nth 1 f)))

;; Auto identify
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode ((erc-nick . ,erc-password)))
        (mozilla  ((erc-nick . ,erc-password)))))


;;; Evil Mode ;;;

;; Remap escape to quit out of things.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use evil's search instead of isearch.
(setq-default evil-ex-search-persistent-highlight nil)
(setq-default evil-search-module 'evil-search)

(require 'evil)
(require 'evil-anzu)
(require 'evil-jumper)
(require 'evil-magit)

(require 'evil-exchange)
(evil-exchange-install)

(require 'evil-escape)
(evil-escape-mode)
(setq-default evil-escape-delay 0.10)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-inhibit-functions '(evil-visual-state-p))
(setq-default evil-escape-excluded-major-modes
              '(magit-mode magit-log-mode magit-cherry-mode
                magit-diff-mode magit-log-mode magit-log-select-mode
                magit-process-mode magit-reflog-mode magit-refs-mode
                magit-revision-mode magit-stash-mode magit-stashes-mode
                magit-status-mode))

;; On multi-line evil jump, add to the jump list.
(defadvice evil-next-visual-line
  (before evil-next-visual-line-before activate)
  (unless (eq (ad-get-arg 0) nil)
    (evil-jumper--set-jump)))

(defadvice evil-previous-visual-line
  (before evil-previous-visual-line-before activate)
  (unless (eq (ad-get-arg 0) nil)
    (evil-jumper--set-jump)))

;; On ace jump, add to the jump list.
(defadvice ace-jump-mode
  (before ace-jump-mode-before activate) (evil-jumper--set-jump))

(require 'evil-leader)
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-numbers)

(require 'evil-args)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

(define-key evil-insert-state-map (kbd "C-;") 'company-complete)
(define-key evil-normal-state-map (kbd "gy") (kbd "gg y G C-o"))

;; Some modes aren't for text editing and thus don't need the full range of evil
;; bindings. We still want movement to work smoothly across all modes though, so
;; these are the base movement bindings.
(defun bind-essential-evil (map)
  (evil-define-key 'motion map "h" 'evil-backward-char)
  (evil-define-key 'motion map "j" 'evil-next-visual-line)
  (evil-define-key 'motion map "k" 'evil-previous-visual-line)
  (evil-define-key 'motion map "l" 'evil-forward-char)
  (evil-define-key 'motion map "/" 'evil-search-forward)
  (evil-define-key 'motion map "?" 'evil-search-backward)
  (evil-define-key 'motion map ":" 'evil-ex)
  (evil-define-key 'motion map "n" 'evil-search-next)
  (evil-define-key 'motion map "N" 'evil-search-previous)
  (evil-define-key 'motion map "v" 'evil-visual-char)
  (evil-define-key 'motion map "V" 'evil-visual-line)
  (evil-define-key 'motion map "y" 'evil-yank)
  (evil-define-key 'motion map "s" 'ace-jump-mode)
  (evil-define-key 'motion map "gg" 'evil-goto-first-line)
  (evil-define-key 'motion map (kbd "C-j") 'evil-scroll-down)
  (evil-define-key 'motion map (kbd "C-k") 'evil-scroll-up)
  (evil-define-key 'motion map (kbd "C-<SPC>") 'helm-M-x)
  (bind-window-movements map))

(defun bind-window-movements (map)
  (evil-define-key 'motion map "gh" 'windmove-left)
  (evil-define-key 'motion map "gj" 'windmove-down)
  (evil-define-key 'motion map "gk" 'windmove-up)
  (evil-define-key 'motion map "gl" 'windmove-right))

(bind-window-movements doc-view-mode-map)

(require 'man)
(bind-essential-evil Man-mode-map)

(require 'compile)
(bind-essential-evil compilation-mode-map)

(require 'help-mode)
(bind-essential-evil help-mode-map)

;; Mode specific evil init modes .
(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

(evil-set-initial-state 'occur-mode 'normal)
(evil-define-key 'normal occur-mode-map (kbd "RET")
  'occur-mode-goto-occurrence)

;; Mode specific evil mappings.
(evil-define-key 'normal eshell-mode-map (kbd "RET")
  'eshell-send-input)

(require 'evil-surround)

;; Use evil (mostly) everywhere.
(global-evil-leader-mode)

;; Global evil leaders.
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "/" 'helm-projectile-ag
  ";" 'helm-M-x
  "a" 'ace-window
  "A" 'align
  "b" 'switch-to-last-buffer
  "c" 'recompile
  "C" 'compile
  "d" 'dired
  "e" 'eval-last-sexp
  "E" 'helm-calcul-expression
  "g" 'magit-status
  "G" 'google-this
  "i" 'open-conf
  "I" 'helm-imenu
  "l" 'flycheck-list-errors
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
  "Q" 'kill-and-quit-buffer
  "s" 'split-eshell
  "S" 'sort-lines
  "T" 'eshell-new
  "u" 'undo-tree-visualize
  "U" 'browse-url
  "v" 'evil-window-vsplit
  "V" 'evil-window-split
  "w" 'save-buffer
  "W" 'delete-other-windows
  "z" 'open-scratch
  "Z" 'zeal-at-point
)

;; Autoadd curly brackets.
(defun auto-add-curly ()
  (interactive)
  (insert "{")
  (newline-and-indent)
  (insert "}")
  (evil-shift-left-line 1)
  (evil-open-above 0))


;; Curly bracket insertion
(define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)

;; Remove digraph key (useless, interferes with company)
(define-key evil-insert-state-map (kbd "C-k") nil)

;; Evil window movement.
(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)

;; Make * and # search for symbols rather than words.
(setq-default evil-symbol-word-search t)

;; Use global regexes by default.
(setq-default evil-ex-substitute-global t)

;; Line completion
(define-key evil-insert-state-map (kbd "<backtab>") 'evil-complete-next-line)

;; Visual line information
(define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)

;; Visual repeat command
(define-key evil-visual-state-map (kbd ".")
  (lambda () (interactive) (execute-kbd-macro ":norm .")))

;; Evil window scrolling.
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

;; Use helm for man pages.
;; (define-key evil-normal-state-map "K" 'helm-man-woman)

;; Use man for man pages instead of woman.
(setq evil-lookup-func (lambda ()
  (interactive)
  (man (current-word))))

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

;; Evil jumper (C-o / C-i functionality)
(global-evil-jumper-mode)

;; Evil surround settings.
(global-evil-surround-mode 1)

;; Evil increment.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

;; Evil scrolling.
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

;; Use gtags instead of etags for tag lookup.
(define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-dwim)

;; Eshell history
(define-key evil-insert-state-map (kbd "C-l") 'helm-eshell-history)

;; Evil shift.
(setq-default evil-shift-width 4)

(evil-mode 1)


;;; Flex / Bison ;;;

(add-to-list 'auto-mode-alist '("\\.yy\\'" . bison-mode))


;;; Flycheck ;;;

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11")
            (setq flycheck-clang-language-standard "c++11")))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; Expand region ;;;

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;; GNU Global ;;;

(require 'ggtags)

;; Enable gtags for c/c++.
(add-hook 'c-mode-common-hook (lambda ()
  (when (derived-mode-p 'c-mode 'c++-mode)
        ;; (ggtags-mode 1)
        (cscope-setup)
        (cscope-minor-mode))))

(defvar helm-gtags-ignore-case t)
(defvar helm-gtags-auto-update t)
(defvar helm-gtags-use-input-at-cursor t)

(require 'helm-gtags)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)


;;; Golden ratio ;;;

(require 'golden-ratio)
(setq golden-ratio-auto-scale)
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-exclude-buffer-names " *guide-key*") ;; FIXME
(add-to-list 'golden-ratio-inhibit-functions 'helm-active)
(setq-default window-combination-resize t)
(golden-ratio-mode 1)

(defun helm-active ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))


;;; Google this ;;;

(google-this-mode 1)


;;; Guide key ;;;

(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/idle-delay 0.3)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)


;;; Haskell ;;;

;; TODO setup hasktags
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(add-hook 'haskell-mode-hook (lambda ()
  ;; (add-to-list 'company-backends 'company-ghc)
  (turn-on-haskell-indent)))

(add-hook 'interactive-haskell-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-ghci)))

(defun haskell-run-other-window ()
  (interactive)
  (let ((current-win (selected-window)))
    (inferior-haskell-load-and-run "main")
    (select-window current-win)
    (golden-ratio)))

;; Evil mappings for haskell.
(evil-leader/set-key-for-mode 'haskell-mode
  "r" 'haskell-run-other-window
  "xd" 'inferior-haskell-send-decl
  "xi" 'run-haskell
)


;;; Helm ;;;

(require 'helm)
(require 'helm-ag)
(require 'helm-imenu)
(require 'helm-projectile)
(require 'grep)
(helm-mode 1)
(helm-projectile-on)

;; Helm fuzzy-finding.
(defvar helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)

;; Consistent movement with company.
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

;; Use the silver searcher ag with Helm.
(setq helm-ag-insert-at-point 'symbol)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))


;;; Irony ;;;

(require 'irony)

;; Use irony for C/C++
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

;; Add irony backend for company.
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Extra completions.
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;;; LaTeX ;;;

(require 'tex)

(require 'company-math)
(defun enable-company-math ()
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (setq company-tooltip-align-annotations t))

(defun latex-compile ()
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file -1))

;; Evil mappings for latex.
(evil-leader/set-key-for-mode 'latex-mode
  "r" 'latex-compile
)

(add-hook 'latex-mode-hook (lambda ()
  (enable-company-math)
  (enable-company)))


;;; Lisp ;;;

(add-hook 'emacs-lisp-mode-hook (lambda ()
  (prettify-symbols-mode 1)))


;;; Magit ;;;

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)


;;; Markdown ;;;

;; Filetypes to apply markdown to.
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

;; Wrap settings by filetype.
(add-hook 'text-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

(add-hook 'markdown-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

;; Evil mappings for markdown.
(evil-leader/set-key-for-mode 'markdown-mode
  "P" 'pandoc-convert-to-pdf
)


;;; Midnight ;;;

; Clean out old buffers at midnight.
(require 'midnight)


;;; Octave / Matlab ;;;

(add-to-list 'auto-mode-alist
    '("\\.m$" . octave-mode))

(add-hook 'octave-mode-hook (lambda ()
    (setq-local evil-shift-width 2)))

;; Evil mappings for octave.
(evil-leader/set-key-for-mode 'octave-mode
  "r" 'octave-send-buffer
  "xi" 'run-octave
  "xr" 'octave-send-region
)


;;; Org Mode ;;;

(require 'org)

;; Org files
(setq org-agenda-files '("~/org/tracking"))
(setq org-default-notes-file "~/org/notes.org")

;; Org mappings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(evil-define-key 'normal org-mode-map "t" 'org-todo)

(defvar org-log-done t)

(defun org-archive-done ()
  "Removes all DONE entries and places them into an archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun org-export-latex-no-preamble ()
  "Exports to latex without any preamble."
  (interactive)
  (org-latex-export-to-latex nil nil nil t nil)
)

;; Properly indent src blocks.
(setq org-src-tab-acts-natively t)

;; Load babel languages.
(org-babel-do-load-languages
  'org-babel-load-languages
  '((C . t)
    (haskell . t)
    (python . t)
    (sh . t)))

;; Do not prompt for babel code execution
(setq org-confirm-babel-evaluate nil)

;; Correct fonts for code blocks.
(setq-default org-src-fontify-natively t)

;; Capture templates
(defvar org-capture-templates
  '(("d" "Dreams" entry
      (file+headline "~/org/dream.org" "Dreams")
                     "*** %t\n")))

(add-hook 'org-mode-hook (lambda ()
  (turn-on-flyspell)
  (enable-company-math)
  (setq-local company-math-allow-latex-symbols-in-faces t)
  (enable-company)
  (setq-local company-minimum-prefix-length 100) ; Never complete.
  ))

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
  "+" (lambda () (interactive) (org-table-sort-lines nil ?a))
)


;;; Pandoc ;;;

(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;;; Prog mode ;;;

(require 'hideshow)

(add-hook 'prog-mode-hook (lambda ()
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key global-map (kbd "<C-return>") 'indent-new-comment-line)
  (enable-company)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (rainbow-delimiters-mode)))


;;; Projectile ;;;

(require 'projectile)
(projectile-global-mode)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

;; Evil mappings for projectile.
(define-key evil-normal-state-map (kbd "C-<SPC>") 'helm-M-x)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'helm-M-x)

;; Use project root as cscope database.
(defadvice helm-projectile-switch-project
  (after helm-projectile-switch-project-after activate)
  (cscope-set-initial-directory (projectile-project-root)))

;;; Python ;;;

(require 'python)

;; Evil mappings for python.
(evil-leader/set-key-for-mode 'python-mode
  "r" 'python-shell-send-buffer
  "xi" 'python-shell-switch-to-shell
  "xr" 'python-shell-send-region
)

;; Faster printing of large lines
(setq python-shell-enable-font-lock nil)

;; Custom Python mode hook.
(add-hook 'python-mode-hook
  (lambda ()
    (add-to-list 'company-backends 'company-jedi)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (defvar python-indent 4)))


;;; Relative line numbers ;;;

(require 'linum-off)
(require 'linum-relative)
;; (global-linum-mode t)


;;; Rust ;;;

(require 'rust-mode)

(defun cargo-cmd (cmd)
  (interactive)
  (compile (format "cargo %s" cmd)))

;; Evil mappings for rust.
(evil-leader/set-key-for-mode 'rust-mode
  "r" (lambda () (interactive) (cargo-cmd "run"))
  "t" (lambda () (interactive) (cargo-cmd "test"))
)

(exec-path-from-shell-copy-env "RUST_SRC_PATH")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . text-mode))
(add-hook 'rust-mode-hook (lambda ()
  (racer-mode)
  (setq tab-width 4)
  (setq rust-indent-offset 4)
  (setq evil-shift-width 4)))


;;; Terminal ;;;

(add-to-list 'linum-disabled-modes-list 'term-mode)
(delete 'org-mode linum-disabled-modes-list)

(add-hook 'term-mode-hook (lambda ()
  (yas-minor-mode -1)))


;;; Undo Tree ;;;

;; Persistent undo
(setq undo-tree-auto-save-history t)


;;; Web ;;;

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (yas-activate-extra-mode 'html-mode))

(add-hook 'web-mode-hook 'my-web-mode-hook)


;;; Which-function ;;;

(require 'which-func)
(which-function-mode 1)


;;; Winner ;;;

(winner-mode 1)


;;; YASnippet ;;;

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
(setq yas-triggers-in-field t)


;;; Diminish ;;;

;; This must be done after everything is loaded.
(diminish 'abbrev-mode)
(diminish 'anzu-mode)
(diminish 'company-mode)
(diminish 'evil-escape-mode)
(diminish 'flycheck-mode)
(diminish 'golden-ratio-mode)
(diminish 'google-this-mode)
(diminish 'guide-key-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'hs-minor-mode)
(diminish 'projectile-mode)
(diminish 'undo-tree-mode)
(diminish 'yas-minor-mode)
