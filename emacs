; Chris Laverdiere's .emacs file.


;; Package management ;;

; Package repositories
(require 'package)
(setq package-archives '(
  ("elpa" . "http://tromey.com/elpa/")
  ("gnu" . "http://elpa.gnu.org/packages/")
  ("melpa" . "http://melpa.milkbox.net/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")
))

; Package list
(setq package-list '(
  ace-jump-mode
  auctex
  auto-complete
  dash
  epl
  evil
  evil-jumper
  evil-leader
  evil-matchit
  evil-numbers
  evil-org
  evil-surround
  flycheck
  goto-chg
  guide-key
  haskell-mode
  helm
  helm-ag
  helm-projectile
  key-chord
  linum-relative
  magit
  markdown-mode
  pandoc-mode
  pkg-info
  popup
  projectile
  solarized-theme
  undo-tree
  visual-fill-column
  writeroom-mode
  yasnippet
))

(package-initialize)

; Fetch list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Vanilla Emacs Behavior ;;

; Backup settings
(setq make-backup-files nil)

; Color theme
(load-theme 'solarized-dark t)

; Config file shortcut to open this file.
(setq conf-file "~/.emacs.d/init.el")
(defun open-conf()
  "Opens the emacs config file."
  (interactive)
  (find-file conf-file))

; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Font settings.
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'regular
                    :width 'normal)

; GUI settings. This disables all the toolbar / extra GUI crap.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; History settings
(savehist-mode 1)

; Sentence definition should be one space after a period.
(setf sentence-end-double-space nil)

; Session saving
(desktop-save-mode 1)

; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq evil-shift-width 2)

; Transparency enable / disable functions.
(defun transparency-on ()
  "Set to 95% transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(95 95)))

(defun transparency-off ()
  "Set to 100% transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

; Wrap settings
(setq-default fill-column 80)


;; Plugin-dependent Emacs behavior (Small plugins) ;;

; Ace jump settings
(require 'ace-jump-mode)

; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)

; Magit (Git integration)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

; YASnippet (Tab-completed snippets)
(require 'yasnippet)
(yas-global-mode 1)

; Guide key (Pops up help menu for prefix-keys)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(guide-key-mode 1)

; Relative line numbers.
(require 'linum-relative)
(global-linum-mode t)


;; Org Mode (Life organizer) ;;

; Org files
(setq org-agenda-files (list "~/org/school.org"))
(setq org-default-notes-file "~/org/notes.org")

; Org mappings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(defun org-archive-done ()
  "Removes all DONE entries and places them into an archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


;; Evil Mode (Vim emulation) ;;

; Remap escape to quit out of things.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'evil)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-org)
(require 'evil-surround)

; Use evil (mostly) everywhere.
(global-evil-leader-mode)

; Global evil leaders.
(evil-leader/set-leader ",")
(evil-leader/set-key
  "/" 'helm-ag
  "b" 'helm-buffers-list
  "c" 'org-capture
  "d" 'dired
  "e" 'eval-last-sexp
  "g" 'magit-status
  "i" 'open-conf
  "l" 'flycheck-list-errors
  "f" 'helm-for-files
  "j" 'ace-jump-line-mode
  "od" 'org-deadline
  "os" 'org-schedule
  "p" 'helm-projectile-switch-project
  "q" 'kill-buffer
)

; Evil window movement.
(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)

; Evil window scrolling.
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Transpose arguments
(define-key evil-normal-state-map "g>" 'transpose-words)

; Fix wrapped line movement.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

; Comment a region like tcomment from vim.
(define-key evil-normal-state-map "gc" 'comment-dwim)

; Map <ESC> to jk.
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Evil ace-jump
(define-key evil-normal-state-map "s" 'ace-jump-mode)

; Evil jumper (C-o / C-i functionality)
(global-evil-jumper-mode)

; Evil surround settings.
(global-evil-surround-mode 1)

; Evil increment.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

; Evil scrolling.
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

(evil-mode 1)


;; Projectile (Project management) ;;

(require 'projectile)
(projectile-global-mode)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

; Evil mappings for projectile.
(define-key evil-normal-state-map (kbd "<SPC>") 'helm-M-x)
(evil-leader/set-key-for-mode 'projectile-mode
  "/" 'projectile-helm-ag
)


;; Helm (Incremental completion / Selection narrowing) ;;

(require 'helm-projectile)
(helm-projectile-on)

; Helm fuzzy-finding.
(setq helm-M-x-fuzzy-match t)

; Use the silver searcher ag with Helm.
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))


;; Markdown ;;

; Filetypes to apply markdown to.
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

; Wrap settings by filetype.
(add-hook 'text-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

(add-hook 'markdown-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

; Evil mappings for markdown.
(evil-leader/set-key-for-mode 'markdown-mode
  "p" 'pandoc-convert-to-pdf
)


;; Pandoc (Markup conversion) ;;

(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'auto-complete-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;; Flycheck (Syntax checking)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))


;; Haskell ;;

; Haskell indent mode.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

; Evil mappings for haskell.
(evil-leader/set-key-for-mode 'haskell-mode
  "i" 'run-haskell
  "x" 'inferior-haskell-load-and-run
)


;; Python ;;

; Evil mappings for python.
(evil-leader/set-key-for-mode 'python-mode
  "r" 'python-shell-send-buffer
)

; Fix tab settings for python files.
(add-hook 'python-mode-hook
  (lambda ()
    (setq tab-width 4)
    (setq python-indent 4)
    (setq evil-shift-width 4)))
