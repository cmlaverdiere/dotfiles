; Chris Laverdiere's .emacs file.

; Package repositories
(require 'package)
(setq package-archives '(
  ("elpa" . "http://tromey.com/elpa/")
  ("gnu" . "http://elpa.gnu.org/packages/")
  ("melpa" . "http://melpa.milkbox.net/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")
))

; Package management
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
  ;; smooth-scrolling
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

; History settings
(savehist-mode 1)

; Transparency
(defun transparency-on ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(95 95)))

(defun transparency-off ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

; Session saving
(desktop-save-mode 1)

; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

; GUI settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Org mappings
(setq org-agenda-files (list "~/org/school.org"))
(setq org-default-notes-file "~/org/notes.org")

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(defun org-archive-done ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq evil-shift-width 2)

; Backup settings
(setq make-backup-files nil)

; Wrap settings
(setq-default fill-column 80)

; Escape remap
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; Line settings
(require 'linum-relative)
(global-linum-mode t)

; Color theme
(load-theme 'solarized-dark t)

;; (require 'smooth-scrolling)

(require 'evil)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-org)
(require 'evil-surround)

; Projectile settings
(require 'projectile)
(projectile-global-mode)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

; Helm settings
(require 'helm-projectile)
(helm-projectile-on)
(setq helm-M-x-fuzzy-match t)
(define-key evil-normal-state-map (kbd "<SPC>") 'helm-M-x)

(evil-leader/set-key-for-mode 'projectile-mode
  "/" 'projectile-helm-ag
)

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

; Autocomplete settings
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)

; Magit settings
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

; YASnippet settings
(require 'yasnippet)
(yas-global-mode 1)

; Guide key settings
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(guide-key-mode 1)

; Ace jump settings
(require 'ace-jump-mode)

; Change sentence definition to one space after a period.
(setf sentence-end-double-space nil)

; Markdown settings
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

(add-hook 'text-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

(add-hook 'markdown-mode-hook (lambda ()
  (turn-on-auto-fill) (set-fill-column 80)))

; Transpose arguments
(define-key evil-normal-state-map "g>" 'transpose-words)

; Evil settings
(global-evil-leader-mode)

; Config file shortcut.
(setq conf-file "~/.emacs.d/init.el")
(defun open-conf()
  "Opens the emacs config file."
  (interactive)
  (find-file conf-file))

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
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Fix wrapped line movement.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

; Comment a region like tcomment.
(define-key evil-normal-state-map "gc" 'comment-dwim)

; Pandoc mode settings
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'auto-complete-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(evil-leader/set-key-for-mode 'markdown-mode
  "p" 'pandoc-convert-to-pdf
)

; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Map <ESC> to jk.
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Misc evil bindings.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

; Evil surround settings.
(global-evil-surround-mode 1)

; Evil jumper settings
(global-evil-jumper-mode)

; Evil ace-jump settings
(define-key evil-normal-state-map "s" 'ace-jump-mode)

; Flycheck settings
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

; Haskell settings
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(evil-leader/set-key-for-mode 'haskell-mode
  "i" 'run-haskell
  "x" 'inferior-haskell-load-and-run
)

; Python settings
(evil-leader/set-key-for-mode 'python-mode
  "r" 'python-shell-send-buffer
)

(add-hook 'python-mode-hook
  (lambda ()
    (setq tab-width 4)
    (setq python-indent 4)
    (setq evil-shift-width 4)))

(evil-mode 1)
