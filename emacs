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
  helm-projectile
  key-chord
  magit
  markdown-mode
  pkg-info
  popup
  projectile
  relative-line-numbers
  solarized-theme
  undo-tree
  visual-fill-column
  writeroom-mode
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

; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

; GUI settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
  
; Magit settings
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

; Org mappings
(setq org-agenda-files (list "~/org/school.org"))
(setq org-default-notes-file "~/org/notes.org")
          
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

; Color theme
(load-theme 'solarized-dark t)
  
; Scroll settings
(setq scroll-step 1)

(setq evil-want-C-u-scroll t)

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

; Autocomplete settings
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)

; Escape remap
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; Guide key settings
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
(guide-key-mode 1)

; Ace jump settings
(require 'ace-jump-mode)

; Markdown settings
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

; Evil settings
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "c" 'org-capture
  "e" 'eval-last-sexp
  "g" 'magit-status
  "l" 'flycheck-list-errors
  "f" 'helm-for-files
  "j" 'ace-jump-line-mode
  "p" 'projectile-switch-project
  "q" 'kill-buffer
)

; Evil window movement.
(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Map <ESC> to jk.
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Misc evil bindings.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

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

(evil-mode 1)
