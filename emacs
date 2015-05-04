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
  color-theme-sanityinc-solarized
  dash
  epl
  evil
  evil-jumper
  evil-leader
  evil-matchit
  evil-numbers
  evil-surround
  flycheck
  goto-chg
  haskell-mode
  helm
  helm-projectile
  key-chord
  pkg-info
  popup
  projectile
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

; GUI settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Org mappings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

; Color theme
(load-theme 'sanityinc-solarized-dark t)
  
; Scroll settings
(setq scroll-step 1)

; Projectile settings
(require 'projectile)
(projectile-global-mode)

; Autocomplete settings
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)

; Ace jump settings
(require 'ace-jump-mode)

; Evil settings
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'helm-buffers-list
  "c" 'compile
  "e" 'eval-last-sexp
  "f" 'flycheck-list-errors
  "j" 'ace-jump-line-mode
  "p" 'projectile-switch-project
)

; Evil window movement.
(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Evil projectile bindings.
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

; Map <ESC> to jk.
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Misc evil bindings.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

; Evil surround settings.
(global-evil-surround-mode 1)

; Evil jumper settings
(evil-jumper-mode)
  
; Flycheck settings
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

; Haskell settings
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(evil-mode 1)

(require 'evil)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-surround)
