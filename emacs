; TODO package.el

; Package repositories
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

; GUI settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Org mappings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(package-initialize)

; Color theme
(load-theme 'sanityinc-solarized-dark t)

; Scroll settings
(setq scroll-step 1)

; Projectile settings
(require 'projectile)
(projectile-global-mode)

; Evil settings
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode)

(evil-leader/set-key
  "gk" 'windmove-up
  "gj" 'windmove-down
  "gl" 'windmove-right
  "gh" 'windmove-left)
(evil-mode 1)

(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)

(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(require 'evil)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-surround)
(global-evil-surround-mode 1)
