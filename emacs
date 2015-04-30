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

; Evil settings
(global-evil-leader-mode)
(evil-mode 1)

(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-surround)
(global-evil-surround-mode 1)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
