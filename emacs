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
(defvar package-list '(
  ace-jump-mode
  ace-window
  auctex
  bison-mode
  company
  company-c-headers
  dash
  epl
  evil
  evil-jumper
  evil-leader
  evil-matchit
  evil-numbers
  evil-org
  evil-surround
  expand-region
  flycheck
  ggtags
  goto-chg
  guide-key
  haskell-mode
  helm
  helm-ag
  helm-gtags
  helm-projectile
  key-chord
  linum-off
  linum-relative
  magit
  markdown-mode
  multi-term
  pandoc-mode
  pkg-info
  popup
  projectile
  solarized-theme
  warm-night-theme
  undo-tree
  visual-fill-column
  writeroom-mode
  xcscope
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
(setq backup-inhibited t)
(setq auto-save-default nil)

; Color theme
(load-theme 'solarized-dark t)
(defvar solarized-scale-org-headlines nil)
; (load-theme 'warm-night t)

; Config file location.
(defvar conf-file "~/.emacs.d/init.el")

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

; Auto reload buffers when changed on disk.
(global-auto-revert-mode t)

; Sentence definition should be one space after a period.
(setf sentence-end-double-space nil)

; Session saving
(desktop-save-mode 1)

; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 2)
(define-key global-map (kbd "RET") 'newline-and-indent)

; Time in mode-line
(defvar display-time-format "%I:%M %p")
(display-time-mode 1)

; Wrap settings
(setq-default fill-column 80)


;; Utility functions ;;

; Quick configuration opening.
(defun open-conf()
  "Opens the emacs config file."
  (interactive)
  (find-file conf-file))

; Quick buffer switching.
(defun switch-to-last-buffer ()
  "Toggle between last buffer open."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

; Transparency enable.
(defun transparency-on ()
  "Set to 95% transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 85)))

; Transparency disable.
(defun transparency-off ()
  "Set to 100% transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

(defun read-lines (fp)
  "Read lines of file fp into a list"
  (with-temp-buffer
    (insert-file-contents fp)
    (split-string (buffer-string) "\n" t)))

(defun split-term ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))


;; Plugin-dependent Emacs behavior (Small plugins) ;;

; Ace jump settings
(require 'ace-jump-mode)

; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

; Magit (Git integration)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

; YASnippet (Tab-completed snippets)
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

; Guide key (Pops up help menu for prefix-keys)
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

; Relative line numbers.
(require 'linum-off)
(require 'linum-relative)
; (global-linum-mode t)


;; Org Mode (Life organizer) ;;

; Org files
(defvar org-agenda-files (list "~/org/school.org"))
(defvar org-default-notes-file "~/org/notes.org")

; Org mappings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(defvar org-log-done t)

(defun org-archive-done ()
  "Removes all DONE entries and places them into an archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

; Load babel languages.
(org-babel-do-load-languages
  'org-babel-load-languages
  '((C . t)
    (haskell . t)
    (python . t)
    (sh . t)))

; Correct fonts for code blocks.
(defvar org-src-fontify-natively t)

; Capture templates
(defvar org-capture-templates
      '(("d" "Dreams" entry
         (file+headline "~/org/dream.org" "Dreams")
         "*** %t\n")))

; Enable spell checking in org mode.
(add-hook 'org-mode-hook 'turn-on-flyspell)

; Open PDF links in apvlv.
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))


;; Company mode (Autocompletion)

(require 'company)
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.1.0/")

(setq-default company-idle-delay 0.2)
(setq-default company-echo-delay 0)
(add-hook 'prog-mode-hook (lambda () (company-mode 1)))

;; ERC (IRC client)

; Hide joins / parts / quits.
(defvar erc-hide-list '("JOIN" "PART" "QUIT"))

; Account login info
(let ((f (read-lines "~/.erc-login")))
  (defvar erc-nick (car f))
  (defvar erc-password (nth 1 f)))


;; Evil Mode (Vim emulation) ;;

; Remap escape to quit out of things.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'evil)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
; (require 'evil-org)
(require 'evil-surround)

; Use evil (mostly) everywhere.
(global-evil-leader-mode)

; Global evil leaders.
(evil-leader/set-leader ",")
(evil-leader/set-key
  "/" 'helm-projectile-ag
  "." 'search-word-under-cursor
  "b" 'switch-to-last-buffer
  "c" 'compile
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
  "t" 'split-term
  "w" 'ace-window
)

; Autoadd curly brackets.
(defun auto-add-curly ()
  (interactive)
  (insert "{")
  (newline-and-indent)
  (insert "}")
  (evil-shift-left-line 1)
  (evil-open-above 0))

; Curly bracket insertion
(define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)

; Evil window movement.
(define-key evil-normal-state-map "gh" 'windmove-left)
(define-key evil-normal-state-map "gj" 'windmove-down)
(define-key evil-normal-state-map "gk" 'windmove-up)
(define-key evil-normal-state-map "gl" 'windmove-right)

; Evil window scrolling.
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Use helm for man pages.
(define-key evil-normal-state-map "K" 'helm-man-woman)

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

; Evil company.
(setq evil-complete-next-func 'bw/company-complete-lambda)
(setq evil-complete-previous-func 'bw/company-complete-lambda)

; Evil jumper (C-o / C-i functionality)
(global-evil-jumper-mode)

; Evil surround settings.
(global-evil-surround-mode 1)

; Evil increment.
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)

; Evil scrolling.
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-up)

; Evil shift.
(setq-default evil-shift-width 2)

(evil-mode 1)


;; Projectile (Project management) ;;

(require 'projectile)
(projectile-global-mode)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

; Evil mappings for projectile.
(define-key evil-normal-state-map (kbd "<SPC>") 'helm-M-x)
(evil-leader/set-key-for-mode 'projectile-mode
  "/" 'projectile-helm-ag
  "sw" 'projectile-find-other-file
)

(defun search-word-under-cursor ()
  "Searches for the word under the cursor using projectile."
  (interactive)
  (er/expand-region 1)
  (projectile-helm-ag))


;; Helm (Incremental completion / Selection narrowing) ;;

(require 'helm-projectile)
(helm-projectile-on)

; Helm fuzzy-finding.
(defvar helm-M-x-fuzzy-match t)

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
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;; Flex / Bison ;;
(add-to-list 'auto-mode-alist '("\\.yy\\'" . bison-mode))


;; Flycheck (Syntax checking)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))


;; Haskell ;;

; Haskell indent mode.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

; Evil mappings for haskell.
(evil-leader/set-key-for-mode 'haskell-mode
  "xb" 'inferior-haskell-load-and-run
  "xd" 'inferior-haskell-send-decl
  "xi" 'run-haskell
)


;; Python ;;

; Evil mappings for python.
(evil-leader/set-key-for-mode 'python-mode
  "xb" 'python-shell-send-buffer
  "xi" 'python-shell-switch-to-shell
  "xr" 'python-shell-send-region
)

; Fix tab settings for python files.
(add-hook 'python-mode-hook
  (lambda ()
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (defvar python-indent 4)))


;; Terminal ;;

(add-to-list 'linum-disabled-modes-list 'term-mode)
(delete 'org-mode linum-disabled-modes-list)

(add-hook 'term-mode-hook (lambda()
        (yas-minor-mode -1)))


;; GNU Global ;;
(require 'ggtags)

; Enable gtags for c/c++.
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t)

(require 'helm-gtags)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Cscope (tag system) ;;
(require 'xcscope)
(cscope-setup)
(setq cscope-program "gtags-cscope")


;; Semantic (Source parsing) ;;
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)
