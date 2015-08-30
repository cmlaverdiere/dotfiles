;;; Chris Laverdiere's Emacs config ;;;

; TODO
;  - Write a fn to load all included header files into buffers.
;  - helm complete at point tab colon search

; FIXME
;  - company eshell
;  - lisp indent comment
;  - company irony c headers


;; Package management ;;

; Package repositories
(require 'package)
(setq package-archives '(
  ("melpa" . "http://melpa.milkbox.net/packages/")
  ("gnu" . "http://elpa.gnu.org/packages/")
  ("elpa" . "http://tromey.com/elpa/")
))


; Package list
(defvar package-list '(
  ace-jump-mode
  ace-window
  auctex
  bison-mode
  company
  company-c-headers
  company-ghc
  company-ghci
  company-irony
  company-jedi
  company-math
  company-quickhelp
  dash
  epl
  eshell-autojump
  evil
  evil-args
  evil-escape
  evil-jumper
  evil-leader
  evil-matchit
  evil-numbers
  evil-org
  evil-surround
  exec-path-from-shell
  expand-region
  ; fixme-mode
  flycheck
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
  linum-off
  linum-relative
  magit
  markdown-mode
  multi-term
  pandoc-mode
  pkg-info
  popup
  projectile
  rainbow-delimiters
  solarized-theme
  tao-theme
  undo-tree
  visual-fill-column
  warm-night-theme
  web-mode
  writeroom-mode
  xcscope
  yasnippet
))

(package-initialize)

; Fetch list of packages available.
(setq package-archive-contents nil)
(unless package-archive-contents
  (package-refresh-contents))

; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Vanilla Emacs Behavior ;;

; Backup settings
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)

; Color theme
(load-theme 'solarized-dark t)
; (load-theme 'solarized-light t)
; (load-theme 'warm-night t)
; (load-theme 'tao-yin t)

(defvar solarized-scale-org-headlines nil)

; Config file location.
(defvar conf-file "~/.emacs.d/init.el")

; Emacs source location.
(setq source-directory (format "/usr/local/src/emacs-%d.%d/src"
  emacs-major-version emacs-minor-version))

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

; Highlight current line
(global-hl-line-mode)

; Highlight all search matches line
(highlight-symbol-mode)

; History settings
(savehist-mode 1)

; Auto reload buffers when changed on disk.
(global-auto-revert-mode t)

; Sentence definition should be one space after a period.
(setf sentence-end-double-space nil)

; Shell settings
(setenv "SHELL" "/usr/bin/zsh")

; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 2)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "<C-return>") 'indent-new-comment-line)

; Find mappings
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

; Find at point mappings
(define-prefix-command 'help-at-point-map)
(global-set-key (kbd "C-h C-p") 'help-at-point-map)
(define-key 'help-at-point-map (kbd "f") 'find-function-at-point)
(define-key 'help-at-point-map (kbd "v") 'find-variable-at-point)


; Time in mode-line
(defvar display-time-format "%I:%M %p")
(display-time-mode 1)

(defun ins-date ()
  "Insert date into current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%m:%S")))

; Wrap settings
(setq-default fill-column 80)


;; Utility functions ;;

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

; Transparency disable.
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

(defun split-term () (interactive) (do-in-split 'multi-term))


;; Ace jump ;;

(require 'ace-window)
(require 'ace-jump-mode)
(ace-window-display-mode)
(setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))


;; C/C++ ;;

(require 'company)
(add-hook 'cc-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-c-headers)
  ; (flycheck-select-checker 'c/c++-gcc)
  ))


;; Comint ;;

(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)


;; Company mode (Autocompletion)

(require 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.1.0/")

(company-quickhelp-mode 1)

; Rebind moving down company suggestion list.
(define-key company-active-map (kbd "M-n") 'nil)
(define-key company-active-map (kbd "M-p") 'nil)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

(setq-default company-idle-delay 0.0)
(setq-default company-echo-delay 0)

(defun enable-company ()
  (company-mode 1))

; Let yas play nicely with company completion.
(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

(add-hook 'company-mode-hook (lambda ()
  (substitute-key-definition 'company-complete-common
                             'company-yasnippet-or-completion
                              company-active-map)))


;; Compilation mode ;;
(setq-default compilation-scroll-output 'first-error)


;; Cscope (Tag system) ;;
(defvar cscope-program "gtags-cscope")
(require 'xcscope)


;; Doc-view ;;

(require 'doc-view)
(setf doc-view-continuous t)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-page)
(define-key doc-view-mode-map (kbd "g") nil)
(define-key doc-view-mode-map (kbd "h") nil)
(key-chord-define doc-view-mode-map "gh" 'windmove-left)
(key-chord-define doc-view-mode-map "gj" 'windmove-down)
(key-chord-define doc-view-mode-map "gk" 'windmove-up)
(key-chord-define doc-view-mode-map "gl" 'windmove-right)

;; Eshell ;;

(require 'eshell)
(require 'eshell-autojump)

(setq-default eshell-save-history-on-exit t)

(require 'em-term)
(add-to-list 'eshell-visual-commands "sl")
(add-to-list 'eshell-visual-commands "git")


; Set path to shell path.
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

; On multi-line evil jump, add to the jump list.
(defadvice evil-next-visual-line
    (before evil-next-visual-line-before activate)
    (unless (eq (ad-get-arg 0) nil)
        (evil-jumper--set-jump)))

(defadvice evil-previous-visual-line
    (before evil-previous-visual-line-before activate)
    (unless (eq (ad-get-arg 0) nil)
        (evil-jumper--set-jump)))

; On ace jump, add to the jump list.
(defadvice ace-jump-mode
    (before ace-jump-mode-before activate) (evil-jumper--set-jump))

(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)

(require 'evil-args)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

; TODO remove this and just take from it the keybindings we really want.
(require 'evil-org)

; Some modes aren't for text editing and thus don't need the full range of evil
; bindings. We still want movement to work smoothly across all modes though, so
; these are the base movement bindings.
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

; Mode specific evil init modes.
(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

(evil-set-initial-state 'occur-mode 'normal)
(evil-define-key 'normal occur-mode-map (kbd "RET")
  'occur-mode-goto-occurrence)

; Mode specific evil mappings.
(evil-define-key 'normal eshell-mode-map (kbd "RET")
  'eshell-send-input)

; The evil-org plugin overrides our window movement keys, which we don't want.
(evil-leader/set-key-for-mode 'org-mode "a" 'ace-window)
(evil-define-key 'normal evil-org-mode-map
  "gh" nil "gj" nil "gk" nil "gl" nil)

(evil-define-key 'normal evil-org-mode-map "O" nil)
(evil-leader/set-key-for-mode 'org-mode
 "T" (lambda () (interactive) (org-table-sort-lines nil ?a))
)

(require 'evil-surround)

; Use evil (mostly) everywhere.
(global-evil-leader-mode)

; Global evil leaders.
; TODO move these into their own modes.
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "." 'search-word-under-cursor
  "/" 'helm-projectile-ag
  "a" 'ace-window
  "A" 'align
  "b" 'switch-to-last-buffer
  "c" 'compile
  "C" 'quick-compile-and-run
  "d" 'dired
  "e" 'eval-last-sexp
  "g" 'magit-status
  "G" 'google-this
  "i" 'open-conf
  "l" 'flycheck-list-errors
  "f" 'helm-for-files
  "j" 'winner-undo
  "k" 'winner-redo
  "n" 'flycheck-next-error
  "N" 'flycheck-previous-error
  "o" 'occur
  "O" 'projectile-find-other-file
  "p" 'helm-projectile-switch-project
  "P" 'prev-window
  "r" 'projectile-run-async-shell-command-in-root
  "R" 'revert-buffer
  "q" 'evil-quit
  "Q" 'kill-buffer
  "s" 'split-eshell
  "S" 'eshell-new
  "t" 'split-term
  "u" 'undo-tree-visualize
  "v" 'evil-window-vsplit
  "V" 'evil-window-split
  "w" 'save-buffer
  "W" 'delete-other-windows
  "z" 'open-scratch
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

; Make * and # search for symbols rather than words.
(setq-default evil-symbol-word-search t)

; Use global regexes by default.
(setq-default evil-ex-substitute-global t)

; Use evil's search instead of isearch.
; (setq-default evil-search-module 'evil-search)

; Line completion
(define-key evil-insert-state-map (kbd "<backtab>") 'evil-complete-next-line)

; Insert line on enter
(define-key evil-normal-state-map (kbd "RET")
  (lambda (x)
    (interactive "p")
    (save-excursion
      (evil-open-below x)
      (evil-normal-state))))

; Visual line information
(define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)

; Visual repeat command
(define-key evil-visual-state-map (kbd ".")
  (lambda () (interactive) (execute-kbd-macro ":norm .")))

; Evil window scrolling.
(define-key evil-normal-state-map (kbd "C-S-d") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-u") 'scroll-other-window-down)

; Use helm for man pages.
; (define-key evil-normal-state-map "K" 'helm-man-woman)

; Use man for man pages instead of woman.
(setq evil-lookup-func (lambda ()
  (interactive)
  (man (current-word))))

; Transpose arguments
(define-key evil-normal-state-map "g>" 'transpose-words)

; Fix wrapped line movement.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

; Comment a region like tcomment from vim.
(define-key evil-visual-state-map "gc" 'comment-dwim)

; Quick buffer closing from insert mode.
(define-key evil-insert-state-map (kbd "C-q") 'evil-quit)

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

; Use gtags instead of etags for tag lookup.
(define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-dwim)

; Eshell history
(define-key evil-insert-state-map (kbd "C-l") 'helm-eshell-history)

; Evil shift.
(setq-default evil-shift-width 2)

(evil-mode 1)


;; Flex / Bison ;;
(add-to-list 'auto-mode-alist '("\\.yy\\'" . bison-mode))


;; Flycheck (Syntax checking)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))


;; Expand region ;;
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; GNU Global ;;

(require 'ggtags)

; Enable gtags for c/c++.
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (cscope-setup)
              (cscope-minor-mode)
              ; (ggtags-mode 1)
          )))

(defvar helm-gtags-ignore-case t)
(defvar helm-gtags-auto-update t)
(defvar helm-gtags-use-input-at-cursor t)

(require 'helm-gtags)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)


;; Golden ratio (auto window resizing) ;;
(require 'golden-ratio)
(setq golden-ratio-auto-scale)
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-exclude-buffer-names " *guide-key*") ; FIXME
(setq-default window-combination-resize t)
(golden-ratio-mode 1)


;; Google this ;;
(google-this-mode 1)


;; Guide key (Prefix-keys menu) ;;
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/idle-delay 0.4)
(guide-key-mode 1)


;; Haskell ;;

; TODO setup hasktags
; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

(add-hook 'haskell-mode-hook (lambda ()
  ; (add-to-list 'company-backends 'company-ghc)
  (turn-on-haskell-indent)))

(add-hook 'interactive-haskell-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-ghci)))

(defun haskell-run-other-window ()
  (interactive)
  (let ((current-win (selected-window)))
    (inferior-haskell-load-and-run "main")
    (select-window current-win)
    (golden-ratio)))

; Evil mappings for haskell.
(evil-leader/set-key-for-mode 'haskell-mode
  "xb" 'haskell-run-other-window
  "xd" 'inferior-haskell-send-decl
  "xi" 'run-haskell
)


;; Helm (Incremental completion / Selection narrowing) ;;

(require 'helm-projectile)
(require 'grep)
(helm-projectile-on)

; Helm fuzzy-finding.
(defvar helm-M-x-fuzzy-match t)

; Consistent movement with company.
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

; Use the silver searcher ag with Helm.
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))


;; Irony (clang completion) ;;

(require 'irony)

; Use irony for C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'cc-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

; Replace completion functions.

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

; Add irony backend for company.
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

; Extra completions.
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;; LaTeX ;;
(require 'tex)
(add-hook 'LaTeX-mode-hook (lambda ()
  (enable-company)
  (add-to-list 'company-backends 'company-math)))


;; Lisp ;;
(add-hook 'emacs-lisp-mode-hook (lambda ()
  (prettify-symbols-mode 1)))


;; Magit (Git integration) ;;
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)


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
  "P" 'pandoc-convert-to-pdf
)


;; Octave / Matlab ;;
(add-to-list 'auto-mode-alist
    '("\\.m$" . octave-mode))


; Evil mappings for python.
(evil-leader/set-key-for-mode 'octave-mode
  "xb" 'octave-send-buffer
  "xi" 'run-octave
  "xr" 'octave-send-region
)

;; Org Mode ;;

(require 'org)

; Org files
(setq org-agenda-files '("~/org"))
(setq org-default-notes-file "~/org/notes.org")

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

; Do not prompt for babel code execution
(setq org-confirm-babel-evaluate nil)

; Correct fonts for code blocks.
(setq-default org-src-fontify-natively t)

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

(evil-leader/set-key-for-mode 'org-mode
  "P" 'org-latex-export-to-pdf
)

;; Pandoc ;;

(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)


;; Prog mode ;;

(add-hook 'prog-mode-hook (lambda ()
  (enable-company)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (rainbow-delimiters-mode)))

;; Projectile (Project management) ;;

(require 'projectile)
(projectile-global-mode)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

; Evil mappings for projectile.
(define-key evil-normal-state-map (kbd "C-<SPC>") 'helm-M-x)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'helm-M-x)

; Use project root as cscope database.
(defadvice helm-projectile-switch-project
    (after helm-projectile-switch-project-after activate)
    (cscope-set-initial-directory (projectile-project-root)))

(defun search-word-under-cursor ()
  "Searches for the word under the cursor using projectile."
  (interactive)
  (er/expand-region 1)
  (projectile-helm-ag))


;; Python ;;

; Evil mappings for python.
(evil-leader/set-key-for-mode 'python-mode
  "xb" 'python-shell-send-buffer
  "xi" 'python-shell-switch-to-shell
  "xr" 'python-shell-send-region
)

; Custom Python mode hook.
(add-hook 'python-mode-hook
  (lambda ()
    (add-to-list 'company-backends 'company-jedi)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (defvar python-indent 4)))


;; Relative line numbers. ;;
(require 'linum-off)
(require 'linum-relative)
; (global-linum-mode t)


;; Terminal ;;

(add-to-list 'linum-disabled-modes-list 'term-mode)
(delete 'org-mode linum-disabled-modes-list)

(add-hook 'term-mode-hook (lambda ()
        (yas-minor-mode -1)))


;; Web ;;

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (yas-activate-extra-mode 'html-mode)
)

(add-hook 'web-mode-hook 'my-web-mode-hook)


;; Winner (Window management) ;;

(winner-mode 1)


;; YASnippet (Tab-completed snippets) ;;
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
(setq yas-triggers-in-field t)
