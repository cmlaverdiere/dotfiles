;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     csv
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     clojure
     (colors :variables colors-enable-nyan-cat-progress-bar nil)
     emacs-lisp
     finance
     git
     (haskell :variables
              haskell-completion-backend 'intero
              haskell-enable-hindent-style "fundamental")

     html
     ivy
     javascript
     latex
     markdown
     (org :variables org-enable-bootstrap-support t)
     (python :variables python-test-runner 'pytest)
     racket
     restclient
     rust
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables spell-checking-enable-by-default nil)
     shaders
     sql
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     themes-megapack
     version-control
     vinegar
     yaml
     )
   dotspacemacs-additional-packages '(
                                      ag
                                      evil-extra-operator
                                      fzf
                                      key-chord
                                      ob-ipython
                                      google-this
                                      wavefront-obj-mode
                                      )
   dotspacemacs-excluded-packages '(
                                    org-bullets
                                    evil-escape
                                    evil-org
                                    smartparens
                                    highlight-parentheses
                                    vi-tilde-fringe
                                    )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'random
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(
                         gruvbox
                         solarized-light
                         phoenix-dark-mono
                         sanityinc-tomorrow-night
                         spacemacs-dark
                         spacemacs-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '(("Roboto Mono"
                               :size 13
                               :weight medium
                               :width normal)

                               ("Source Code Pro"
                               :size 13
                               :weight medium
                               :width normal))
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-large-file-size 50
   dotspacemacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.3
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  (load-file "~/elisp/dash.el")
  (load-file "~/elisp/autothemer.el")
  (setq-default evil-want-C-i-jump t
                custom-file "~/.emacs.d/private/custom.el"))

(defun dotspacemacs/user-config ()
  (setq-default
   backup-directory-alist '(("." . "~/.emacsbak"))
   c-basic-offset 4
   compilation-scroll-output t
   create-lockfiles nil
   evil-ex-substitute-global t
   evil-extra-operator-eval-modes-alist '((python-mode python-shell-send-region))
   evil-lookup-func (lambda () (interactive) (man (current-word)))
   fill-column 79
   key-chord-two-keys-delay 0.1
   make-backup-files t
   neo-theme 'nerd
   powerline-default-separator 'slant
   tags-add-tables nil
   tags-revert-without-query 1
   vc-follow-symlinks t
   vc-make-backup-files t
   web-mode-enable-auto-pairing nil
   web-mode-enable-auto-closing nil
   web-mode-enable-auto-indentation nil
   web-mode-enable-auto-opening nil
   web-mode-enable-auto-quoting nil
   ;; hippie-expand-try-functions-list (delete 'yas-hippie-try-expand
   ;;                                          hippie-expand-try-functions-list)
   yas-snippet-dirs '("~/.emacs.d/private/snippets")
   )

  ;; TODO temp fix ipython warning
  ;; TODO move some out of python eval
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (with-eval-after-load 'python
    (dolist (key (list "C-k" "C-j" "C-l"))
      (define-key inferior-python-mode-map (kbd key) nil)
      (evil-define-key 'insert term-raw-map (kbd key) nil)
      ; (evil-define-key 'insert eshell-mode-map (kbd key) nil)
      (evil-define-key 'evilified dired-mode-map (kbd key) nil)
      (evil-define-key 'insert comint-mode-map (kbd key) nil)
      (evil-define-key 'normal comint-mode-map (kbd key) nil))
    (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython"))


  ;; (spacemacs|use-package-add-hook org
  ;;   :post-config
  (defvar org-log-done t)

  (setq-default
   org-agenda-files '("~/org/tracking")
   org-agenda-span 3
   org-agenda-repeating-timestamp-show-all nil
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-skip-scheduled-if-done t
   org-agenda-timegrid-use-ampm t
   org-confirm-babel-evaluate nil
   org-default-notes-file "~/org/notes.org"
   ;; org-enable-reveal-js-support t
   org-src-tab-acts-natively t
   org-agenda-prefix-format '((agenda  . " - ")))

  (evil-define-key 'normal org-mode-map (kbd "<left>") 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map (kbd "<right>") 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "<up>") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "<down>") 'org-metadown)

  (defvar org-capture-templates
    '(("d" "Dreams" entry
       (file+headline "~/org/dream.org" "Dreams")
       "*** %t\n")))

  (defun org-archive-done ()
    "Removes all DONE entries and places them into an archive file."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  ;; TODO remove this (and package) when merged into ipython-notebook layer
  ;; https://github.com/syl20bnr/spacemacs/pull/4914
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages '((ipython . t))))

  ;; Org leaders.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "D" 'org-archive-done
    "r" 'org-latex-export-to-pdf)
    ;; )

  ;; (spacemacs|use-package-add-hook company
  ;;   :post-config
  (spacemacs|disable-company eshell-mode)
  (define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)
  (define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-previous-line)
  ;; (setq-default company-idle-delay nil
  ;;               company-minimum-prefix-length 0)
  ;; )

  ;; (spacemacs|use-package-add-hook ivy
  ;;   :post-config
  (defun ivy-insert-action (x)
    (with-ivy-window
      (insert x)))

  (defun ivy-yank-action (x)
    (kill-new x))

  (ivy-set-actions
   t
   '(("i" ivy-insert-action "insert")
     ("y" ivy-yank-action "yank")))

  (setq-default ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-dispatching-done)
  ;; )

  ;; (spacemacs|use-package-add-hook racket
  ;; :post-config
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
  ;; )

  (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

  ;; Autoadd curly brackets.
  (defun auto-add-curly ()
    (interactive)
    (insert "{")
    (newline-and-indent)
    (insert "}")
    (evil-shift-left-line 1)
    (evil-open-above 0))

  (defun repl-activate ()
    (interactive)
    (rename-buffer "*custom-repl*"))

  (defun send-region-repl (beg end)
    (let ((text (buffer-substring beg end)))
      (if (not (get-buffer "*custom-repl*"))
          (message "No repl buffer active!")
        (comint-send-string "*custom-repl*" (concat text "\n")))))

  ;; TODO make custom repl-mode, register with evil-send.
  (evil-define-operator evil-send-to-repl (beg end)
    "Send text to a comint repl process."
    :move-point nil
    :repeat t
    (send-region-repl beg end))

  (defun open-urxvt-cwd ()
    (interactive)
    (call-process-shell-command
     (format "urxvt -cd %s" default-directory) nil 0))

  (spacemacs/set-leader-keys
    "Q" (lookup-key spacemacs-default-map "q")
    "V" 'split-window-below
    "br" 'rename-buffer
    "cc" (lambda () (interactive) (helm-make-projectile 9))
    "fl" (lambda () (interactive) (fzf-directory "~/"))
    "gg" 'google-this-mode-submap
    "q" 'evil-quit
    "ra" 'repl-activate
    "sap" 'projectile-ag
    "v" 'split-window-right
    "RET" 'open-urxvt-cwd
    )

  (define-key evil-outer-text-objects-map "e" 'evil-inner-buffer)
  (define-key evil-inner-text-objects-map "e" 'evil-inner-buffer)

  (define-key global-map (kbd "C-h") 'windmove-left)
  (define-key global-map (kbd "C-j") 'windmove-down)
  (define-key global-map (kbd "C-k") 'windmove-up)
  (define-key global-map (kbd "C-l") 'windmove-right)

  ;; TODO fix in ipython / other comint repls (also org).
  (dolist (keymap (list
                   ;; evil-insert-state-map
                   evil-motion-state-map
                   evil-normal-state-map
                   evil-evilified-state-map-original
                   evil-evilified-state-map))
    (define-key keymap (kbd "<down>") 'enlarge-window)
    (define-key keymap (kbd "<up>") 'shrink-window)
    (define-key keymap (kbd "<right>") 'enlarge-window-horizontally)
    (define-key keymap (kbd "<left>") 'shrink-window-horizontally)

    (define-key keymap (kbd "C-h") #'evil-window-left)
    (define-key keymap (kbd "C-j") #'evil-window-down)
    (define-key keymap (kbd "C-k") #'evil-window-up)
    (define-key keymap (kbd "C-l") #'evil-window-right))

  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)
  (define-key evil-insert-state-map (kbd "C-y") 'hippie-expand)
  (define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)
  (define-key evil-visual-state-map (kbd ".")
    (lambda () (interactive) (execute-kbd-macro ":norm .")))

  ;; (define-key evil-normal-state-map "gs" 'evil-send-to-repl)
  (define-key evil-motion-state-map "gs" 'evil-operator-eval)

  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-centered-point-globally-on)

  (dolist (mode (list 'text-mode-hook 'prog-mode-hook))
    (add-hook mode 'spacemacs/toggle-auto-fill-mode-on))

  (google-this-mode)
  (spacemacs|diminish google-this-mode)

  (add-to-list 'auto-mode-alist '("\\.obj\\'" . wavefront-obj-mode))
  (add-to-list 'auto-mode-alist '("\\.mtl\\'" . wavefront-obj-mode))

  (key-chord-mode 1)
  (display-time-mode 1)

  (load custom-file 'noerror)
  )
