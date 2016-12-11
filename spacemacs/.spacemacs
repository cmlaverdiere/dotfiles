;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
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
                         phoenix-dark-mono
                         sanityinc-tomorrow-night
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
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
  (setq-default evil-want-C-i-jump t))

;; TODO
;; Scan through old init.el and find what should be transferred.
;; Learn how to configure modes for layers.
(defun dotspacemacs/user-config ()
  (setq-default
   backup-directory-alist '(("." . "~/.emacsbak"))
   c-basic-offset 4
   create-lockfiles nil
   custom-file "~/.emacs.d/private/custom.el"
   evil-ex-substitute-global t
   evil-extra-operator-eval-modes-alist '((python-mode python-shell-send-region))
   evil-lookup-func (lambda () (interactive) (man (current-word)))
   key-chord-two-keys-delay 0.1
   make-backup-files t
   neo-theme 'nerd
   powerline-default-separator 'slant
   tags-add-tables nil
   tags-revert-without-query 1
   vc-follow-symlinks t
   vc-make-backup-files t
   )

  ;; (spacemacs|use-package-add-hook org
  ;;   :post-config
  (defvar org-log-done t)

  (setq-default
   org-agenda-files '("~/org/tracking")
   org-agenda-repeating-timestamp-show-all nil
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-skip-scheduled-if-done t
   org-agenda-timegrid-use-ampm t
   org-confirm-babel-evaluate nil
   org-default-notes-file "~/org/notes.org"
   ;; org-enable-reveal-js-support t
   org-src-tab-acts-natively t
   )

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
  (define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)
  (define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-previous-line)
  (setq-default company-idle-delay nil
                company-minimum-prefix-length 0)
  ;; )

  ;; (spacemacs|use-package-add-hook ivy
  ;;   :post-config
  (defun ivy-insert-action (x)
    (with-ivy-window
      (insert x)))

  (ivy-set-actions
   t
   '(("i" ivy-insert-action "insert")
     ("y" ivy-insert-action "yank")))

  (setq-default ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "C-o") 'ivy-dispatching-done)
  ;; )

  ;; (spacemacs|use-package-add-hook racket
  ;; :post-config
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
  ;; )

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
    )

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
    (define-key keymap (kbd "C-h") #'evil-window-left)
    (define-key keymap (kbd "C-j") #'evil-window-down)
    (define-key keymap (kbd "C-k") #'evil-window-up)
    (define-key keymap (kbd "C-l") #'evil-window-right))

  (define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)
  (define-key evil-insert-state-map (kbd "C-y") 'hippie-expand)
  (define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)
  (define-key evil-visual-state-map (kbd ".")
    (lambda () (interactive) (execute-kbd-macro ":norm .")))

  ;; (define-key evil-normal-state-map "gs" 'evil-send-to-repl)
  (define-key evil-motion-state-map "gs" 'evil-operator-eval)

  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (add-hook 'text-mode-hook 'spacemacs/toggle-auto-fill-mode-on)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-auto-fill-mode-on)

  (google-this-mode)
  (spacemacs|diminish google-this-mode)

  (add-to-list 'auto-mode-alist '("\\.obj\\'" . wavefront-obj-mode))
  (add-to-list 'auto-mode-alist '("\\.mtl\\'" . wavefront-obj-mode))

  (load custom-file 'noerror)

  (key-chord-mode 1)
  (display-time-mode 1)
  )
