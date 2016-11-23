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
     (colors :variables colors-enable-nyan-cat-progress-bar t)
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
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight medium
                               :width normal)
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
  (setq-default vc-follow-symlinks t)

  (setq powerline-default-separator 'slant)

  (defvar org-log-done t)

  ;; Do not prompt for babel code execution
  (setq-default org-confirm-babel-evaluate nil)

  ;; Disable deadline warning in agenda
  (setq-default org-agenda-skip-deadline-prewarning-if-scheduled t)

  ;; Don't show DONE tasks in the agenda view.
  (setq-default org-agenda-skip-scheduled-if-done t)

  ;; Use am/pm in agenda view.
  (setq-default org-agenda-timegrid-use-ampm t)

  ;; Only show repeating events daily.
  (setq-default org-agenda-repeating-timestamp-show-all nil)

  ;; Properly indent src blocks.
  (setq-default org-src-tab-acts-natively t)

  ;; (setq org-enable-reveal-js-support t)

  (setq org-agenda-files '("~/org/tracking"))
  (setq org-default-notes-file "~/org/notes.org")

  (evil-define-key 'normal org-mode-map (kbd "<left>") 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map (kbd "<right>") 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map (kbd "<up>") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "<down>") 'org-metadown)

  (setq-default neo-theme 'nerd)

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

  (define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-previous-line)
  (define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)
  (setq-default company-idle-delay nil)

  ;; Visual line information
  (define-key evil-visual-state-map (kbd "g C-g") 'count-words-region)

  (define-key evil-insert-state-map (kbd "C-y") 'hippie-expand)

  ;; Autoadd curly brackets.
  (defun auto-add-curly ()
    (interactive)
    (insert "{")
    (newline-and-indent)
    (insert "}")
    (evil-shift-left-line 1)
    (evil-open-above 0))

  (define-key evil-insert-state-map (kbd "C-]") 'auto-add-curly)

  (spacemacs/set-leader-keys "sap" 'projectile-ag)

  ;; Visual repeat command
  (define-key evil-visual-state-map (kbd ".")
    (lambda () (interactive) (execute-kbd-macro ":norm .")))

  ;; Capture templates
  (defvar org-capture-templates
    '(("d" "Dreams" entry
       (file+headline "~/org/dream.org" "Dreams")
       "*** %t\n")))

  (defun org-archive-done ()
    "Removes all DONE entries and places them into an archive file."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  ;; Org leaders.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "D" 'org-archive-done
    "r" 'org-latex-export-to-pdf)

  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1)

  (setq create-lockfiles nil)
  (setq-default evil-ex-substitute-global t)

  (setq-default c-basic-offset 4)

  ;; Use man for man pages instead of woman.
  (setq evil-lookup-func
        (lambda ()
          (interactive)
          (man (current-word))))

  ;; Add time to the mode-line.
  (display-time-mode 1)

  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))

  (spacemacs/toggle-auto-fill-mode-on)

  (google-this-mode)
  (spacemacs/set-leader-keys "gg" 'google-this-mode-submap)
  (spacemacs|diminish google-this-mode)
  (spacemacs|diminish ggtags-mode)

  (spacemacs/set-leader-keys "br" 'rename-buffer)
  (spacemacs/set-leader-keys "ra" 'repl-activate)

  (defun repl-activate ()
    (interactive)
    (rename-buffer "*custom-repl*"))

  (spacemacs/set-leader-keys "fl"
    (lambda () (interactive) (fzf-directory "~/")))

  (spacemacs/set-leader-keys "cc"
    (lambda () (interactive) (helm-make-projectile 9)))

  (add-to-list 'auto-mode-alist '("\\.obj\\'" . wavefront-obj-mode))
  (add-to-list 'auto-mode-alist '("\\.mtl\\'" . wavefront-obj-mode))

  ;; TODO remove this (and package) when merged into ipython-notebook layer
  ;; https://github.com/syl20bnr/spacemacs/pull/4914
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages '((ipython . t))))

  ;; TODO add check for this.
  (spacemacs/set-leader-keys "Q" (lookup-key spacemacs-default-map "q"))

  (spacemacs/set-leader-keys "q" 'evil-quit)
  (spacemacs/set-leader-keys "v" 'split-window-right)
  (spacemacs/set-leader-keys "V" 'split-window-below)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (wavefront-obj-mode rainbow-mode rainbow-identifiers color-identifiers-mode ag helm-gtags ggtags glsl-mode org-bullets evil-escape yaml-mode restclient ob-http ledger-mode flycheck-ledger fzf ein websocket ox-twbs pcache ob-ipython web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data google-this web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode flyspell-popup company-quickhelp powerline alert log4e gntp markdown-mode parent-mode projectile request gitignore-mode fringe-helper git-gutter+ git-gutter pos-tip flx iedit evil goto-chg diminish ghc company hydra highlight seq spinner pkg-info epl rust-mode bind-map bind-key packed pythonic f s avy auto-complete popup package-build hide-comnt auctex yasnippet dash clojure-snippets clj-refactor inflections edn multiple-cursors paredit peg cider-eval-sexp-fu cider queue clojure-mode racket-mode faceup stickyfunc-enhance srefactor org anaconda-mode anzu smartparens undo-tree flyspell-correct helm helm-core haskell-mode flycheck magit magit-popup git-commit with-editor async ox-reveal key-chord zonokai-theme zenburn-theme zen-and-art-theme yapfify xterm-color ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toml-mode toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme restart-emacs rainbow-delimiters railscasts-theme racer quelpa pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox orgit organic-green-theme org-projectile org-present org-pomodoro org-plus-contrib org-download open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum live-py-mode linum-relative link-hint light-soap-theme jbeans-theme jazz-theme ir-black-theme intero inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md geiser gandalf-theme flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump dracula-theme django-theme disaster diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-statistics company-ghci company-ghc company-cabal company-c-headers company-auctex company-anaconda column-enforce-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmm-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme cargo busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-std=c++11" "-I./lib" "-L./lib")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
