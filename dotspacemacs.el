;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     auto-completion
     better-defaults
     bibtex
     emacs-lisp
     git
     helm
     markdown
     org
     python
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     )
   dotspacemacs-additional-packages '(openwith)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((todos . 5)
                                (agenda . 5)
                                (recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 80
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 70
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-mode-line-theme 'spacemacs
   ))

(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config ()
  (ido-mode -1)

  (openwith-mode 1)
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

  (defmacro find-a-file-defun (defun-name file)
    `(defun ,defun-name () (interactive) (find-file-existing ,file)))

  (find-a-file-defun dl93/find-dot-spacemacs "~/repos/eccc-spacemacs-config/dotspacemacs.el")
  (find-a-file-defun dl93/find-notes "~/org/notes.org")

  ;; Global shortcuts.
  (evil-leader/set-key "fa"  'dl93/find-notes)
  (evil-leader/set-key "fed" 'dl93/find-dot-spacemacs)
  (evil-leader/set-key "w1"  'delete-other-windows)
  (evil-leader/set-key "ot"  'terminal-here)
  (evil-leader/set-key "ow"  'ace-window)

  ;; Ignore some directories in project search.
  (setq helm-ag-use-grep-ignore-list t)
  (setq helm-grep-ignored-directories '(".git/" "build/"))

  ;; C/C++
  (evil-leader/set-key-for-mode 'c++-mode "fb" 'clang-format-buffer)
  (evil-leader/set-key-for-mode 'c++-mode "fr" 'clang-format-region)

  ;; org-mode
  (setq org-agenda-files '("~/org/")
        org-catch-invisible-edits "show"
        org-default-notes-file "~/org/inbox.org"
        org-directory "~/org/"
        org-enforce-todo-dependencies t
        org-refile-targets '((org-agenda-files :maxlevel . 4))
        org-tags-match-list-sublevels 'indented
        org-todo-keywords '((sequence "BACKLOG" "TODO" "IN PROGRESS" "WAITING" "|" "DONE")
                            (sequence "QUESTION" "|" "ANSWER")
                            (sequence "TODISCUSS" "|" "DISCUSSED"))
        org-refile-targets '((nil :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4)))

  (add-hook 'org-mode-hook (lambda() (org-indent-mode -1)))

  ;; Bibtex
  (let ((bibliography-files '("~/TemporaryDocuments/bibliography.bib"))
        (paper-directory "~/drive2/papers")
        (notes-file "~/org/bibliography-notes.org"))
    (setq org-ref-default-bibliography bibliography-files
        org-ref-pdf-directory paper-directory
        org-ref-bibliography-notes notes-file
        bibtex-completion-bibliography bibliography-files
        bibtex-completion-library-path paper-directory
        bibtex-completion-notes-path notes-file))

  (evil-leader/set-key "ab" 'helm-bibtex)
  (evil-leader/set-key "fB" (find-a-file-defun
                             dl93/find-bibliography
                             "~/TemporaryDocuments/bibliography.bib")))
