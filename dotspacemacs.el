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
     c-c++
     csv
     emacs-lisp
     git
     helm
     html
     (imenu-list :variables
                 imenu-list-position 'left)
     javascript
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
   dotspacemacs-additional-packages '(dracula-theme
                                      forest-blue-theme
                                      gotham-theme
                                      howm
                                      mediawiki
                                      monokai-theme
                                      northcode-theme
                                      openwith
                                      org-super-agenda
                                      zenburn-theme
                                      cyberpunk-theme)
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
   dotspacemacs-verbose-loading t
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
   dotspacemacs-themes (if (boundp 'dl93/default-themes)
                           dl93/default-themes
                         '(monokai
                           gotham
                           spacemacs-dark
                           spacemacs-light))
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font (if (boundp 'dl93/font)
                                 dl93/font
                               '("Hack"
                                 :size 15
                                 :weight normal
                                 :width normal
                                 :powerline-scale 1.1))
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
   dotspacemacs-mode-line-unicode-symbols nil
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
   dotspacemacs-line-numbers '(:relative nil :disabled-for-modes org-mode markdown-mode)
   dotspacemacs-folding-method 'origami
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

(defun dotspacemacs/user-init ()
  ;; On the EC network your TMPDIR gets assigned to the process id... but it is not automatically created for emacs.
  (when (boundp 'dl93/tmpdir)
    (setenv "TMPDIR" dl93/tmpdir))

  (let ((tmpdir (getenv "TMPDIR")))
    (when tmpdir
     (when (not (file-directory-p tmpdir))
       (make-directory tmpdir))))

  (setq custom-file "~/.spacemacs.custom"))



(defun dotspacemacs/user-config ()
  ;; Git-gitter workaround with tramp.
  ;; See https://github.com/nonsequitur/git-gutter-plus/issues/42
  (with-eval-after-load 'git-gutter+
    (defun git-gutter+-remote-default-directory (dir file)
      (let* ((vec (tramp-dissect-file-name file))
             (method (tramp-file-name-method vec))
             (user (tramp-file-name-user vec))
             (domain (tramp-file-name-domain vec))
             (host (tramp-file-name-host vec))
             (port (tramp-file-name-port vec)))
        (tramp-make-tramp-file-name method user domain host port dir)))

    (defun git-gutter+-remote-file-path (dir file)
      (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
        (replace-regexp-in-string (concat "\\`" dir) "" file))))

  (defun dl93/find-project-venv ()
    (let ((potential-venvs '("venv" "env"))
          (picked-venv))
      (dolist (potential-venv (reverse potential-venvs) picked-venv)
        (let ((venv (concat (file-name-as-directory (projectile-project-root)) potential-venv)))
          (when (file-directory-p venv)
            (setq picked-venv venv))))))

  ;; Convenience function to load virtual env from projects more easliy.
  (defun dl93/projectile-activate-venv ()
    (interactive)
    (let ((venv-location (dl93/find-project-venv)))
      (message "Loading virtual environment: %s" venv-location)
      (pyvenv-activate venv-location)))

  (evil-leader/set-key-for-mode 'python-mode "vv" 'dl93/projectile-activate-venv)


  (ido-mode -1)

  (defun proced-settings ()
    (proced-toggle-auto-update t))
  (add-hook 'proced-mode-hook 'proced-settings)

  ;; TRAMP
  (setq tramp-debug-on-error t)
  (eval-after-load 'tramp (lambda () (add-to-list 'tramp-remote-path "/home/dav000/local/bin")))

  (openwith-mode 1)
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

  (defmacro find-a-file-defun (defun-name file)
    `(defun ,defun-name () (interactive) (find-file-existing ,file)))

  (find-a-file-defun dl93/find-dot-spacemacs "~/repos/spacemacs-config/dotspacemacs.el")

  ;; Global shortcuts.
  (evil-leader/set-key "fa" 'dl93/find-notes)
  (evil-leader/set-key "fed" 'dl93/find-dot-spacemacs)
  (evil-leader/set-key "ot" 'terminal-here)
  (evil-leader/set-key "ow" 'ace-window)
  (evil-leader/set-key "pO" 'org-projectile/goto-todos)
  (evil-leader/set-key "po" 'projectile-find-file-other-window)
  (evil-leader/set-key "w1" 'delete-other-windows)

  ;; Code folding shortcuts using hideshow.
  (define-key evil-normal-state-map "za" 'hs-toggle-hiding)
  (define-key evil-normal-state-map "zc" 'hs-hide-block)
  (define-key evil-normal-state-map "zC" 'hs-hide-all)
  (define-key evil-normal-state-map "zx" 'hs-show-block)
  (define-key evil-normal-state-map "zX" 'hs-show-all)
  (define-key evil-normal-state-map "zm" 'hs-hide-level)


  ;; Ignore some directories in project search.
  (setq helm-ag-use-grep-ignore-list t)
  (setq helm-ag-command-option "--noaffinity") ;; Affinity causes problems on PPP1 with AG.
  (setq helm-grep-ignored-directories '(".git/" "build/"))

  ;; C/C++
  (evil-leader/set-key-for-mode 'c++-mode "fb" 'clang-format-buffer)
  (evil-leader/set-key-for-mode 'c++-mode "fr" 'clang-format-region)

  ;; org-mode
  (setq org-agenda-files '("~/gtd/")
        org-agenda-file-regexp "\\`[^.].*\\.org\\'"
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-todo-ignore-timestamp 'future
        org-agenda-tags-todo-honor-ignore-options t
        org-archive-location "~/gtd/archive.org::datetree/"
        org-catch-invisible-edits "show"
        org-default-notes-file "~/gtd/inbox.org"
        org-directory "~/gtd/"
        org-enforce-todo-dependencies t
        org-icalendar-use-scheduled '(event-if-todo)
        org-refile-targets '((org-agenda-files :maxlevel . 4))
        org-tags-match-list-sublevels 'indented
        org-todo-keywords '((sequence "BACKLOG" "TODO" "NEXT" "WAITING" "|" "DONE" "CANCELLED"))
        org-refile-targets '((nil :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4)))
  ;; (evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)


  (setq org-stuck-projects '("+projects-reference-TODO=\"DONE\"-TODO=\"BACKLOG\"-TODO=\"CANCELLED\"-TODO=\"TODO\"" ("NEXT" "WAITING") nil ""))


  (add-hook 'org-mode-hook (lambda() (org-indent-mode -1) (org-super-agenda-mode -1)))

  (setq org-agenda-custom-commands
        '(("x" "Next @home" todo "NEXT"
           ((org-agenda-tag-filter-preset '("+@home"))
            (org-super-agenda-groups
             '((:auto-parent t)))))
          ("d" "Done @work" agenda ""
           ((org-agenda-start-with-log-mode '(closed))
            (org-agenda-start-day "-7d")
            (org-agenda-tag-filter-preset '("+@work"))
            (org-agenda-span 14)))
          ("z" "Next @work" todo "NEXT"
           ((org-agenda-tag-filter-preset '("+@work"))
            (org-super-agenda-groups
             '((:auto-parent t)))))))

  (defun dl93/org-todo-done ()
    (interactive)
    (org-todo "DONE"))
  (evil-leader/set-key-for-mode 'org-mode "D" 'dl93/org-todo-done)



  ;; Bibtex
  (let ((bibliography-files '("~/papers/database.bib"))
        (paper-directory "~/papers/")
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
                             "~/TemporaryDocuments/bibliography.bib"))


  ;; Howm
  (setq howm-directory "~/howm/"
        howm-file-name-format "%Y/%U/%Y-%m-%d-%H%M%S.txt"))
