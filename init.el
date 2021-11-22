(require 'package)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.org/packages/")))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(defvar my-packages '(
          ;; ac-js2
          ag
          all-the-icons
          ;; auto-complete
          cider
          clj-refactor
          clojure-mode
          csv-mode
          centaur-tabs
          dumb-jump
          dashboard
          doom-modeline
          doom-themes
          tern
          tern-auto-complete
          emmet-mode
          evil
          evil-escape
          evil-leader
          evil-mc
          evil-numbers
          evil-surround
          exec-path-from-shell
          flycheck
          flycheck-flow
          flyspell
          forge
          highlight-indentation
          idle-highlight
          ido-vertical-mode
          js2-mode
          json-mode
          lsp-mode
          magit
          markdown-mode
          move-text
          multiple-cursors
          pdf-tools
          projectile
          rainbow-delimiters
          rainbow-mode
          ranger
          restclient
          robe
          rubocop
          ruby-electric
          rvm
          sass-mode
          smart-tab
          smex
          terraform-mode
          tide
          visual-fill-column
          which-key
          wttrin
          ac-cider
          ))
(package-initialize)

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))




;; Dashboard setup
;; (setq dashboard-startup-banner "/Users/alex/.emacs./ardeo-logo-red.svg")
;; End Dashboard setup
;; (ido-mode t)
;; (ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-vertical-show-count t)
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq epa-pinentry-mode 'loopback)

(setq sentence-end-double-space nil)
;; increase the font globally for bigger resolutions
;(set-face-attribute 'default nil :height 140)

(exec-path-from-shell-initialize)

(set-frame-font "Menlo 18")
(exec-path-from-shell-initialize)

(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq dired-dwim-target t)

(global-set-key (kbd "C-x g") 'magit-status)

(with-eval-after-load 'magit
  (require 'forge))

;; (setq auth-sources '("~/.authinfo"))


(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.200ok.ch"
                 "gitlab.200ok.ch/api/v4"
                 "gitlab.200ok.ch"
                 forge-gitlab-repository))
  (setq auth-sources '("~/.authinfo")))




(global-set-key [f8] 'neotree-toggle)
;; (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; (setq projectile-switch-project-action 'neotree-projectile-action)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq magit-branch-read-upstream-first 'fallback)

(savehist-mode 1)

;; GTD Setup and Org mode tweaks

(setq org-todo-keywords
    '((sequence "TODO" "WORKING" "|" "DONE")
      (sequence "PROJECT" "AGENDA" "|" "MINUTES")
      (sequence "WAITING" "|" "PROGRESS")))

(setq org-adapt-indentation nil)

(setq org-fontify-done-headline t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-todo ((t (:inherit hl-todo :italic t))))
 '(js2-private-function-call ((t (:foreground "goldenrod"))))
 '(js2-private-member ((t (:foreground "ff0000")))))
(setq org-todo-keyword-faces
      '(("WORKING" . "#F57F17")))
;(add-to-list 'org-tag-faces '(".*" . (:foreground "cyan")))


(add-hook 'org-mode-hook 'which-key-mode)
(add-hook 'cider-mode-hook 'which-key-mode)


(defun gtd ()
  "Open main GTD file"
  (interactive)
  (find-file "/Users/alex/Documents/GTD/things.org")
  (shrink-window-if-larger-than-buffer)
  (other-window 1))

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/GTD/INBOX.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
         ("c" "Chores" entry (file+headline "~/Documents/GTD/things.org" "Chore")
         "* TODO %?\n  %i\n  %a")
        ))

(setq org-agenda-files (list "~/Documents/GTD/things.org"
                             ))

(define-key global-map "\C-ca" 'org-agenda)

(move-text-default-bindings)


(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-s->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-s-l") 'mc/edit-lines)
(define-key mc/keymap (kbd "<return>") nil)


;; (projectile-mode +1)

;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-c d") 'dired-jump)

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(doom-modeline-mode 1)
(winner-mode 1)
(delete-selection-mode 1)
(global-prettify-symbols-mode +1)
(global-display-line-numbers-mode 1)
(global-git-gutter-mode +1)
(show-paren-mode 1)
;;(hs-minor-mode 1)
(dumb-jump-mode)
(global-auto-revert-mode)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'js2-mode 'hs-minor-mode)

;; (require 'lsp-mode)

(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js2-mode #'lsp)


;; (ac-config-default)

(with-eval-after-load 'evil-maps (define-key evil-motion-state-map [down-mouse-1] nil))

(setq-default tab-width 2)

(setq-default tab-width 2 indent-tabs-mode nil)

(setq-default typescript-indent-level 4)

(setq-default indent-tabs-mode nil)

(setq js-indent-level 2)

(setq coffee-tab-width 2)

(setq python-indent 2)

(setq css-indent-offset 2)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-parse-warnings nil)

(add-hook 'sh-mode-hook
    (lambda ()
      (setq sh-basic-offset 2
      sh-indentation 2)))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-css-colorization t)
;; (define-key web-mode-map (kbd "C-tab") 'emmet-expand-line)
;;(define-key emmet-mode-keymap [tab] 'emmet-expand-line)

(dashboard-setup-startup-hook)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled



;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.

(load-theme 'doom-peacock t)

;; change the highlight color
(set-face-attribute 'region nil :background "#666")

(setq wttrin-default-cities '("Zurich"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))



;;(load-theme 'webstorm t)

;; (desktop-save-mode 1)

(require 'org-tempo)

(setq org-agenda-files (list "~/Documents/notes/notes.org"))

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)



(defun visual-clean ()
  "Cleanup messy buffers."
  (interactive)
  (visual-line-mode)
  (visual-fill-column-mode))


(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(require 'js2-mode)
(define-key js2-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
(define-key js2-mode-map (kbd "C-c C-d") 'tide-documentation-at-point)

(defun setup-tide-mode ()
  (interactive)
  ;; For bigger JS projects and intense tasks like =tide=references=
  ;; the default of 2s will time out
  (setq tide-sync-request-timeout 10)
  (tide-setup)
  ;; Increase sync request timeout for bigger projects
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)

(highlight-indentation-mode 1)
(set-face-background 'highlight-indentation-face "#393939")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(setq-default flycheck-temp-prefix ".")
(setq flycheck-javascript-eslint-executable "eslint-project-relative")

(add-hook 'js2-mode-hook
          (lambda () (setq flycheck-javascript-eslint-executable (expand-file-name "node_modules/.bin/eslint" (locate-dominating-file (buffer-file-name) "package.json")))))
(setq js2-mode-show-strict-warnings nil)
(setq js2-mode-show-parse-warnings nil)

;; automatically show colors as backgrounds to hex
(add-hook 'prog-mode-hook 'rainbow-mode)

;; highlight words
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; EVIL MODE SETTING

(evil-mode t)
;; Enable "M-x" in evil mode

;(global-set-key (kbd "M-x") 'execute-extended-command)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "w" 'basic-save-buffer
  "s" 'flyspell-buffer
  "b" 'evil-buffer
  "q" 'evil-quit)

(setq evil-want-fine-undo 'fine)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-evil-mc-mode 1)


(global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-insert-state-map (kbd "C-v") 'evil-visual-paste)


(mapc (lambda (mode)
  (evil-set-initial-state mode 'emacs)) '(elfeed-show-mode
                                          elfeed-search-mode
                                          dired-mode
                                          tide-references-mode
                                          image-dired-mode
                                          image-dired-thumbnail-mode
                                          Stacktrace
                                          eww-mode
                                          magit-popup-mode))

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)

(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jk")
(evil-escape-mode)
;;;; END EVIL MODE SETTINGS


;;;; CUSTOM SHORTCUTS;;;;

(define-key global-map (kbd "C-1") 'text-scale-increase)
(define-key global-map (kbd "C-0") 'text-scale-decrease)

(define-key global-map (kbd "s-/") 'comment-line)

;;;: END CUSTOM SHORTCUTS;;;;


;; General Settings
(setq gc-cons-threshold 20000000)
(setq make-backup-files nil)
(setq backup-directory-alist
    `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
    `((".*" ,(concat user-emacs-directory "backups"))))

;;; PRETTIER

(defun autoformat ()
  "Automatically format current buffer."
  (interactive)
  (let ((eslint-path (concat (projectile-project-root)
           ".eslintrc.json")))
    (autoformat-with
     (cond ((derived-mode-p 'web-mode) 'autoformat-html-command)
     ((derived-mode-p 'css-mode) 'autoformat-css-command)
     ((derived-mode-p 'json-mode) 'autoformat-json-command)
     ((derived-mode-p 'sass-mode) 'autoformat-sass-command)
     ((derived-mode-p 'sacs-mode) 'autoformat-scss-command)
     ((derived-mode-p 'yaml-mode) 'autoformat-yaml-command)
     ;; JS projects with eslint config
     ((and (file-exists-p eslint-path)
     (derived-mode-p 'js2-mode))
      'autoformat-prettier-eslint-command)
     ((derived-mode-p 'js2-mode) 'autoformat-javascript-command)))))

(defun autoformat-with (strategy)
  "Automatically format current buffer using STRATEGY."
  (let ((p (point))
  (s (window-start)))
    ;; Remember the current position
    (save-mark-and-excursion
      ;; Call prettier-eslint binary with the contents of the current
      ;; buffer
      (shell-command-on-region
       (point-min) (point-max)
       (funcall strategy)
       ;; Write into a temporary buffer
       (get-buffer-create "*Temp prettier buffer*")
       ;; Replace the current buffer with the output of
       ;; prettier-eslint
       t))
    ;; Return to the previous point and scrolling position (the point
    ;; was lost, because the whole buffer got replaced.
    (set-window-start (selected-window) s)
    (goto-char p)))

(defun autoformat-javascript-command ()
  "CLI tool to format Javascript."
  "prettier --parser babel")

(defun autoformat-html-command ()
  "CLI tool to format HTML."
  "prettier --parser html")

(defun autoformat-css-command ()
  "CLI tool to format CSS."
  "prettier --parser css")

(defun autoformat-sass-command ()
  "CLI tool to format SASS."
  "prettier --parser sass")

(defun autoformat-sass-command ()
  "CLI tool to format SASS."
  "prettier --parser scss")

(defun autoformat-json-command ()
  "CLI tool to format JSON."
  "prettier --parser json")

(defun autoformat-yaml-command ()
  "CLI tool to format YAML."
  "prettier --parser yaml")

(defun autoformat-prettier-eslint-command ()
  "CLI tool to format Javascript with .eslintrc.json configuration."
  (concat "prettier-eslint --eslint-config-path "
    ;; Hand over the path of the current projec
    (concat
     (projectile-project-root)
     ".eslintrc.json")
    " --parser babel --stdin"))


(setq ok-autoformat-modes (list 'web-mode
    'css-mode
    'json-mode
    'sass-mode
    'yaml-mode
    'js2-mode))

(dolist (mode ok-autoformat-modes)
  (evil-leader/set-key-for-mode mode "f" 'autoformat))

;;; END PRETRIER

;;; Ruby
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))
;; Don't compile scss on save
(setq scss-compile-at-save nil)

(add-to-list 'auto-mode-alist '("\\.rb?\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake?\\'" . enh-ruby-mode))


(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . robe-mode))

(add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))


;;;

(add-hook 'before-save-hook '(lambda()
                              (when (not (or (derived-mode-p 'markdown-mode)))
                                (delete-trailing-whitespace))))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; MU4E
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; default
(setq mu4e-maildir "~/.Mail/mihov.alex@gmail.com")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "mihov.alex@gmail.com"
   user-full-name  "Alex Mihov"
   mu4e-compose-signature
    (concat
     "Alex Mihov\n"
      "https://mihov.ch\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(setq auth-sources
      '((:source "~/.authinfo2.pg")))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "mihov.alex@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

(add-to-list 'mu4e-view-actions '("View In Browser" . mu4e-action-view-in-browser) t)

;; (setq fancy-splash-image-file "splash.pn")
(setq fancy-splash-image "~/.emacs.d/ardeo-logo-red.svg")

(set-fontset-font
 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
(define-key mu4e-headers-mode-map (kbd "C-c c") 'mu4e-org-store-and-capture)
(define-key mu4e-view-mode-map (kbd "C-c c") 'mu4e-org-store-and-capture)

(setq mu4e-view-show-addresses t)
(setq mu4e-update-interval (* 10 60))
(setq mu4e-index-update-in-background t)
;;; END MU4E

;;; LSP/Company
;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-selection-wrap-around t)
;; (setq company-minimum-prefix-length 0
      ;; company-idle-delay 0.0)
;; (global-set-key (kbd "C-SPC") 'company-complete)

;; (company-tng-configure-default)

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))
;; (require 'company-lsp)
;; (push 'company-lsp company-backends)
;;; END LSP/Company

(global-smart-tab-mode 1)

;; Tabs

(setq centaur-tabs-set-icons t)
(setq centaur-tabs-style "bar")


(require 'use-package)
(setq use-package-always-ensure t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "/Users/alex/Google Drive/_GTD/OrgRoam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ;;("C-c n j" . org-roam-dailies-capture-today)
         )
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;; NEW TESTS
(use-package ivy
  :diminish
  :bind (("s-f" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
           ("M-x" . counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))
;; (setq projectile-completion-system 'ivy)

;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package omnisharp)


;; (defun efs/lsp-mode-setup ()
  ;; (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;; (lsp-headerline-breadcrumb-mode))
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; END NEW TESTS


;; (defun add-emmet-expand-to-smart-tab-completions ()
;;   ;; Add an entry for current major mode in
;;   ;; `smart-tab-completion-functions-alist' to use
;;   ;; `emmet-expand-line'.
;;   (add-to-list 'smart-tab-completion-functions-alist
;;                (cons major-mode #'emmet-expand-line)))

;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook 'add-emmet-expand-to-smart-tab-completions)
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; (add-hook 'css-mode-hook 'add-emmet-expand-to-smart-tab-completions)

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-material))
 '(custom-safe-themes
   '("b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "1817f2521f95cd2ff06084845ee94d1a1c4fd60dd47959574581687f904721fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "ba13202a1b1f987600fe2e33df9abcf9c0131d99b16d57dddf65096a292403c4" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" default))
 '(eldoc-minor-mode-string " Eldoc-eval")
 '(flycheck-checker-error-threshold 1000)
 '(global-display-line-numbers-mode t)
 '(global-linum-mode nil)
 '(global-prettify-symbols-mode t)
 '(js2-highlight-level 3)
 '(js2-init-hook '(ignore))
 '(org-adapt-indentation nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(company-box lsp-ui omnisharp writeroom-mode telega org-bullets counsel-projectile eterm-256color counsel paradox org-roam use-package move-text handlebars-mode fancy-mode fancy-battery lsp-treemacs lsp-intellij magit-delta mu4e-views ruby-electric ruby-extra-highlight hl-todo imenu-list centaur-tabs zen-mode spotify smooth-scrolling smooth-scroll docker gnu-elpa-keyring-update treemacs-evil apache-mode dired-rainbow company-inf-ruby company-suggest company-restclient smart-tab emmet-mode company-web lsp-ivy impatient-mode php-mode forge twig-mode buffer-move company org-super-agenda vue-mode lorem-ipsum dotenv-mode dockerfile-mode rake rvm ruby-tools idle-highlight-mode idle-highlight adaptive-wrap rainbow-mode enh-ruby-mode sass-mode treemacs-projectile js3-mode ng2-mode yasnippet-bundle json-mode tern-auto-complete smart-jump dumb-jump js2-mode ido-vertical-mode ag tern eslint-fix wttrin yahoo-weather all-the-icons-dired git-gutter flyspell evil-surround evil-numbers evil-mc evil-leader evil-escape rainbow-delimiters csv-mode smex pdf-tools auto-complete ac-js2 ac-cider clojure-mode clj-refactor cider doom-themes all-the-icons doom-modeline ranger dashboard flyspell-correct flycheck-flow exec-path-from-shell restclient))
 '(paradox-github-token t))

(put 'erase-buffer 'disabled nil)

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
