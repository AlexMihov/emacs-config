(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(defvar my-packages '(
		      ac-cider
		      ac-js2
		      all-the-icons
		      auto-complete
		      cider
		      clj-refactor
		      clojure-mode
		      csv-mode
		      dashboard
		      doom-modeline
		      doom-themes
		      evil
		      exec-path-from-shell
		      flycheck
		      flycheck-flow
		      flyspell
		      js2-mode
		      magit
		      markdown-mode
		      multiple-cursors
		      pdf-tools
		      projectile
		      rainbow-delimiters
		      ranger
		      restclient
		      smex
		      visual-fill-column
		      which-key
		      wttrin
                      evil-escape
                      evil-leader
                      evil-mc
                      evil-numbers
                      evil-surround
		      ))
(package-initialize)

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    
    
    (package-install p))
  (add-to-list 'package-selected-packages p))

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq epa-pinentry-mode 'loopback)

(setq sentence-end-double-space nil)
;; increase the font globally for bigger resolutions
;(set-face-attribute 'default nil :height 140)

(set-frame-font "Menlo 18")
(exec-path-from-shell-initialize)

(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-branch-read-upstream-first 'fallback)

(setq org-todo-keywords
          '((sequence "TODO" "|" "WORKING" "|" "DONE")
            (sequence "PROJECT" "AGENDA" "|" "MINUTES")
            (sequence "WAITING" "|" "PROGRESS")))

(add-hook 'org-mode-hook 'which-key-mode)
(add-hook 'cider-mode-hook 'which-key-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-s->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-s-l") 'mc/edit-lines)
(define-key mc/keymap (kbd "<return>") nil)


(projectile-mode +1)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-c d") 'dired-jump)


(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(doom-modeline-mode 1)
(winner-mode 1)
(delete-selection-mode 1)
(global-prettify-symbols-mode +1)
(global-display-line-numbers-mode 1)
(global-git-gutter-mode +1)
;(editorconfig-mode 1)
(show-paren-mode 1)

(ac-config-default)

(dashboard-setup-startup-hook)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled



;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
					;
(load-theme 'doom-peacock t)

(setq wttrin-default-cities '("Zurich"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))



;;(load-theme 'webstorm t)

(desktop-save-mode 1)

(setq org-agenda-files (list "~/Documents/notes/notes.org"))

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)



(defun visual-clean ()
  "Cleanup messy buffers."
  (interactive)
  (visual-line-mode)
  (visual-fill-column-mode))

;;;; EVIL MODE SETTING

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
                                                image-dired-mode
                                                image-dired-thumbnail-mode
                                                eww-mode))

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
  "prettier --stdin --parser babel")

(defun autoformat-html-command ()
  "CLI tool to format HTML."
  "prettier --stdin --parser html")

(defun autoformat-css-command ()
  "CLI tool to format CSS."
  "prettier --stdin --parser css")

(defun autoformat-sass-command ()
  "CLI tool to format SASS."
  "prettier --stdin --parser sass")

(defun autoformat-json-command ()
  "CLI tool to format JSON."
  "prettier --stdin --parser json")

(defun autoformat-yaml-command ()
  "CLI tool to format YAML."
  "prettier --stdin --parser yaml")

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
      '((:source "~/.authinfo2.gpg")))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "mihov.alex@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)


;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;;; END MU4E

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1817f2521f95cd2ff06084845ee94d1a1c4fd60dd47959574581687f904721fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "ba13202a1b1f987600fe2e33df9abcf9c0131d99b16d57dddf65096a292403c4" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" default)))
 '(eldoc-minor-mode-string " Eldoc-eval")
 '(global-display-line-numbers-mode t)
 '(global-linum-mode nil)
 '(global-prettify-symbols-mode t)
 '(js2-highlight-level 3)
 '(js2-init-hook (quote (ignore)))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (wttrin yahoo-weather all-the-icons-dired git-gutter flyspell evil-surround evil-numbers evil-mc evil-leader evil-escape evil rainbow-delimiters yaml-mode csv-mode smex visual-fill-column pdf-tools multiple-cursors auto-complete ac-js2 ac-cider clojure-mode clj-refactor cider doom-themes all-the-icons doom-modeline projectile ranger dashboard markdown-mode flyspell-correct flycheck-flow flycheck exec-path-from-shell which-key magit restclient))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "#F1C410"))))
 '(font-lock-variable-name-face ((t (:foreground "#FF1FEE"))))
 '(js2-function-call ((t (:foreground "#0CFF31"))))
 '(js2-function-param ((t (:foreground "#E67E24"))))
 '(js2-object-property ((t (:foreground "#06CBFF"))))
 '(js2-private-function-call ((t (:foreground "goldenrod"))))
 '(js2-private-member ((t (:foreground "ff0000")))))
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
