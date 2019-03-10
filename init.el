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
		      exec-path-from-shell
		      evil
                      evil-escape
                      evil-leader
                      evil-mc
                      evil-numbers
                      evil-surround
		      flycheck
		      flycheck-flow
		      flyspell
		      js2-mode
		      magit
		      markdown-mode
		      multiple-cursors
		      pdf-tools
		      projectile
		      ranger
		      restclient
		      smex
		      visual-fill-column
		      which-key))
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


;; increase the font globally for bigger resolutions
;(set-face-attribute 'default nil :height 140)

(set-frame-font "Menlo 18")
(exec-path-from-shell-initialize)

(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


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
 '(js2-highlight-level 3)
 '(js2-init-hook (quote (ignore)))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (yaml-mode csv-mode smex visual-fill-column pdf-tools multiple-cursors auto-complete ac-js2 ac-cider clojure-mode clj-refactor cider doom-themes all-the-icons doom-modeline projectile ranger dashboard markdown-mode flyspell-correct flycheck-flow flycheck exec-path-from-shell which-key magit restclient))))
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
