;;; .emacs --- CONFIG
;;; Commentary:

;;; Dependencies
(require 'cl)
(require 'package)

;;; Code:

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar required-packages
  '(
    desktop
    flycheck
    magit
    molokai-theme
    projectile
    recentf
    relative-line-numbers
    smooth-scrolling
    spaceline
    sr-speedbar
    yasnippet
    ;; company
    company
    company-jedi
    ;; evil
    evil
    evil-leader
    evil-magit
    evil-multiedit
    evil-org
    evil-surround
    ;; ivy
    ivy
    counsel
    counsel-projectile
    smex
    ;; org
    org
    ;; language specific packages
    go-mode
    markdown-mode
    less-css-mode
    ;php-mode
    yaml-mode
   )
)

;; define package installer
(defun install-packages ()
  (interactive)
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; load required packages
(loop for p in required-packages
  do (require p))

;; UI
(load-theme 'molokai t)
(set-face-foreground 'font-lock-comment-face "dark grey")
(set-face-foreground 'font-lock-comment-delimiter-face "dark grey")

(setq-default show-trailing-whitespace t)
(set-face-attribute 'default nil :height 80)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode t)
(scroll-bar-mode -1)
(global-relative-line-numbers-mode)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil)
(blink-cursor-mode 0)

(smooth-scrolling-mode 1)
(setq scroll-preserve-screen-position t)

;; CUSTOMISATIONS
;; use 4 spaces instead of tabs
(setq-default tab-width 4 indent-tabs-mode nil)
;; don't create backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
;; copy/paste
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(setq select-enable-primary t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;; history
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
;; disable autosave
(setq auto-save-default nil)
;; remap c-x to c-a too
(keyboard-translate ?\C-a ?\C-x)

;; KEY BINDINGS
(global-set-key [f8] 'global-relative-line-numbers-mode)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; ELECTRIC-PAIR-MODE

(electric-pair-mode)

;;; PACKAGE SPECIFIC SETTINGS

;; EVIL MODE
(evil-mode 1)
(global-evil-surround-mode 1)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-l") 'evil-window-right)
(loop for (mode . state) in '((dired-mode . normal) ; can be normal, insert, emacs, etc..
                              (org-agenda-mode . normal)
                              (magit-diff-mode . normal)
                              (magit-log-mode . normal))
      do (evil-set-initial-state mode state))

(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

(define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-normal-state-map (kbd "C-p") 'evil-multiedit-prev)
(define-key evil-visual-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-visual-state-map (kbd "C-p") 'evil-multiedit-prev)

(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\t" 'company-complete-common)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; FLYCHECK
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq flycheck-highlighting-mode 'lines)

;; SPACELINE
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; YASNIPPET
(yas-global-mode 1)

;; IVY
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 30)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
;(setq ivy-re-builders-alist
;      '((ivy-switch-buffer . ivy--regex-plus)
;        (t . ivy--regex-fuzzy)))
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-x C-f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-x C-o") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'counsel-recentf)

;; ORG
(global-set-key "\C-xa" 'org-agenda)
(setq org-directory "~/d/org")
(setq org-agenda-files '("~/d/org"))
(setq org-default-notes-file "~/d/org/notes.org")
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; RECENTF
(recentf-mode 1)
(setq recentf-max-menu-items 512)
(setq recentf-auto-cleanup 'never)

;; MAGIT
(global-set-key (kbd "C-x C-g") 'magit-status)

;; SPEEDBAR
(global-set-key (kbd "<f3>") 'sr-speedbar-toggle)
(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 40)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-max-width 70)
(setq sr-speedbar-width-console 40)

;; GO-MODE
(add-hook 'before-save-hook 'gofmt-before-save)

;; PYTHON-MODE
;(define-key global-map (kbd "RET") 'newline-and-indent)

;; PHP-MODE
; (add-to-list 'auto-mode-alist
;              '("\\.php[34567]?\\'\\|\\.phtml\\'" . php-mode))

;; YAML-MODE
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide '.emacs)
;;; emacs.el ends here
