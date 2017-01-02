;;; .emacs --- CONFIG
(require 'cl)
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar required-packages
  '(
    desktop
    dired
    magit
    molokai-theme
    projectile
    recentf
    relative-line-numbers
    smooth-scrolling
    spaceline-config
    yasnippet
    ;; evil
    evil
    evil-leader
    evil-magit
    evil-org
    ;; helm
    helm
    helm-projectile
    ;; org
    org
    ;; language specific packages
    go-mode
    markdown-mode
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
(setq x-select-enable-primary t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;; history
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
;; disable autosave
(setq auto-save-default nil)
;; remap c-x to c-a too
(keyboard-translate ?\C-a ?\C-x)
(keyboard-translate ?\C-x ?\C-x)

;; KEY BINDINGS
(global-set-key [f8] 'global-relative-line-numbers-mode)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; PACKAGE SPECIFIC SETTINGS

;; EVIL MODE
(evil-mode 1)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(loop for (mode . state) in '((dired-mode . normal) ; can be normal, insert, emacs, etc..
                              (org-agenda-mode . normal)
                              (magit-diff-mode . normal)
                              (magit-log-mode . normal))
      do (evil-set-initial-state mode state))

;; FLYCHECK
;; (global-flycheck-mode)

;; SPACELINE
(spaceline-spacemacs-theme)

;; YASNIPPET
(yas-global-mode 1)

;; HELM
(require 'helm-config)
(setq helm-split-window-in-side-p           nil
      helm-move-to-line-cycle-in-source     t
      helm-display-header-line              nil
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      fit-window-to-buffer-horizontally     1
      helm-ff-file-name-history-use-recentf t
      helm-recentf-fuzzy-match              t)

(setq helm-autoresize-max-height 50)
(setq helm-autoresize-min-height 20)
(set-face-attribute 'helm-source-header nil :height 0.1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-o") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(helm-autoresize-mode 1)
(helm-projectile-on)
(helm-mode 1)

;; ORG
(global-set-key "\C-xa" 'org-agenda)
(setq org-directory "~/d/org")
(setq org-agenda-files '("~/d/org"))
(setq org-default-notes-file "~/d/org/notes.org")
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; RECENTF
(recentf-mode 1)
(setq recentf-max-menu-items 512)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; GO-MODE
(add-hook 'before-save-hook 'gofmt-before-save)

;; YAML-MODE
(add-hook 'yaml-mode-hook
  '(lambda ()
      (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide '.emacs)
;;; .emacs ends here
