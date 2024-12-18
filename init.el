(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))  
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;BASIC UI CONFIGURATION
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1) 
(set-fringe-mode 10)

(setq blink-cursor-blinks 0)

;; tabs
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)

;;image in org mode size
(setq org-image-actual-width 600)

(menu-bar-mode -1)

;; modeline bar doom
(doom-modeline-mode t)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;;colors CURSOR
(set-frame-parameter nil 'cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(cursor-color . "#ffffff"))

;;line-numbers-mode off 
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;FONT
(set-face-attribute 'default nil :font "Hack Nerd Font-11")

;;set "gnu" style for c
(setq c-deafault-style "linux"
      c-basic-offset 4)

;;garbage
(setq gc-cons-threshold (* 10 1000 1000))
(setq gc-cons-percentage 0.6)

;;auto pair
(electric-pair-mode 1)

;; view image in org mode
(setq org-src-fontify-natively 't)
(setq org-startup-with-inline-images t)

;; Включить плавную прокрутку
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Дополнительно: более плавная прокрутка с мышью
(pixel-scroll-precision-mode t)

;;Ivy and Counsel
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :init
  (ivy-posframe-mode t))


(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;;Mapping
(global-set-key (kbd "C-M-p") 'windmove-up)
(global-set-key (kbd "C-M-n") 'windmove-down)
(global-set-key (kbd "C-M-b") 'windmove-left)
(global-set-key (kbd "C-M-f") 'windmove-right)

;;resize window
(defun enlarge-vert ()
  (interactive)
  (enlarge-window 2))

(defun shrink-vert ()
  (interactive)
  (enlarge-window -2))

(defun enlarge-horz ()					
  (interactive)
  (enlarge-window-horizontally 2))

(defun shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))

(define-prefix-command 'my-mapping)
(define-key my-mapping (kbd "C-c k") 'shrink-vert)
(define-key my-mapping (kbd "C-c i") 'enlarge-vert)
(define-key my-mapping (kbd "C-c j") 'shrink-horz)
(define-key my-mapping (kbd "C-c l") 'enlarge-horz)

(define-prefix-command 'window-resize-map)
(global-set-key (kbd "C-x w") 'window-resize-map)

(define-key window-resize-map (kbd "p") 'enlarge-window)
(define-key window-resize-map (kbd "n") (lambda () (interactive) (enlarge-window -4)))
(define-key window-resize-map (kbd "f") 'enlarge-window-horizontally)
(define-key window-resize-map (kbd "b") (lambda () (interactive) (enlarge-window-horizontally -4)))

;;LSP SERVERS
(use-package lsp-mode
  :ensure t
  :hook
  (c++-mode . lsp) 
  (java-mode . lsp)
  (c-mode . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))

(add-hook 'after-init-hook 'global-company-mode)

;;IMPROVED CANDIDATE SORTING WITH PRESCIENT.EL 
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

;; git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ORGmode files;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun efs/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Hack Nerd Font" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;;(setq dashboard-startup-banner "/home/dima/Pictures/demon.png") ; Логотип Emacs
  (setq dashboard-center-content t)         ; Центрирование содержимого
  (setq dashboard-items '((recents  . 5)    ; Последние файлы
                          (projects . 5)    ; Последние проекты
                          (agenda   . 5))) ; События из Org Mode
  (setq dashboard-banner-logo-title "Welcome to Emacs!"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" default))
 '(package-selected-packages
   '(ivy-posframe dashboard gcmh el-fetch doom-modeline exwm auto-org-md projectile sr-speedbar buffer-move org-tempo company lsp-java forge magit helpful ivy-prescient flycheck lsp-ui lsp-mode counsel ivy-rich ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
