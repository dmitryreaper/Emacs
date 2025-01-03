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

(defun my-spray-align-center ()
  "Align text to center in spray-mode."
  (let* ((window-width (window-total-width))
         (spray-width 70) ; Ширина текста в spray-mode
         (margin (/ (- window-width spray-width) 2)))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    (set-window-buffer (selected-window) (current-buffer))))

(add-hook 'spray-mode-hook 'my-spray-align-center)

(menu-bar-mode -1)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-collection-init)
  (setq evil-insert-state-cursor 'box)
  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state))

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
				pdf-view-mode-hook
                eshell-mode-hook
				nov-mode-hook))
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

;;(use-package ivy-posframe
;;  :ensure t
;;  :after ivy
;;  :init
;;  (ivy-posframe-mode t))

(use-package spray
  :ensure t
  :config
  (setq spray-wpm 400) ; Установить скорость 400 слов в минуту
  (global-set-key (kbd "C-c s") 'spray-mode)
  (setq spray-align-to-center t)) 

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)) ;; Авто-перенос строк

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

(use-package org-pdftools
  :hook
  (org-mode . org-pdftools-setup-link))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
	 default))
 '(package-selected-packages
   '(company counsel dashboard doom-modeline evil evil-collection forge
			 ivy-posframe ivy-prescient ivy-rich lsp-java lsp-ui nov
			 org-bullets org-pdftools org-pdfview pdf-tools spray
			 visual-fill-column))
 '(warning-suppress-log-types '((evil-collection))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
