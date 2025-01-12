;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OPTIONAL SETTING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(column-number-mode)
(global-display-line-numbers-mode t)

;;Blinking cursor
(setq blink-cursor-blinks 0)

;; tabs
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)

;;image in org mode size
(setq org-image-actual-width 600)

;; Set up the visible bell
(setq visible-bell t)

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

;;font
;;(set-face-attribute 'default nil :font "Iosevka Nerd Font 12")
(set-face-attribute 'default nil :font "terminus-12")

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

;; Scroll
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; Scroll Mouse
(pixel-scroll-precision-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAPPING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Настраиваем перемещение для русской раскладки
(defun setup-ru-layout-keys ()
  "Переназначить клавиши перемещения для русской раскладки."
  (let ((key-remap '(
                     ;; Перемещение
                     ("C-а" . "C-f")  ;; Вперёд (forward-char)
                     ("C-и" . "C-b")  ;; Назад (backward-char)
                     ("C-т" . "C-n")  ;; Вниз (next-line)
                     ("C-з" . "C-p")  ;; Вверх (previous-line)
					 ("C-ф" . "C-a")  ;; В начало строки
					 ("C-у" . "C-e")  ;; В конец строки
                     ;; Удаление
                     ("C-в" . "C-d")  ;; Удалить символ (delete-char)
                     ("C-л" . "C-k")  ;; Удалить строку (kill-line)

					 ;; Перемещение по словам
                     ("M-а" . "M-f")  ;; перемещение по слову вмеред (kill-line)
					 ("M-и" . "M-b")

                     )))
    (dolist (pair key-remap)
      (define-key key-translation-map (kbd (car pair)) (kbd (cdr pair))))))

(setup-ru-layout-keys)

(global-set-key (kbd "C-M-p") 'windmove-up)
(global-set-key (kbd "C-M-n") 'windmove-down)
(global-set-key (kbd "C-M-b") 'windmove-left)
(global-set-key (kbd "C-M-f") 'windmove-right)

;;resize window
(defun enlarge-vert ()
  (interactive)
  (enlarge-window 4))

(defun shrink-vert ()
  (interactive)
  (enlarge-window -4))

(defun enlarge-horz ()					
  (interactive)
  (enlarge-window-horizontally 4))

(defun shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -4))

(define-prefix-command 'my-mapping)
(define-key my-mapping (kbd "C-c k") 'shrink-vert)
(define-key my-mapping (kbd "C-c i") 'enlarge-vert)
(define-key my-mapping (kbd "C-c j") 'shrink-horz)
(define-key my-mapping (kbd "C-c l") 'enlarge-horz)

(define-prefix-command 'window-resize-map)
(global-set-key (kbd "C-x w") 'window-resize-map)

(define-key window-resize-map (kbd "p") (lambda () (interactive) (enlarge-window 4)))
(define-key window-resize-map (kbd "n") (lambda () (interactive) (enlarge-window -4)))
(define-key window-resize-map (kbd "f") (lambda () (interactive) (enlarge-window-horizontally 4)))
(define-key window-resize-map (kbd "b") (lambda () (interactive) (enlarge-window-horizontally -4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PACKAGES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Theme gruber-darker
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

;; Company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Ivy and Counsel
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

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Lsp servers
(use-package lsp-mode
  :ensure t
  :hook
  (c++-mode . lsp) 
  (java-mode . lsp)
  (c-mode . lsp)
  (js-mode . lsp))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

;; Improved candidate sorting with prescient.el 
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

;; Git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;; Startup logo
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)    
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda   . 5)))
  (setq dashboard-banner-logo-title "Welcome to Emacs!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pdf-tools annalist company counsel dashboard doom-modeline forge goto-chg gruber-darker-theme helm-core ivy-prescient ivy-rich llm lsp-java lsp-ui org-bullets org-pdftools org-present queue shell-maker wfnames))
 '(warning-suppress-log-types '((evil-collection))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
