(require 'package)
(add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;maximize screen
;;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;;start buffer
(setq initial-scratch-message "")

;;default directory
(setq default-directory "~/") 

;;inhibit splash screen
(setq inhibit-splash-screen t)

;;change the font
;;(set-face-attribute 'default nil :font "Hack Nerd Font-12")

;;disable menu bar
(menu-bar-mode -1)

;;disable tool bar
(tool-bar-mode -1)

;;cursor-blinks forever
(setq blink-cursor-blinks 0)

;;disable scrol bar
(scroll-bar-mode -1)

;; set "gnu" style for c
(setq c-deafault-style "bsd"
      c-basic-offset 4)

;; doom themes
;;(use-package doom-themes
;;  :init (load-theme 'doom-ir-black t))

(use-package exwm
  :ensure t)

(require 'exwm)
;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name.
(add-hook 'exwm-update-class-hook
  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
;; Global keybindings.
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
        ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
        ;; s-N: Switch to certain workspace.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))
;; my shortcut 
(exwm-input-set-key (kbd "C-s-<return>") '(lambda (cmd)
   (interactive (list (read-shell-command "$ ")))
   (start-process-shell-command cmd nil cmd)))

(exwm-input-set-key (kbd "s-<return>") '(lambda ()
   (interactive)
   (start-process-shell-command "urxvt" nil "urxvt")))

(exwm-input-set-key (kbd "<print>") '(lambda ()
   (interactive)
   (start-process-shell-command "flameshot" nil "flameshot gui")))


;; enable EXWM
(exwm-enable)

;; configuration for ivy
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

    (counsel-mode 1))

;; mode line
(display-battery-mode 1)
(setq display-time-day-and-date t)
(display-time-mode 1)

;; auto pairnn
(electric-pair-mode 1)

;; transparent
;;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; dired icon
(add-hook 'dired-mode-hook 'dired-icon-mode)

;; cursor color
;;(set-frame-parameter nil 'cursor-color "#ff0000")
;;(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

;;evil-mode for vim layout
(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(package-selected-packages
   '(evil dired-icon counsel exwm-modeline exwm graphql-mode haskell-mode ivy swiper ivy-rich ivy-avy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
