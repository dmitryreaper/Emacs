(require 'package)
(add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;maximize screen
;;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

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

;; config exwm

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "C-s-<return>") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-<return>") 'eshell)
  
  (exwm-enable))


;; configuration for ivy
(use-package ivy
    :diminish
    :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :after ivy)

(counsel-mode 1)

;; mode line
(display-battery-mode 1)
(setq display-time-day-and-date t)
(display-time-mode 1)

;; auto pairnn
(electric-pair-mode 1)

;; transparent
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 .50)))

;; dired icon
(add-hook 'dired-mode-hook 'dired-icon-mode)

;; cursor color
;;(set-frame-parameter nil 'cursor-color "#ff0000")
;;(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

;;evil-mode for vim layout
;;(use-package evil
;;:ensure t
;;  :init (evil-mode 1)
;;:config
;;  (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(package-selected-packages
   '(dired-icon counsel exwm-modeline exwm graphql-mode haskell-mode ivy swiper ivy-rich ivy-avy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
