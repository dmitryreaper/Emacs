(require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;inhibit splash screen
(setq inhibit-splash-screen t)

;;change the font
(set-face-attribute 'default nil :font "Hack Nerd Font-12")

;;disable menu bar
(menu-bar-mode -1)

;;disable tool bar
(tool-bar-mode -1)

;;cursor-blinks forever
(setq blink-cursor-blinks 0)

;;disable scrol bar
(scroll-bar-mode -1)

;;set "gnu" style for c
(setq c-deafault-style "linux"
      c-basic-offset 4)

;;auto pairnn
(electric-pair-mode 1)

;;transparent
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 .50)))

;;cursor color
(set-frame-parameter nil 'cursor-color "#ff0000")
(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))
