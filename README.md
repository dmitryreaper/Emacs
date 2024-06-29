# Emacs

## Package setup

В Emacs есть встроенный менеджер пакетов, но он не упрощает
автоматическую установку пакетов в новую систему при первом изменении
конфигурации. package-use - пакет, используемый в этой конфигурации,
который значительно упрощает автоматизацию установки и настройки всего
остального, что мы используем.

``` {.commonlisp org-language="emacs-lisp"}
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                 ("org" . "https://orgmode.org/elpa/")
                 ("elpa" . "https://elpa.gnu.org/packages/")))  
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
```

## Basic UI configurationl

В этом разделе настраиваются основные параметры пользовательского
интерфейса, которые удаляют ненужные элементы, чтобы сделать Emacs более
минималистичным и современным.

``` {.commonlisp org-language="emacs-lisp"}
;;basic ui configuration
(setq inhibit-startup-message t)

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  (setq blink-cursor-blinks 0)


  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell t)

  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Set frame transparency
  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

  ;;colors CURSOR
  (set-frame-parameter nil 'cursor-color "#ff0000")
  (add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))  ;; Disable line numbers for some modes

  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
```

## Font configuration

Я использую шрифты [Hack Nerd
Font](https://www.nerdfonts.com/font-downloads) для этой конфигурации,
которые, скорее всего, потребуется установить на вашем компьютере. И то,
и другое обычно можно найти в различных менеджерах пакетов дистрибутивов
Linux или загрузить по ссылкам, приведенным выше.

``` {.commonlisp org-language="emacs-lisp"}
  ;;font
(set-face-attribute 'default nil :font "Hack Nerd Font-11")

```

## Keybinding Configuration

В этой конфигурации используется
[evil-mode](https://evil.readthedocs.io/en/latest/index.html) для
модального редактирования, подобного Vi.
[evil-collection](https://github.com/emacs-evil/evil-collection)
используется для автоматической настройки различных режимов Emacs с
помощью Vi-подобных привязок клавиш для evil-mode.

``` {.commonlisp org-language="emacs-lisp"}
;;keybinding 
  (use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "M-h") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "M-j") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "M-l") 'evil-normal-state)

    ;; use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

```

## UI configuration

### Color Theme

[doom-themes](https://github.com/doomemacs/themes)- это отличный набор
разнообразных тем, поддерживающих множество различных режимов работы в
Emacs. Возможно, просмотр
[скриншотов](https://github.com/hlissner/emacs-doom-themes/tree/screenshots)
поможет вам решить, какая из них вам больше нравится. Вы также можете
запустить `Mx counsel-load-theme`{.verbatim}, чтобы легко выбирать между
ними.

``` {.commonlisp org-language="emacs-lisp"}

(use-package doom-themes
  :init (load-theme 'doom-ir-black t))

```

### Better Modeline

[doom-modeline](https://github.com/seagle0128/doom-modeline) - это очень
привлекательная и богатая (но все же минимальная) конфигурация линейки
режимов для Emacs. Конфигурация по умолчанию довольно хорошая, но вы
можете ознакомиться с [параметрами
конфигурации](https://github.com/seagle0128/doom-modeline#customize),
чтобы узнать больше о том, что вы можете включить или отключить.

**ПРИМЕЧАНИЕ:** При первой загрузке конфигурации на новом компьютере вам
необходимо запустить \"M-x all-the-icons-install-fonts\", чтобы значки
строк режима отображались корректно.

``` {.commonlisp org-language="emacs-lisp"}
;;Better Modeline
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
```

### Auto-completion of brackets, quotes

Функция для автоматического закрытия скобок, ковычек

``` {.commonlisp org-language="emacs-lisp"}
;; skeleton
(setq skeleton-pair t)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

;;AUTO CLOSE
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию  electric-indent-mod'ом (default in Emacs-24.4)
(setq electric-pair-pairs '(
                                (?\{ . ?\})
                                (?\( . ?\))
                (?\[ . ?\])
                            ))
```

### PDF

Функция для просмотрка pdf документов внутри emacs

``` {.commonlisp org-language="emacs-lisp"}
;;PDF VIEW FILE
(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "'\\.pdf\\'"
  :bind (:map pdf-view-mode-map
          ("j" . pdf-view-next-line-or-next-page)
          ("k" . pdf-view-previous-line-or-previous-page)
          ("C-=" . pdf-view-enlarge)
          ("C--" . pdf-view-shrink))            

  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda ()(interactive) (display-line-numbers-mode -1)))
```

### Ivy and Counsel

[Ivy](https://oremacs.com/swiper/) - отличная платформа для завершения
работы с Emacs. Она предоставляет минимальное, но мощное меню выбора,
которое появляется при открытии файлов, переключении буферов и для
многих других задач в Emacs. Counsel - это настраиваемый набор команд
для замены \"find-file\" на \"counsel-find-file\" и т.д., которые
предоставляют полезные команды для каждой из команд завершения по
умолчанию.

[ivy-rich](https://github.com/Yevgnen/ivy-rich) добавляет дополнительные
столбцы к нескольким командам Counsel, чтобы предоставить больше
информации о каждом элементе.

``` {.commonlisp org-language="emacs-lisp"}

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

```

## Helpful help commands

[Helpful](https://github.com/Wilfred/helpful) добавляет много очень
полезной (понимаете?) информации в буферы команд Emacs\'
`describe-`{.verbatim}. Например, если вы используете
`describe-function`{.verbatim}, вы не только получите документацию о
функции, но и увидите исходный код функции и то, как она используется в
других местах конфигурации Emacs. Это очень полезно для понимания того,
как все работает в Emacs.

``` {.commonlisp org-language="emacs-lisp"}
(use-package helpful
    :commands (helpful-callable helpful-variable helpful-command helpful-key)
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

```

## Languages

### lsp-mode

Мы используем превосходный
[lsp-режим](https://emacs-lsp.github.io/lsp-mode/) для обеспечения
функциональности, подобной IDE, для многих различных языков
программирования с помощью \"языковых серверов\", которые поддерживают
[Протокол языкового
сервера](https://microsoft.github.io/language-server-protocol/). Прежде
чем пытаться настроить `lsp-mode`{.verbatim} для определенного языка,
ознакомьтесь с [документацией для вашего
языка](https://emacs-lsp.github.io/lsp-mode/page/languages/), чтобы
узнать, какие языковые серверы доступны и как их установить.

Параметр `lsp-keymap-prefix`{.verbatim} позволяет вам определить префикс
для того места, где будут добавлены привязки клавиш
`lsp-mode`{.verbatim} по умолчанию. Я **настоятельно рекомендую**
использовать префикс, чтобы выяснить, что вы можете сделать с
`lsp-mode`{.verbatim} в буфере.

Интеграция с `which-key`{.verbatim} добавляет полезные описания
различных клавиш, так что вы сможете многому научиться, просто нажав
`C-c l`{.verbatim} в буфере `lsp-mode`{.verbatim} и попробовав различные
варианты, которые вы там найдете.

``` {.commonlisp org-language="emacs-lisp"}
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
        (c-mode . lsp))

;;python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

```

### lsp-ui

[lsp-ui](https://emacs-lsp.github.io/lsp-ui/) - это набор улучшений
пользовательского интерфейса, созданных поверх `lsp-mode`{.verbatim},
которые делают Emacs еще более похожим на IDE. Посмотрите скриншоты на
домашней странице `lsp-ui`{.verbatim} (ссылка приведена в начале этого
абзаца), чтобы увидеть примеры того, что он может делать.

``` {.commonlisp org-language="emacs-lisp"}
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

```

### flycheck

[flycheck](https://github.com/flycheck/flycheck) - cовременное
расширение для проверки синтаксиса \"на лету\" для GNU Emacs

``` {.commonlisp org-language="emacs-lisp"}

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

```

### Improved candidate sorting with prescient.el

prescient.el предоставляет некоторые полезные функции для сортировки
кандидатов на завершение Ivy в зависимости от того, как недавно или как
часто вы их выбираете. Это может быть особенно полезно при использовании
`M-x`{.verbatim} для запуска команд, которые не привязаны к ключу, но к
которым вам все равно нужно время от времени обращаться.

``` {.commonlisp org-language="emacs-lisp"}
(use-package ivy-prescient
    :after counsel
    :custom
    (ivy-prescient-enable-filtering nil)
    :config
    ;; Uncomment the following line to have sorting remembered across sessions!
    ;(prescient-persist-mode 1)
    (ivy-prescient-mode 1))
```

# ORG MODE

[Org Mode](https://orgmode.org/) - одна из отличительных особенностей
Emacs. Это многофункциональный редактор документов, планировщик
проектов, отслеживание задач и времени, механизм ведения блогов и
утилита для грамотного программирования, объединенные в одном пакете.

## Better Fonts Faces

Функция `efs/org-font-setup`{.verbatim} настраивает различные начертания
текста для настройки размеров заголовков и использования шрифтов
переменной ширины в большинстве случаев, чтобы было больше похоже, что
мы редактируем документ в режиме `org-mode`{.verbatim}. Мы возвращаемся
к шрифтам фиксированной ширины (моноширинным) для блоков кода и таблиц,
чтобы они отображались корректно.

``` {.commonlisp org-language="emacs-lisp"}
(defun efs/org-font-setup ()
    ;; Replace list hyphen with dot
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
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))
```

## Basic Config

Этот раздел содержит базовую конфигурацию для `org-mode`{.verbatim}, а
также конфигурацию для организационных повесток дня и шаблонов сбора
данных. Здесь многое предстоит разобрать, поэтому я бы порекомендовал
посмотреть видеоролики [Часть 5](https://youtu.be/VcgjTEa0kU4) и [Часть
6](https://youtu.be/PNE-mgkZ6HM) для получения полного объяснения.

``` {.commonlisp org-language="emacs-lisp"}
(defun efs/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (use-package org
    :pin org
    :commands (org-capture org-agenda)
    :hook (org-mode . efs/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")

    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)

    (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    (setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))

    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-tag-alist
      '((:startgroup)
         ; Put mutually exclusive tags here
         (:endgroup)
         ("@errand" . ?E)
         ("@home" . ?H)
         ("@work" . ?W)
         ("agenda" . ?a)
         ("planning" . ?p)
         ("publish" . ?P)
         ("batch" . ?b)
         ("note" . ?n)
         ("idea" . ?i)))

    ;; Configure custom agenda views
    (setq org-agenda-custom-commands
     '(("d" "Dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT"
          ((org-agenda-overriding-header "Next Tasks")))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

      ("n" "Next Tasks"
       ((todo "NEXT"
          ((org-agenda-overriding-header "Next Tasks")))))

      ("W" "Work Tasks" tags-todo "+work-email")

      ;; Low-effort next actions
      ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
       ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))

      ("w" "Workflow Status"
       ((todo "WAIT"
              ((org-agenda-overriding-header "Waiting on External")
               (org-agenda-files org-agenda-files)))
        (todo "REVIEW"
              ((org-agenda-overriding-header "In Review")
               (org-agenda-files org-agenda-files)))
        (todo "PLAN"
              ((org-agenda-overriding-header "In Planning")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "BACKLOG"
              ((org-agenda-overriding-header "Project Backlog")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
        (todo "READY"
              ((org-agenda-overriding-header "Ready for Work")
               (org-agenda-files org-agenda-files)))
        (todo "ACTIVE"
              ((org-agenda-overriding-header "Active Projects")
               (org-agenda-files org-agenda-files)))
        (todo "COMPLETED"
              ((org-agenda-overriding-header "Completed Projects")
               (org-agenda-files org-agenda-files)))
        (todo "CANC"
              ((org-agenda-overriding-header "Cancelled Projects")
               (org-agenda-files org-agenda-files)))))))

    (define-key global-map (kbd "C-c j")
      (lambda () (interactive) (org-capture nil "jj")))

    (efs/org-font-setup))
```

## Nicer Heading Bullets

[org-bullets](https://github.com/sabof/org-bullets) заменяет звездочки
заголовка в `org-mode`{.verbatim} буферах на более привлекательные
символы, которыми вы можете управлять. Другой вариант для этого -
[org-superstar-mode](https://github.com/integral-dw/org-superstar-mode),
который мы, возможно, рассмотрим в следующем видео.

``` {.commonlisp org-language="emacs-lisp"}
(use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;CENTER ORG BUFFER
(defun efs/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . efs/org-mode-visual-fill))

```
