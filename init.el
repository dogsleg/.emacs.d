;;; init.el -- Lev Lamberov's GNU Emacs configuration file

;; Author: Lev Lamberov <dogsleg@debian.org>
;; Homepage: https://www.pimentola.ru
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my GNU Emacs configuration file.

;; Since I run Debian on my machines and I am a member of Debian
;; Emacsen team [https://wiki.debian.org/EmacsenTeam], typically I
;; install Emacs packages from the official Debian repository. And if
;; I need some Emacs package that is not in the official Debian
;; repository yet, usually I create that Debian package with help of
;; magical dh-elpa and dh-make-elpa utilities and upload it to the
;; Debian archive.

;; My GPG key: DE6B A671 D57D 9B00 9CF6  8650 5EE7 6EE2 0216 D2A5

;;; Code:

;; Disable garbage collection during the startup time
(setq gc-cons-threshold 536870912
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 16777216
                                   gc-cons-percentage 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === PERSONAL === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set my personal info
(setq user-full-name "Lev Lamberov"
      user-mail-address "dogsleg@debian.org")

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === BASIC === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Prefer newer bytecode
(setq load-prefer-newer t)

;; Disable menubar, toolbar, scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Ask for confirmation when killing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable Transient Mode
(transient-mark-mode t)

;; Enable automatic compress/uncompress of files
(auto-compression-mode t)

;; Disable local variables
(setq enable-local-variables nil)

;; Load network security manager
(require 'nsm)

;; Paranoid network security
(setq network-security-level 'paranoid)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === PACKAGE === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize package.el infrastructure
(load "package")
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === DIRECTORIES === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set custom directory to search for Emacs packages
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Put Customize settings to custom.el, not here
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load-file custom-file))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === THEME === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Load my favorite theme
(load-theme 'monokai)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === BROWSER === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define alias to xdg-open URL
(defalias 'gk-urls-external-browser 'browse-url-xdg-open)

;; Define function to open URL in EWW or to xdg-open it
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse with external browser, pass ARGS."
  (apply
   (if (y-or-n-p "Browse with EWW? ")
       'eww-browse-url
     #'gk-urls-external-browser)
   args))

(setq browse-url-browser-function #'gk-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === UI and MISC === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make Emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))

;; Do not show startup menu
(setq inhibit-startup-screen t)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Enable visible bell
(setq visible-bell t)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "Terminus 11"))

;; Show column number
(setq column-number-mode t)

;; Show time in 24 hours format in mode-line
(setq display-time-mode t)
(setq display-time-24hr-format t)

;; Show the time on the status bar in 24 hours format
(setq display-time-day-and-date t)
(display-time)

;; Set frame title format
(setq frame-title-format  (concat invocation-name "@" system-name ": %b %+%+ %f"))

;; Highlight text that matches regular expression (globally)
(global-hi-lock-mode -1)

;; Highlight current line globally when Emacs run in GUI mode
(when window-system
  (global-hl-line-mode))

;; Disable line highlight in eshell
(add-hook 'eshell-mode-hook (lambda () (global-hl-line-mode -1)))

;; Enable automatic updating a buffer if a file changes on disk
(global-auto-revert-mode 1)

;; Ask for new line in the end of a file
(setq require-final-newline 'query)

;; Add new line when cursor moves beyond the end of a buffer
(setq next-line-add-newlines t)

;; Refont current and subsequent line when changes occur
(setq jit-lock-contextually t)

;; Stealth fontification should show status messages
(setq jit-lock-stealth-verbose t)

;; Translate SGR control sequences into text properties
(ansi-color-for-comint-mode-on)

;; When using GUI keep mouse cursor away from Emacs cursor
(if window-system
    (mouse-avoidance-mode 'cat-and-mouse))

;; Yank at point and don't move the cursor
(setq mouse-yank-at-point t)

;; Enable smooth scrolling
(setq scroll-step 1)

;; When scrolling up show 5 previously visible lines
(setq scroll-conservatively 5)

;; Use clipboard to copy/paste
(setq select-enable-clipboard t)

;; Do not put double space between sentences
(setq sentence-end-double-space nil)

;; Make cursor movement stop in between camelCase words
(global-subword-mode 1)

;; Delete duplicates in the Minibuffer's history
(setq history-delete-duplicates t)

;; Enable winner-mode
(winner-mode 1)

;; Don't spawn a dialog box when clicking on buttons or links
(setq use-dialog-box nil)

;; Highlight the current match during search
(setq search-highlight t)

;; Highlight matches during query replacement
(setq query-replace-highlight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === CALENDAR === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq calendar-latitude 56.833333)
(setq calendar-longitude 60.583333)
(setq calendar-location-name "Екатеринбург, Россия")

;; Set start of week to Monday
(setq calendar-week-start-day 1)

;; Use Russian words for week days
(setq calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник" "Среда"
                               "Четверг" "Пятница" "Суббота"])

;; Use Russian words for months
(setq calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])

;; Use Russian words for seasons
(setq solar-n-hemi-seasons '("Весна" "Лето" "Осень" "Зима"))

;; Disable holidays that are not relevant for me
(setq holiday-general-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           ;;;;
;;;; === BACKUP & AUTOSAVE === ;;;;
;;;;                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put backup files to a special directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; Use version for backup files and delete old versions
(setq version-control t)
(setq delete-old-versions t)

;; Put autosave files to a special directory
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === IDENT === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Do not use tabs for indentation
(setq-default indent-tabs-mode nil)

;; Set distance between tab characters
(setq tab-width 2)

;; Set indentation level for CC modes
(setq c-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === UTF-8 === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Enable UTF-8 encoding
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Make Emacs accept UTF-8 (spelled uppercase) encoding
(define-coding-system-alias 'UTF-8 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === SPELLING === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)

;; Use aspell to spellcheck
(with-eval-after-load "ispell"
  (setq ispell-program-name "/usr/bin/aspell"))

(defun switch-dictionary (choice)
   "Switch between language dictionaries (optionally switched to CHOICE value)."
   (interactive "cChoose:  (1) English | (2) Русский")
    (cond ((eq choice ?1)
           (setq ispell-dictionary "english")
           (ispell-kill-ispell)
           (message "Switched to English."))
          ((eq choice ?2)
           (setq ispell-dictionary "russian")
           (ispell-kill-ispell)
           (message "Switched to Russian."))
          (t (message "No changes have been made."))))

(global-set-key (kbd "C-c M-s") 'switch-dictionary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         ;;;;
;;;; === AUTH-PASS-STORE === ;;;;
;;;;                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable auth-source-password-store
(require 'auth-source-pass)
(auth-source-pass-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === USE-PACKAGE === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable use-package infrastructure
(require 'use-package)

;; Enable imenu support for use-package
(setq use-package-enable-imenu-support t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === ENVIRONMENT === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === SERVER === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Start Emacs server to connect to it with emacsclient
(use-package server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === FONT-LOCK === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))

;;;;;;;;;;;;;;;;;;;;;;
;;;;              ;;;;
;;;; === ESUP === ;;;;
;;;;              ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package esup)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === DIMMER === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable dimmer
(use-package dimmer
  :config
  ;; Better handling for which-key
  (dimmer-configure-which-key)
  ;; Better handling for org
  (dimmer-configure-org)
  ;; Enable by default
  (dimmer-mode t))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === ASYNC === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Load async
(use-package async
  :config
  ;; Enable asynchronous compilation
  (async-bytecomp-package-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === BIND-KEY === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load bind-key
(use-package bind-key
  ;; Use C-h B to show a list of user-defined bindings
  :bind ("C-h B" . describe-personal-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;; === BROWSE-KILL-RING === ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load browse-kill-ring
(use-package browse-kill-ring
  ;; Set up default keybindings
  :config (browse-kill-ring-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === BEGINEND === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load beginend
(use-package beginend
  :config (beginend-global-mode)
  :diminish (beginend-bs-mode
             beginend-compilation-mode
             beginend-dired-mode
             beginend-elfeed-search-mode
             beginend-global-mode
             beginend-ibuffer-mode
             beginend-magit-status-mode
             beginend-message-mode
             beginend-notmuch-search-mode
             beginend-occur-mode
             beginend-org-agenda-mode
             beginend-org-mode
             beginend-outline-mode
             beginend-prodigy-mode
             beginend-prog-mode
             beginend-recentf-dialog-mode
             beginend-vc-dir-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === ESHELL === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Load eshell
(use-package eshell
  :config
  (setq eshell-banner-message "" ;; Do not show anything in front of eshell prompt
        eshell-history-size 1024 ;; History size
        eshell-hist-ignoredups t ;; Ignore duplicates
        eshell-scroll-to-bottom-on-input 'all))

;; Set environment variables
(setenv "DEBEMAIL" "dogsleg@debian.org")
(setenv "DEBFULLNAME" "Lev Lamberov")
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

;; Use ido-completing-read in eshell history
(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h")
                           (lambda ()
                             (interactive)
                             (insert
                              (ido-completing-read "Eshell history: "
                                                   (delete-dups
                                                    (ring-elements eshell-history-ring))))))
            (local-set-key (kbd "C-c C-h") 'eshell-list-history)))

;; Load eshell-up
(use-package eshell-up
  :config
  ;; Do not ignore case
  (setq eshell-up-ignore-case nil))

;; Load eshell-bookmark
(use-package eshell-bookmark
  :config
  ;; Integrate bookmark support to Eshell
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

;; Load eshell-git-prompt
(use-package eshell-git-prompt
  :config
  ;; Enable powerline theme
  (eshell-git-prompt-use-theme 'powerline))

;; Load eshell-z
(use-package eshell-z
  :config
  ;; Use eshell-z in eshell
  (add-hook 'eshell-mode-hook
            (defun my-eshell-mode-hook ()
              (require 'eshell-z))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === TRAMP === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Set SSH as a default method for TRAMP
(setq tramp-default-method "ssh")

;; Store backup files locally, not remotely
(setq tramp-backup-directory-alist backup-directory-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === WITH-EDITOR === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package with-editor
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === IBUFFER === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load ibuffer
(use-package ibuffer
  :config
  ;; Do not show empty filter groups
  (setq ibuffer-show-empty-filter-groups nil)
  :bind ("C-x C-b" . ibuffer))

;; Load ibuffer-vc
(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; Make buffer names unique (based on path)
(setq uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;
;;;;             ;;;;
;;;; === IDO === ;;;;
;;;;             ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Load ido
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t))

;; Load smex
(use-package smex
  :bind ("M-x" . smex)
  :config
  (smex-initialize))

;; Load ido-vertical-mode
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; Load ido-ubiquitous
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; Bind `~` to go to homedir when in ido-find-file
(add-hook 'ido-setup-hook
 (lambda ()
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === IEDIT === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Load iedit
(use-package iedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === MOVE-TEXT === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load move-text
(use-package move-text
  ;; Use the default bindings
  :init (move-text-default-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === FLYCHECK === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === POWERLINE === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load powerline
(use-package powerline
  :config
  ;; Use default powerline theme
  (powerline-default-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === EYEBROWSE === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable eyebrowse
(eyebrowse-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;;;;              ;;;;
;;;; === ANZU === ;;;;
;;;;              ;;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load anzu
(use-package anzu
  ;; Do not show anzu in a list of enabled modes
  :diminish anzu-mode
  :config
  ;; Enable anzu globally
  (global-anzu-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === BEACON === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Load beacon
(use-package beacon
  ;; Do not show beacon in a list of enabled modes
  :diminish beacon-mode
  :config
  ;; Enable beacon mode
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === UNDO-TREE === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load undo-tree
(use-package undo-tree
  ;; Do not show undo-tree in a list of enabled modes
  :diminish undo-tree-mode
  :config
  ;; Enable undo-tree globally
  (global-undo-tree-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === ZAP-TO-CHAR === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load zzz-to-char
(use-package zzz-to-char
  ;; Bind some keys to use zzz-to-char
  :bind ("C-M-z" . zzz-to-char)
  ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; === RESTART-EMACS === ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load restart-enacs
(use-package restart-emacs
  ;; Bind a key to restart Emacs
  :bind ("C-x M-c" . restart-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;; === RECURSIVE-NARROW === ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load recursive-narrow
(use-package recursive-narrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; === EXPAND-REGION === ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load expand-region
(use-package expand-region
  ;; Bind a key to expand region
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === HYDRA === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Load hydra
(use-package hydra)

;; Define hydra to hydra-change font size
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;; Define hydra to hydra-change transparency of Emacs window
(defun hydra-transparency-helper (inc)
  "Increase or decrease the selected frame transparency for INC value."
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defhydra hydra-transparency (:columns 2)
  "
  ALPHA : [ %(frame-parameter nil 'alpha) ]
  "
  ("j" (lambda () (interactive) (hydra-transparency-helper +1)) "+ more")
  ("k" (lambda () (interactive) (hydra-transparency-helper -1)) "- less")
  ("J" (lambda () (interactive) (hydra-transparency-helper +10)) "++ more")
  ("K" (lambda () (interactive) (hydra-transparency-helper -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

(bind-key "C-c z" 'hydra-transparency/body)

(defhydra hydra-yasnippet (:color blue)
  "
  ^
  ^YASnippet^          ^Do^
  ^─────────^──────────^──^────────
  _q_ quit             _i_ insert
  ^^                   _m_ mode
  ^^                   _n_ new
  ^^                   ^^
  "
  ("q" nil)
  ("i" yas-insert-snippet)
  ("m" yas-minor-mode)
  ("n" yas-new-snippet))

(bind-key "C-c s" 'hydra-yasnippet/body)

;;;;;;;;;;;;;;;;;;;;;
;;;;             ;;;;
;;;; === FCI === ;;;;
;;;;             ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Load fill-column-indicator
(use-package fill-column-indicator
  :config
  ;; Set column to show fill-column-indicator
  (setq fci-rule-column '80)
  ;; Set fill-column-indicator color
  (setq fci-rule-color "orange"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === HIGHLIGHT === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load highlight-indentation
(use-package highlight-indentation
  :config
  ;; Set highlight-indentation-face to darker dim grey
  (set-face-background 'highlight-indentation-face "#333333")
  ;; Set highlight-indentation-current-column-face to dim grey
  (set-face-background 'highlight-indentation-current-column-face "#646464")
  :diminish (highlight-indentation-mode
             highlight-indentation-current-column-mode))

;; Enable highlight-indentation-mode in programming modes
(add-hook 'prog-mode-hook #'highlight-indentation-mode)

;; Enable highlight-indentation-current-column-mode in programming modes
(add-hook 'prog-mode-hook #'highlight-indentation-current-column-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === ZTREE === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Load ztree
(use-package ztree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           ;;;;
;;;; === DISCOVER-MY-MAJOR === ;;;;
;;;;                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind a key to show hints on current major mode
(bind-key "C-h M-m" 'discover-my-major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === WHICH-KEY === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load which-key and diminish it
(use-package which-key
  :diminish which-key-mode)

;; Enable which-key
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === SHUT UP! === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make Emacs silent when it is run non-interactively
(when noninteractive
  (shut-up-silence-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === PARENS === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Load paren
(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0))

;; Enable automatic parens pairing
(electric-pair-mode 1)

;; Use colorful parens in all programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable paredit
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                   ;;;;
;;;; === AVY, ACE-LINK, ACE-WINDOW, ACE-POPUP-MENU === ;;;;
;;;;                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load avy
(use-package avy
  :config
  ;; Use default avy settings
  (avy-setup-default)
  ;; Bind some keys to use avy-jumping
  :bind ("C-:" . avy-goto-char)
        ("M-g f" . avy-goto-line)
        ("M-g w" . avy-goto-word-0)
        ("M-g e" . avy-goto-word-1))

;; Load ace-link
(use-package ace-link
  :config
  ;; Use default ace-link settings
  (ace-link-setup-default))

;; Load ace-window
(use-package ace-window
  ;; Bind a key to change windows with ace-window
  :bind ("M-g z" . ace-window))

;; Load ace-popup-menu
(use-package ace-popup-menu
  :config (ace-popup-menu-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === VIMISH-FOLD === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load vimish-fold
(use-package vimish-fold
  ;; Bind keys to fold/unfold using vimish-fold
  :bind ("M-g v f" . vimish-fold)
        ("M-g v v" . vimish-fold-delete))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === MAGIT === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Bind some keys to use with magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Enable magit-todos
(use-package magit-todos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; === PROJECTILE === ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load projectile
(use-package projectile
  :config (projectile-mode +1)
  ;; Bind key to projectile-command-map
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === ELFEED === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Define feeds to look up with elfeed
(setq elfeed-feeds
      '( ("http://planet.debian.org/rss20.xml" debian)
         ("http://www.debian.org/News/news" debian)
         ("http://www.debian.org/security/dsa.en.rdf" debian)
         ("https://micronews.debian.org/feeds/feed.rss" debian)
         ("http://lwn.net/headlines/rss" foss)
         ("https://opensource.com/feed" foss)
         ("http://planet.fsfe.org/en/rss20.xml" foss)
         ("http://www.opennet.ru/opennews/opennews_all.rss" ru-foss)
         ("http://feeds.feedburner.com/org/LOR" ru-foss)
         ("http://feeds.feedburner.com/rus-linux/VAK" ru-foss)
         ("http://zenway.ru/feed" ru-foss)
         ("http://urfu.ru/ru/?type=2" ekb)
         "https://yakking.branchable.com/blog/index.rss"
         "http://feeds.feedburner.com/Phoronix"
         "http://slashdot.org/index.rss"
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === ENGINE-MODE === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load engine-mode
(use-package engine-mode
  :config
  ;; Define DuckDuckGo search engine
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  ;; Define Wikipedia search engine
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  ;; Enable engine-mode globally
  (engine-mode t))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === DIRED === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Enable dired-find-alternate-file for all sessions
(put 'dired-find-alternate-file 'disabled nil)

;; Enable async in dired
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Set format of ls output for dired
(setq dired-listing-switches "-lGha --group-directories-first")

;; Allow editing file permissions
(setq wdired-allow-to-change-permissions t)

;; Easily copy/move from one dir to the next dir shown in a split
(setq dired-dwim-target t)

;; Do not hide symbolic link targets
(setq dired-hide-details-hide-symlink-targets nil)

;; Always copy recursively
(setq dired-recursive-copies 'always)

;; Define programs to open specific files
(setq dired-open-extensions
      '(("djvu" . "zathura")
        ("mkv" . "mplayer")
        ("avi" . "mplayer")))

;; Automatic refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Load dired-quick-sort
(use-package dired-quick-sort)
(dired-quick-sort-setup)

;; Load dired-rsync
(use-package dired-rsync
  :config
  ;; Bind a key to run dired-rsync
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

;; Enable sxiv
(use-package sxiv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         ;;;;
;;;; === SYSTEM-PACKAGES === ;;;;
;;;;                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable system-packages
(use-package system-packages
  :config
  ;; Don't use sudo
  (setq system-packages-use-sudo nil)
  ;; Use apt
  (setq system-packages-package-manager 'apt))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === ELDOC === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Enable documentation in several modes
(add-hook 'cperl-mode-hook 'eldoc-mode)
(add-hook 'eshell-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Show eldoc output faster
(setq eldoc-idle-delay 0.1)

;; Don't allow eldoc to resize eacho area display
(setq eldoc-echo-area-use-multiline-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; === HUNGRY-DELETE === ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hungry-delete)

;; Enable hungry-delete globally
(global-hungry-delete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      ;;;;
;;;; === GOLDEN-RATIO === ;;;;
;;;;                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package golden-ratio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               ;;;;
;;;; === LINUM, LINUM-RELATIVE === ;;;;
;;;;                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line numbers (globally)
(global-display-line-numbers-mode)

;; Disable display-line-numbers-mode in pdf-tools-mode
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in eshell
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in EWW
(add-hook 'eww-after-render-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in notmuch-hello
(add-hook 'notmuch-hello-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in notmuch-search
(add-hook 'notmuch-search-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in notmuch-show
(add-hook 'notmuch-show-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in org-mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Disable display-line-numbers-mode in nov
(add-hook 'nov-mode-hook (lambda () (display-line-numbers-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === PDF-TOOLS === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :config
  ;; Open PDF files scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; Better zooming
  (setq pdf-view-resize-factor 1.1)
  ;; Automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; Use ordinary isearch
  :bind ("C-s" . isearch-forward))

(pdf-tools-install)

;;;;;;;;;;;;;;;;;;;;;
;;;;             ;;;;
;;;; === NOV === ;;;;
;;;;             ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Set width of text in nov-mode to 80 characters
(setq nov-text-width 80)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === NOTMUCH === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load notmuch
(use-package notmuch
  :config
  ;; Show newest messages first
  (setq notmuch-search-oldest-first nil)
  ;; Display all specified headers by default
  (setq notmuch-message-headers-visible t)
  ;; Check signatures automatically
  (setq notmuch-crypto-process-mime t)
  ;; Hide deleted and spam tags
  (setq notmuch-hello-hide-tags '("deleted" "spam")))

;; Define saved searches
(setq notmuch-saved-searches '((:name "unread" :query "tag:inbox AND tag:unread" :key "u")
                               (:name "inbox" :query "tag:inbox" :key "i")
                               (:name "local" :query "tag:local" :key "l")
                               (:name "debian-mailing-lists" :query "to:lists.debian.org")
                               (:name "bugs" :query "tag:bugs" :key "b")))

;; Read and verify encrypted and signed MIME messages
(setq notmuch-crypto-process-mime t)

;; Bind "U" to remove tag unread in search-mode
(define-key notmuch-search-mode-map "u"
  (lambda (&optional beg end)
    "Remove unread tag"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-remove-tag (list "-unread") beg end)))

;; Bind "S" to tag as spam in search-mode
(define-key notmuch-search-mode-map "S"
  (lambda (&optional beg end)
    "Tag thread as spam"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "+spam" "-inbox") beg end)))

;; Bind "S" to toggle spam tag in show-mode
(define-key notmuch-show-mode-map "S"
  (lambda ()
    "Toggle spam tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-spam"))
      (notmuch-show-tag (list "+spam")))))

;; Bind "d" to toggle deleted tag in show-mode
(define-key notmuch-show-mode-map "d"
  (lambda ()
    "Toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted")))))

;; Bind "D" to tag as deleted in search-mode
(define-key notmuch-search-mode-map "D"
  (lambda (&optional beg end)
    "Tag thread as deleted"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "+deleted" "-inbox") beg end)))

;;;;;;;;;;;;;;;;;;;;;;
;;;;              ;;;;
;;;; === SMTP === ;;;;
;;;;              ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package smtpmail-async)

;; Send mails asynchronously
(setq message-send-mail-function 'async-smtpmail-send-it
      ;; Use ssl
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "dogsleg"
      smtpmail-default-smtp-server "mail.riseup.net"
      smtpmail-smtp-server "mail.riseup.net"
      smtpmail-smtp-service 465
      ;; Enable debugging
      smtpmail-debug-info t)

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Define citation format
(setq message-citation-line-format "%a %d %b %Y @ %R %f:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;;;;;;;;;;;;;;;;;;;;;;
;;;;              ;;;;
;;;; === GNUS === ;;;;
;;;;              ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-select-method '(nntp "news.gnus.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === DUMB-JUMP === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load dumb-jump
(use-package dumb-jump
  :config
  ;; Enable dumb-jump
  (dumb-jump-mode)
  ;; Make dumb-jump silent
  (setq dumb-jump-quiet t)
  ;; Prefer ag
  (setq dumb-jump-prefer-searcher 'ag))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === DIMINISH === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load diminish
(use-package diminish
  :config
  ;; Show abbreviations instead of some modes
  (diminish 'auto-revert-mode "♻ ")
  (diminish 'eldoc-mode "⁈ ")
  (diminish 'flycheck-mode "↑ ")
  (diminish 'hungry-delete-mode "☣ ")
  (diminish 'paredit-mode "() ")
  (diminish 'rainbow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; === VISUAL-REGEXP === ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package visual-regexp
  ;; Bind a key to repalce with visual-regexp
  :bind ("C-c r" . vr/replace)
  ;; Bind a key to query-replace with visual-regexp
  ("C-c q" . vr/query-replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === ORG-MODE === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)
(require 'org-contacts)

;; Bind some useful keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Set agenda files
(setq org-agenda-files '("~/Documents/Org/agenda.org"))

;; Show ⤵ instead of (...)
(setq org-ellipsis "⤵")

;; Record DONE time
(setq org-log-done t)

;; Archive DONE stuff to *.org_archive
(setq org-archive-location "%s_archive::")

;; Define my status keywords
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAIT(w)" "PROJ(p)" "|" "DONE(d)" "CANCELLED(c)")))

;; Do not DONE supertask in case there are still any TODO subtask
(setq org-enforce-todo-dependencies t)

(use-package org-bullets
  :config
  ;; Enable org-bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; Use these cool bullets to mark levels
  (setq org-bullets-bullet-list '("⊢" "⋮" "⋱" "⸳")))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === AUCTEX === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package tex-site
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

;; Use xetex as TeX engine
(setq-default TeX-engine 'xetex)

;; Revert PDF buffer after successful compilation of TeX file
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Use pdf-tools with auctex
(add-hook 'LaTeX-mode-hook 'pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
      TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

;; Load RefTeX after AUCTeX
(use-package reftex :after auctex)

;; Prompt for empty optional arguments in cite
(setq reftex-cite-prompt-optional-args t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === YASNIPPET === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load yasnippet
(use-package yasnippet
  ;; Enable yasnippet globally
  :config (yas-global-mode 1)
  ;; Bind keys
  :bind (("C-c y l" . yas-describe-tables)
         ("C-c y x" . yas-expand)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y d" . yas-load-directory)
         ("C-c y n" . yas-new-snippet)
         ("C-c y a" . yas-reload-all)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y f" . yas-visit-snippet-file)))

(use-package yasnippet-snippets
  ;; Initialize yasnippet-snippets after yasnippet itself
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === KEYBINDINGS === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-current-buffer ()
  "Function to kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; Bind a key to kill the current buffer
(bind-key "C-x C-k" 'kill-current-buffer)

;; Bind a key to check the current buffer with flyspell
(bind-key "<f12>" 'flyspell-buffer)

;; Cycle through amounts of spacing
(bind-key "M-SPC" 'cycle-spacing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === AUTOCOMPILE === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autocompile nil
  "Autocompile function."
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
                                                     default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === LOREM-IPSUM === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lorem ()
  "Insert Lorem ipsum filling text."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;
;;;; === INSERT-DATE === ;;;;
;;;;                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-date ()
  "Insert current date and time."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === SPLITTING === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-switch ()
  "Split vertically and switch to other buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun hsplit-switch ()
  "Split horisontally and switch to other buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

;; Bind keys to v/h split and switch to other buffer
(bind-keys
 ("C-x 2" . vsplit-switch)
 ("C-x 3" . hsplit-switch))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === ALIGN === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun align-equals (begin end)
  "Align equal symbols from BEGIN to END."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))

(bind-key "C-c =" 'align-equals)

(defun align-arrows (begin end)
  "Align arrow symbols from BEGIN to END."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)->" 1 1))

(bind-key "C-c -" 'align-arrows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; === SORT-WORDS === ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-words (reverse begin end)
  "Sort words in region alphabetically, in REVERSE if negative, BEGIN to END.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" begin end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; === WHITESPACE === ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load whitespace
(use-package whitespace)

;; Delete trailing whitespaces on file write
(add-hook 'write-file-hooks
          'delete-trailing-whitespace)

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  "Don't show trailing whitespace in some modes."
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'eshell-mode-hook
          'no-trailing-whitespace)
(add-hook 'info-mode-hook
          'no-trailing-whitespace)
(add-hook 'diff-mode-hook
          'no-trailing-whitespace)
(add-hook 'elfeed-show-mode-hook
          'no-trailing-whitespace)
(add-hook 'elfeed-search-mode-hook
          'no-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === MODES === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; *.mdwn files are in Markdown format
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

;; Cask files are also Emacs Lisp files
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; *.epub files are to open in nov-mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === PROLOG === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Load ediprolog
(use-package ediprolog
  :bind ("<f11>" . ediprolog-dwim))

;;;;;;;;;;;;;;;;;;;;;;
;;;;              ;;;;
;;;; === PERL === ;;;;
;;;;              ;;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

(defun pde-perl-mode-hook ()
  "Configure cperl."
  (abbrev-mode t)
  (add-to-list 'cperl-style-alist
               '("PDE"
                 (cperl-auto-newline                         . t)
                 (cperl-brace-offset                         . 0)
                 (cperl-close-paren-offset                   . -4)
                 (cperl-continued-brace-offset               . 0)
                 (cperl-continued-statement-offset           . 4)
                 (cperl-extra-newline-before-brace           . nil)
                 (cperl-extra-newline-before-brace-multiline . nil)
                 (cperl-indent-level                         . 4)
                 (cperl-indent-parens-as-block               . t)
                 (cperl-label-offset                         . -4)
                 (cperl-merge-trailing-else                  . t)
                 (cperl-tab-always-indent                    . t)))
  (cperl-set-style "PDE"))

;; Make eldoc and cperl-mode friends
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for eldoc."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

(add-hook 'cperl-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-documentation-function)
                 'my-cperl-eldoc-documentation-function)))

;; Use perltidy from Emacs
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

;; Better style
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === PYTHON === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar python_version_to_use "3")

(use-package elpy
  :init
  (elpy-enable))

;; Use ipython3 as Python interpreter
(setq python-shell-interpreter "ipython3")

;; Load ipython3 with some specific options
(setq python-shell-interpreter-args "--pdb --nosep --classic")

;; Change some regexps to better handling Python interpreter
(setq python-shell-prompt-regexp ">>> ")
(setq python-shell-prompt-output-regexp "")

;; Better completion in ipython3
(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(add-hook 'python-mode-hook 'fci-mode)

(add-hook 'python-mode-hook
  (lambda()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq python-indent-offset 4)))

;; Load pi-isort
(use-package py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === COMPANY === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

;;; init.el ends here
