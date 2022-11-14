(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq visible-bell t)

;; line number
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		neotree-mode-hook
		eshell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable backup
(setq make-backup-files nil)

;; font
(set-face-attribute 'default nil :font "Mononoki Nerd Font" :height 125)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)



;; Style
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;keymap
(use-package general
  :config
  (general-evil-setup t))

(nvmap :prefix "SPC"
       "SPC"   '(counsel-M-x :which-key "M-x")
       "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
       ;;Buffer
       "b b"   '(ibuffer :which-key "Ibuffer")
       "b s"   '(ivy-switch-buffer :which-key "switch buffer list")
       "b k"   '(kill-buffer :which-key "kill buffer list")
       ;;File
       "."     '(find-file :which-key "Find file")
       "d d" '(dired :which-key "Open dired")
       "d j" '(dired-jump :which-key "Dired jump to current")
       ;;Window
       "w s"   '(evil-window-vsplit :which-key "Open vertical split")
       "w w"   '(evil-window-next :which-key "Move to the next window")
       "w t"   '(term :which-key "Run terminal")
       
       ;;font
       "="   '(text-scale-increase :which-key "Increase font")
       "-"   '(text-scale-decrease :which-key "Decrease font")
       ;;org-mode
       "TAB"   '(org-cycle :which-key "fould/unflould")
       "l p" '(org-latex-preview :which-key "show latex in line")
       "o i t" '(org-toggle-inline-images :which-key "toggle in line img")
       "s c" '(flyspell-mode :which-key "Active spell check")
       ;;org-agenda
       "o a"   '(org-agenda :which-key "Org agenda")
       "a l" '(org-agenda-list :which-key "Org agenda list")
       "t l"   '(org-todo-list :which-key "Org tasks")
       ;;code 
       "c c"   '(company-mode :which-key "Active IDE")
       )

;;Ivy
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
  :init (ivy-mode 1)
  :config
  (ivy-mode 1))

;;Evil
(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor)

;;Rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mod . rainbow-delimiters-mode ))

;;which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idle-delay 0.3))

;;Dired
(use-package all-the-icons-dired
  :ensure t
  :init (setq all-the-icons-dired-monochrome nil))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

;;Org-mode

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
(efs/org-font-setup)

(defun efs/org-mode-setup()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files
	'("~/Org"))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;org-img
(setq org-image-actual-width nil)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/Prj")
    (setq projectile-project-search-path '("~/Prj")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

;;Org bable
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

;; Lang
(use-package haskell-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package gdscript-mode
  :init)

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")
(use-package typescript-mode)

;;lisp
(use-package lsp-mode
  :ensure nil
  :commands (lsp lsp-deferred)
  :init
  :config
  (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs
  :after lsp)
(use-package lsp-ui)

;;Java
;;(use-package java
;;  :ensure nil
;;  :after lsp-java)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))


;;Magit
(use-package magit)

;;ispell
(with-eval-after-load "ispell"
  (setenv "LANG" "it_IT.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "it_IT,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "it_IT,en_US"))

;;Tamplates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;;Neotree
(use-package neotree
  :bind (("TAB" . neotree-enter))
  :config
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        ;;neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))


(setq-default neo-show-hidden-files t)

;;vterm
(use-package vterm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm tide typescript-mode rjsx-mode web-mode json-mode dirtree magit java lsp-ui helm-lps lsp-java lisp-java auctex lsp-treemacs lisp-treeemacs company-box company lsp-mode gdscript-mode dired projectile org-bullets all-the-icons-dired which-key rainbow-delimiters doom-mode-line all-the-icons use-package doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
