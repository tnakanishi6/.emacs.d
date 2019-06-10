(setq warning-minimum-level :emergency)
;; initial settings
(require 'cl)
(global-set-key "\C-z" nil)

;; enviroment settings
(defun mac? ()    (string-match "apple-darwin" system-configuration))
(defun linux? ()  (string-match "linux" system-configuration))
(defun win? ()    (string-match "mingw" system-configuration))
(defun use-proxy? () nil)
(defun http-proxy-host () "")
(defun https-proxy-host () "")

(if (use-proxy?)  (setq url-proxy-services (list (cons "http" (http-proxy-host)) (cons "https" (https-proxy-host)))))

(if (win?) (progn (setq w32-lwindow-modifier 'super)
                  (set-face-attribute 'default nil :family "Consolas" :height 104);; font 設定
                  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "ＭＳ ゴシック"));; font 設定
                  (setq face-font-rescale-alist '(("ＭＳ ゴシック" . 1.08)));; font 設定
                  (setq scheme-program-name "gosh -i")
                  (setenv "PATH"(concat "c:/Program Files (x86)/PuTTY;" (getenv "PATH")))
                  (add-to-list 'exec-path"c:/Program Files (x86)/PuTTY;")))
(if (mac?) (progn
             (setq scheme-program-name "/Users/nakanishi/homebrew/bin/gosh -i")
             (let* ((font-family "Source Code Pro")
                    (font-size 12)
                    (font-height (* font-size 10))
                    (jp-font-family "Ricty Diminished")
                    ;;(jp-font-family "Hiragino Kaku Gothic ProN")
                    )
               (set-face-attribute 'default nil :family font-family :height font-height)
               (let ((name (frame-parameter nil 'font))
                     (jp-font-spec (font-spec :family jp-font-family))
                     (jp-characters '(katakana-jisx0201
                                      cp932-2-byte
                                      japanese-jisx0212
                                      japanese-jisx0213-2
                                      japanese-jisx0213.2004-1))
                     (font-spec (font-spec :family font-family))
                     (characters '((?\u00A0 . ?\u00FF)    ; Latin-1
                                   (?\u0100 . ?\u017F)    ; Latin Extended-A
                                   (?\u0180 . ?\u024F)    ; Latin Extended-B
                                   (?\u0250 . ?\u02AF)    ; IPA Extensions
                                   (?\u0370 . ?\u03FF)))) ; Greek and Coptic
                 (dolist (jp-character jp-characters)
                   (set-fontset-font name jp-character jp-font-spec))
                 (dolist (character characters)
                   (set-fontset-font name character font-spec))
                 (add-to-list 'face-font-rescale-alist (cons jp-font-family 1))))))

(if (linux?) (progn 
               (let* ((font-family "Source Code Pro")
                      (font-size 10)
                      (font-height (* font-size 10))
                      (jp-font-family "Ricty Diminished")
                      ;;(jp-font-family "Hiragino Kaku Gothic ProN")
                      )
                 (set-face-attribute 'default nil :family font-family :height font-height)
                 (let ((name (frame-parameter nil 'font))
                       (jp-font-spec (font-spec :family jp-font-family))
                       (jp-characters '(katakana-jisx0201
                                        cp932-2-byte
                                        japanese-jisx0212
                                        japanese-jisx0213-2
                                        japanese-jisx0213.2004-1))
                       (font-spec (font-spec :family font-family))
                       (characters '((?\u00A0 . ?\u00FF)    ; Latin-1
                                     (?\u0100 . ?\u017F)    ; Latin Extended-A
                                     (?\u0180 . ?\u024F)    ; Latin Extended-B
                                     (?\u0250 . ?\u02AF)    ; IPA Extensions
                                     (?\u0370 . ?\u03FF)))) ; Greek and Coptic
                   (dolist (jp-character jp-characters)
                     (set-fontset-font name jp-character jp-font-spec))
                   (dolist (character characters)
                     (set-fontset-font name character font-spec))
                   (add-to-list 'face-font-rescale-alist (cons jp-font-family 1))))))

;; bookmark settings
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-book 'bookmark-arrange-latest-top))

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key "\C-x;" 'comment-region)
(global-set-key "\C-x:" 'uncomment-region)

;; user functions
(defun swap-screen()
  (interactive)
  (let ((thiswin (selected-window)) (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

(defun w32-isearch-update ()
  (interactive)
  (isearch-update))
(define-key isearch-mode-map [compend] 'w32-isearch-update)
                                        ;(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)
(add-hook 'isearch-mode-hook
          (lambda () (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook
          (lambda () (setq w32-ime-composition-window nil)))

(defun current-buffer-mode ()
  "Returns the major mode associated with a buffer."
  (interactive)  (message "%s" major-mode))

(defun php-class-setter-from-string ()
  (interactive)
  (let ((str (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (insert (concat "->set" (s-upper-camel-case str) "($inputData['" str "'])" "\n"))
    (kill-line)))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun php-class-validation-from-string()
  (interactive)
  (let ((str (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (insert (concat "$this->validate['" str "'] = $this->__ValidationRule->get" (s-upper-camel-case str) "ValidationRule();" "\n"))
    (kill-line)))

(defun php-class-getter-function-from-string (str)
  (concat "public function get" (s-upper-camel-case str) "(){" "\n"
          "	return isset($this->" str ") ? $this->" str ": false;\n"
          "}" "\n"))

(defun php-class-setter-function-from-string (str)
  (concat "public function set" (s-upper-camel-case str) "($val){" "\n"
        "	$this->" str " = $val;" "\n"
        "	return $this;" "\n"
        "}" "\n"))

(defun php-class-function-from-string ()
  (interactive)
  (let ((str (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (delete-region (point-at-bol) (point-at-eol))
    (insert (php-class-getter-function-from-string str))
    (insert (php-class-setter-function-from-string str))
    (kill-line)))

(defun projectile-remove-cache-file ()
  (interactive)
  (delete-file "~/.emacs.d/projectile.cache"))
(when (file-exists-p "~/.emacs.d/projectile.cache") 
    (delete-file "~/.emacs.d/projectile.cache"))

;; general settings
(setq initial-scratch-message nil) ;; This buffer〜で始まる文章を表示しない
(setq initial-major-mode 'emacs-lisp-mode) 

(setq  gc-cons-threshold (* 10 gc-cons-threshold))
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 4) ;;scroll
(setq process-kill-without-query t) ;; kill auto sub-process
(when window-system (tool-bar-mode -1)) ;; hide tool-bar
(when window-system (scroll-bar-mode -1)) ;; hide scroll-bar
(menu-bar-mode -1) ;; hide menu-bar

(setq inhibit-startup-message t) ;; hide wellcome page
(setq-default line-spacing 7) ;; line-spacing
(add-to-list 'default-frame-alist '(cursor-type . bar)) ;; cursor-type
(set-frame-parameter nil 'alpha 97) ;; window-transparent
(setq-default tab-width 2 indent-tabs-mode nil) ;;default tab-mode
;;インデントをタブでするかスペースでするか
(setq-default truncate-lines t) (setq-default truncate-partial-width-windows t) ;; no truncate-lines
(setq visible-bell t) (setq ring-bell-function 'ignore);; turn off beep
(setq kill-whole-line t) ;; allow kill-line
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backups")) backup-directory-alist))
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/savefiles") t)))
(setq split-height-threshold nil) (setq split-width-threshold nil) ;; window split controll
(show-paren-mode t) ;;
(progn  (cua-mode t)
        (setq cua-enable-cua-keys nil)) ;; rectangle select
(global-hl-line-mode t)
(when (not window-system)
  (custom-set-faces
   '(hl-line ((t (:background "#000000"))))))

;; eol settings
(progn (setq eol-mnemonic-unix "(Unix)")
       (setq eol-mnemonic-dos "(Dos)")
       (setq eol-mnemonic-mac "(Mac)"))

;; keybinding settings
(global-set-key (kbd "C-x SPC") 'cua-set-rectangle-mark)
(global-set-key [f2] 'swap-screen)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "C-x TAB") 'indent-region)
(global-set-key "\M-g" 'goto-line)
(progn (define-prefix-command 'windmove-map)
       (global-set-key (kbd "C-c") 'windmove-map)
       (define-key windmove-map "0" 'delete-window)
       (define-key windmove-map "1" 'delete-other-windows)
       (define-key windmove-map "2" 'split-window-vertically)
       (define-key windmove-map "3" 'split-window-horizontally)) ;; move split window

;; dired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map "\C-f" 'dired-find-file)
                             (define-key dired-mode-map "\C-b" 'dired-up-directory)))

;; diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; eshell
(progn
  (add-hook 'set-language-environment-hook
            (lambda ()
              (when (equal "ja_JP.UTF-8" (getenv "LANG"))
                (setq default-process-coding-system '(utf-8 . utf-8))
                (setq default-file-name-coding-system 'utf-8))
              (when (equal "Japanese" current-language-environment)
                (setq default-buffer-file-coding-system 'iso-2022-jp))))
  (defun eshell-on-current-dir (&optional arg)
    "invoke eshell and cd to current directory"
    (interactive "P")
    (let ((dir default-directory))
      (eshell arg)
      (cd dir))
    (eshell-emit-prompt)
    (goto-char (point-max))))


;;whitespace
(require 'whitespace)
(setq whitespace-style '(face trailings spaces space-mark tab-mark tabs))
(setq whitespace-action '(auto-cleanup))
(setq whitespace-space-regexp "\\(\u3000+\\)")

(setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1]) (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)

;; org
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map "\C-a" 'seq-home)
                           (define-key org-mode-map "\C-e" 'seq-end)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"));; MELPAを追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"));; Marmaladeを追加
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/"));; 
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)



(use-package doom-themes
  :ensure t
  ;;:disabled t
  :config (progn
            (load-theme 'doom-one t)
            (doom-themes-enable-italic t)
            (doom-themes-enable-bold t)))

(use-package material-theme
  :ensure t
  :disabled t
  :config (load-theme 'material t))

(use-package atom-one-dark-theme
  :ensure t
  :disabled t
  :config (load-theme 'atom-one-dark t))

(use-package monokai-theme
  :ensure t
  :disabled t
  :config (load-theme 'monokai t))


(use-package elscreen
  :ensure t
  :config (progn (setq elscreen-prefix-key (kbd "C-z"))
                 (elscreen-start)
                 (setq elscreen-tab-display-kill-screen nil)
                 (setq elscreen-tab-display-control nil)
                 (setq elscreen-buffer-to-nickname-alist
                       '(("^dired-mode$" .
                          (lambda ()
                            (format "Dired(%s)" dired-directory)))
                         ("^Info-mode$" .
                          (lambda ()
                            (format "Info(%s)" (file-name-nondirectory Info-current-file))))
                         ("^mew-draft-mode$" .
                          (lambda ()
                            (format "Mew(%s)" (buffer-name (current-buffer)))))
                         ("^mew-" . "Mew")
                         ("^irchat-" . "IRChat")
                         ("^liece-" . "Liece")
                         ("^lookup-" . "Lookup")))
                 (setq elscreen-mode-to-nickname-alist
                       '(("[Ss]hell" . "shell")
                         ("compilation" . "compile")
                         ("-telnet" . "telnet")
                         ("dict" . "OnlineDict")
                         ("*WL:Message*" . "Wanderlust")))

))

(use-package helm-elscreen 
  :ensure t
  :config (global-set-key (kbd "C-z b") 'helm-elscreen))

(use-package company-tabnine 
  :ensure t
  :config (add-to-list 'company-backends #'company-tabnine)
)

(use-package company
  :ensure t
  :config (progn (global-company-mode)
                 (setq company-idle-delay 0)
                 ;; Number the candidates (use M-1, M-2 etc to select completions).
                 (setq company-show-numbers t)
                 ;; Use the tab-and-go frontend.
                 ;; Allows TAB to select and complete at the same time.
                 (company-tng-configure-default)
                 (setq company-frontends
                       '(company-tng-frontend
                         company-pseudo-tooltip-frontend
                         company-echo-metadata-frontend))
                 (setq company-auto-expand t)
                 (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
                 (define-key company-active-map (kbd "C-p") 'company-select-previous)
                 (define-key company-search-map (kbd "C-n") 'company-select-next)
                 (define-key company-search-map (kbd "C-p") 'company-select-previous)
                 (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
                 (define-key company-active-map (kbd "RET") 'company-complete-selection) ;; TABで候補を設定
                 (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
                 (define-key company-active-map (kbd "C-h") nil) ;; C-hはバックスペース割当のため無効化
                 (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
                 (define-key global-map (kbd "C-M-i") 'company-complete)))

(use-package smartparens
  :ensure t
  :config (progn (smartparens-global-mode t)
                 (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
                 (sp-local-pair 'scheme-mode "'" nil :actions nil)))

(use-package anzu
  :ensure t
  :config (global-anzu-mode +1))

(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t))

(use-package php-boris-minor-mode
  :ensure t
  :defer t
  :config (setq php-scratch-boris-command "~/bin/boris"))

(use-package php-mode
  :ensure t
  :defer t
  :mode (("\.php$" . php-mode))
  :bind (("M-p p" . php-class-function-from-string)
         ("M-p s" . php-class-setter-from-string)
         ("M-p v" . php-class-validation-from-string))
  :config (progn (add-hook 'php-mode-hook (lambda ()
                                            (setq c-basic-offset 4)
                                            (setq tab-width 4)
                                            (setq indent-tabs-mode t)
                                            (c-set-offset 'case-label' 4)
                                            (c-set-offset 'arglist-intro' 4)
                                            (c-set-offset 'arglist-cont-nonempty' 4)
                                            (c-set-offset 'arglist-close' 0)))))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t))

(use-package color-moccur
  :ensure t
  :defer t
  :config (setq moccur-split-word t)
  :bind (("M-o" . occur-by-moccur )))

(use-package moccur-edit
  :ensure t
  :defer t)

(use-package esup
  :ensure t
  :defer t)

(use-package sequential-command
  :ensure t
  :defer t
  :config (progn (require 'sequential-command)
                 (define-sequential-command seq-home
                   beginning-of-line beginning-of-buffer seq-return)
                 (define-sequential-command seq-end
                   end-of-line end-of-buffer seq-return))
  :bind (("\C-a" . seq-home)
         ("\C-e" . seq-end)))

(use-package yasnippet
  :ensure t
  :init
  ;; (bind-keys :map yas-keymap
  ;;            ("C-n" . yas-next-field-or-maybe-expand)
  ;;            ("C-p" . yas-prev))
  :config (progn (yas-global-mode 1)
                 (setq yas-snippet-dirs
                       (list "~/.emacs.d/snippets")))
  ;; :bind (("C-x i i" . yas-insert-snippet)
  ;;        ("C-x u n" . yas-new-snippet)
  ;;        ("C-x i v" . yas-visit-snippet-file))
  )

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-x SPC" . ace-jump-mode)
         ("C-c SPC" . ace-jump-mode)))

(use-package avy
  :ensure t
  :config (progn (setq avy-timeout-seconds 0.4))
  :bind (("M-g" . avy-goto-line)
         ("C-;" . avy-goto-word-1)
         ("C-:" . avy-goto-word-1)))

(defun add-keys-to-ace-jump-mode (prefix c &optional mode)
  (define-key global-map
    (read-kbd-macro (concat prefix (string c)))
    `(lambda ()
       (interactive)
       (funcall #'avy-goto-word-1 ,c))))

(loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "s-" c))
(loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "s-" c))

(use-package helm
  :ensure t
  :config (progn (helm-mode +1)
                 (global-set-key (kbd "M-x") 'helm-M-x)
                 (global-set-key (kbd "C-x C-f") 'helm-find-files)
                 (global-set-key (kbd "C-x b") 'helm-mini)
                 (global-set-key (kbd "C-c i") 'helm-imenu)
                 (global-set-key (kbd "C-x C-r") 'helm-recentf)
                 (global-set-key (kbd "C-x r l") 'helm-bookmarks)
                 (define-key helm-find-files-map "\C-f" 'helm-execute-persistent-action)
                 (define-key helm-find-files-map "\C-k" 'kill-line)
                 (define-key helm-find-files-map "\C-h" 'delete-backward-char)
                 (define-key helm-find-files-map "\C-b" 'helm-find-files-up-one-level)
                 (define-key helm-read-file-map "\C-f" 'helm-execute-persistent-action)
                 (define-key helm-read-file-map "\C-k" 'kill-line)
                 (define-key helm-read-file-map "\C-b" 'helm-find-files-up-one-level)
                 (define-key helm-find-files-map "\C-k" 'kill-line)
                 
                 (setq helm-mode-fuzzy-match t)
                 (setq helm-imenu-fuzzy-match t)
                 (setq helm-recentf-fuzzy-match t)
                 (setq helm-buffers-fuzzy-matching t)
                 (setq helm-M-x-fuzzy-match t)
                 (setq helm-mode-fuzzy-match t)
                 (setq helm-completion-in-region-fuzzy-match t)
                 (setq helm-candidate-number-limit 25)
                 (global-set-key (kbd "M-y") 'helm-show-kill-ring)))

(use-package helm-ext
  :ensure t
  :config (progn
            ;;(helm-ext-ff-enable-skipping-dots t)
    (setq helm-ext-ff-skipping-dots-recenter t)
    (helm-ext-ff-enable-auto-path-expansion t)
    (helm-ext-minibuffer-enable-header-line-maybe t)))

(use-package swiper-helm
  :ensure t
  :config (progn (global-set-key (kbd "M-i") 'swiper)))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package smartrep
  :ensure t 
  :config (progn (declare-function smartrep-define-key "smartrep")
                 (global-unset-key "\C-t")
                 (smartrep-define-key global-map "C-t"
                   '(("C-t"      . 'mc/mark-next-like-this)
                     ("n"        . 'mc/mark-next-like-this)
                     ("p"        . 'mc/mark-previous-like-this)
                     ("m"        . 'mc/mark-more-like-this-extended)
                     ("u"        . 'mc/unmark-next-like-this)
                     ("U"        . 'mc/unmark-previous-like-this)
                     ("s"        . 'mc/skip-to-next-like-this)
                     ("S"        . 'mc/skip-to-previous-like-this)
                     ("*"        . 'mc/mark-all-like-this)
                     ("d"        . 'mc/mark-all-like-this-dwim)
                     ("i"        . 'mc/insert-numbers)
                     ("o"        . 'mc/sort-regions)
                     ("O"        . 'mc/reverse-regions)))))

(use-package git-gutter
  :ensure t
  :config (progn (global-git-gutter-mode t)
                 (setq git-gutter:window-width 2)
                 (set-face-foreground 'git-gutter:added  "green")
                 (set-face-foreground 'git-gutter:deleted  "yellow")
                 (set-face-background 'git-gutter:modified "magenta")
                 (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
                 (smartrep-define-key
                     global-map  "C-x" '(("p" . 'git-gutter:previous-hunk)
                                         ("n" . 'git-gutter:next-hunk)))))

(use-package s
  :ensure t)

(use-package pcre2el
  :ensure t
  :disabled t)

(use-package visual-regexp-steroids
  :ensure t
  :defer t
  :config (setq vr/engine 'pcre2el) 
  :bind (("C-M-%" . vr/query-replace)
         ("C-c m" . vr/mc-mark)
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward)))

(use-package paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'scheme-mode-hook
              (lambda ()
                (enable-paredit-mode)
                (define-key scheme-mode-map "\C-h" 'paredit-backward-delete)))))

(add-hook 'scheme-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package web-mode
  :ensure t
  :config (progn (add-hook 'web-mode-hook (lambda ()
                                            (setq tab-width 4)
                                            (setq indent-tabs-mode t))))
  :mode (("\\.ctp" . web-mode)
         ("\\.html" . web-mode)
         ("\\.vue" . web-mode)))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj" . clojure-mode)
         ("\\.cljs" . clojure-mode)
         ("\\.cljc" . clojure-mode)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package org-present
  :ensure t
  :defer t
  :config (progn (add-hook 'org-present-mode-hook
                           (lambda ()
                             (org-present-big)
                             (org-display-inline-images)
                             (org-present-hide-cursor)
                             (org-present-read-only)))
                 (add-hook 'org-present-mode-quit-hook
                           (lambda ()
                             (org-present-small)
                             (org-remove-inline-images)
                             (org-present-show-cursor)
                             (org-present-read-write)))
                 (setq org-present-text-scale 5)
                 (define-key org-present-mode-keymap (kbd "C-c C-;") 'org-present-big)))

(use-package scss-mode
  :ensure t
  :defer t
  :init
  (add-hook 'scss-mode-hook  '(lambda () (set (make-local-variable 'scss-compile-at-save) nil))))

(use-package flycheck
  :ensure t
  :init
  (progn (add-hook 'php-mode-hook 'flycheck-mode)
         (add-hook 'web-mode-hook 'flycheck-mode)))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode (("\\.js$" . rjsx-mode)
         ("\\.jsx$" . rjsx-mode))
  :config (progn
            (add-hook 'rjsx-mode-hook (lambda ()
                                        (setq indent-tabs-mode nil)
                                        (setq js-indent-level 2)
                                        (setq js2-strict-missing-semi-warning nil)))))


(use-package js2-mode
  :disabled t
  :ensure t
  :defer t
  :mode (("\\.js$" . js2-jsx-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :config (progn
            (add-hook 'js-mode-hook (lambda ()
                                      (setq js2-cleanup-whitespace nil
                                            js2-mirror-mode nil
                                            js2-bounce-indent-flag nil
                                            js2-basic-offset 2
                                            js2-strict-inconsistent-return-warning nil
                                            js2-strict-missing-semi-warning nil
                                            js2-missing-semi-one-line-override nil)
                                      
                                      (setq tab-width 2)
                                      (setq indent-tabs-mode nil)))))

(use-package coffee-mode
  :ensure t
  :defer t
  :mode (("\\.coffee$" . js2-mode))
  :config (progn (add-hook 'coffee-mode-hook (lambda ()
                                            (setq tab-width 4)
                                            (setq indent-tabs-mode t)))))

(use-package helm-hunks
  :ensure t
  :defer t
  :commands helm-hunks)

(use-package grizzl 
  :ensure t
  :disabled t)

(use-package helm-projectile
  :ensure t
  :bind (("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile)
         ("C-c p g" . helm-projectile-ag))
  :init (progn
          (projectile-global-mode)
          (helm-projectile-on)
          (setq projectile-switch-project-action 'helm-projectile)
          (defvar helm-source-file-not-found
            (helm-build-dummy-source
                "Create file"
              :action 'find-file))
          (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
          (setq projectile-indexing-method 'alien)
          (setq projectile-use-native-indexing t)
          (setq projectile-enable-caching t)
          (setq projectile-globally-ignored-directories 
                (append '(".git"
                          "gulp/node_modules"
                          "cakephp/lib"
                          "php-vendor"
                          "html.bak") projectile-globally-ignored-directories))))

(defun projectile-remove-cache-file ()
  (interactive)
  (delete-file "~/.emacs.d/projectile.cache"))

(use-package switch-window
:ensure t
:config (progn
          (setq switch-window-shortcut-style 'qwerty)
          (add-hook 'switch-window-finish-hook 'golden-ratio)
          (global-set-key (kbd "C-x o") 'switch-window)))

(setq org-directory "~/org")
(setq org-capture-templates
   '(("t" "Todo" entry (file "~/org/todo.org")
      "* TODO %?\n    %i\n   %a\n    %T")
     ("n" "note" entry (file "~/org/note.org")
      "* %?\n   %a\n    %T")
     ("c" "Code" entry (file "~/org/code.org")
      "* %?\n   %a\n    %T\n    %F")))
(global-set-key "\C-cc" 'org-capture)

;; language settings
(when (win?)
    (progn (set-language-environment "Japanese")
           (prefer-coding-system 'utf-8)
           (set-default-coding-systems 'utf-8)))
(when (linux?)
  (use-package mozc
    :ensure t
    :config (progn
              (require 'mozc)
              (set-language-environment "Japanese")
              (setq default-input-method "japanese-mozc")
              (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
              (prefer-coding-system 'utf-8)
              (setq mozc-candidate-style 'overlay))))


(defun align-regexp-repeated (start stop regexp)
  (interactive "r\nsAlign regexp: ")
  (let ((spacing 1)
        (old-buffer-size (buffer-size)))
    ;; If our align regexp is just spaces, then we don't need any
    ;; extra spacing.
    (when (string-match regexp " ")
      (setq spacing 0))
    (align-regexp start stop
                  ;; add space at beginning of regexp
                  (concat "\\([[:space:]]*\\)" regexp)
                  1 spacing t)    
    (while (read-string "Repeat:")
      (progn     (align-regexp start stop
                              ;; add space at beginning of regexp
                              (concat "\\([[:space:]]*\\)" regexp)
                              1 spacing t)))))

(use-package dashboard
  :ensure t
  :config (progn
      (setq dashboard-startup-banner "~/top.png")
      (dashboard-setup-startup-hook)
      (setq dashboard-items '( (projects . 5)(recents  . 20)))))

(use-package helm-tramp
  :ensure t
  )

(use-package plantuml-mode
  :ensure t
  :mode (("\.puml$" . plantuml-mode) ("\.plantuml$" . plantuml-mode))
  :config (progn
      (powerline-my-theme)))

(use-package google-translate
  :ensure t
  :config (progn
            (global-set-key "\C-ct" 'google-translate-at-point)
            (custom-set-variables
             '(google-translate-default-source-language "en")
             '(google-translate-default-target-language "ja"))))


(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq powerline-current-separator 'utf-8)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                          (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-id nil )
                                     (powerline-raw " [ ")
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-raw "%*" nil)
                                     (powerline-raw " |")
                                     (powerline-process nil)
                                     (powerline-vc)
                                     (powerline-raw " ]")
                                     ))
                          (rhs (list (powerline-raw "%4l" 'l)
                                     (powerline-raw ":" 'l)
                                     (powerline-raw "%2c" 'l)
                                     (powerline-raw " | ")                                  
                                     (powerline-raw "%6p" )
                                     (powerline-raw " ")
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill nil (powerline-width rhs)) 
                             (powerline-render rhs)))))))

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))
(make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#3E4451" 'bold)
(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)

(use-package powerline
  :ensure t
  :config (progn
      (powerline-my-theme)))

(use-package dumb-jump
  :ensure t
  :config (progn
            (setq dumb-jump-force-searcher 'ag)
            (global-set-key (kbd "C-M-w") 'dumb-jump-go-other-window)
            (dumb-jump-mode)))

(use-package foreign-regexp
  :ensure t
  :config (progn
            (custom-set-variables '(foreign-regexp/regexp-type 'perl))
            (global-set-key (kbd "C-M-s") 'foreign-regexp/isearch-forward)
            (global-set-key (kbd "C-M-r") 'foreign-regexp/isearch-backward)
            (global-set-key (kbd "C-M-%") 'foreign-regexp/query-replace)
            '(reb-re-syntax 'foreign-regexp)))

(use-package popwin
  :ensure t
  :config (progn
            (add-to-list 'popwin:special-display-config
                         '("*eshell*" :regexp t :dedicated t :position bottom :height 0.3))
            (add-to-list 'popwin:special-display-config
                         '("*Google Translate*" ))))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "~/.emacs.d/private/settings")

(require 'auto-rsync)
(require 'git-complete)
(require 'operate-chrome)
(global-set-key (kbd "C-c g") 'git-complete)

(auto-rsync-mode t)

(setq open-junk-file-format "~/.tmp/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
(global-set-key (kbd "C-x j") 'open-junk-file)

(global-set-key (kbd "M-z") 'avy-zap-up-to-char)
(require 'tramp)

(require 'ob-js)
(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)

(setf wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

(defhydra hydra-zoom (global-map "C-x C-0") 
  "zoom" 
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
