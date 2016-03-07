;; initial settings
(require 'cl)
(setq initial-major-mode 'emacs-lisp-mode)

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
                  (setq scheme-program-name "gosh -i")))
(if (mac?) (progn (setq ns-command-modifier (quote meta))
                  (setq ns-alternate-modifier (quote super))
                  (setq scheme-program-name "/opt/local/bin/gosh -i")
                  (set-face-attribute 'default nil :family "Menlo"  :height 130) ;; font 設定
                  (set-fontset-font  nil 'japanese-jisx0208  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font 設定
                  (setq face-font-rescale-alist  '((".*Hiragino_Kaku_Gothic_ProN.*" . 0.9))))) ;; font 設定

;; tag
(setq tags-file-name "~/.emacs.d/TAGS")

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

(defun use (package)
  (when (not (package-installed-p package))
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

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

;; general settings
(setq  gc-cons-threshold (* 10 gc-cons-threshold))
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 4) ;;scroll
(setq process-kill-without-query t) ;; kill auto sub-process
(when window-system (tool-bar-mode -1)) ;; hide tool-bar
(when window-system (scroll-bar-mode -1)) ;; hide scroll-bar
(menu-bar-mode -1) ;; hide menu-bar

(setq inhibit-startup-message t) ;; hide wellcome page
(setq-default line-spacing 7) ;; line-spacing
(add-to-list 'default-frame-alist '(cursor-type . bar)) ;; cursor-type
(set-frame-parameter nil 'alpha 95) ;; window-transparent
(setq-default tab-width 2 indent-tabs-mode nil) ;;default tab-mode
(setq-default truncate-lines t) (setq-default truncate-partial-width-windows t) ;; no truncate-lines
(setq visible-bell t) (setq ring-bell-function 'ignore);; turn off beep
(setq kill-whole-line t) ;; allow kill-line
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backups")) backup-directory-alist)) ;; backup file
(setq auto-save-default nil) ;; unauto savea
(setq split-height-threshold nil) (setq split-width-threshold nil) ;; window split controll
(show-paren-mode t) ;;
(progn  (cua-mode t)
        (setq cua-enable-cua-keys nil)) ;; rectangle select
(global-hl-line-mode t)
;; language settings
(progn (set-language-environment "Japanese")
       (prefer-coding-system 'utf-8)
       (set-default-coding-systems 'utf-8))

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
       (define-key windmove-map "b" 'windmove-left)
       (define-key windmove-map "n" 'windmove-down)
       (define-key windmove-map "p" 'windmove-up)
       (define-key windmove-map "f" 'windmove-right)
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
(add-hook 'set-language-environment-hook
          (lambda ()
            (when (equal "ja_JP.UTF-8" (getenv "LANG"))
              (setq default-process-coding-system '(utf-8 . utf-8))
              (setq default-file-name-coding-system 'utf-8))
            (when (equal "Japanese" current-language-environment)
              (setq default-buffer-file-coding-system 'iso-2022-jp))))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face trailings spaces space-mark tab-mark tabs))
(setq whitespace-action '(auto-cleanup))
(setq whitespace-space-regexp "\\(\u3000+\\)")

(defvar my/bg-color "#263238")
(if window-system
  (progn (setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1]) (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
         (set-face-attribute 'whitespace-trailing nil :foreground my/bg-color :foreground "GreenYellow" :underline t)
         (set-face-attribute 'whitespace-space nil    :background my/bg-color :foreground "GreenYellow" :weight 'bold)
         (set-face-attribute 'whitespace-tab nil      :background my/bg-color :foreground "GreenYellow" :weight 'bold))
  (progn (setq whitespace-display-mappings '((space-mark ?\u3000 [?\ ?\ ]) (tab-mark ?\t [?\>?\.?\.?\.] [?\\ ?\t])))
         (set-face-attribute 'whitespace-trailing nil :background "Black"  :foreground "GreenYellow" :underline t)
         (set-face-attribute 'whitespace-space nil    :background "GreenYellow" :foreground my/bg-color :weight 'bold)
         (set-face-attribute 'whitespace-tab nil      :background "Black" :foreground "Blue" :underline t)))
(global-whitespace-mode 1)

;; org
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map "\C-a" 'seq-home)
                           (define-key org-mode-map "\C-e" 'seq-end)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"));; MELPAを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"));; Marmaladeを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(use 'material-theme)
(load-theme 'material t)

(use 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.1)
(setq ac-auto-start 2)
(setq ac-use-menu-map t)
(setq ac-dwim t)
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers ac-source-yasnippet))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140322.321/dict")
(ac-config-default)

(use 'smartparens)
(smartparens-global-mode t)

(use 'anzu)
(global-anzu-mode +1)

(use 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(use 'php-mode)

(use 'undo-tree)
(global-undo-tree-mode t)

(use 'color-moccur)
(setq moccur-split-word t)
(global-set-key (kbd "M-o") 'occur-by-moccur)

(use 'moccur-edit)

(use 'esup)

(use 'sequential-command)
(require 'sequential-command)
(define-sequential-command seq-home
  beginning-of-line beginning-of-buffer seq-return)
(define-sequential-command seq-end
  end-of-line end-of-buffer seq-return)
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)

(use 'yasnippet)
(let ((install-version (aref (cdr (assoc 'yasnippet package-alist)) 0)))
  (setq yas-snippet-dirs
        (list "~/.emacs.d/snippets")))
(yas-global-mode 1)
;(define-key yas-keymap (kbd "C-n") 'yas-next-field-or-maybe-expand)
;(define-key yas-keymap (kbd "C-p") 'yas-prev)
;(custom-set-variables '(yas-trigger-key "TAB"))
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet) ;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet) ;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(use 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(use 'helm)
(helm-mode +1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(define-key helm-find-files-map "\C-f" 'helm-execute-persistent-action)
(define-key helm-find-files-map "\C-k" 'kill-line)
(define-key helm-find-files-map "\C-h" 'delete-backward-char)
(define-key helm-find-files-map "\C-b" 'helm-find-files-up-one-level)
(define-key helm-read-file-map "\C-f" 'helm-execute-persistent-action)
(define-key helm-read-file-map "\C-k" 'kill-line)
(define-key helm-read-file-map "\C-b" 'helm-find-files-up-one-level)
(define-key helm-find-files-map "\C-k" 'kill-line)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


(use 'helm-c-yasnippet)
(global-set-key (kbd "C-c y")  'helm-c-yas-complete)

(use 'geiser)
(setq geiser-racket-binary "/Applications/Racket v6.3/bin/racket")
(setq geiser-active-implementations '(racket))
(setq geiser-repl-read-only-prompt-p nil)

(use 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use 'multiple-cursors)
(use 'smartrep)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
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
    ("O"        . 'mc/reverse-regions)))

(use 'git-gutter)
(global-git-gutter-mode t)
(setq git-gutter:window-width 2)
(set-face-foreground 'git-gutter:added  "green")
(set-face-foreground 'git-gutter:deleted  "yellow")
(set-face-background 'git-gutter:modified "magenta")
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
(global-set-key (kbd "C-x n")  'git-gutter:next-diff)
(global-set-key (kbd "C-x p")  'git-gutter:previous-diff)

(use 's)
(require 's)

(use 'visual-regexp-steroids)
(use 'pcre2el)
(setq vr/engine 'pcre2el) 
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "C-c m") 'vr/mc-mark)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)

(use 'paredit);http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html
(add-hook 'emacs-lisp-mode-hook (lambda ()
  (enable-paredit-mode)
  (define-key emacs-lisp-mode-map "\C-h" 'paredit-backward-delete)))
(add-hook 'scheme-mode-hook (lambda ()
  (enable-paredit-mode)
  (define-key scheme-mode-map "\C-h" 'paredit-backward-delete)))

(use 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ctp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
