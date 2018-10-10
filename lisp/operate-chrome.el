;; pathを通す

(add-to-list 'exec-path "~/.emacs.d/applescript")
(setenv "PATH"(concat "~/.emacs.d/applescript:" (getenv "PATH")))

;; ChromeをReloadする
(defun my-reload-chrome()
    (interactive)
    (shell-command "reload_chrome"))
(global-set-key (kbd "H-r") 'my-reload-chrome) ;; お好みで

;; ChromeのTabを切り替える
(defun my-switch-chrome-tab()
    (interactive)
    (shell-command "switch_chrome_tab"))
(global-set-key (kbd "H-t") 'my-switch-chrome-tab) ;; お好みで

;; Chromeを下にスクロールする
(defun my-scroll-chrome-down()
  (interactive)
  (shell-command "scroll_chrome down"))
(global-set-key (kbd "H-j") 'my-scroll-chrome-down) ;; お好みで

;; Chromeを上にスクロールする
(defun my-scroll-chrome-up()
  (interactive)
  (shell-command "scroll_chrome up"))
(global-set-key (kbd "H-k") 'my-scroll-chrome-up) ;; お好みで

(provide 'operate-chrome)
