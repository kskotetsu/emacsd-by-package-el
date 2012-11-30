; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ------------------------------------------------------------------------
;; パッケージ関連
(require 'package)

; Add package-archives
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; ついでにmarmaladeも追加

; Initialize
(package-initialize)

; melpa.el
(require 'melpa)

;; ------------------------------------------------------------------------
;; 基本的な設定

;; 日本語入力のための設定
(set-keyboard-coding-system 'cp932)

(prefer-coding-system 'utf-8-dos)
(set-file-name-coding-system 'cp932)
(setq default-process-coding-system '(cp932 . cp932))

(set-language-environment "Japanese")

;; IME状態のモードライン表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
(w32-ime-initialize)			;IMEの初期化
;(set-cursor-color "red")		;IME OFF時の初期カーソルカラー
(setq w32-ime-buffer-switch-p nil)	;バッファ切り替え時にIME状態を引き継ぐ
(setq default-input-method "W32-IME") ;標準IMEの設定
;(set-input-method "W32-IME") ;標準IMEの設定

;IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
	  (lambda() (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
	  (lambda() (set-cursor-color "white")))

(setq tab-width 4)
(setq c-basic-offset 4)
;; バッファ画面外文字の切り詰め表示
(setq truncate-lines t)                 ;長い行を折り返し表示しない
;(setq truncate-lines nil)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-o" 'toggle-input-method)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M- " 'dabbrev-expand)  ;;; Alt-spaceでキーワード展開
(global-set-key '[home] 'other-window)
(global-set-key '[f1] 'help)
(global-set-key '[f4] 'speedbar-get-focus)
(global-set-key '[f7] 'compile)
;(global-set-key '[f8] 'ecb-toggle-ecb-windows)
;(global-set-key '[f9] 'ecb-goto-window-edit1)
;(global-set-key '[f10] 'ecb-goto-window-speedbar)
;(global-set-key '[f11] 'ecb-goto-window-sources)
;(global-set-key '[f12] 'ecb-goto-window-methods)

;(global-set-key '[f10] 'hide-ifdef-block)
;(global-set-key '[f11] 'show-ifdef-block)

;; shift+方向キーでウィンドウの移動
(windmove-default-keybindings)
;; C-C bfpnでウィンドウ移動
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)

;; y/n, yes/no の問い合わせ時に IME をオフにする
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)

;; GCの頻度を増やす
(setq gc-cons-threshold 100000) 
;; GCの頻度を減らす
;(setq gc-cons-threshold 5242880)

;;---------------------------------------------------------------------
;; 検索設定
(setq case-fold-search t)               ;検索では大文字小文字を区別しない

;; ------------------------------------------------------------------------
;; カーソル

;; カーソル点滅表示
(blink-cursor-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; ------------------------------------------------------------------------
;; 見た目

;ツールバーを消す
(tool-bar-mode 0)

;; スプラッシュスクリーンなし
(setq inhibit-splash-screen t)

; フレームタイトルの設定
(setq frame-title-format "%b")

;; 重複ファイルを hoge<dir1>などのように
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; フォント
(set-default-font "Consolas 10")

;; カラーテーマ
(color-theme-molokai)

;; ------------------------------------------------------------------------
;; helm-mode
(global-set-key [?\C-;] 'helm-mini)
(helm-mode 1)

;; ------------------------------------------------------------------------
;; auto-complete

;; なぜかパッケージインストールができないので手動インストール
(setq load-path (cons "~/.emacs.d/elisp/auto-complete-1.4" load-path))
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(global-set-key [?\M-/] 'auto-complete)

;; ------------------------------------------------------------------------
;; org-mode

(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; use org-remember
(org-remember-insinuate)

(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
;(setq org-agenda-files (concat org-directory "agenda.org"))
(setq org-agenda-files (list
						(concat org-directory "agenda.org")
						(concat org-directory "note.org")
						(concat org-directory "cafe.org")
						))

(setq org-agenda-ndays 7)
(setq org-agenda-span 20)
(setq org-deadline-warning-days 7)
(setq org-agenda-time-grid
      '((daily today require-timed)
		"----------------"
		(800 1000 1200 1400 1600 1800 2000 2200 2400 2600)))

;; (custom-set-faces(defadvice org-agenda (around org-agenda-around)
;;   (let ((system-time-locale "English"))
;;     ad-do-it))

(defadvice org-agenda-redo (around org-agenda-redo-around)
  (let ((system-time-locale "English"))
    ad-do-it))

(custom-set-variables
  '(org-agenda-format-date "%Y/%m/%d (%a)"))

(custom-set-faces
 '(org-agenda-date ((t :weight bold))))

(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" "~/memo/agenda.org" "Inbox")
        ("Schedule" ?s "** %?\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Tasks")
        ("Work" ?w "** TODO %?   :work:\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Inbox")
        ("Log"  ?l "** %t   :log:\n   \n" "~/memo/agenda.org" "Logs")
        ("Home" ?h "** TODO %?   :home:\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Inbox")
        ("Memo" ?m "** %?\n   %i\n   %a\n   %U" "~/memo/note.org" "Memo")
        ))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-ca" 'org-agenda)

;; org-capture
;; (("t" "Todo" entry (file+headline "~/memo/agenda.org" "Inbox") "** TODO %?\n   %i\n   %a  %t")
;;  ("s" "Schedule" entry (file+headline "~/memo/agenda.org" "Tasks") "** %?  %i  %a  %U")
;;  ("w" "Work" entry (file+headline "~/memo/agenda.org" "Inbox") "** TODO %?   :work:  %i  %a  %U")
;;  ("l" "WorkLog" entry (file+headline "~/memo/agenda.org" "Logs") "** %t   :log:")
;;  ("h" "Home" entry (file+headline "~/memo/agenda.org" "Inbox") "** TODO %?   :home: %i %a %U")
;;  ("c" "CafeTODO" entry (file+headline "~/memo/cafe.org" "Inbox") "** TODO %?   :cafe: %i %a %U")
;;  ("m" "Memo" entry (file+headline "~/memo/note.org" "Memo") "** %? %i %a %U"))
;(define-key global-map "\C-cr" 'org-capture)
(setq org-startup-truncated nil)
(defun org-change-truncation()
  (interactive)
  (cond ((eq truncate-lines nil)
         (setq truncate-lines t))
        (t
         (setq truncate-lines nil))))

(custom-set-faces
 '(org-level-1 ((t (:foreground "LightSkyBlue" :height 150))) t)
 '(org-level-2 ((t (:foreground "DarkGoldenrod" :height 100))) t)
 '(org-level-3 ((t (:foreground "DodgerBlue3" :height 100))) t)
 '(org-level-4 ((t (:foreground "chocolate1" :height 100))) t)
 '(org-level-5 ((t (:foreground "PaleGreen" :height 100))) t)
 '(org-level-6 ((t (:foreground "Aquamarine" :height 100) )) t)
 '(org-date ((t (:foreground "DodgerBlue1" :underline t) )) t)
 '(org-link ((t (:foreground "saddle brown" :underline t) )) t)
 )
