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
(global-set-key "\C-h" 'backward-delete-char)

;ツールバーを消す
(tool-bar-mode 0)

; フレームタイトルの設定
(setq frame-title-format "%b")

;; GCの頻度を増やす
(setq gc-cons-threshold 100000) 
;; GCの頻度を減らす
;(setq gc-cons-threshold 5242880)


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
;; helm-mode
(global-set-key [?\C-;] 'helm-mini)
(helm-mode 1)

;; ------------------------------------------------------------------------
;; カラーテーマ
(color-theme-molokai)

