; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ------------------------------------------------------------------------
;; @ coding system

;; 日本語入力のための設定
(set-keyboard-coding-system 'cp932)

(prefer-coding-system 'utf-8-dos)
(set-file-name-coding-system 'cp932)
(setq default-process-coding-system '(cp932 . cp932))

(setq tab-width 4)
(setq c-basic-offset 4)
(global-set-key "\C-h" 'backward-delete-char)

;ツールバーを消す
(tool-bar-mode 0)

;; GCの頻度を増やす
(setq gc-cons-threshold 100000) 
;; GCの頻度を減らす
;(setq gc-cons-threshold 5242880)

;; IME状態のモードライン表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
(w32-ime-initialize)			;IMEの初期化
;(set-cursor-color "red")		;IME OFF時の初期カーソルカラー
(setq w32-ime-buffer-switch-p nil)	;バッファ切り替え時にIME状態を引き継ぐ
(setq default-input-method "W32-IME") ;標準IMEの設定
;(set-input-method "W32-IME") ;標準IMEの設定
