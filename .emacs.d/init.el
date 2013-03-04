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

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)
;; ------------------------------------------------------------------------
;; 基本的な設定

;; 日本語入力のための設定
;(set-keyboard-coding-system 'cp932)

(prefer-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
;(set-file-name-coding-system 'cp932)
;(setq default-process-coding-system '(cp932 . cp932))
(setq default-process-coding-system '(utf8-dos . utf8-dos))

(set-language-environment "Japanese")

;; grep
;(if (file-exists-p "/usr/bin/lgrep")
(setq grep-command "lgrep -nk -Os ")
(setq grep-program "lgrep")
;(setq helm-c-grep-default-command "lgrep -nK -Os ")

;; IME状態のモードライン表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
(w32-ime-initialize)			;IMEの初期化
;(set-cursor-color "red")		;IME OFF時の初期カーソルカラー
(setq w32-ime-buffer-switch-p nil)		;バッファ切り替え時にIME状態を引き継ぐ
(setq default-input-method "W32-IME")	;標準IMEの設定
;(set-input-method "W32-IME") ;標準IMEの設定

;IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
	  (lambda() (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
	  (lambda() (set-cursor-color "white")))

(setq tab-width 4)
(setq default-tab-width 4)
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

(global-set-key (kbd "C-r") 'helm-resume)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c g") 'helm-do-grep)
(global-set-key (kbd "C-c s") 'helm-semantic-or-imenu)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
;; (eval-after-load 'helm
;;   '(progn
;;      (define-key helm-map (kbd "C-h") 'delete-backward-char)
;;      ))

;; backspaceキーをインクリメンタルサーチ中のミニバッファで有効にする
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

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

;; マウスのホイールスクロールスピードを調節
;; (連続して回しているととんでもない早さになってしまう。特にLogicoolのマウス)
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

(global-set-key [wheel-up]
                '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [wheel-down]
                '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [double-wheel-up]
                '(lambda () "" (interactive) (scroll-down 2)))
(global-set-key [double-wheel-down]
                '(lambda () "" (interactive) (scroll-up 2)))
(global-set-key [triple-wheel-up]
                '(lambda () "" (interactive) (scroll-down 3)))
(global-set-key [triple-wheel-down]
                '(lambda () "" (interactive) (scroll-up 3)))

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

;; linum-mode時に遅延リフレッシュさせて動作を早くする
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; ------------------------------------------------------------------------
;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; ------------------------------------------------------------------------
;; helm-mode
;(global-set-key [?\C-;] 'helm-mini)
(define-key global-map [(C \;)] 'helm-mini)
(helm-mode 1)

(defun helm-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files" . "--modified")
          ("Untracked files" . "--others --exclude-standard")
          ("All controlled files in this project" . nil))
        for title  = (format "%s (%s)" (car elt) pwd)
        for option = (cdr elt)
        for cmd    = (format "git ls-files %s" (or option ""))
        collect
        `((name . ,title)
          (init . (lambda ()
                    (unless (and (not ,option) (helm-candidate-buffer))
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (call-process-shell-command ,cmd nil t nil)))))
          (candidates-in-buffer)
          (type . file))))

(defun helm-git-project-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-git-project ()
  (interactive)
  (let ((topdir (helm-git-project-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let* ((default-directory topdir)
           (sources (helm-c-sources-git-project-for default-directory)))
      (helm-other-buffer sources "*helm git project*"))))

;(global-set-key C-x C-r 'helm-git-project)
(define-key global-map [(C x) (C p)] 'helm-git-project)
;; ------------------------------------------------------------------------
;; auto-complete

;; なぜかパッケージインストールができないので手動インストール
(setq load-path (cons "~/.emacs.d/elisp/auto-complete-1.4" load-path))
(setq load-path (cons "~/.emacs.d/elisp/" load-path))
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(require 'ac-python)

(define-key global-map [(M /)] 'auto-complete)

(require 'auto-complete-clang)
;; C++モードの設定
(defun my-ac-cc-mode-setup ()
  ;;4文字入力時点で補完画面を出す．nilなら補完キーによって出る
  (setq ac-auto-start 4)
  ;;(setq ac-clang-prefix-header "~/.emacs.d/ac-dict/stdafx.pch")
  (setq ac-clang-flags '("-w" "-ferror-limit" "1"))
  (setq ac-sources '(
		     ac-source-clang
		     ac-source-yasnippet  
		     ac-source-gtags
		     ))
  )

;; ------------------------------------------------------------------------
;; C++モード
; プリコンパイルヘッダの作り方
; clang++ -cc1 -emit-pch -x c++-header ./stdafx.h -o stdafx.pch　-I(インクルードディレクトリ)
(add-hook 'c++-mode-hook '(lambda ()
			    (my-ac-cc-mode-setup)
			    (gtags-mode 1)
			    (setq c-auto-newline nil)
			    
			    (setq c-auto-newline nil)
														;;(linum-mode)
			    (setq c++-tab-always-indent nil)		; [TAB] キーで、TABコードを入力
			    (setq c-tab-always-indent nil)			; [TAB] キーで、TABコードを入力
			    (setq indent-tabs-mode t)
			    (show-paren-mode t)						;対応する括弧を表示
			    (setq tab-width 4)
			    (c-toggle-hungry-state -1)
			    (setq truncate-lines t)					;長い行を折り返し表示しない
				;(setq show-paren-style 'expression)
			    (c-set-style "stroustrup")	
			    (local-set-key "\C-m" 'newline-and-indent)
			    (local-set-key "\C-j" 'newline-and-indent)
			    (setq dabbrev-case-fold-search nil)
				;; (setq comment-start "// "
				;; 		comment-end " "
				;; 		)
			    (font-lock-fontify-buffer)
			    (setq font-lock-keywords c++-font-lock-keywords-2)
				;; http://d.hatena.ne.jp/i_s/20091026/1256557730
			    (c-set-offset 'innamespace 0)			; namespace {}の中はインデントしない
			    (c-set-offset 'arglist-close 0)			; 関数の引数リストの閉じ括弧はインデントしない
			    (c-set-offset 'label 0)
			    (c-set-offset 'substatement-open 0)
			    (c-set-offset 'statement-case-intro 2)
			    (c-set-offset 'inline-open 0)
			    (c-set-offset 'case-label 2)
			    
			    ))

;; ------------------------------------------------------------------------
;; シェル
(require 'cygwin-mount)
(cygwin-mount-activate)
;; shellの文字化けを回避
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
            ))

;; (setq shell-mode-hook
;;       (function
;;        (lambda ()

;; 	 ;; シェルモードの入出力文字コード
;; 	 (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
;; 	 (set-buffer-file-coding-system    'sjis-unix)
;; 	 )))

(setq cygwin-mount-cygwin-bin-directory
      (concat (getenv "CYGWIN_DIR") "\\bin"))
;(require 'setup-cygwin)

;; ------------------------------------------------------------------------
;; @ shell
(require 'shell)
(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "-c")
(setq shell-file-name "bash.exe")

;; (M-! and M-| and compile.el)
(setq shell-file-name "bash.exe")
(modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

;; shellモードの時の^M抑制
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; shell-modeでの補完 (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

;; エスケープシーケンス処理の設定
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)


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
      '(("Todo"		?t "** TODO %?\n   %i\n   %a\n   %t" "~/memo/agenda.org" "Inbox")
        ("Schedule"	?s "** %?\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Tasks")
        ("Work"		?w "** TODO %?   :work:\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Inbox")
        ("Log"		?l "** %t   :log:\n   \n" "~/memo/agenda.org" "Logs")
        ("Home"		?h "** TODO %?   :home:\n   %i\n   %a\n   %U" "~/memo/agenda.org" "Inbox")
        ("Memo"		?m "** %?\n   %i\n   %a\n   %U" "~/memo/note.org" "Memo")
        ))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-ca" 'org-agenda)

;; org-capture
(setq org-capture-templates
      '(("t" "Todo"		entry (file+headline "~/memo/agenda.org" "Inbox")	"** TODO %?\n   %i\n   %a  %T")
		("s" "Schedule" entry (file+headline "~/memo/agenda.org" "Tasks")	"** %?  %i  %a  %U")
		("w" "Work"		entry (file+headline "~/memo/agenda.org" "Inbox")	"** TODO %?   :work:  %i  %a  %U")
		("l" "WorkLog"	entry (file+headline "~/memo/agenda.org" "Logs")	"** %t   :log:")
		("h" "Home"		entry (file+headline "~/memo/agenda.org" "Inbox")	"** TODO %?   :home: %i %a %U")
		("c" "CafeTODO" entry (file+headline "~/memo/cafe.org"   "Inbox")	"** TODO %?   :cafe: %i %a %U")
		("m" "Memo"		entry (file+headline "~/memo/note.org"   "Memo")	"** %? %i %a %U")
		))
(define-key global-map "\C-cc" 'org-capture)

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

;; ------------------------------------------------------------------------
;; Python

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; ------------------------------------------------------------------------
;; expand-region
(global-set-key "\M-\[" 'er/expand-region) 

;; ------------------------------------------------------------------------
;; gtags
(autoload 'gtags-mode "gtags" "" t)
;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-t" 'gtags-find-tag)
;;          (local-set-key "\M-r" 'gtags-find-rtag)
;;          (local-set-key "\M-s" 'gtags-find-symbol)
;;          (local-set-key "\C-t" 'gtags-pop-stack)
;;          ))
(setq gtags-mode-hook
      '(lambda ()
		 (helm-gtags-mode 1)
         (local-set-key "\M-t" 'helm-gtags-find-tag)
         (local-set-key "\M-r" 'helm-gtags-find-rtag)
         (local-set-key "\M-s" 'helm-gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
;; ------------------------------------------------------------------------
;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(setq guide-key/highlight-command-regexp "rectangle")
(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
(guide-key-mode 1)  ; guide-key-mode を有効にする


; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;; ------------------------------------------------------------------------
;; popwin 
(push '("*grep*" :noselect t) popwin:special-display-config)

;; ------------------------------------------------------------------------
;; hit-a-hint
(require 'jaunte)
(global-set-key (kbd "C-'") 'jaunte)

;; ------------------------------------------------------------------------
;; git-gutter
(global-git-gutter-mode t)

;; ------------------------------------------------------------------------
;; firefox 
(require 'moz)

(defun moz-browser-reload ()
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "BrowserReload();"))

(global-set-key '[f5] 'moz-browser-reload)

;; ------------------------------------------------------------------------
;; move to home dir
(cd "~")
