;;; Load path
;; set user home directory
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; prepare utility function for load path setting
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; add elisp, conf, public_repos to the load path
(add-to-load-path "elisp")

;;; Color Theme
(if (> emacs-major-version 23)
  ;; If the version is 24 or above, use built-in color theme
  (load-theme 'wheatgrass)
  ;; If the version is under 24, use color-theme (https://code.google.com/p/gnuemacscolorthemetest/)
  ;; select Clarity and Dark
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-clarity))
  )

;;; Font setting
;; Use other font if it is mac
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/
(when (eq window-system 'ns)
  (message "Loading settings for Mac...")
  ;; English
  (set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 120)    ;; font size

  ;; Japanese
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

  ;; Make sure the ration between Hankaku and Zenkaku is 1:2
  (setq face-font-rescale-alist
	'((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2))))

;;; Window setting
;; maximize the window
(set-frame-parameter nil 'fullscreen 'maximized)

;;; Key binding
;; C-h -> Backspace
(global-set-key "\C-h" 'backward-delete-char)

;; C-c ; -> Comment region
(global-set-key "\C-c;" 'comment-region)

;; C-c : -> Uncomment region
(global-set-key "\C-c:" 'uncomment-region)

;; Assign newline-and-indent for Ctrl-m
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Delete region with C-d without saving it to kill-ring
(defun delete-char-or-region ()
  (interactive)
  "Delete region or a following character"
  (if mark-active
      (delete-region (point) (mark))
    (delete-char 1)
    )
  )
(global-set-key "\C-d" 'delete-char-or-region)


;;; Text editing
;; Highlight parentheses
(show-paren-mode t)

;; Disable auto-fill-mode
(auto-fill-mode nil)

;; Do not show start up message
(setq inhibit-startup-message t)

;; Save back up files (hoge~) under ~/bak directory
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/bak"))
            backup-directory-alist))

;; Sync Ubuntu clipboard and emacs kill ring
(cond (window-system
       (setq x-select-enable-clipboard t)
       ))

;; make search case insensitive by default
(setq-default case-fold-search t)

;;; Programming
;; Load jka-compr to look for a function with find-tag in a compressed EmacsLisp file
(load "jka-compr")

;; Load Geiser
;; (require 'geiser-install)
;;(load "~/lisp/geiser/elisp/geiser.el")
;;(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;;; Gauche
(setq scheme-program-name "gosh")
(require 'cmuscheme)

;; Split in two windows and run scheme on the other window
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
;; short cut
(define-key global-map
  "\C-cS" 'scheme-other-window)

;; Load xscheme.el for MIT-Scheme
;; (load "xscheme")

;;; Evernote client
;; evernote-modemacs
(when (require 'evernote-mode nil t)
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
  (setq evernote-developer-token "S=s49:U=4f8f15:E=14b8f58a1c2:C=14437a775c6:P=1cd:A=en-devtoken:V=2:H=eed8e46323ace28685f7a657fb121953")
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
)

;;; SKK settings
(setq skk-user-directory (expand-file-name (concat user-emacs-directory "ddskk/"))) ; ディレクトリ指定
(when (require 'skk-autoloads nil t)
  ;; SKK を Emacs の input method として使用する
  (setq default-input-method "japanese-skk")
  ;; Emacs 起動時に SKK を前もってロードする
  (setq skk-preload t)
  ;; C-xj は auto-fill にしない
  (global-set-key "\C-xj" 'skk-mode)
  ;; C-x C-j は auto-fill
  (global-set-key "\C-x\C-j" 'skk-auto-fill-mode)
  ;; 送り仮名が厳密に正しい候補を優先して表示
  (setq skk-henkan-strict-okuri-precedence t)
  ;; 漢字登録時、送り仮名が厳密に正しいかをチェック
  (setq skk-check-okurigana-on-touroku t)
  ;; `(' や `)' で全角の括弧を入力する。半角の括弧は、`z(' や `z)' で入力する。
  (setq skk-rom-kana-rule-list
	'(("hh" "h" ("ッ" . "っ"))
	  ("(" nil "（")
	  (")" nil "）")
	  ("z(" nil "(")
	  ("z)" nil ")")
	  ))
  ;; インジケータに色を付けない
  (setq skk-indicator-use-cursor-color nil)
  ;; Allow edit of private dictionary
  (setq skk-jisyo-edit-user-accepts-editing t))

;; Info settings
(when (require 'info nil t)
  (add-to-list 'Info-additional-directory-list (expand-file-name (concat user-emacs-directory "info"))))


;;; Google docs client
(when (require 'g nil t)
  (setq gdocs-user-email "hydrakecat@gmail.com"))


;;; W3M
;; (require 'w3m-load)
;; (setq browse-url-browser-function 'w3m-browse-url)


;;; Dired settings
;; Open the file with gnome-open
(add-hook 'dired-load-hook (function (lambda ()
    (define-key dired-mode-map "z" 'dired-gnome-open-file)
)))
(defun dired-gnome-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)
))

;;; Buffer setting
;; Enable iswitchb
(iswitchb-mode 1)
;; Show file content while walking through buffers
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "Show content of the buffer"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))

;; point goes to the last place where it was when the same file was previously visited
(when (require 'saveplace nil t)
  (setq-default save-place t))

;;; Version Control setting
(require 'egg nil t)

;;; Howm
;; http://howm.sourceforge.jp/
;; Loads only when user hits ¥C-c,,
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
;; a file per a day
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.txt")
;; howm-keys location
(setq howm-keyword-file "~/howm/.howm-keys")

;;; Auto-complete
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (expand-file-name (concat user-emacs-directory "elisp/auto-complete/ac-dict")))
  (ac-config-default)
  (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))

;;; Go language
;; golang mode
(when (require 'go-mode-load nil t)
  (require 'go-autocomplete)
  (require 'speedbar)
  ;; speedbar
  ;; (speedbar 1)
  (speedbar-add-supported-extension ".go")
  (add-hook
   'go-mode-hook
   '(lambda ()
      ;; gocode
      (auto-complete-mode 1)
      (setq ac-sources '(ac-source-go))
      ;; Imenu & Speedbar
      (setq imenu-generic-expression
	    '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
	      ("func" "^func *\\(.*\\) {" 1)))
      (imenu-add-to-menubar "Index")
      ;; Outline mode
      (make-local-variable 'outline-regexp)
      (setq outline-regexp "//\\.\\|//[^\r\n\f][^\r\n\f]\\|pack\\|func\\|impo\\|cons\\|var.\\|type\\|\t\t*....")
      (outline-minor-mode 1)
      (local-set-key "\M-a" 'outline-previous-visible-heading)
      (local-set-key "\M-e" 'outline-next-visible-heading)
      ;; Menu bar
      (require 'easymenu)
      (defconst go-hooked-menu
        '("Go tools"
	  ["Go run buffer" go t]
	  ["Go reformat buffer" go-fmt-buffer t]
	  ["Go check buffer" go-fix-buffer t]))
      (easy-menu-define
        go-added-menu
        (current-local-map)
        "Go tools"
        go-hooked-menu)

      ;; Other
      (setq show-trailing-whitespace t)
      ))
  ;; helper function
  (defun go ()
    "run current buffer"
    (interactive)
    (compile (concat "go run " (buffer-file-name))))

  ;; helper function
  (defun go-fmt-buffer ()
    "run gofmt on current buffer"
    (interactive)
    (if buffer-read-only
	(progn
	  (ding)
	  (message "Buffer is read only"))
      (let ((p (line-number-at-pos))
	    (filename (buffer-file-name))
	    (old-max-mini-window-height max-mini-window-height))
        (show-all)
        (if (get-buffer "*Go Reformat Errors*")
	    (progn
	      (delete-windows-on "*Go Reformat Errors*")
	      (kill-buffer "*Go Reformat Errors*")))
        (setq max-mini-window-height 1)
        (if (= 0 (shell-command-on-region (point-min) (point-max) "gofmt" "*Go Reformat Output*" nil "*Go Reformat Errors*" t))
	    (progn
	      (erase-buffer)
	      (insert-buffer-substring "*Go Reformat Output*")
	      (goto-char (point-min))
	      (forward-line (1- p)))
	  (with-current-buffer "*Go Reformat Errors*"
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward "<standard input>" nil t)
		(replace-match filename))
	      (goto-char (point-min))
	      (compilation-mode))))
        (setq max-mini-window-height old-max-mini-window-height)
        (delete-windows-on "*Go Reformat Output*")
        (kill-buffer "*Go Reformat Output*"))))
  ;; helper function
  (defun go-fix-buffer ()
    "run gofix on current buffer"
    (interactive)
    (show-all)
    (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))
  )
;;; Go flymake
(require 'go-flymake nil t)
