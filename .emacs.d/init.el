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
(if (>= emacs-major-version 23)
  ;; If the version is 24 or above, use built-in color theme
  (load-theme 'wheatgrass)
  ;; If the version is under 24, use color-theme (https://code.google.com/p/gnuemacscolorthemetest/)
  ;; select Clarity and Dark
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-clarity))
  )

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
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
)

;;; SKK settings
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
