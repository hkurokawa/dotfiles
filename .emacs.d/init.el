;;; Version predicates
(defvar oldemacs-p (<= emacs-major-version 22)) ; <= 22
(defvar emacs23-p (<= emacs-major-version 23))  ; <= 23
(defvar emacs24-p (>= emacs-major-version 24))  ; >= 24
;;; Platform predicates
(defvar darwin-p (eq system-type 'darwin))      ; Mac OS X
(defvar nt-p (eq system-type 'windows-nt))      ; Windows
(defvar linux-p (eq system-type 'gnu/linux))

;;; Load path
;; set user home directory
(when oldemacs-p
  (defvar user-emacs-directory "~/.emacs.d/"))


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  )

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

;;: Exec path
(add-to-list 'exec-path (expand-file-name "/usr/local/bin")) ;; Mac

;;; Utility
(defun apply-region-replace (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (apply-region-replace start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (apply-region-replace start end #'url-unhex-string))

;;; Color Theme
(if emacs24-p
  ;; If the version is 24 or above, use built-in color theme
  (load-theme 'wheatgrass)
  ;; If the version is under 24, use color-theme (https://code.google.com/p/gnuemacscolorthemetest/)
  ;; select Clarity and Dark
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-clarity))
  )

;;; Settings for Mac
(when darwin-p
  (message "Loading settings for Mac...")
  ;;; Font setting
  ;; Use other font if it is mac
  ;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/
 
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
	'((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))

  ;; Replace `¥' with `\' forcibly
  (define-key global-map [?¥] [?\\])

  ;; Copy value of SHELL or other environment variables into exec-path
  ;; so that it is the same as that of Emacs launched from shell
  (when (require 'exec-path-from-shell nil t)
    (exec-path-from-shell-initialize))
 )

;;; Settings for Ubuntu
(when linux-p
  (message "Loading settings for Linux...")

  ;; Use Ricty font
  ;; ref. http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html
  ;; How to configure is as follows.
  ;; ref. http://www.gfd-dennou.org/member/uwabami/cc-env/emacs/frame_config.html
  (add-to-list 'default-frame-alist '(font . "ricty-13.5"))
  (custom-set-faces
   '(variable-pitch ((t (:family "Ricty"))))
   '(fixed-pitch ((t (:family "Ricty"))))
   )

  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Noto Sans Mono CJK JP"))
  (add-to-list 'face-font-rescale-alist
               '("Noto Sans Mono CJK JP" . 1.0))
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
(defun move-end-of-line-and-new-line-and-indent ()
  (interactive)
  "Move to the end of the line and feed the line, then indent"
  (move-end-of-line nil)
  (newline-and-indent)
  )
(global-set-key (kbd "C-j") 'move-end-of-line-and-new-line-and-indent)

;; Delete region with C-d without saving it to kill-ring
(defun delete-char-or-region ()
  (interactive)
  "Delete region or a following character"
  (if mark-active
      (delete-region (point) (mark))
    (delete-char 1)
    )
  )
(global-set-key (kbd "C-d") 'delete-char-or-region)


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

;; Open current file with the default application
(defun gnome-open-file ()
  "Open the current buffer with the default application"
  (interactive)
  (let* ((file (buffer-file-name)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)
))
(defun mac-open-file ()
  "Open the current buffer with the default application"
  (interactive)
  (let* ((file (buffer-file-name)))
    (message "Opening %s..." file)
    (call-process "open" nil 0 nil file)
    (message "Opening %s done" file)
))

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

;; autoinsert
(when (require 'autoinsert nil t)
  ;; replace function
  (defvar template-replacements-alists
    '(("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" . (lambda () 
                                (setq file-without-ext (file-name-sans-extension
                                                        (file-name-nondirectory (buffer-file-name))))))
      ("%name%" . user-full-name)
      ("%mail%" . (lambda () (identity user-mail-address)))
      ))
  (defun template-replace ()
    (time-stamp)
    (mapc #'(lambda(c)
              (progn
                (goto-char (point-min))
                (replace-string (car c) (funcall (cdr c)) nil)))
          template-replacements-alists)
    (goto-char (point-max))
    (message "done."))

  (add-hook 'find-file-not-found-hooks 'auto-insert)
  ;; template files
  (setq auto-insert-directory "~/.emacs.d/insert/")
  ;; map an extenstion to a template file
  (setq auto-insert-alist
        (append '(
                  ("\\.py$" . ["template.py" template-replace])
                  ) auto-insert-alist))
  (add-hook 'find-file-hooks 'auto-insert))

;;; Evernote client
;; evernote-modemacs
(when (require 'evernote-mode nil t)
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
  (setq evernote-developer-token "S=s49:U=4f8f15:E=1568d5e5073:C=14f35ad21d8:P=1cd:A=en-devtoken:V=2:H=fd5b13848fe663d9de885124144f84fa")
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
  (setq skk-jisyo-edit-user-accepts-editing t)

  ;; ddskk が起動しているときは付属のインクリメント検索を使う
  (add-hook 'isearch-mode-hook
          #'(lambda ()
              (when (and (boundp 'skk-mode)
                         skk-mode
                         skk-isearch-mode-enable)
                (skk-isearch-mode-setup))))
  (add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (when (and (featurep 'skk-isearch)
                         skk-isearch-mode-enable)
                (skk-isearch-mode-cleanup))))
  ;; ;; SKK server settings
  ;; (setq skk-server-host "localhost")
  ;; Local large dictionary
  (setq skk-large-jisyo (if darwin-p "~/Library/Application Support/AquaSKK/SKK-JISYO.L" "/usr/share/skk/SKK-JISYO.L"))
  ;; Enable dynamic completion
  (setq skk-dcomp-activate t)
  ;; Show annotations
  (setq skk-show-annotatio t)
  )

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
    (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
    (define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-filenames-regexp)
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
;; (iswitchb-mode 1)
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

;;; Coding helper
;; Toggle comment and uncomment of the current line with C-c/
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key "\C-c/" 'toggle-comment-on-line)

;;; Go language
;; Set up environment variables
(setenv "GOPATH" (expand-file-name "~/go"))

;; Set up exec path
;; TODO: should use environment variable "GOPATH" instead
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin")) ;; Mac
(add-to-list 'exec-path (expand-file-name "/usr/lib/go/bin")) ;; Ubuntu

;;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; js2-refactor
(when (require 'js2-refactor nil t)
  (js2r-add-keybindings-with-prefix "C-c m")
  )

;;; Set tab width
(setq default-tab-width 4)

;;; Set Working directory
(cd "~")

;;; Quickrun
(when (require 'quickrun nil t)
  (global-set-key "\C-cr" 'quickrun))

;;; Util
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(70 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
;; Clean up
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))
(global-set-key (kbd "C-c n") 'cleanup-buffer)
;;; Zencoding
;; Auto-start on any markup modes
(when (require 'zencoding-mode nil t)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (define-key zencoding-mode-keymap (kbd "C-c C-j") 'zencoding-expand-line)
  (define-key zencoding-mode-keymap (kbd "C-j") nil))

;;; Setting for anything
;; (defvar org-directory "")
;; (when (require 'anything nil t)
;;   (require 'anything-config)
;;   (require 'anything-match-plugin)
;;   (require 'anything-complete)
;;   ;; (anything-read-string-mode '(string variable command))
;;   (anything-read-string-mode 1)
;;   (require 'anything-show-completion)
;;   (define-key global-map (kbd "C-x f") 'anything-filelist+)
;;   )

;;; Setting for Helm
(ignore-errors (when (require 'helm-config nil t)))

;;; Setting for Google This
;; (https://github.com/Bruce-Connor/emacs-google-this)
(require 'google-this nil t)

(when (require 'expand-region nil t)
  (global-set-key (kbd "C-M-@") 'er/expand-region))

;;; Twittering mode
(when (require 'twittering-mode nil t)
  ;; Use master password to avoid input one-time-password every time I launch emacs.
  (setq twittering-use-master-password t)
  ;; Show icons
  (setq twittering-icon-mode nil)
  ;; Show number of unread messages
  (twittering-enable-unread-status-notifier)
  ;; Show number of remaining API calls
  (setq twittering-display-remaining t)
  ;; Twitter status format
  ;;  (setq twittering-status-format "%i %S (%s), %FACE[font-lock-preprocessor-face]{%p} %FACE[font-lock-comment-face]{%@}:\n%FILL{  %T %FACE[font-lock-comment-face]{// from %f%L%r%R} %FACE[font-lock-keyword-face]{%e} %FACE[font-lock-function-name-face]{%F}}")
  (setq twittering-status-format "%i %s, %FACE[font-lock-preprocessor-face]{%p} %FACE[font-lock-comment-face]{%@}:\n%FILL{  %T %FACE[font-lock-keyword-face]{%e} %FACE[font-lock-function-name-face]{%F}}")
  ;; Custom keys
  (add-hook 'twittering-mode-hook
           (define-key twittering-mode-map (read-kbd-macro "F") 'twittering-favorite))
  )

;;; restclient-mode
(when (require 'restclient nil t)
  (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
  (when (require 'jq-mode nil t)
    (add-hook 'restclient-response-loaded-hook 'jq-mode)))

;;; jq-mode
(when (require 'jq-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
  (custom-set-variables '(jq-interactive-default-options "")))

(when (require 'wakatime-mode nil t)
  (global-wakatime-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(indent-tabs-mode nil)
 '(jq-interactive-default-options "")
 '(package-selected-packages
   (quote
    (restclient sml-mode ddskk wakatime-mode twittering-mode graphviz-dot-mode direx anything))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Ricty"))))
 '(variable-pitch ((t (:family "Ricty")))))
