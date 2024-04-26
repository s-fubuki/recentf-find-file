;;; recentf-find-file.el --- Complete reading for recef.
;; Copyright (C) 2016 - 2024 fubuki

;; Author:   fubuki at frill.org
;; Keywords: files
;; Version:  $Revision: 1.5 $

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; recentf が蓄積したファイルを補完コマンドから選択するインターフェイス.

;;; Installation:

;; ;; load-path 上に置き init.el に以下を書く
;; (autoload 'recentf-find-file "recentf-find-file" nil t)

;; ;; 例えば ido を使い `C-c r' にバインド.
;; ;; (ido-completing-read+ が入っていれば不要)
;; (setq recentf-find-file-completing-function 'ido-completing-read)
;; (global-set-key "\C-cr" 'recentf-find-file)

;;; Code:

(require 'recentf)

(defcustom recentf-find-file-completing-function 'completing-read
  "Completing function."
  :type  '(choice (const :tag "Default" completing-read)
                  (const :tag "Ido"     ido-completing-read)
                  function)
  :group 'recentf)

(defvar recentf-alist nil) ; Work Value

(defun recentf-parent-directory (file)
  (car (last (split-string (file-name-directory file) "/" t))))

(defun recentf-dup-list (lst)
  "重複して\"いる\"エレメントだけリストで戻す."
  (let ((lst (sort lst #'string-lessp))
        result)
    (while (cdr lst)
      (if (equal (car lst) (cadr lst))
          (push (car lst) result))
      (setq lst (cdr lst)))
    (seq-uniq result)))

(defun recentf-find-file-make-complete-alist ()
  "更新順に並べ換えた `recentf-list' を補完コマンド用連想リストに再編成."
  (let* ((lst (seq-uniq (mapcar #'expand-file-name recentf-list))) ;; ~ の整理
         (col (mapcar #'(lambda (f) (cons (file-name-nondirectory f) f)) lst))
         (dup (recentf-dup-list (mapcar #'car col)))
         result)
    (dolist (f col)
      (if (member (car f) dup)
          (push (cons
                 (format
                  "%s<%s>" (car f) (recentf-parent-directory (cdr f)))
                 (cdr f))
                result)
        (push f result)))
     (setq recentf-alist (nreverse result))))

(advice-add 'recentf-track-opened-file :after 'recentf-find-file-make-complete-alist)

;;;###autoload  
(defun recentf-find-file (prefix)
  "`recentf-list' のエレメンツを `completing-read' 関数で選択する."
  (interactive "P")
  (recentf--find-file (if prefix 'switch-to-buffer-other-tab 'switch-to-buffer)))

(defun recentf--find-file (switch-func)
  (interactive)
  (let ((func recentf-find-file-completing-function)
        file full)
    (or recentf-alist (recentf-find-file-make-complete-alist))
    (setq file (funcall func "recentf: " recentf-alist)
          full (cdr (assoc file recentf-alist)))
    (funcall switch-func (find-file-noselect full))))

(provide 'recentf-find-file)
;; fin.
