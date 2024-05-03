;;; recentf-find-file.el --- Complete reading for recef.
;; Copyright (C) 2016 - 2024 fubuki

;; Author:   fubuki at frill.org
;; Keywords: files
;; Version:  $Revision: 1.11 $

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
(require 'cl-lib)

(defcustom recentf-find-file-completing-function 'completing-read
  "Completing function."
  :type  '(choice (const :tag "Default" completing-read)
                  (const :tag "Ido"     ido-completing-read)
                  function)
  :group 'recentf)

(defvar recentf-alist nil) ; Work Value

(defun recentf-find-file-make-complete-alist ()
  "更新順に並べ換えた `recentf-list' を補完コマンド用連想リストに再編成."
  (let* ((lst (seq-uniq (mapcar #'expand-file-name recentf-list)))) ;; ~ の正規化
    (setq recentf-alist (recentf-uniq-list lst))))

(defun recentf-equal-all (lst)
  (cond
   ((null (cdr lst))
    t)
   ((equal (car lst) (cadr lst))
    (recentf-equal-all (cdr lst)))))

(defun recentf-beginning-uniq-position (lst)
  "LST の各文字列を末尾からスキャンして不一致になる前のポジションを戻す."
  (let* ((lst (mapcar #'reverse lst))
         (len (eval (cons #'min (mapcar #'length lst)))))
    (catch 'out
      (dotimes (i len (- i))
        (if (not (recentf-equal-all (mapcar #'(lambda (a) (aref a i)) lst)))
            (throw 'out (- i)))))))

(defun recentf-same-collect (lst)
  "LST からファイル名部分が同じものをグループ化した alist で戻す.
CAR には共通ファイル名、 CDR はフルパスが続けて入る."
  (let* ((lst (mapcar
               #'(lambda (a) (list (file-name-nondirectory a) a)) lst))
         (alst (mapcar #'list (seq-uniq (mapcar #'car lst))))
         tmp)
    (dolist (a lst alst)
      (setq tmp (assoc (car a) alst)
            alst (cons (append a (cdr tmp)) (delete tmp alst))))))

(defun recentf-numbering-list (lst)
  (let ((i 0))
    (mapcar #'(lambda (a)
                (prog1
                    (propertize a 'number i)
                  (setq i (1+ i))))
            lst)))

(defun recentf-resort (lst)
  (sort lst
        #'(lambda (a b)
            (< (get-text-property 0 'number (cdr a))
               (get-text-property 0 'number (cdr b))))))
        
(defun recentf-uniq-list (lst)
  "フルパス名 LST を一意なファイル名をつけたコンスセルにして戻す."
  (let ((lst (recentf-same-collect (recentf-numbering-list lst)))
        result)
    (dolist (a lst)
      (if (= 1 (length (cdr a)))
          (push (cons (car a) (cadr a)) result)
        (setq result (append (recentf--uniq-list a) result))))
    (recentf-resort result)))

(defun recentf--uniq-list (lst)
  (let* ((name (car lst))
         (lst  (cdr lst))
         (pos (recentf-beginning-uniq-position lst))
         result)
    (dolist (a lst result)
      (push
       (cons
        (format "%s<%s>" name (file-name-nondirectory (substring a 0 pos)))
        a)
       result))))

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
