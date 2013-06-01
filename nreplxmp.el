;;; nreplxmp.el --- lispxmp porting for clojure nrepl.

;; Author: wakala (https://github.com/wakala)

;;; license:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'cl)
(require 'newcomment)
(require 'pp)
(eval-when-compile (require 'paredit nil t))
(defgroup nreplxmp nil
  "nreplxmp"
  :group 'emacs)

(defcustom nreplxmp-string-no-properties t
  "*When non-nil, remove text priperties of strings in annotation.

Need paredit.el.
http://mumble.net/~campbell/emacs/paredit.el"
  :type 'boolean  
  :group 'nreplxmp)

(defvar nreplxmp-temp-buffer " *nreplxmp tmp*")
(defvar nreplxmp-results nil)

(defun nreplxmp-eval-buffer ()
  (let ((buffer (current-buffer)))
    (nrepl-send-string-sync (buffer-substring-no-properties (point-min) (point-max))
			    nrepl-buffer-ns)))

(defun nreplxmp ()
  "Annotate value of lines containing `; =>' ."
  (interactive)
  (let ((pt (point)) (wstart (window-start (selected-window))))
    (nreplxmp-create-code (current-buffer))
    (erase-buffer)
    (insert-buffer-substring nreplxmp-temp-buffer)
    (unwind-protect
	(progn
	  (let ((result (nreplxmp-eval-buffer)))
	    (setq nreplxmp-results result)
	    result))
      (nreplxmp-create-annotations (current-buffer) nreplxmp-results)
      (goto-char pt)
      (set-window-start (selected-window) wstart))))

(defun nreplxmp-create-code (buf)
  (setq nreplxmp-results nil)
  (with-current-buffer (get-buffer-create nreplxmp-temp-buffer)
    (buffer-disable-undo)
    (erase-buffer) 
    (let (emacs-lisp-mode-hook after-change-major-mode-hook) (emacs-lisp-mode))
    (goto-char (point-min))
    
    (insert-buffer-substring buf)

    (goto-char (point-min))
    (kill-sexp)
    (yank)
    
    (insert "\n")
    (insert "(def nreplxmp-result nil)\n")
    (insert "(def nreplxmp-evaluating true)\n")
    (insert "(defn %nreplxmp-out [use-pp semicolons-len index result]\n  (def nreplxmp-result (cons (list index result) nreplxmp-result)) result)\n")
    (insert ";; (%nreplxmp-header-end)\n");
    (yank)

    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (goto-char 1)
    (nreplxmp-adjust-pp-annotations)
    (nreplxmp-add-out-markers)
    (goto-char (point-max))
    (insert ";; (%nreplxmp-footer-begin)\n");
    (insert "(def nreplxmp-evaluating false)\n")
    (insert "nreplxmp-result")))

(defun nreplxmp-adjust-pp-annotations ()
  (save-excursion
    (loop while (re-search-forward "^\\(;+\\)\\( +=> \\)" nil t)
          for next-line-re = (concat
                              "^"
                              (regexp-quote
                               (concat (match-string 1)
                                       (make-string (- (match-end 2) (match-beginning 2))
                                                    ?\s)))
                              ".+\n")
          do
          (forward-line 1)
          (while (looking-at next-line-re)
            (delete-region (point) (progn (forward-line 1) (point)))))))

(defun nreplxmp-add-out-markers ()
  (save-excursion
    (loop while (re-search-forward "\\(;+\\) +=>" nil t)
          for use-pp = (eq (point-at-bol) (match-beginning 0))
          for semicolons = (match-string 1)
          for i from 0
          when (nreplxmp-annotation-p) do
          (delete-region (match-beginning 0) (point-at-eol))
          (nreplxmp-out-make-sexp use-pp (length semicolons) i)
          (insert (format "%s <<%%nreplxmp-out-marker %d %d>>"
                          semicolons (length semicolons) i)))))

(defun nreplxmp-debug-buffer ()
  (interactive)
  (display-buffer nreplxmp-temp-buffer))


(defun nreplxmp-annotation-p ()
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (ignore-errors (comment-search-forward (point-at-eol) t))
      (looking-at "=>"))))

(defun nreplxmp-out-make-sexp (use-pp semicolons-len i)
  (end-of-line)
  (let ((e (make-marker)))
    (set-marker e (point))
    (forward-sexp -1)
    (insert (format "(%%nreplxmp-out %s %d %s " use-pp semicolons-len i))
    (goto-char e)
    (insert ")")))

(defun nreplxmp-out-remove ()
  (goto-char (point-min))
  (while (re-search-forward "(%nreplxmp-out [a-z]+ [0-9]+ [0-9]+ " nil t)
    (replace-match "")
    (save-excursion
      (forward-sexp)
      (and (search-forward ")" nil t) (replace-match ""))))
  (goto-char (point-min))
  (while (re-search-forward ".*(%nreplxmp-header-end)" nil t)
    (delete-region (point-min) (+ 1 (match-end 0))))
  (while (re-search-forward ".*(%nreplxmp-footer-begin)" nil t)
    (delete-region (match-beginning 0) (point-max))))

(defvar nreplxmp-results nil)
(defun %nreplxmp-out (use-pp semicolons-len index result)
  (push (cons index (%nreplxmp-prin1-to-string use-pp semicolons-len result)) nreplxmp-results)
  result)

(defun %nreplxmp-prin1-to-string (use-pp semicolons-len object)
  (let ((print-func (if use-pp 'pp-to-string 'prin1-to-string)))
    (with-temp-buffer
      (insert (let (pp-escape-newlines) (funcall print-func object)))
      (goto-char 1)
      (save-excursion
        (when (and nreplxmp-string-no-properties
                   (require 'paredit nil t))
          (while (search-forward "#(\"" nil t)
            (forward-char -1)
            (paredit-raise-sexp)
            (delete-backward-char 1)
            (forward-sexp 1))))
      (save-excursion
        (if (eq print-func 'prin1-to-string)
            ;; escape newlines
            (while (search-forward "\n" nil t)
              (replace-match "\\\\n"))
          ;; add paddings
          (goto-char 1)
          (forward-line 1)
          (unless (eobp)
            (string-rectangle (point) (point-max)
                             (concat (make-string semicolons-len ?\;)  "    ")))
          ;; delete last newline
          (goto-char (point-max))
          (and (bolp) (delete-backward-char 1))))
      (buffer-string))))

(defun nreplxmp-create-annotations (buf results)
  (set-buffer buf)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(;+\\) <<%nreplxmp-out-marker \\([0-9]+\\) \\([0-9]+\\)>> *$" nil t)
      (let ((index (string-to-number (match-string 3)))
            (semicolons (match-string 1))
	    (results-list (car (read-from-string (cadr results)))))
        ;; I do not use `replace-match', because it interprets backslashes.
        ;; Insert replacement string literally.
        (delete-region (match-beginning 0) (match-end 0))
        (insert semicolons
                " => "
                ;; pair := (INDEX . VALUE)
		(mapconcat (lambda (x)
			     (format "%S" (cadr x)))
			   (remove-if-not (lambda (pair) (= index (car pair)))
                                          (reverse results-list))
                           ", ")))))
  (nreplxmp-out-remove))
;; (with-new-window (find-epp nreplxmp-results))

(defmacro nreplxmp-comment-advice (func)
  `(defadvice ,func (around nreplxmp-hack activate)
     ,(format "If `%s' is successively called, add => mark." func)
     (if (and (eq major-mode 'emacs-lisp-mode)
              (eq last-command ',func)
              (not (member "=>" (list (ignore-errors (buffer-substring (- (point) 2) (point)))
                                      (ignore-errors (buffer-substring (point) (+ (point) 2)))))))
         (insert " =>")
       ad-do-it)))
(nreplxmp-comment-advice comment-dwim)
(nreplxmp-comment-advice paredit-comment-dwim)

(provide 'nreplxmp)

;;; nreplxmp.el ends here
