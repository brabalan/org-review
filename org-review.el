;;; org-review.el --- schedule reviews for Org entries
;;
;; Copyright 2007-2014 Free Software Foundation, Inc.
;;
;; Author: Alan Schmitt
;; Version: 0.1
;; Keywords: org review

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This allows to schedule reviews of org entries.
;;
;; Entries will be scheduled for review only if their LAST_REVIEW
;; property is set. The next review date is computed from the
;; LAST_REVIEW property and the REVIEW_DELAY period, such as "+1m". If
;; REVIEW_DELAY is absent, a default period is used. Note that the
;; LAST_REVIEW property is not considered as inherited, but REVIEW_DELAY
;; is, allowing to set it for whole subtrees.
;;
;; Checking of review dates is done through an agenda view, using the
;; `org-review-skip' skipping function. This function is based
;; on `org-review-toreview-p', that returns `nil' if no review
;; is necessary (no review planned or it happened recently), otherwise
;; it returns the date the review was first necessary (LAST_REVIEW +
;; REVIEW_DELAY if it is in the past).
;;
;; When the entry is marked as reviewed, the LAST_REVIEW date is set to
;; the current date. The function
;; `org-review-insert-last-review' may be used for this.
;;
;; Example use.
;; 
;; 1 - To display the things to review.
;; 
;;   (setq org-agenda-custom-commands (quote ( ... ("R" "Review
;;         projects" tags-todo "-CANCELLED/"
;;         ((org-agenda-overriding-header "Reviews Scheduled")
;;         (org-agenda-skip-function 'org-review-skip)
;;         (org-agenda-cmp-user-defined 'org-review-compare)
;;         (org-agenda-sorting-strategy '(user-defined-down)))) ... )))
;;
;; 2 - To set a key binding to review from the agenda
;;
;;   (add-hook 'org-agenda-mode-hook (lambda () (local-set-key (kbd "C-c
;;        C-r") 'org-review-insert-last-review)))
;;
;; 3 - To find projects or single tasks with no last review date.
;; 
;; To find the projects (tasks with sub tasks) or single tasks that have
;; no review, you can use the following: (some is taken from
;; http://doc.norang.ca/org-mode.html#Projects)
;; 
;;  (tags-todo "-CANCELLED/" ((org-agenda-overriding-header "Missing
;;Last Review") (org-agenda-skip-function
;;'as/skip-non-project-non-single-no-review)))
;; 
;;  (defun bh/is-project-p () "Any task with a todo keyword subtask"
;;(save-restriction (widen) (let ((has-subtask) (subtree-end
;;(save-excursion (org-end-of-subtree t))) (is-a-task (member (nth
;;2 (org-heading-components)) org-todo-keywords-1))) (save-excursion
;;(forward-line 1) (while (and (not has-subtask) (< (point) subtree-end)
;;(re-search-forward "^\*+ " subtree-end t)) (when (member
;;(org-get-todo-state) org-todo-keywords-1) (setq has-subtask t)))) (and
;;is-a-task has-subtask))))
;; 
;;  (defun bh/is-project-subtree-p () "Any task with a todo keyword that
;;     is in a project subtree. Callers of this function already widen
;;     the buffer view." (let ((task (save-excursion
;;     (org-back-to-heading 'invisible-ok) (point)))) (save-excursion
;;     (bh/find-project-task) (if (equal (point) task) nil t))))
;; 
;;   (defun bh/is-task-p () "Any task with a todo keyword and no
;;subtask" (save-restriction (widen) (let ((has-subtask) (subtree-end
;;(save-excursion (org-end-of-subtree t))) (is-a-task (member (nth
;;2 (org-heading-components)) org-todo-keywords-1))) (save-excursion
;;(forward-line 1) (while (and (not has-subtask) (< (point) subtree-end)
;;(re-search-forward "^\*+ " subtree-end t)) (when (member
;;(org-get-todo-state) org-todo-keywords-1) (setq has-subtask t)))) (and
;;is-a-task (not has-subtask)))))
;; 
;; (defun as/skip-non-project-non-single-no-review () "Skip non
;;     projects, non single tasks, and those without last review
;;     marker." (save-restriction (widen) (let ((next-headline
;;     (save-excursion (or (outline-next-heading) (point-max))))) (cond
;;     ((bh/is-project-p) (when (org-review-last-review-prop)
;;     next-headline)) ((and (bh/is-task-p) (not
;;     (bh/is-project-subtree-p))) (when
;;     (org-review-last-review-prop) next-headline)) (t
;;     next-headline)))))
;; 

;;; Code:

;;; User variables:

(defgroup org-review nil
  "Org review scheduling."
  :tag "Org Review Schedule"
  :group 'org)

(defcustom org-review-inactive-timestamps t
  "Insert inactive timestamps for last review properties."
  :type 'boolean
  :group 'org-review)

(defcustom org-review-last-property-name "LAST_REVIEW"
  "The name of the property for the date of the last review."
  :type 'string
  :group 'org-review)

(defcustom org-review-delay-property-name "REVIEW_DELAY"
  "The name of the property for setting the delay before the next review."
  :type 'string
  :group 'org-review)

(defcustom org-review-delay "+1m"
  "Time span between the date of last review and the next one.
The default value for this variable (\"+1m\") means that entries
will be marked for review one month after their last review.

If the review delay cannot be retrieved from the entry or the
subtree above, this delay is used."
  :type 'string
  :group 'org-review)

;;; Functions:

(defun org-review-last-planned (last delay)
  "Computes the next planned review, given the LAST review
  date (in string format) and the review DELAY (in string
  format)."
  (let* ((lt (org-read-date nil t last))
         (ct (current-time)))
    (time-add lt (time-subtract (org-read-date nil t delay) ct))))

(defun org-review-last-review-prop ()
  "Return the value of the last review property of the current
headline."
  (let ((lr-prop org-review-last-property-name))
    (org-entry-get (point) lr-prop)))

(defun org-review-toreview-p ()
  "Check if the entry at point should be marked for review.
Return nil if the entry does not need to be reviewed. Otherwise return
the number of days between the past planned review date and today.

If there is no last review date, return nil.
If there is no review delay period, use `org-review-delay'."
  (let* ((lr-prop org-review-last-property-name)
         (lp (org-entry-get (point) lr-prop)))
    (when lp 
      (let* ((dr-prop org-review-delay-property-name)
             (dr (or (org-entry-get (point) dr-prop t) 
                     org-review-delay))
             (nt (org-review-last-planned lp dr))
             )
        (if (time-less-p nt (current-time)) nt)))))

(defun org-review-insert-last-review (&optional prompt)
  "Insert the current date as last review. If prefix argument:
prompt the user for the date."
  (interactive "P")
  (let* ((ts (if prompt
                (concat "<" (org-read-date) ">")
              (format-time-string (car org-time-stamp-formats)))))
    (save-excursion
      (org-entry-put
       (if (equal (buffer-name) org-agenda-buffer-name)
           (or (org-get-at-bol 'org-marker)
               (org-agenda-error))
         (point))
       org-review-last-property-name
       (if org-review-inactive-timestamps
           (concat "[" (substring ts 1 -1) "]")
         ts)))))

(defun org-review-skip ()
  "Skip entries that are not scheduled to be reviewed."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (cond
       ((org-review-toreview-p)
        nil)
       (t
        next-headline)))))

(defun org-review-compare (a b)
  "Compares the date of scheduled review for the two agenda
entries, to be used with `org-agenda-cmp-user-defined'. Returns
+1 if A has been scheduled for longer and -1 otherwise."
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (pal (org-entry-get ma org-review-last-property-name))
         (pad (or (org-entry-get ma org-review-delay-property-name t)
                  org-review-delay))
         (pbl (org-entry-get mb org-review-last-property-name))
         (pbd (or (org-entry-get mb org-review-delay-property-name t)
                  org-review-delay))
         (sa (org-review-last-planned pal pad))
         (sb (org-review-last-planned pbl pbd)))
    (if (time-less-p sa sb) 1 -1)))

(provide 'org-review)

;;; org-review.el ends here
