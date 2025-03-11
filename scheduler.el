(require 'cl-lib)

(defun scheduler-org-skip-to-first-todo ()
  "Skips siblings until a 'TODO' todo item is found."
  (while (and (org-get-todo-state)
	      (not (string= (org-get-todo-state)
			    "TODO")))
    (org-get-next-sibling)))

(defun scheduler-org-is-todo ()
  "Returns true when the current headline is a todo item with state of 'TODO'"
  (if-let* ((state (org-get-todo-state)))
      (string= state "TODO")))

(defun org-todo-children ()
  (when (org-goto-first-child)
    (scheduler-org-skip-to-first-todo)
    (if-let* ((child (org-element-at-point))
	      (children (list child)))
	(progn
	  (while (org-get-next-sibling)
	    (when (scheduler-org-is-todo)
	      (push (org-element-at-point)
		    children)))
	       (reverse children)))))


(defun scheduler-org-tasks ()
  "Returns a list of lists of todo items, one for each project"
  (org-map-entries 'org-todo-children
		   "+LEVEL=1"
		   'agenda))

(defun scheduler-interleave-lists (lists)
  "Interleave LISTS of lists into a single list by taking elements cyclically from each sublist.
Handles lists of different lengths."
  (let ((results '())
        (all-empty nil))
    (while (not all-empty)
      (setq all-empty t)
      (setq lists (cl-remove-if-not #'identity
                                    (mapcar (lambda (lst)
                                              (when lst
                                                (setq results (append results (list (car lst))))
                                                (cdr lst)))
                                            lists)))
      (setq all-empty (null lists)))
    results))

(defun scheduler-org-timestamp (date)
  (format-time-string "<%Y-%m-%d>" date))

(defun scheduler-set-scheduled (element date)
  "Set the SCHEDULED property of element to date"
  (org-entry-put element
	       "SCHEDULED"
	       (scheduler-org-timestamp date)))

;; Example usage
(let* ((project-tasks (scheduler-org-tasks))
       (interleaved-tasks (scheduler-interleave-lists project-tasks)))
  (mapcar (lambda (e)
	    (org-element-property :title e))
	  interleaved-tasks))


