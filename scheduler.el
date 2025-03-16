(require 'dash)

(defvar scheduler-daily-capacity 3
  "The number of tasks that can be taken on in one day.")
(defvar scheduler-day-availability '(1 2 3 4 5)
  "The days of the week that can be scheduled. Corresponds to the indices
of days used by 'calendar'")

(defun scheduler-org-is-todo ()
  "Returns true when the current element is a TODO item is TODO state"
  (if-let* ((state (org-get-todo-state)))
      (string= state "TODO")))

(defun scheduler-org-is-unscheduled-todo ()
  "Returns true when the current headline is a todo item with state of 'TODO' AND it is not yet scheduled"
  (if-let* ((state (org-get-todo-state)))
      (and (string= state "TODO")
	   (not (org-get-scheduled-time
		 (org-element-at-point))))))

(defun scheduler-org-is-scheduled-todo ()
  "Returns true when the current headline is a todo item with state of 'TODO' AND it is already scheduled"
  (if-let* ((state (org-get-todo-state)))
      (and (string= state "TODO")
	   (org-get-scheduled-time
	    (org-element-at-point)))))

(defun scheduler-org-skip-to-first-match (filter-fun)
  "Skips siblings filter-fun returns true. Returns true when a match is
found an nil when all siblings have been exhausted without a match."
  (if (funcall filter-fun)
      't
    (if (org-get-next-sibling)
	(scheduler-org-skip-to-first-match filter-fun)
      nil)))


(defun scheduler-org-todo-children (filter-fun)
  "Return all children of the current headline that pass `filter-fun`"
  (let ((children '()))
    (when (org-goto-first-child)
      (while (and (scheduler-org-skip-to-first-match filter-fun)
		  (not (equal (car children)
			      (org-element-at-point))))
	(push (org-element-at-point) children)
	(org-get-next-sibling)))
    (reverse children)))


(defun scheduler-org-tasks (filter-fun)
  "Returns a list of lists of todo items, one for each project, filtered by filter-fun"
  (org-map-entries (apply-partially 'scheduler-org-todo-children
				    filter-fun)
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

(defun scheduler-date-sequence (start-time repeat size)
  "Returns a list of time objects starting at start-time, repeating each
`repeat` times before incrementing by 1 day and stopping once reaching
`size`"
  (let* ((nums (number-sequence 0
			       (1- (/ size repeat))))
	 (nums-repeated (apply #'append
			       (mapcar (lambda (n)
					 (make-list repeat n))
				       nums))))
    (mapcar (lambda (n)
	      (time-add start-time (days-to-time n)))
	    nums-repeated)))
(scheduler-date-sequence (current-time)
			 3
			 20)
(mapcar 'scheduler-org-timestamp
	(scheduler-date-sequence (current-time)
				 3
				 20))


;; Example usage
(-> (scheduler-org-tasks 'scheduler-org-is-scheduled-todo)
    (-interleave)
    (-map (lambda (x)
	    (org-element-property :title x))))

(scheduler-org-tasks
 'scheduler-org-is-scheduled-todo)

;; Before dash
(let* ((project-tasks (scheduler-org-tasks
		       'scheduler-org-is-scheduled-todo))
       (interleaved-tasks (scheduler-interleave-lists project-tasks)))
  (mapcar (lambda (e)
	    (org-element-property :title e))
	  interleaved-tasks))


;; hashmap with `equal`
(let* ((ct (current-time))
       (tomorrow (time-add ct (days-to-time 1)))
       (tomorrow-again (time-add ct (days-to-time 1))))
  (equal tomorrow tomorrow-again))
(equal (current-time) (current-time))
