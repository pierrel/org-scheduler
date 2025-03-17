(require 'dash)

(defvar scheduler-weekly-capacity 2
  "The number of tasks that can be taken on in one day.")
(defvar scheduler-days-of-week-available '(1 2 3 4 5)
  "The integer-representation of the day of the week that can be scheduled")

(defun scheduler-new-schedule ()
  "Assigns a date to each todo according to the capacity and daily
availability.

For items that already have a schedule, honor that schedule.
For items that has a deadline, schedule for 2 weeks before or today."
  (let ((date-tasks (make-hash-table :test 'equal))
	(all-tasks (scheduler-all-tasks-ordered))
	(dates-to-fill (scheduler-date-sequence (current-time)
						scheduler-daily-capacity)))))

(defun scheduler-all-tasks-ordered ()
  (->> (scheduler-org-tasks 'scheduler-org-is-todo)
       (-remove 'seq-empty-p)
       (apply 'scheduler-interleave)))

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
  (let* ((even-len (/ size repeat))
	 (nums (number-sequence 0 (1- even-len)))
	 (leftover (- size (* repeat even-len)))
	 (makeup (make-list leftover (+ 1 (car (last nums)))))
	 (even-list (->> nums
			 (-map (apply-partially #'make-list
						repeat))
			 (apply #'append))))
    (->> (append even-list makeup)
	 (-map #'days-to-time)
	 (-map (apply-partially #'time-add start-time)))))

(defun saved-date-sequence (start-time repeat size)
  (let* ((nums (number-sequence 0
				(1- (/ size repeat))))
	 (nums-repeated (apply #'append
			       (mapcar (lambda (n)
					 (make-list repeat n))
				       nums))))
    (mapcar (lambda (n)
	      (time-add start-time (days-to-time n)))
	    nums-repeated)))

(defun scheduler-day-of-week (time)
  (->> time
       (format-time-string "%u")
       (string-to-number)))

(-map 'scheduler-org-timestamp
      (scheduler-date-sequence (current-time)
			       3
			       20))
(mapcar 'scheduler-org-timestamp
	(scheduler-date-sequence (current-time)
				 3
				 20))

(defun scheduler-interleave (&rest lists)
  "Interleave lists (like '-interleave') but don't stop when the shortest
list is exhausted."
  (when lists
    (->> lists
	 (-map 'length)
	 (apply 'max)
	 (1-)
	 (number-sequence 0)
	 (-map (lambda (index)
		 (-map (lambda (list)
			 (nth index list))
		       lists)))
	 (-flatten-n 1)
	 (-non-nil))))


;; All scheduled tasks
(length (->> (scheduler-org-tasks 'scheduler-org-is-todo)
	     (-remove 'seq-empty-p)
	     (apply 'scheduler-interleave)
	     (-map (apply-partially 'org-element-property :title))))
(length (->> (scheduler-org-tasks 'scheduler-org-is-todo)
	     (-remove 'seq-empty-p)
	     (apply '-interleave)
	     (-map (apply-partially 'org-element-property :title))))
(length (->> (scheduler-org-tasks 'scheduler-org-is-todo)
	 (-flatten-n 1)
	 (-map (apply-partially 'org-element-property :title))))

(-interleave '(1 1) '(2 2 2 2 2 2) '(3))

;; hashmap with `equal`
(let* ((ct (current-time))
       (tomorrow (time-add ct (days-to-time 1)))
       (tomorrow-again (time-add ct (days-to-time 1))))
  (equal tomorrow tomorrow-again))
(equal (current-time) (current-time))
