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
  (let* ((date-tasks (make-hash-table :test 'equal))
	(all-tasks (scheduler-all-tasks-ordered))
	(scheduled-tasks (-filter #'org-get-scheduled-time all-tasks))
	(unscheduled-tasks (-remove #'org-get-scheduled-time all-tasks)))
    (-> (scheduler-dates-init)
	(scheduler-commit-scheduled-tasks))
    (scheduler-commit-scheduled-tasks (scheduler-dates-init) scheduled-tasks)))

(defun scheduler-commit-scheduled-tasks (date-buckets scheduled-tasks)
  "Takes tasks with a schedule and adds them to a hash of dates"
  (-reduce-from (lambda (acc task)
		  (scheduler-dates-put acc
				       (org-get-scheduled-time
					task)
				       task))
		date-buckets
		scheduled-tasks))

(defun scheduler-commit-scheduled-task (date-buckets date task)
  "Takes and commits a task to the hash on date"
  (scheduler-dates-put date-buckets date task))

(defun scheduler-dates-init ()
  "Returns the initial hash used to bucket tasks into dates"
  '())

(defun scheduler-dates-get (date dates)
  "Returns the bucket of tasks for given `date'"
  (assoc-default date dates #'equal nil))

(defun scheduler-dates-put (date task dates)
  "Returns adds `task' to the bucket of `date'. Returns the new dates
object to be used for lookup later."
  (cons (cons date
	      (if-let (task-bucket (scheduler-dates-get date
							dates))
		  (cons task task-bucket)
		(list task)))
	dates))

(let* ((d1 (current-time))
       (d2 (time-add (days-to-time 1) d1))
       (tasks (scheduler-all-tasks-ordered)))
  (->> (scheduler-dates-init)
       (scheduler-dates-put d1 (nth 1 tasks))
       (scheduler-dates-put d1 (nth 2 tasks))
       (scheduler-dates-put d2 (nth 3 tasks))
       (-map #'scheduler-date-pp)))

(defun scheduler-dates-pp (dates)
  "Returns a string representation of dates that's easy to read for debugging."
  )

(defun scheduler-date-pp (date-alist)
  "Returns a string presentation of date-assoc that's easy to read for
debugging."
  (format "%s - %s"
	  (scheduler-org-timestamp (car date-alist))
	  (-map #'scheduler-org-title 
		(cdr date-alist))))

(defun scheduler-org-title (task)
  (org-element-property :title task))

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


(defun scheduler-date-sequence (start-time repeat size availability)
  "Returns a list of time objects starting at `start-time', repeating each
a `repeat' number of times before incrementing by 1 day and stopping
once reaching `size' and only using days-of-week in `availability'."
  (let* ((even-len (/ size repeat))
	 (nums (number-sequence 0 (1- even-len)))
	 (leftover (- size (* repeat even-len)))
	 (makeup (make-list leftover (+ 1 (car (last nums)))))
	 (even-list (->> nums
			 (-map (apply-partially #'make-list
						repeat))
			 (apply #'append))))
    (->> (append even-list makeup)
	 (-map (apply-partially #'scheduler-time-add-on-availability
				start-time
				availability)))))

(defun scheduler-next-available-day-after (availability day)
  "Return the next day that's available according to `availability'."
  (when availability
    (let* ((next-day (time-add day (days-to-time 1)))
	   (next-day-of-week (scheduler-day-of-week next-day)))
      (if (-contains? availability next-day-of-week)
	  next-day
	(scheduler-next-available-day-after availability next-day)))))

(let ((availability '(1 3 4))
      (cur-time (time-add (days-to-time 0) (current-time))))
  (list
   availability
   (scheduler-day-of-week cur-time)
   (scheduler-org-timestamp (scheduler-next-available-day-after availability cur-time))))

(defun scheduler-time-add-on-availability (start-time availability days)
  "Add `days' number of days to `start-time' according to `availability'.

TODO: Make this not have a loop. There must be a way to do this in O(1)."
  (let ((left days)
	(cur-time start-time))
    (while (> left 0)
      (setq cur-time (time-add cur-time (days-to-time 1)))
      (if (-contains? availability (scheduler-day-of-week cur-time))
	  (setq left (1- left))))
    cur-time))

(scheduler-org-timestamp (scheduler-time-add-on-availability (current-time) '(1 2 3) 10))

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
