;;; epcs.el --- EPC Server              -*- lexical-binding: t; -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(defmacro tabnine-capf-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar tabnine-capf-deferred-debug nil
  "Debug output switch.")

(defvar tabnine-capf-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun tabnine-capf-deferred-log (&rest args)
  "[internal] Debug log function."
  (when tabnine-capf-deferred-debug
    (with-current-buffer (get-buffer-create "*tabnine-capf-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" tabnine-capf-deferred-debug-count (apply #'format args)))))
    (cl-incf tabnine-capf-deferred-debug-count)))

(defvar tabnine-capf-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro tabnine-capf-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`tabnine-capf-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal tabnine-capf-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar tabnine-capf-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar tabnine-capf-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `tabnine-capf-deferred-post-task' and `tabnine-capf-deferred-worker'.")

(defun tabnine-capf-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`tabnine-capf-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack tabnine-capf-deferred-queue)
    (tabnine-capf-deferred-log "QUEUE-POST [%s]: %s" (length tabnine-capf-deferred-queue) pack)
    (run-at-time tabnine-capf-deferred-tick-time nil 'tabnine-capf-deferred-worker)
    d))

(defun tabnine-capf-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when tabnine-capf-deferred-queue
    (let* ((pack (car (last tabnine-capf-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq tabnine-capf-deferred-queue (nbutlast tabnine-capf-deferred-queue))
      (condition-case err
          (setq value (tabnine-capf-deferred-exec-task d which arg))
        (error
         (tabnine-capf-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: tabnine-capf-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `tabnine-capf-deferred-resignal')
;; cancel      : a canceling function (default `tabnine-capf-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct tabnine-capf-deferred-object
  (callback 'identity)
  (errorback 'tabnine-capf-deferred-resignal)
  (cancel 'tabnine-capf-deferred-default-cancel)
  next status value)

(defun tabnine-capf-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun tabnine-capf-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (tabnine-capf-deferred-log "CANCEL : %s" d)
  (setf (tabnine-capf-deferred-object-callback d) 'identity)
  (setf (tabnine-capf-deferred-object-errorback d) 'tabnine-capf-deferred-resignal)
  (setf (tabnine-capf-deferred-object-next d) nil)
  d)

(defun tabnine-capf-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (tabnine-capf-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "tabnine-capf-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (tabnine-capf-deferred-object-callback d)
                    (tabnine-capf-deferred-object-errorback d)))
        (next-deferred (tabnine-capf-deferred-object-next d)))
    (cond
     (callback
      (tabnine-capf-deferred-condition-case err
        (let ((value (funcall callback arg)))
          (cond
           ((tabnine-capf-deferred-object-p value)
            (tabnine-capf-deferred-log "WAIT NEST : %s" value)
            (if next-deferred
                (tabnine-capf-deferred-set-next value next-deferred)
              value))
           (t
            (if next-deferred
                (tabnine-capf-deferred-post-task next-deferred 'ok value)
              (setf (tabnine-capf-deferred-object-status d) 'ok)
              (setf (tabnine-capf-deferred-object-value d) value)
              value))))
        (error
         (cond
          (next-deferred
           (tabnine-capf-deferred-post-task next-deferred 'ng err))
          (t
           (tabnine-capf-deferred-log "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (tabnine-capf-deferred-object-status d) 'ng)
           (setf (tabnine-capf-deferred-object-value d) err)
           err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (tabnine-capf-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (tabnine-capf-deferred-resignal arg)))))))

(defun tabnine-capf-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (tabnine-capf-deferred-object-next prev) next)
  (cond
   ((eq 'ok (tabnine-capf-deferred-object-status prev))
    (setf (tabnine-capf-deferred-object-status prev) nil)
    (let ((ret (tabnine-capf-deferred-exec-task
                next 'ok (tabnine-capf-deferred-object-value prev))))
      (if (tabnine-capf-deferred-object-p ret) ret
        next)))
   ((eq 'ng (tabnine-capf-deferred-object-status prev))
    (setf (tabnine-capf-deferred-object-status prev) nil)
    (let ((ret (tabnine-capf-deferred-exec-task next 'ng (tabnine-capf-deferred-object-value prev))))
      (if (tabnine-capf-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun tabnine-capf-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-tabnine-capf-deferred-object :callback callback)
    (make-tabnine-capf-deferred-object)))

(defun tabnine-capf-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (tabnine-capf-deferred-exec-task d 'ok arg))

(defun tabnine-capf-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (tabnine-capf-deferred-exec-task d 'ng arg))

(defun tabnine-capf-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (tabnine-capf-deferred-post-task d 'ok arg))

(defun tabnine-capf-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (tabnine-capf-deferred-callback-post (tabnine-capf-deferred-new callback))."
  (let ((d (if callback
               (make-tabnine-capf-deferred-object :callback callback)
             (make-tabnine-capf-deferred-object))))
    (tabnine-capf-deferred-callback-post d arg)
    d))

(defun tabnine-capf-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-tabnine-capf-deferred-object :callback callback)))
    (tabnine-capf-deferred-set-next d nd)))

(defun tabnine-capf-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-tabnine-capf-deferred-object :errorback callback)))
    (tabnine-capf-deferred-set-next d nd)))

(defvar tabnine-capf-epc-debug nil)

(defun tabnine-capf-epc-log (&rest args)
  (when tabnine-capf-epc-debug
    (with-current-buffer (get-buffer-create "*tabnine-capf-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun tabnine-capf-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar tabnine-capf-epc-uid 1)

(defun tabnine-capf-epc-uid ()
  (cl-incf tabnine-capf-epc-uid))

(defvar tabnine-capf-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct tabnine-capf-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun tabnine-capf-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return tabnine-capf-epc-connection object."
  (tabnine-capf-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (tabnine-capf-epc-uid))
         (connection-name (format "epc con %s" connection-id))
         (connection-buf (tabnine-capf-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-tabnine-capf-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (tabnine-capf-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (tabnine-capf-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (tabnine-capf-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun tabnine-capf-epc-process-sentinel (connection process msg)
  (tabnine-capf-epc-log "!! Process Sentinel [%s] : %S : %S"
                      (tabnine-capf-epc-connection-name connection) process msg)
  (tabnine-capf-epc-disconnect connection))

(defun tabnine-capf-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (tabnine-capf-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (tabnine-capf-epc-connection-process connection)))
    (tabnine-capf-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun tabnine-capf-epc-disconnect (connection)
  (let ((process (tabnine-capf-epc-connection-process connection))
        (buf (tabnine-capf-epc-connection-buffer connection))
        (name (tabnine-capf-epc-connection-name connection)))
    (tabnine-capf-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (tabnine-capf-epc-log "!! Disconnected finished [%s]" name)))

(defun tabnine-capf-epc-process-filter (connection process message)
  (tabnine-capf-epc-log "INCOMING: [%s] [%S]" (tabnine-capf-epc-connection-name connection) message)
  (with-current-buffer (tabnine-capf-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (tabnine-capf-epc-process-available-input connection process)))

(defun tabnine-capf-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (tabnine-capf-deferred-new callback)
             (tabnine-capf-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun tabnine-capf-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (tabnine-capf-deferred-callback-post d event))))

(defun tabnine-capf-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (tabnine-capf-epc-net-have-input-p)
      (let ((event (tabnine-capf-epc-net-read-or-lose process))
            (ok nil))
        (tabnine-capf-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'tabnine-capf-epc-signal-send
                         (cons (tabnine-capf-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (tabnine-capf-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (tabnine-capf-epc-process-available-input connection process)))))))

(defun tabnine-capf-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (tabnine-capf-epc-net-decode-length))))

(defun tabnine-capf-epc-net-read-or-lose (_process)
  (condition-case error
      (tabnine-capf-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun tabnine-capf-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (tabnine-capf-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun tabnine-capf-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun tabnine-capf-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct tabnine-capf-epc-manager
  "Root object that holds all information related to an EPC activity.

`tabnine-capf-epc-start-epc' returns this object.

title          : instance name for displaying on the `tabnine-capf-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : tabnine-capf-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct tabnine-capf-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar tabnine-capf-epc-live-connections nil
  "[internal] A list of `tabnine-capf-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun tabnine-capf-epc-server-process-name (uid)
  (format "tabnine-capf-epc-server:%s" uid))

(defun tabnine-capf-epc-server-buffer-name (uid)
  (format " *%s*" (tabnine-capf-epc-server-process-name uid)))

(defun tabnine-capf-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (tabnine-capf-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (tabnine-capf-epc-disconnect (tabnine-capf-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 tabnine-capf-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq tabnine-capf-epc-live-connections (delete mngr tabnine-capf-epc-live-connections))
    ))

(defun tabnine-capf-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun tabnine-capf-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an tabnine-capf-epc-connection instance."
  (let* ((mngr mngr)
         (conn (tabnine-capf-epc-manager-connection mngr))
         (channel (tabnine-capf-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (tabnine-capf-epc-log "SIG CALL: %S" args)
                    (apply 'tabnine-capf-epc-handler-called-method ,mngr (tabnine-capf-epc-args args))))
               (return
                . (lambda (args)
                    (tabnine-capf-epc-log "SIG RET: %S" args)
                    (apply 'tabnine-capf-epc-handler-return ,mngr (tabnine-capf-epc-args args))))
               (return-error
                . (lambda (args)
                    (tabnine-capf-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'tabnine-capf-epc-handler-return-error ,mngr (tabnine-capf-epc-args args))))
               (epc-error
                . (lambda (args)
                    (tabnine-capf-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'tabnine-capf-epc-handler-epc-error ,mngr (tabnine-capf-epc-args args))))
               (methods
                . (lambda (args)
                    (tabnine-capf-epc-log "SIG METHODS: %S" args)
                    (tabnine-capf-epc-handler-methods ,mngr (caadr args))))
               ) do
             (tabnine-capf-epc-signal-connect channel method body))
    (push mngr tabnine-capf-epc-live-connections)
    mngr))

(defun tabnine-capf-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (tabnine-capf-epc-manager-connection mngr)))
    (tabnine-capf-epc-net-send conn (cons method messages))))

(defun tabnine-capf-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (tabnine-capf-epc-manager-methods mngr)
           if (eq method-name (tabnine-capf-epc-method-name i))
           do (cl-return i)))

(defun tabnine-capf-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (tabnine-capf-epc-manager-methods mngr)
                  collect
                  (list
                   (tabnine-capf-epc-method-name i)
                   (or (tabnine-capf-epc-method-arg-specs i) "")
                   (or (tabnine-capf-epc-method-docstring i) "")))))
    (tabnine-capf-epc-manager-send mngr 'return uid info)))

(defun tabnine-capf-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (tabnine-capf-epc-manager-methods mngr))
           (method (tabnine-capf-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (tabnine-capf-epc-log "ERR: No such method : %s" name)
        (tabnine-capf-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (tabnine-capf-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((tabnine-capf-deferred-object-p ret)
                (tabnine-capf-deferred-nextc ret
                  (lambda (xx) (tabnine-capf-epc-manager-send mngr 'return uid xx))))
               (t (tabnine-capf-epc-manager-send mngr 'return uid ret))))
          (error
           (tabnine-capf-epc-log "ERROR : %S" err)
           (tabnine-capf-epc-manager-send mngr 'return-error uid err))))))))

(defun tabnine-capf-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (tabnine-capf-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (tabnine-capf-epc-manager-sessions mngr) ret)))

(defun tabnine-capf-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (tabnine-capf-epc-manager-sessions mngr))))
    (cond
     (pair
      (tabnine-capf-epc-log "RET: id:%s [%S]" uid args)
      (tabnine-capf-epc-manager-remove-session mngr uid)
      (tabnine-capf-deferred-callback (cdr pair) args))
     (t                                 ; error
      (tabnine-capf-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun tabnine-capf-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (tabnine-capf-epc-manager-sessions mngr))))
    (cond
     (pair
      (tabnine-capf-epc-log "RET-ERR: id:%s [%S]" uid args)
      (tabnine-capf-epc-manager-remove-session mngr uid)
      (tabnine-capf-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (tabnine-capf-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun tabnine-capf-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (tabnine-capf-epc-manager-sessions mngr))))
    (cond
     (pair
      (tabnine-capf-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (tabnine-capf-epc-manager-remove-session mngr uid)
      (tabnine-capf-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (tabnine-capf-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun tabnine-capf-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (tabnine-capf-epc-uid))
        (sessions (tabnine-capf-epc-manager-sessions mngr))
        (d (tabnine-capf-deferred-new)))
    (push (cons uid d) sessions)
    (setf (tabnine-capf-epc-manager-sessions mngr) sessions)
    (tabnine-capf-epc-manager-send mngr 'call uid method-name args)
    d))

(defun tabnine-capf-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-tabnine-capf-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (tabnine-capf-epc-manager-methods mngr))))
    (setf (tabnine-capf-epc-manager-methods mngr) methods)
    method))

(defun tabnine-capf-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'tabnine-capf-epc-nothing))
    (tabnine-capf-deferred-chain
      d
      (tabnine-capf-deferred-nextc it
        (lambda (x) (setq result x)))
      (tabnine-capf-deferred-error it
        (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'tabnine-capf-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (tabnine-capf-epc-connection-process (tabnine-capf-epc-manager-connection mngr))
         0 tabnine-capf-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun tabnine-capf-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (tabnine-capf-epc-sync mngr (tabnine-capf-epc-call-deferred mngr method-name args)))

(defun tabnine-capf-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (tabnine-capf-epc-connection-process (tabnine-capf-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar tabnine-capf-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`tabnine-capf-epc-manager' instance]).
When the server process accepts the client connection, the
`tabnine-capf-epc-manager' instance is created and stored in this variable
`tabnine-capf-epc-server-client-processes'. This variable is used for the management
purpose.")

;; tabnine-capf-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `tabnine-capf-epc-manager' instances
(cl-defstruct tabnine-capf-epc-server name process port connect-function)

(defvar tabnine-capf-epc-server-processes nil
  "[internal] A list of ([process object] . [`tabnine-capf-epc-server' instance]).
This variable is used for the management purpose.")

(defun tabnine-capf-epc-server-get-manager-by-process (proc)
  "[internal] Return the tabnine-capf-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in tabnine-capf-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun tabnine-capf-epc-server-accept (process)
  "[internal] Initialize the process and return tabnine-capf-epc-manager object."
  (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (tabnine-capf-epc-uid))
         (connection-name (format "epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-tabnine-capf-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (tabnine-capf-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (tabnine-capf-epc-process-sentinel connection p e)))
    (make-tabnine-capf-epc-manager :server-process process :port t
                                 :connection connection)))

(defun tabnine-capf-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (tabnine-capf-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (tabnine-capf-epc-server-accept process)))
            (push (cons process mngr) tabnine-capf-epc-server-client-processes)
            (tabnine-capf-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process tabnine-capf-epc-server-client-processes)) _d)
        (when pair
          (tabnine-capf-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (tabnine-capf-epc-stop-epc (cdr pair))
          (setq tabnine-capf-epc-server-client-processes
                (assq-delete-all process tabnine-capf-epc-server-client-processes))
          ))
      nil))))

(defun tabnine-capf-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (tabnine-capf-epc-uid)))
       (buf (tabnine-capf-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (tabnine-capf-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-tabnine-capf-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          tabnine-capf-epc-server-processes)
    main-process))

(provide 'tabnine-capf-epc)
;;; tabnine-capf-epc.el ends here
