;; tabnine-capf.el --- A company-mode backend for TabNine ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Tommy Xiang, John Gong
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/50ways2sayhard/tabnine-capf/
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.16.0"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; Commentary:
;;
;; Description:
;;
;; A capf verison of `company-tabnine`.
;;
;; Installation:
;;
;; 1. Add `tabnine-completion-at-point` to `completion-at-point-functions`
;;    (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;; 2. Run M-x tabnine-capf-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; See M-x customize-group RET tabnine-capf RET for customizations.
;;
;;

;;; Code:

;;
;; Dependencies
;;

;;  TODO:
;; [  ] make the tabnine-capf show up automatically
;; [  ] start the tabnine more naturally
;; [  ] check whether prefix influences the results
;; [  ] refactor the code, especially the name of variables


(require 'cl-lib)
(require 'url)
(require 'dash)
(require 'tabnine-epc)
(require 'corfu)

;;
;; Constants
;;

(defconst tabnine-capf--hooks-alist
  ;; '((after-change-functions . tabnine-capf-query))
  nil
  )

(defconst tabnine-capf-python-file (expand-file-name "tabnine.py" (file-name-directory load-file-name)))

;;
;; Customization
;;

(defgroup tabnine-capf nil
  "Options for tabnine-capf."
  :link '(url-link :tag "Github" "https://github.com/theFool32/tabnine-capf")
  :group 'company
  :prefix "tabnine-capf-")

(defcustom tabnine-capf-max-num-results 10
  "Maximum number of results to show."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-context-radius 3000
  "The number of chars before point to send for completion.

Note that setting this too small will cause TabNine to not be able to read the entire license activation key."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-context-radius-after 1000
  "The number of chars after point to send for completion."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group 'tabnine-capf
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar tabnine-capf--process nil
  "TabNine server process.")

(defvar tabnine-capf--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine-capf--disabled nil
  "Variable to temporarily disable tabnine-capf and pass control to next backend.")

(defvar tabnine-capf--corfu-tick nil)

;;
;; Major mode definition
;;

;;
;; Global methods
;;

;;;###autoload
(defun tabnine-capf-install-binary ()
  "Install Tabnine binary."
  (interactive)
  (when (tabnine-epc:live-p tabnine-capf--process)
    (tabnine-epc:call-deferred tabnine-capf--process 'install_tabnine nil)))

(defun tabnine-capf-start-process ()
  "Start TabNine process."
  (setq tabnine-capf--disabled t)
  (tabnine-capf-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine-capf--process
          (tabnine-epc:start-epc "python3"
                                 (list tabnine-capf-python-file)))
    (tabnine-epc:define-method tabnine-capf--process
                               'tabnine-capf-callback
                               #'tabnine-capf-callback))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook tabnine-capf--hooks-alist)
    (add-hook (car hook) (cdr hook)))
  (setq tabnine-capf--disabled nil))

(defun update-corfu ()
  "Update corfu candidate manually."
  ;;  HACK: still suffer from wrong icons
  (corfu--auto-complete nil)
  ;; (pcase (while-no-input ;; Interruptible capf query
  ;;          (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper))
  ;;   (`(,fun ,beg ,end ,table . ,plist)
  ;;    (let ((completion-in-region-mode-predicate
  ;;           (lambda () (eq beg (car-safe (funcall fun)))))
  ;;          (completion-extra-properties plist))
  ;;      (setq completion-in-region--data
  ;;            (list (if (markerp beg) beg (copy-marker beg))
  ;;                  (copy-marker end t)
  ;;                  table
  ;;                  (plist-get plist :predicate)))

  ;;      ;; Refresh candidates forcibly!!!
  ;;      (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
  ;;                   (pt (- (point) beg))
  ;;                   (str (buffer-substring-no-properties beg end)))
  ;;        (corfu--update-candidates str pt table (plist-get plist :predicate)))

  ;;      (corfu--setup)
  ;;      (let ((corfu-on-exact-match 'quit))
  ;;        (corfu--update))))))
  )

(defun tabnine-capf-callback (&rest args)
  "Callback from python."
  (setq tabnine-capf--response args)
  ;;  TODO: need a more elegant way to handle this
  (when (and
         (< corfu--index 0)
         (or (not (featurep 'evil)) (evil-insert-state-p))
         (equal tabnine-capf--corfu-tick (corfu--auto-tick)))
    (update-corfu)))

(defun tabnine-capf-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine-capf--process
    (let ((process tabnine-capf--process))
      (setq tabnine-capf--process nil)
      (tabnine-epc:stop-epc process)))
  ;; hook remove
  (dolist (hook tabnine-capf--hooks-alist)
    (remove-hook (car hook) (cdr hook)))
  (setq tabnine-capf--disabled t))

(defun tabnine-capf-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null tabnine-capf--process)
    (tabnine-capf-start-process))
  (let* ((before (plist-get request :before))
         (after (plist-get request :after))
         (filename (plist-get request :filename))
         (region_includes_beginning (plist-get request :region_includes_beginning))
         (region_includes_end (plist-get request :region_includes_end))
         (max_num_results (plist-get request :max_num_results)))
    (tabnine-epc:call-deferred tabnine-capf--process 'complete (list before after filename
                                                                     region_includes_beginning region_includes_end
                                                                     max_num_results))))

(defun tabnine-capf--make-request ()
  "Create request body for method METHOD and parameters PARAMS."
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (before-point
          (max (point-min) (- (point) tabnine-capf-context-radius)))
         (after-point
          (min (point-max) (+ (point) tabnine-capf-context-radius-after))))

    (list
     :before (buffer-substring-no-properties before-point (point))
     :after (buffer-substring-no-properties (point) after-point)
     :filename (or (buffer-file-name) nil)
     :region_includes_beginning (if (= before-point buffer-min)
                                    t nil)
     :region_includes_end (if (= after-point buffer-max)
                              t nil)
     :max_num_results tabnine-capf-max-num-results)))

(defun tabnine-capf-query ()
  "Query TabNine server for auto-complete."
  (interactive)
  (unless tabnine-capf--disabled
    ;; (setq tabnine-capf-last-change-tick (corfu--auto-tick))
    (let ((request (tabnine-capf--make-request)))
      (tabnine-capf-send-request request))))

(defun tabnine-capf--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when tabnine-capf-show-annotation
    (-if-let (annotation (get-text-property 0 'annotation candidate))
        annotation
      (let ((kind (get-text-property 0 'kind candidate))
            ;; (return-type (get-text-property 0 'return_type candidate))
            (params (get-text-property 0 'params candidate)))
        (when kind
          (concat params
                  ;; (when (s-present? return-type)
                  ;;   (s-prepend " -> " return-type))
                  (when (s-present? kind)
                    (format " [%s]" kind))))))))

(defun tabnine-capf--candidates ()
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `tabnine-capf-query'."
  (mapcar
   (lambda (item)
     (propertize
      (plist-get item :new_prefix)
      'old_suffix (plist-get item :old_suffix)
      'new_suffix (plist-get item :new_suffix)
      'annotation (or (plist-get item :detail) "")
      ))
   (plist-get tabnine-capf--response :results)))

(defun tabnine-capf--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when tabnine-capf-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
          (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
                     (min (+ (point) (length old_suffix))
                          (point-max)))
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

;;
;; Interactive functions
;;

(defun tabnine-capf-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-capf-start-process))

;;;###autoload
(defun tabnine-completion-at-point (&optional interactive)
  "TabNine Completion at point function."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'tabnine-completion-at-point)))
        (completion-at-point))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (candidates (tabnine-capf--candidates))
           (get-candidates (lambda () candidates))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (unless tabnine-capf--disabled
        (let ((tmp-tick (corfu--auto-tick)))
          (unless (equal tabnine-capf--corfu-tick tmp-tick)
            (setq tabnine-capf--corfu-tick tmp-tick)
            (tabnine-capf-query)))
        (list
         start
         end
         candidates
         :exclusive 'no
         :company-kind (lambda (_) (intern "tabnine"))
         :annotation-function
         (lambda (candidate)
           "Extract integer from company-tabnine's CANDIDATE."
           (concat "  "(get-text-property 0 'annotation candidate)))
         :exit-function
         (lambda (candidate status)
           "Post-completion function for tabnine."
           (let ((item (cl-find candidate (funcall get-candidates) :test #'string=)))
             (tabnine-capf--post-completion item))))))))


(provide 'tabnine-capf)

;;; tabnine-capf.el ends here
