;; tabnine-capf.el --- A company-mode backend for TabNine ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Tommy Xiang, John Gong
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/50ways2sayhard/tabnine-capf/
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.16.0") (epc "0.1.1"))
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

(require 'cl-lib)
(require 'url)
(require 'dash)
(require 'epc)
;; (require 'corfu)

;;
;; Constants
;;

(defconst tabnine-capf--hooks-alist
  ;; '((after-change-functions . tabnine-capf-query))
  nil
  )
(defconst tabnine-capf--protocol-version "1.0.14")

;; tmp file put in tabnine-capf-binaries-folder directory
(defconst tabnine-capf--version-tempfile "version")

(defvar tabnine-capf-last-change-tick nil)
(defvar tabnine-capf-python-file (expand-file-name "tabnine.py" (file-name-directory load-file-name)))
;;
;; Macros
;;

(defmacro tabnine-capf-with-disabled (&rest body)
  "Run BODY with `tabnine-capf' temporarily disabled.
Useful when binding keys to temporarily query other completion backends."
  `(let ((tabnine-capf--disabled t))
     ,@body))

;;
;; Customization
;;

(defgroup tabnine-capf nil
  "Options for tabnine-capf."
  :link '(url-link :tag "Github" "https://github.com/50ways2sayhard/tabnine-capf")
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

(defcustom tabnine-capf-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-capf-install-binary' will use this directory."
  :group 'tabnine-capf
  :type 'string)

(defcustom tabnine-capf-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'tabnine-capf
  :type 'boolean)

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

;;
;; Major mode definition
;;

;;
;; Global methods
;;


;;  TODO: maybe we should do this in python?
(defun tabnine-capf--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-capf-install-binary to download binaries"))

(defun tabnine-capf--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (s-split "-" system-configuration)))
         (tabnine-architecture
          (cond
           ((or (string= system-architecture "aarch64")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((or (string= system-architecture "arm")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((string= system-architecture "x86_64")
            "x86_64")
           ((string-match system-architecture "i.86")
            "i686")
           (t
            (error "Unknown or unsupported architecture %s" system-architecture))))

         (os
          (cond
           ((or (eq system-type 'ms-dos)
                (eq system-type 'windows-nt)
                (eq system-type 'cygwin))
            "pc-windows-gnu")
           ((or (eq system-type 'darwin))
            "apple-darwin")
           (tabnine-capf-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun tabnine-capf--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    "TabNine.exe")
   (t
    "TabNine")))

(defun tabnine-capf--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-capf-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
                              (--remove (member it '("." "..")))
                              (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
                              (--filter (ignore-errors (version-to-list it)))
                              (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (tabnine-capf--get-target))
               (filename (tabnine-capf--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine-capf--error-no-binaries)))
      (tabnine-capf--error-no-binaries))))

(defun tabnine-capf-start-process ()
  "Start TabNine process."
  (tabnine-capf-kill-process)
  (setq tabnine-capf--disabled nil)
  (let ((process-connection-type nil))
    (setq tabnine-capf--process
          (epc:start-epc "python3"
                         (list tabnine-capf-python-file)))
    (epc:call-deferred tabnine-capf--process 'set_executable_path (list (tabnine-capf--executable-path)))
    (epc:define-method tabnine-capf--process
                       'tabnine-capf-callback
                       #'tabnine-capf-callback))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook tabnine-capf--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-capf-callback (&rest args)
  "From python"
  (setq tabnine-capf--response args)
  ;; (when tabnine-capf-last-change-tick
  ;;   (corfu--auto-complete tabnine-capf-last-change-tick))
  )

(defun tabnine-capf-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine-capf--process
    (let ((process tabnine-capf--process))
      (setq tabnine-capf--process nil) ; this happens first so sentinel don't catch the kill
      ;; (delete-process process)
      (epc:stop-epc process)))
  ;; hook remove
  (dolist (hook tabnine-capf--hooks-alist)
    (remove-hook (car hook) (cdr hook)))
  (setq tabnine-capf--disabled t))

(defun tabnine-capf-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null tabnine-capf--process)
    (tabnine-capf-start-process))
  (when tabnine-capf--process
    (let* ((version (plist-get request :version))
           (item (plist-get (plist-get request :request) :Autocomplete))
           (before (plist-get item :before))
           (after (plist-get item :after))
           (filename (plist-get item :filename))
           (region_includes_beginning (plist-get item :region_includes_beginning))
           (region_includes_end (plist-get item :region_includes_end))
           (max_num_results (plist-get item :max_num_results)))
      ;; (setq tabnine-capf--response nil)
      (epc:call-deferred tabnine-capf--process 'complete (list version before after filename
                                                               region_includes_beginning region_includes_end
                                                               max_num_results)))))

(defun tabnine-capf--make-request ()
  "Create request body for method METHOD and parameters PARAMS."
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (before-point
          (max (point-min) (- (point) tabnine-capf-context-radius)))
         (after-point
          (min (point-max) (+ (point) tabnine-capf-context-radius-after))))

    (list
     :version tabnine-capf--protocol-version
     :request
     (list :Autocomplete
           (list
            :before (buffer-substring-no-properties before-point (point))
            :after (buffer-substring-no-properties (point) after-point)
            :filename (or (buffer-file-name) nil)
            :region_includes_beginning (if (= before-point buffer-min)
                                           t nil)
            :region_includes_end (if (= after-point buffer-max)
                                     t nil)
            :max_num_results tabnine-capf-max-num-results)))))

(defun tabnine-capf-query (&optional begin end length)
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

(defun tabnine-capf-install-binary ()
  "Install TabNine binary into `tabnine-capf-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-capf-binaries-folder)
                           tabnine-capf--version-tempfile))
        (target (tabnine-capf--get-target))
        (exe (tabnine-capf--get-exe))
        (binaries-dir tabnine-capf-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case ex
            (let ((default-directory target-directory))
              (if (or (eq system-type 'ms-dos)
                      (eq system-type 'windows-nt)
                      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (expand-file-name bundle-path)))
                (shell-command (format "unzip -o %s -d %s"
                                       (expand-file-name bundle-path)
                                       (expand-file-name target-directory)))))
          ('error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]."
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
              (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

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
      ;; (message "[candidates] %s %s" candidates tabnine-capf--response)
      (unless tabnine-capf--disabled
        (tabnine-capf-query)
        (list
         start
         end
         candidates
         :exclusive 'no
         :company-kind (lambda (_) nil)
         :annotation-function
         (lambda (candidate)
           "Extract integer from company-tabnine's CANDIDATE."
           (concat "  "(get-text-property 0 'annotation candidate)))
         :exit-function
         (lambda (candidate status)
           "Post-completion function for tabnine."
           (let ((item (cl-find candidate (funcall get-candidates) :test #'string=)))
             (tabnine-capf--post-completion item)
             )
           ))))))

;;
;; Advices
;;


;;
;; Hooks
;;


(provide 'tabnine-capf)

;;; tabnine-capf.el ends here
