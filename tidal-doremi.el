;;; tidal-doremi.el --- TidalCycles interactive number adjustment -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides an interactive command for adjusting numbers
;; in TidalCycles patterns in real-time. It leverages the doremi logic
;; for incremental changes using arrow keys or the mouse wheel.
;;
;; The core doremi logic was originally from doremi.el by Drew Adams.
;; It has been merged into this file to create a self-contained utility.

;;; Code:

(require 'ring)
(unless (fboundp 'ring-member) ; Emacs 23
  (require 'ring+))
(require 'mwheel nil t)
(require 'tidal-macros)

;; Quiet the byte-compiler.
(defvar mouse-wheel-down-event)
(defvar mouse-wheel-left-event)
(defvar mouse-wheel-right-event)
(defvar mouse-wheel-up-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Options (Variables) from doremi.el

(defgroup doremi nil
  "Do Re Mi: Incremental change using arrow keys or mouse wheel.
Define commands to perform repetitive or incremental operations."
  :prefix "doremi-"
  :group 'convenience)

(defcustom doremi-up-keys '(up)
  "*Keys (events) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-down-keys'.

The value must be a list of keyboard events: characters or symbols."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp)))
  :group 'doremi)

(defcustom doremi-down-keys '(down)
  "*Keys (events) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-up-keys'.

The value must be a list of keyboard events: characters or symbols."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp)))
  :group 'doremi)

(defcustom doremi-boost-up-keys '(M-up)
  "*Like `doremi-up-keys', but increments by `doremi-boost-scale-factor'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp)))
  :group 'doremi)

(defcustom doremi-boost-down-keys '(M-down)
  "*Like `doremi-down-keys', but increments by `doremi-boost-scale-factor'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp)))
  :group 'doremi)

(defcustom doremi-boost-scale-factor 10
  "*Factor to boost incremental change of numerical properties."
  :type 'integer
  :group 'doremi)

(defcustom doremi-boost-wheel-scale-factors '(10 10 10)
  "*Boost factors for mouse wheel rotation with a modifier key."
  :type
  '(list
    (integer :tag "Single wheel event (slow)")
    (integer :tag "Double wheel event (medium speed)")
    (integer :tag "Triple wheel event (fast)"))
  :group 'doremi)

(defvar doremi-modified-single-wheel-events
  (append
   '(M-wheel-up M-wheel-down) '(C-wheel-up C-wheel-down)
   (if (eq system-type 'darwin)
       '(S-wheel-left S-wheel-right)
     '(S-wheel-up S-wheel-down)))
  "List of modified single mouse wheel events.")

(defvar doremi-modified-double-wheel-events
  (append
   '(M-double-wheel-up M-double-wheel-down)
   '(C-double-wheel-up C-double-wheel-down)
   (if (eq system-type 'darwin)
       '(S-double-wheel-left S-double-wheel-right)
     '(S-double-wheel-up S-double-wheel-down)))
  "List of modified double mouse wheel events.")

(defvar doremi-modified-triple-wheel-events
  (append
   '(M-triple-wheel-up M-triple-wheel-down)
   '(C-triple-wheel-up C-triple-wheel-down)
   (if (eq system-type 'darwin)
       '(S-triple-wheel-left S-triple-wheel-right)
     '(S-triple-wheel-up S-triple-wheel-down)))
  "List of modified triple mouse wheel events.")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core doremi functions

(defun doremi
    (setter-fn init-val incr &optional growth-fn enum allow-new-p)
  "Use arrow keys and/or mouse wheel to adjust or cycle something."
  (setq incr (or incr 1))
  (let ((new-incr incr))
    (when (and enum (sequencep enum))
      (setq enum (ring-convert-sequence-to-ring enum)))
    (let ((prompt
           (format "Use %s, %s, or mouse wheel to adjust value"
                   (single-key-description (car doremi-up-keys))
                   (single-key-description (car doremi-down-keys))))
          (keys
           (append
            doremi-up-keys
            doremi-down-keys
            doremi-boost-up-keys
            doremi-boost-down-keys))
          (echo-keystrokes 0)
          (wheel-down
           (if (boundp 'mouse-wheel-down-event)
               mouse-wheel-down-event
             'wheel-down))
          (wheel-up
           (if (boundp 'mouse-wheel-up-event)
               mouse-wheel-up-event
             'wheel-up))
          (wheel-left
           (if (boundp 'mouse-wheel-left-event)
               mouse-wheel-left-event
             'wheel-left))
          (wheel-right
           (if (boundp 'mouse-wheel-right-event)
               mouse-wheel-right-event
             'wheel-right))
          evnt
          save-prompt)
      (unless enum
        (setq prompt (concat prompt " (modifier key: faster)")))
      (setq
       prompt (format (concat prompt ".  Value now: %s") init-val)
       save-prompt prompt)
      (while (progn
               (setq
                evnt
                (if (if (fboundp 'display-graphic-p)
                        (display-graphic-p)
                      window-system)
                    (read-event prompt)
                  (read-key prompt))
                prompt nil)
               (or (member evnt keys)
                   (and (consp evnt)
                        (member
                         (event-basic-type (car evnt))
                         `(switch-frame
                           mouse-wheel
                           mouse-2
                           ,wheel-up
                           ,wheel-down
                           ,wheel-left
                           ,wheel-right)))))
        (cond
         ((member evnt doremi-up-keys)
          (setq new-incr incr))
         ((member evnt doremi-down-keys)
          (setq new-incr
                (if (atom incr)
                    (- incr)
                  (mapcar #'- incr))))
         ((member evnt doremi-boost-up-keys)
          (setq new-incr
                (if (atom incr)
                    (* doremi-boost-scale-factor incr)
                  (mapcar
                   #'(lambda (in)
                       (* doremi-boost-scale-factor in))
                   incr))))
         ((member evnt doremi-boost-down-keys)
          (setq new-incr
                (if (atom incr)
                    (* doremi-boost-scale-factor (- incr))
                  (mapcar
                   #'(lambda (in)
                       (* doremi-boost-scale-factor (- in)))
                   incr))))
         ((and (consp evnt)
               (equal 'mouse-wheel (event-basic-type (car evnt))))
          (setq new-incr
                (if (< 0 (nth 2 evnt))
                    incr
                  (if (atom incr)
                      (- incr)
                    (mapcar #'- incr))))
          (when (event-modifiers evnt)
            (setq new-incr
                  (if (atom new-incr)
                      (* doremi-boost-scale-factor new-incr)
                    (mapcar
                     #'(lambda (in)
                         (* doremi-boost-scale-factor in))
                     new-incr)))))
         ((and (consp evnt)
               (member
                (event-basic-type (car evnt))
                `(,wheel-up ,wheel-down ,wheel-left ,wheel-right)))
          (let ((button (mwheel-event-button evnt)))
            (cond
             ((memq button (list wheel-down wheel-left))
              (setq new-incr incr))
             ((memq button (list wheel-up wheel-right))
              (setq new-incr
                    (if (atom incr)
                        (- incr)
                      (mapcar #'- incr))))
             (t
              (error "`doremi', bad mwheel-scroll binding"))))
          (let ((evt (car evnt)))
            (if (or (and (> emacs-major-version 22)
                         (doremi-intersection
                          (event-modifiers evnt)
                          '(shift control meta alt hyper super)))
                    (and (<= emacs-major-version 22)
                         (event-modifiers evnt)))
                (let* ((factor (car doremi-boost-wheel-scale-factors))
                       (factor
                        (cond
                         ((and (= factor
                                  (cadr
                                   doremi-boost-wheel-scale-factors))
                               (= factor
                                  (caddr
                                   doremi-boost-wheel-scale-factors)))
                          factor)
                         ((memq
                           evt doremi-modified-single-wheel-events)
                          factor)
                         ((memq
                           evt doremi-modified-double-wheel-events)
                          (cadr doremi-boost-wheel-scale-factors))
                         ((memq
                           evt doremi-modified-triple-wheel-events)
                          (caddr doremi-boost-wheel-scale-factors))
                         (t
                          factor))))
                  (setq new-incr
                        (if (atom new-incr)
                            (* factor new-incr)
                          (mapcar
                           #'(lambda (in) (* factor in))
                           new-incr)))))))
         ((and (consp evnt)
               (memq
                (event-basic-type (car evnt))
                '(mouse-2 switch-frame)))
          (message save-prompt))
         (t
          (error "`doremi', unexpected event: `%S'" evnt)))
        (condition-case failure
            (setq
             init-val
             (cond
              ((ring-p enum)
               (when (and allow-new-p
                          (not (ring-member enum init-val)))
                 (ring-insert+extend
                  enum init-val (eq 'extend allow-new-p)))
               (when (< (ring-length enum) 2)
                 (error
                  "`doremi' - Need at least two alternatives: %S"
                  enum))
               (let* ((vec (cdr (cdr enum)))
                      (veclen (length vec)))
                 (doremi-set-new-value
                  setter-fn
                  (if (and (numberp new-incr) (>= new-incr 0))
                      (ring-next enum init-val)
                    (ring-previous enum init-val)))))
              ((functionp growth-fn)
               (if (atom new-incr)
                   (if (and (numberp new-incr) (>= new-incr 0))
                       (doremi-set-new-value setter-fn new-incr)
                     (doremi-set-new-value growth-fn (- new-incr)))
                 (if (and (numberp (car new-incr))
                          (>= (car new-incr) 0))
                     (doremi-set-new-value setter-fn new-incr)
                   (doremi-set-new-value
                    growth-fn (mapcar #'- new-incr)))))
              (growth-fn
               (doremi-set-new-value setter-fn new-incr))
              ((and (numberp new-incr) (numberp init-val))
               (doremi-set-new-value setter-fn (+ init-val new-incr)))
              (t
               (error
                "`doremi' - Bad argument.  INIT-VAL: %S, INCR: %S"
                init-val
                new-incr))))
          (error
           (error "%s" (error-message-string failure)))))
      (message nil)
      (setq unread-command-events
            (cons evnt unread-command-events)))))

(defun doremi-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2."
  (and list1
       list2
       (if (equal list1 list2)
           list1
         (let ((result ()))
           (unless (>= (length list1) (length list2))
             (setq list1
                   (prog1 list2
                     (setq list2 list1))))
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

(defun doremi-set-new-value (setter-fn newval)
  "Apply SETTER-FN to NEWVAL, and return NEWVAL.  Display progress message."
  (prog1 (setq newval (funcall setter-fn newval))
    (message "Use %s, %s, or mouse wheel again.  New value: %s"
             (single-key-description (car doremi-up-keys))
             (single-key-description (car doremi-down-keys))
             newval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tidal-specific commands

(defun tidal-doremi--get-number-at-point ()
  "Find a number at the current point and return its value and bounds."
  (let ((bounds (bounds-of-thing-at-point 'number)))
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (num-str (buffer-substring-no-properties start end)))
        (list
         :value (string-to-number num-str)
         :start start
         :end end)))))

(defun tidal-doremi-adjust-number-at-point (prefix)
  "Interactively adjust the number at the point using `doremi`."
  (interactive "p")
  (let ((number-info (tidal-doremi--get-number-at-point)))
    (if (not number-info)
        (message "No number at point.")
      (let* ((initial-value (plist-get number-info :value))
             (step
              (cond
               ((= prefix 1)
                0.01) ; No prefix
               ((= prefix 4)
                0.1) ; C-u
               ((= prefix 16)
                1.0) ; C-u C-u
               (t
                0.01)))
             (precision
              (cond
               ((= step 0.01)
                2)
               ((= step 0.1)
                1)
               ((= step 1.0)
                0)
               (t
                2)))
             (format-spec (format "%%.%df" precision))
             (current-bounds-start (plist-get number-info :start))
             (current-bounds-end (plist-get number-info :end)))
        (doremi
         (lambda (new-val)
           (delete-region current-bounds-start current-bounds-end)
           (goto-char current-bounds-start)
           (insert (format format-spec new-val))
           (setq current-bounds-end (point))
	   (let ((tidal-highlight-style 'event-only))
             (save-excursion
               (goto-char current-bounds-start)
               (mark-paragraph)
               (tidal-run-multiple-lines)))
           new-val)
         initial-value step)))))

(defun tidal-doremi-rotate-pattern-at-point ()
  "Interactively rotate pattern using doremi."
  (interactive)
  (let ((context (tidal-macros--get-pattern-context-at-point)))
    (if (not context)
        (message "No pattern found at point.")
      (let* ((content (plist-get context :content))
             (tokens (tidal-parser-parse content))
             (current-bounds-start (plist-get context :start))
             (current-bounds-end (plist-get context :end)))
        (doremi
         (lambda (steps)
           (let* ((rotated (tidal-parser-rotate tokens steps))
                  (new-content
                   (tidal-parser-tokens-to-string rotated)))
             (delete-region current-bounds-start current-bounds-end)
             (goto-char current-bounds-start)
             (insert new-content)
             (setq current-bounds-end (point))
             steps))
         0 1)))))

(eval-after-load 'tidal
  '(progn
     (define-key
      tidal-mode-map
      (kbd "C-c M-d")
      'tidal-doremi-adjust-number-at-point)
     (define-key
      tidal-mode-map
      (kbd "C-c M-r")
      'tidal-doremi-rotate-pattern-at-point)))

(provide 'tidal-doremi)

;;; tidal-doremi.el ends here
