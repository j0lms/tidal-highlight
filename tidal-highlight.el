;;; tidal-highlight.el --- Real-time event highlighting for TidalCycles -*- lexical-binding: t; -*-

;;; Commentary:
;; This file implements real-time event highlighting for TidalCycles in Emacs.
;; It provides immediate visual feedback for live coding by flashing musical
;; events as they are played. The system is designed for high performance
;; and responsiveness, leveraging a native Rust module (`tidal-relay`) to
;; handle OSC message processing and event state management.

;;; Code:

(require 'cl-lib)
(require 'tidal-relay)
(require 'tidal-doremi)
(require 'tidal-parser)
(require 'tidal-macros)

;;; Customization & Faces

(defgroup tidal-highlight nil
  "Real-time event highlighting for TidalCycles."
  :group 'tidal)

(defface tidal-highlight-face
  '((t (:background "yellow" :foreground "black")))
  "The face used for highlighting TidalCycles events."
  :group 'tidal-highlight)

(defcustom tidal-highlight-foreground "black"
  "Foreground color for TidalCycles event highlighting."
  :type 'color
  :group 'tidal-highlight
  :set
  (lambda (sym val)
    (set-default sym val)
    (set-face-foreground 'tidal-highlight-face val)))

(defcustom tidal-highlight-background "medium aquamarine"
  "Background color for TidalCycles event highlighting."
  :type 'color
  :group 'tidal-highlight
  :set
  (lambda (sym val)
    (set-default sym val)
    (set-face-background 'tidal-highlight-face val)))

(defcustom tidal-highlight-udp-port 6012
  "The UDP port to listen on for TidalCycles OSC messages."
  :type 'integer
  :group 'tidal-highlight)

(defcustom tidal-highlight-debug-enabled nil
  "If non-nil, enable debug messages for the highlighting system."
  :type 'boolean
  :group 'tidal-highlight)

(defcustom tidal-highlight-style 'both
  "Controls how highlighting interacts with `pulse.el`."
  :type
  '(choice
    (const :tag "Event Only (disables pulse)" event-only)
    (const :tag "Pulse Only (disables events)" pulse-only)
    (const :tag "Both" both))
  :group 'tidal-highlight)

(defcustom tidal-highlight-fps 30
  "The frame rate for event highlighting animation in frames per second."
  :type 'integer
  :group 'tidal-highlight)

(defcustom tidal-highlight-persistence 0.07
  "Seconds to keep an event active after the signal stops.
Lower this value to reduce 'trails', raise it to fix flickering."
  :type 'float
  :group 'tidal-highlight)

(defcustom tidal-highlight-enable-on-startup t
  "If non-nil, automatically enable `tidal-highlight-mode`."
  :type 'boolean
  :group 'tidal-highlight)

;;; ------------------------------------------------------------------
;;; Rust Native Engine
;;; ------------------------------------------------------------------

(defvar tidal-highlight-animation-timer nil)

(defvar tidal-highlight-active-overlays (make-hash-table :test 'equal)
  "Map of stable-id -> overlay object.")

(defvar tidal-highlight-decay-timers (make-hash-table :test 'equal)
  "Map of stable-id -> timer object.")

(defun tidal-highlight-kill-event (stable-id)
  "Remove the overlay and timer for STABLE-ID."
  ;; 1. Kill the overlay
  (let ((ov (gethash stable-id tidal-highlight-active-overlays)))
    (when ov
      (delete-overlay ov)
      (remhash stable-id tidal-highlight-active-overlays)))

  ;; 2. Kill the timer
  (let ((timer (gethash stable-id tidal-highlight-decay-timers)))
    (when timer
      (cancel-timer timer)
      (remhash stable-id tidal-highlight-decay-timers))))

(defun tidal-highlight-process-batch ()
  "Poll the Rust module and update overlays."
  (let ((raw-events (tidal-relay-poll-events)))
    (when raw-events
      (dolist (evt raw-events)
        (let* ((col-start (nth 0 evt))
               (event-id (1- (nth 1 evt))) ;; Adjust 1-based index
               (col-end (nth 2 evt))
               (stable-id (format "%d-%d" event-id col-start)))

          (if (gethash stable-id tidal-highlight-active-overlays)
              (let ((timer
                     (gethash
                      stable-id tidal-highlight-decay-timers)))
                (when timer
                  (cancel-timer timer))
                (puthash
                 stable-id
                 (run-at-time tidal-highlight-persistence
                              nil
                              #'tidal-highlight-kill-event
                              stable-id)
                 tidal-highlight-decay-timers))

            (let ((event-data
                   `((id . ,stable-id)
                     (colStart . ,col-start)
                     (eventId . ,event-id)
                     (colEnd . ,col-end))))
              (tidal-highlight-add-overlay event-data)
              (puthash
               stable-id
               (run-at-time tidal-highlight-persistence
                            nil
                            #'tidal-highlight-kill-event
                            stable-id)
               tidal-highlight-decay-timers))))))))

(defun tidal-highlight-start-engine ()
  "Load module and start the poll loop."
  (tidal-relay-load-module)
  (tidal-relay-start-server tidal-highlight-udp-port)

  (unless tidal-highlight-animation-timer
    (setq tidal-highlight-animation-timer
          (run-at-time
           0
           (/ 1.0 tidal-highlight-fps)
           #'tidal-highlight-process-batch))))

(defun tidal-highlight-stop-engine ()
  "Stop the render loop and clean up."
  (when tidal-highlight-animation-timer
    (cancel-timer tidal-highlight-animation-timer)
    (setq tidal-highlight-animation-timer nil))
  (maphash
   (lambda (_ v) (cancel-timer v)) tidal-highlight-decay-timers)
  (clrhash tidal-highlight-decay-timers)
  (tidal-highlight-clear-all))

;;; Highlighting Logic (Markers & Overlays)

(defvar tidal-highlight-source-map (make-hash-table :test 'equal)
  "A map from eventId to a nested map of colStart to position markers.")

(defvar tidal-highlight-event-id-counter 0
  "An integer for generating unique event IDs.")

(defvar tidal-highlight-last-debug-state nil)

(defun tidal-highlight-debug (phase &rest args)
  "Show debug ARGS for the current PHASE."
  (when tidal-highlight-debug-enabled
    (let ((current-state (format "%S" (cons phase args))))
      (unless (equal current-state tidal-highlight-last-debug-state)
        (setq tidal-highlight-last-debug-state current-state)
        (apply 'message
               (concat "DEBUG " phase ": " (car args))
               (cdr args))))))

(defun tidal-highlight-add-overlay (event-data)
  "Create and store an overlay for a given EVENT-DATA alist."
  (let* ((event-id (cdr (assoc 'eventId event-data)))
         (col-start (cdr (assoc 'colStart event-data)))
         (stable-id (cdr (assoc 'id event-data)))
         (marker-table (gethash event-id tidal-highlight-source-map))
         (marker-pair
          (and marker-table (gethash col-start marker-table))))

    (when (consp marker-pair)
      (let* ((start-marker (car marker-pair))
             (end-marker (cdr marker-pair))
             (buffer (marker-buffer start-marker)))
        (when (and buffer (buffer-live-p buffer))
          (let ((start-pos (marker-position start-marker))
                (end-pos (marker-position end-marker)))
            (if (and start-pos end-pos)
                (with-current-buffer buffer
                  (let ((overlay
                         (make-overlay start-pos end-pos buffer t t)))
                    (overlay-put overlay 'face 'tidal-highlight-face)
                    (puthash
                     stable-id
                     overlay
                     tidal-highlight-active-overlays)))
              (tidal-highlight-debug
               "Overlay"
               "Error: Marker position is nil"))))))))

(defun tidal-highlight-remove-overlay (unique-id)
  "Remove an active highlight overlay for UNIQUE-ID."
  (let ((overlay (gethash unique-id tidal-highlight-active-overlays)))
    (when overlay
      (delete-overlay overlay)
      (remhash unique-id tidal-highlight-active-overlays))))

;;; Code Transformation & Metadata Injection

(defun tidal-highlight-valid-word-char-p (char-code)
  "Return t if CHAR-CODE is a valid character for a word in TidalCycles mini-notation."
  (or (and (>= char-code ?a) (<= char-code ?z))
      (and (>= char-code ?A) (<= char-code ?Z))
      (and (>= char-code ?0) (<= char-code ?9))
      (= char-code ?.)
      (= char-code ?-)
      (= char-code ?:)))

(defun tidal-highlight-get-word-ranges (s)
  "Extract all word ranges from string S, returning a list of `((start . N) (end . M))` plists."
  (let ((ranges '())
        (current-word-start nil)
        (i 0))
    (while (< i (length s))
      (if (tidal-highlight-valid-word-char-p (aref s i))
          (unless current-word-start
            (setq current-word-start i))
        (when current-word-start
          (push `((start . ,current-word-start) (end . ,i)) ranges)
          (setq current-word-start nil)))
      (cl-incf i))
    (when current-word-start
      (push
       `((start . ,current-word-start) (end . ,(length s))) ranges))
    (reverse ranges)))

(defun tidal-highlight-create-markers
    (event-id content content-buf-offset str-offset)
  "Create and store Emacs markers for each word within CONTENT, associated with EVENT-ID."
  (let ((word-ranges (tidal-highlight-get-word-ranges content)))
    (dolist (range word-ranges)
      (let* ((rel-start (cdr (assoc 'start range)))
             (abs-start (+ 1 str-offset rel-start))
             (w-start (+ content-buf-offset rel-start))
             (w-end (+ content-buf-offset (cdr (assoc 'end range))))
             (s-marker (make-marker))
             (e-marker (make-marker)))
        (set-marker s-marker w-start)
        (set-marker e-marker w-end)
        (let ((m-table (gethash event-id tidal-highlight-source-map)))
          (unless m-table
            (setq m-table (make-hash-table))
            (puthash event-id m-table tidal-highlight-source-map))
          (puthash abs-start (cons s-marker e-marker) m-table))))))

(defvar tidal-highlight-exception-regex
  "\\(?:numerals\\s-*=\\|p\\s-\"[^\"]*\"\\)"
  "Regular expression to identify code patterns that should NOT be transformed.")

(defun tidal-highlight-transform-string (s buffer-offset)
  "Transform TidalCycles mini-notation string S by injecting metadata."
  (let ((result '())
        (last-end 0)
        (start 0))
    (while (string-match "\"\\(\\(?:.\\|\n\\)*?\\)\"" s start)
      (let* ((m-start (match-beginning 0))
             (m-end (match-end 0))
             (content (match-string 1 s))
             (pre (substring s last-end m-start)))
        (push pre result)
        (if (string-match-p tidal-highlight-exception-regex pre)
            (push (substring s m-start m-end) result)
          (let* ((eid (cl-incf tidal-highlight-event-id-counter))
                 (wrapped
                  (format "(deltaContext %d %d \"%s\")"
                          m-start
                          eid
                          content)))
            (tidal-highlight-create-markers
             eid content (+ buffer-offset m-start 1) m-start)
            (push wrapped result))))
      (setq
       last-end (match-end 0)
       start (match-end 0)))
    (push (substring s last-end) result)
    (apply #'concat (reverse result))))

(defun tidal-highlight-advice-send-string (orig s)
  "Advice for `tidal-send-string` to transform S, injecting highlighting metadata."
  (let ((offset
         (if (use-region-p)
             (region-beginning)
           (line-beginning-position))))
    (if offset
        (funcall orig (tidal-highlight-transform-string s offset))
      (funcall orig s))))

(defun tidal-highlight-advice-eval-multilines (orig)
  "Advice for `tidal-eval-multiple-lines` to send the current paragraph as a block."
  (if (eq tidal-highlight-style 'event-only)
      (progn
        (mark-paragraph)
        (let* ((s
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))
               (s*
                (if (bound-and-true-p tidal-literate-p)
                    (tidal-unlit s)
                  s)))
          (tidal-send-string ":{")
          (tidal-send-string s*)
          (tidal-send-string ":}")))
    (funcall orig)))

(defun tidal-highlight-install-advice ()
  "Install advice for TidalCycles functions to enable highlighting."
  (interactive)
  (advice-add
   'tidal-send-string
   :around #'tidal-highlight-advice-send-string)
  (advice-add
   'tidal-eval-multiple-lines
   :around #'tidal-highlight-advice-eval-multilines)
  (advice-add 'tidal-hush :after #'tidal-highlight-clear-all))

(defun tidal-highlight-uninstall-advice ()
  "Uninstall advice for TidalCycles functions, disabling highlighting."
  (interactive)
  (advice-remove
   'tidal-send-string #'tidal-highlight-advice-send-string)
  (advice-remove
   'tidal-eval-multiple-lines
   #'tidal-highlight-advice-eval-multilines)
  (advice-remove 'tidal-hush #'tidal-highlight-clear-all))

;;; Lifecycle

(defun tidal-highlight-clear-all ()
  "Clear all active highlights, source maps, and decay timers. Interactive."
  (interactive)
  (maphash
   (lambda (_ overlay) (delete-overlay overlay))
   tidal-highlight-active-overlays)
  (clrhash tidal-highlight-active-overlays)
  (clrhash tidal-highlight-source-map)
  (clrhash tidal-highlight-decay-timers)
  (setq tidal-highlight-event-id-counter 0))

(define-minor-mode tidal-highlight-mode
  "A minor mode to enable real-time event highlighting."
  :lighter
  " TidalHL"
  (if tidal-highlight-mode
      (progn
        (tidal-highlight-start-engine)
        (tidal-highlight-install-advice))
    (progn
      (tidal-highlight-stop-engine)
      (tidal-highlight-uninstall-advice)
      (tidal-highlight-clear-all))))

(defun tidal-highlight-cleanup-buffer ()
  "Clean up highlighting-related data when a buffer is killed."
  (let ((killed-buffer (current-buffer)))
    (let ((event-ids-to-remove '()))
      (maphash
       (lambda (event-id marker-table)
         (let ((col-starts-to-remove '()))
           (maphash
            (lambda (col-start marker-pair)
              (when (eq
                     (marker-buffer (car marker-pair)) killed-buffer)
                (push col-start col-starts-to-remove)))
            marker-table)
           (dolist (col-start col-starts-to-remove)
             (remhash col-start marker-table))
           (when (= (hash-table-count marker-table) 0)
             (push event-id event-ids-to-remove))))
       tidal-highlight-source-map)
      (dolist (event-id event-ids-to-remove)
        (remhash event-id tidal-highlight-source-map)))
    (let ((ids-to-remove '()))
      (maphash
       (lambda (id overlay)
         (when (eq (overlay-buffer overlay) killed-buffer)
           (push id ids-to-remove)))
       tidal-highlight-active-overlays)
      (dolist (id ids-to-remove)
        (remhash id tidal-highlight-active-overlays)))))

(add-hook 'kill-buffer-hook #'tidal-highlight-cleanup-buffer)

(add-hook
 'tidal-mode-hook
 (lambda ()
   (when tidal-highlight-enable-on-startup
     (tidal-highlight-mode 1))))

(provide 'tidal-highlight)
;;; tidal-highlight.el ends here
