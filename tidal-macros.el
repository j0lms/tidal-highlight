;;; tidal-macros.el --- TidalCycles pattern manipulation macros -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides Lua-inspired pattern manipulation macros for TidalCycles
;; using tidal-parser for pattern handling.

;;; Code:

(require 'cl-lib)
(require 'tidal-parser)

;;; Core Macro Utilities

(defun tidal-macros--get-pattern-context-at-point ()
  "Get the pattern context at point using the robust parser."
  (tidal-parser-get-context-at-point))

(defun tidal-macros--replace-pattern (new-content context)
  "Replace the pattern at CONTEXT with NEW-CONTENT and re-evaluate."
  (let ((start (plist-get context :start))
        (end (plist-get context :end)))
    (delete-region start end)
    (goto-char start)
    (insert new-content)
    ;(save-excursion
    ;  (mark-paragraph)
    ;  (tidal-run-multiple-lines))
    ))

;;; Pattern Rotation

(defun tidal-macros-rotate-pattern (steps)
  "Rotate the pattern at point by STEPS using parser."
  (interactive "nRotate by: ")
  (let ((context (tidal-macros--get-pattern-context-at-point)))
    (when context
      (let* ((content (plist-get context :content))
             (tokens (tidal-parser-parse content))
             (rotated (tidal-parser-rotate tokens steps))
             (new-content (tidal-parser-tokens-to-string rotated)))
        (tidal-macros--replace-pattern new-content context)))))

(defun tidal-macros-rotate-pattern-left ()
  "Rotate the pattern at point left by 1 position."
  (interactive)
  (tidal-macros-rotate-pattern -1))

(defun tidal-macros-rotate-pattern-right ()
  "Rotate the pattern at point right by 1 position."
  (interactive)
  (tidal-macros-rotate-pattern 1))

;;; Number Range Generation

(defun tidal-macros-create-number-range (count)
  "Create a number range from 1 to COUNT at point.
This mimics the Lua builder.lua functionality."
  (interactive "nRange count: ")
  (let ((numbers
         (mapcar 'number-to-string (number-sequence 1 count))))
    (insert (string-join numbers " "))))

;;; Mathematical Operations

(defun tidal-macros-apply-modulo (modulus)
  "Apply MODULUS operation to all numbers in the pattern at point.
This mimics the Lua math.lua modulo functionality."
  (interactive "nModulus: ")
  (let ((context (tidal-macros--get-pattern-context-at-point)))
    (when context
      (let* ((content (plist-get context :content))
             (tokens (tidal-parser-parse content))
             (transformed
              (tidal-macros--apply-modulo-to-tokens tokens modulus))
             (new-content
              (tidal-parser-tokens-to-string transformed)))
        (tidal-macros--replace-pattern new-content context)))))

(defun tidal-macros--apply-modulo-to-tokens (tokens modulus)
  "Apply modulo operation to number tokens in TOKENS with MODULUS."
  (mapcar
   (lambda (token)
     (pcase token
       (`(word ,value)
        (if (string-match-p "^-?[0-9]+$" value)
            (let ((num (string-to-number value)))
              `(word ,(number-to-string (1+ (mod num modulus)))))
          token))
       (`(bracket (,open . ,content))
        `(bracket
          (,open
           .
           ,(tidal-macros--apply-modulo-to-tokens content modulus))))
       (_ token)))
   tokens))

(defun tidal-macros-randomize-numbers (range)
  "Randomize numbers in the pattern within ±RANGE.
This mimics the Lua math.lua random functionality."
  (interactive "nRandomization range: ")
  (let ((context (tidal-macros--get-pattern-context-at-point)))
    (when context
      (let* ((content (plist-get context :content))
             (tokens (tidal-parser-parse content))
             (transformed
              (tidal-macros--randomize-tokens tokens range))
             (new-content
              (tidal-parser-tokens-to-string transformed)))
        (tidal-macros--replace-pattern new-content context)))))

(defun tidal-macros--randomize-tokens (tokens range)
  "Randomize number tokens in TOKENS within ±RANGE."
  (mapcar
   (lambda (token)
     (pcase token
       (`(word ,value)
        (if (string-match-p "^-?[0-9]+$" value)
            (let* ((num (string-to-number value))
                   (min-val (max 1 (- num range)))
                   (max-val (+ num range))
                   (new-num
                    (+ min-val (random (1+ (- max-val min-val))))))
              `(word ,(number-to-string new-num)))
          token))
       (`(bracket (,open . ,content))
        `(bracket
          (,open . ,(tidal-macros--randomize-tokens content range))))
       (_ token)))
   tokens))

;;; Pattern Grouping

(defun tidal-macros-toggle-grouping (&optional separator)
  "Toggle between grouped and ungrouped pattern elements split by SEPARATOR."
  (interactive)
  (let ((sep
         (or separator
             (read-string "Group separator (e.g., ! or @): " "!"))))
    ;; (message "Toggling grouping with separator: %s" sep)
    (let ((context (tidal-macros--get-pattern-context-at-point)))
      (if (not context)
          (message "No pattern context found at point")
        ;; (message "Context found: %S" context)
        (let*
            ((content (plist-get context :content))
             (tokens (tidal-parser-parse content))
             (has-grouped
              (tidal-macros--has-grouped-tokens tokens sep))
             ;; FIX: Move transformed inside the same let* so it's in scope
             (transformed
              (if has-grouped
                  (tidal-macros--ungroup-tokens tokens sep)
                (tidal-macros--group-tokens tokens sep)))
             (new-content
              (tidal-parser-tokens-to-string transformed)))
          ;; (message "New content: %S" new-content)
          (tidal-macros--replace-pattern new-content context))))))

(defun tidal-macros-toggle-grouping-bang ()
  "Toggle grouping using ! as separator."
  (interactive)
  (tidal-macros-toggle-grouping "!")
  (let ((char-to-check (char-before)))
    (when (and char-to-check (eq (char-syntax char-to-check) ?\ ))
      (delete-char -1))))

(defun tidal-macros-toggle-grouping-at ()
  "Toggle grouping using @ as separator."
  (interactive)
  (tidal-macros-toggle-grouping "@")
  (let ((char-to-check (char-before)))
    (when (and char-to-check (eq (char-syntax char-to-check) ?\ ))
      (delete-char -1))))

(defun tidal-macros--has-grouped-tokens (tokens separator)
  "Check if TOKENS contain any grouped elements with SEPARATOR."
  (cl-some
   (lambda (token)
     (pcase token
       (`(word ,value)
        ;; (message "Checking word: %s for pattern %s" value (format ".*%s[0-9]+" (regexp-quote separator)))  ; Debug
        (string-match-p
         (format ".*%s[0-9]+" (regexp-quote separator)) value))
       (`(bracket (,open . ,content))
        (tidal-macros--has-grouped-tokens content separator))
       (_ nil)))
   tokens))

(defun tidal-macros--group-tokens (tokens separator)
  "Group repeated tokens in TOKENS using SEPARATOR."
  ;; (message "Grouping tokens: %S" tokens)  ; Debug
  (let ((grouped
         (tidal-macros--group-token-sequence tokens separator)))
    ;; (message "Grouped result: %S" grouped)  ; Debug
    grouped))

;; Fixed grouping algorithm that doesn't duplicate whitespace
(defun tidal-macros--group-token-sequence (tokens separator)
  "Group a sequence of TOKENS using SEPARATOR."
  (let ((result '())
        (current nil)
        (count 1)
        (pending-whitespace nil))
    (dolist (token tokens)
      (pcase token
        (`(whitespace ,value) (setq pending-whitespace token))
        (`(word ,value)
         (if (equal `(word ,value) current)
             (cl-incf count)
           (when current
             (if (> count 1)
                 (push (tidal-macros--create-grouped-token
                        current count separator)
                       result)
               (push current result))
             (when pending-whitespace
               (push pending-whitespace result)
               (setq pending-whitespace nil)))
           (setq current `(word ,value))
           (setq count 1)))
        (_
         (when current
           (if (> count 1)
               (push (tidal-macros--create-grouped-token
                      current count separator)
                     result)
             (push current result))
           (setq current nil)
           (setq count 1))
         (when pending-whitespace
           (push pending-whitespace result)
           (setq pending-whitespace nil))
         (push token result))))

    (when current
      (if (> count 1)
          (push (tidal-macros--create-grouped-token
                 current count separator)
                result)
        (push current result))
      (when pending-whitespace
        (push pending-whitespace result)))

    (reverse result)))

(defun tidal-macros--groupable-token-p (token)
  "Return t if TOKEN can be grouped (words and brackets)."
  (pcase token
    (`(word ,_) t)
    (`(bracket ,_) t)
    (_ nil)))

(defun tidal-macros--create-grouped-token (token count separator)
  "Create a grouped token from TOKEN with COUNT and SEPARATOR."
  (pcase token
    (`(word ,value)
     (let ((grouped (format "%s%s%d" value separator count)))
       ;; (message "Creating grouped token: %s -> %s" value grouped)  ; Debug
       `(word ,grouped)))
    (`(bracket (,open . ,content))
     `(bracket
       (,open
        . ,(tidal-macros--group-token-sequence content separator))))
    (_ token)))

(defun tidal-macros--ungroup-tokens (tokens separator)
  "Ungroup tokens in TOKENS that use SEPARATOR."
  (mapcan
   (lambda (token)
     (pcase token
       (`(word ,value)
        (if (string-match
             (format "^\\(.+\\)%s\\([0-9]+\\)$"
                     (regexp-quote separator))
             value)
            (let ((element (match-string 1 value))
                  (count (string-to-number (match-string 2 value))))
              (make-list count `(word ,element)))
          (list token)))
       (`(bracket (,open . ,content))
        (list
         `(bracket
           (,open
            . ,(tidal-macros--ungroup-tokens content separator)))))
       (_ (list token))))
   tokens))

;;; Bracket Wrapping

(defun tidal-macros-wrap-stack ()
  "Wrap the selected region with square brackets [ ] for Stack."
  (interactive)
  (tidal-macros-wrap-region "[" "]"))

(defun tidal-macros-wrap-cat ()
  "Wrap the selected region with angle brackets < > for Cat."
  (interactive)
  (tidal-macros-wrap-region "<" ">"))

(defun tidal-macros-wrap-region (open close)
  "Wrap the selected region with OPEN and CLOSE brackets."
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (content
             (buffer-substring-no-properties
              (region-beginning) (region-end))))
        (delete-region start end)
        (goto-char start)
        (insert open content close)
        ;; (mark-paragraph)
        ;; (tidal-run-multiple-lines)
        )
    (message "No region selected")))

;;; Random Replacement

(defun tidal-macros--collect-leaf-tokens (tokens)
  "Recursively traverse TOKENS and return a list of all 'word' token objects.
Returns the actual cons cells, allowing in-place modification."
  (let ((collection '()))
    (dolist (token tokens)
      (pcase token
        ;; If it's a word, add the token object itself to our collection
        (`(word ,_) (push token collection))

        ;; If it's a bracket, recurse into its content (the cdr of the second element)
        (`(bracket (,open . ,content))
         (setq collection
               (append
                (tidal-macros--collect-leaf-tokens content)
                collection)))

        ;; Ignore whitespace, multipliers, etc.
        (_ nil)))
    ;; Return flattened list of all replaceable word tokens found in the tree
    (nreverse collection)))

(defun tidal-macros-replace-random-recursive (count)
  "Recursively replace COUNT random words with a replacement string.
This dives into brackets (e.g., [bd sn]) and targets individual sounds
while preserving the container structure and multipliers."
  (interactive "nNumber of elements to replace: ")
  (let ((replacement
         (read-string "Replacement string (default ~): " nil nil "~"))
        (context (tidal-macros--get-pattern-context-at-point)))

    (if (not context)
        (message "No pattern context found.")

      (let* ((content (plist-get context :content))
             ;; 1. Parse
             (tokens (tidal-parser-parse content))

             ;; 2. Collect ALL word tokens (flattened references)
             (candidates (tidal-macros--collect-leaf-tokens tokens))
             (total-candidates (length candidates)))

        (if (zerop total-candidates)
            (message "No words found to replace.")

          ;; 3. Determine which unique indices to target
          (let* ((safe-count (min count total-candidates))
                 (indices
                  (tidal-macros--random-indices
                   total-candidates safe-count)))

            ;; 4. Mutate the chosen tokens in place
            (dolist (idx indices)
              (let ((token-to-modify (nth idx candidates)))
                ;; token-to-modify is a reference like (word "bd")
                ;; We change the value (2nd element) to the replacement.
                (setf (nth 1 token-to-modify) replacement)))

            ;; 5. Reconstruct and Replace
            (let ((new-content
                   (tidal-parser-tokens-to-string tokens)))
              (tidal-macros--replace-pattern new-content context)
              (message "Recursively replaced %d element(s) with '%s'"
                       safe-count
                       replacement))))))))

(defun tidal-macros--random-indices (limit count)
  "Return a list of COUNT unique random indices between 0 and LIMIT-1."
  (let ((indices '())
        (available (number-sequence 0 (1- limit))))
    (dotimes (_ (min count limit))
      (let* ((rand-idx (random (length available)))
             (val (nth rand-idx available)))
        (push val indices)
        (setq available
              (append
               (subseq available 0 rand-idx)
               (subseq available (1+ rand-idx))))))
    indices))

;;; Keybindings

;; (defun tidal-macros-setup-keybindings ()
;;   "Set up keybindings for tidal macros."
;;   (eval-after-load 'tidal
;;     '(progn
;;        ;; Rotation
;;        (define-key tidal-mode-map (kbd "C-c t r l") 'tidal-macros-rotate-pattern-left)
;;        (define-key tidal-mode-map (kbd "C-c t r r") 'tidal-macros-rotate-pattern-right)
;;        (define-key tidal-mode-map (kbd "C-c t r") 'tidal-macros-rotate-pattern)

;;        ;; Number ranges
;;        (define-key tidal-mode-map (kbd "C-c t n") 'tidal-macros-create-number-range)

;;        ;; Mathematical operations
;;        (define-key tidal-mode-map (kbd "C-c t %") 'tidal-macros-apply-modulo)
;;        (define-key tidal-mode-map (kbd "C-c t R") 'tidal-macros-randomize-numbers)

;;        ;; Grouping
;;        (define-key tidal-mode-map (kbd "C-c t g !") 'tidal-macros-toggle-grouping-bang)
;;        (define-key tidal-mode-map (kbd "C-c t g @") 'tidal-macros-toggle-grouping-at)
;;        (define-key tidal-mode-map (kbd "C-c t g") 'tidal-macros-toggle-grouping)

;;        ;; Random replacement
;;        (define-key tidal-mode-map (kbd "C-c t ~") 'tidal-macros-replace-random)

;;        ;; Bracket wrapping (visual mode)
;;        (define-key tidal-mode-map (kbd "C-c t [") 'tidal-macros-wrap-stack)
;;        (define-key tidal-mode-map (kbd "C-c t <") 'tidal-macros-wrap-cat))))

(provide 'tidal-macros)
;;; tidal-macros.el ends here
