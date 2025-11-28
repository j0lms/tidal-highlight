;;; tidal-parser.el --- TidalCycles mini-notation parser -*- lexical-binding: t; -*-

;;; Commentary:
;; A state-aware parser for TidalCycles mini-notation that handles nested
;; structures, multipliers, and complex pattern syntax.

;;; Code:

(defun tidal-parser--valid-word-char-p (char)
  "Return t if CHAR is a valid TidalCycles word character."
  (or (and (>= char ?a) (<= char ?z))
      (and (>= char ?A) (<= char ?Z))
      (and (>= char ?0) (<= char ?9))
      (member char '(?\. ?- ?: ?@ ?% ?* ?! ?~ ?'))))

(defun tidal-parser--whitespace-p (char)
  "Return t if CHAR is whitespace."
  (member char '(?  ?\t ?\n)))

(defun tidal-parser--bracket-p (char)
  "Return t if CHAR is an opening bracket."
  (member char '(?\( ?\[ ?\{ ?\<)))

(defun tidal-parser--matching-bracket (open-char)
  "Return the matching closing bracket for OPEN-CHAR."
  (pcase open-char
    (?\( ?\))
    (?\[ ?\])
    (?\{ ?\})
    (?\< ?\>)
    (_ nil)))

(defun tidal-parser--parse-token (string &optional start-index)
  "Parse a single token from STRING starting at START-INDEX.
Returns (token-type token-value new-index)."
  (let ((index (or start-index 0))
        (len (length string)))
    (cond
     ((>= index len)
      '(eof nil index))

     ((tidal-parser--whitespace-p (aref string index))
      (let ((start index))
        (while (and (< index len)
                    (tidal-parser--whitespace-p (aref string index)))
          (cl-incf index))
        `(whitespace ,(substring string start index) ,index)))

     ((tidal-parser--bracket-p (aref string index))
      (let ((open-char (aref string index))
            (start index))
        (cl-incf index)
        (let ((content
               (tidal-parser--parse-bracketed-content
                string
                index
                (tidal-parser--matching-bracket open-char))))
          (if content
              `(bracket
                ,(cons open-char (car content)) ,(cdr content))
            `(error "Unmatched bracket" ,index)))))

     ((= (aref string index) ?*)
      ;; Multiplier token - check if it's attached to a word or bracket
      (if (and
           (> index 0)
           (or
            (tidal-parser--valid-word-char-p (aref string (1- index)))
            (= (aref string (1- index)) ?\]) ; Allow after brackets
            (= (aref string (1- index)) ?\))
            (= (aref string (1- index)) ?\})))
          ;; It's attached to previous token, parse as multiplier
          (let ((start index))
            (cl-incf index)
            (when (and (< index len) (<= ?0 (aref string index) ?9))
              (while (and (< index len)
                          (<= ?0 (aref string index) ?9))
                (cl-incf index)))
            `(multiplier ,(substring string start index) ,index))
        ;; Standalone multiplier (shouldn't happen in valid Tidal)
        (let ((start index))
          (cl-incf index)
          `(symbol "*" ,index))))

     ((= (aref string index) ?/)
      ;; Division token - similar logic to multiplier
      (if (and
           (> index 0)
           (or
            (tidal-parser--valid-word-char-p (aref string (1- index)))
            (= (aref string (1- index)) ?\]) ; Allow after brackets
            (= (aref string (1- index)) ?\))
            (= (aref string (1- index)) ?\})))
          (let ((start index))
            (cl-incf index)
            (when (and (< index len) (<= ?0 (aref string index) ?9))
              (while (and (< index len)
                          (<= ?0 (aref string index) ?9))
                (cl-incf index)))
            `(divisor ,(substring string start index) ,index))
        ;; Standalone division
        (let ((start index))
          (cl-incf index)
          `(symbol "/" ,index))))

     ((= (aref string index) ?,)
      ;; Comma - always parse as symbol
      `(symbol "," ,(1+ index)))

     (t
      ;; Word token (including multipliers/divisors that are part of words)
      (let ((start index))
        (while (and (< index len)
                    (or (tidal-parser--valid-word-char-p
                         (aref string index))
                        (member (aref string index) '(?* ?/))))
          (cl-incf index))
        (if (> index start)
            `(word ,(substring string start index) ,index)
          ;; Single character symbol
          `(symbol
            ,(substring string index (1+ index)) ,(1+ index))))))))

(defun tidal-parser--find-word-start (string index)
  "Find the start of a word that might include multipliers at INDEX in STRING."
  (let ((start index))
    (while (and (> start 0)
                (or (tidal-parser--valid-word-char-p
                     (aref string (1- start)))
                    (member (aref string (1- start)) '(?* ?/))))
      (cl-decf start))
    start))

(defun tidal-parser--parse-bracketed-content
    (string start-index close-char)
  "Parse content between brackets in STRING from  START-INDEX to CLOSE-CHAR.
Returns (parsed-content . new-index) or nil if unmatched."
  (let ((tokens '())
        (index start-index)
        (len (length string)))
    (while (and (< index len)
                (not (= (aref string index) close-char)))
      (let ((token-result (tidal-parser--parse-token string index)))
        (pcase token-result
          (`(eof ,_ ,new-index)
           (setq index new-index)
           nil) ; Unmatched bracket
          (`(error ,_ ,new-index)
           (setq index new-index)
           nil)
          (`(,type ,value ,new-index)
           (push (list type value) tokens)
           (setq index new-index)))))
    (when (and (< index len) (= (aref string index) close-char))
      (cons (reverse tokens) (1+ index))))) ; Reverse to get correct order

(defun tidal-parser-parse (string)
  "Parse a TidalCycles mini-notation STRING into a token stream."
  (let ((tokens '())
        (index 0)
        (len (length string)))
    (while (< index len)
      (let ((token-result (tidal-parser--parse-token string index)))
        (pcase token-result
          (`(eof ,_ ,new-index) (setq index new-index))
          (`(error ,msg ,new-index)
           (message "Parse error at position %d: %s" new-index msg)
           (setq index new-index))
          (`(,type ,value ,new-index)
           (push (list type value) tokens)
           (setq index new-index)))))
    (reverse tokens)))

;;; Pattern Reconstruction

(defun tidal-parser-tokens-to-string (tokens)
  "Convert TOKENS back to a string representation with proper spacing."
  (let ((result "")
        (prev-token nil))
    (dolist (token tokens)
      (pcase token
        (`(word ,value)
         (setq result
               (concat
                result
                (if (and prev-token
                         (not
                          (tidal-parser--whitespace-token-p
                           prev-token))
                         (not
                          (tidal-parser--comma-token-p prev-token)))
                    " "
                  "")
                value)))
        (`(symbol ,value)
         ;; Handle commas and other symbols with special spacing
         (cond
          ((string= value ",")
           (setq result (concat result value))) ; Comma attaches to previous with no space
          (t
           (setq result
                 (concat
                  result
                  (if (and prev-token
                           (not
                            (tidal-parser--whitespace-token-p
                             prev-token)))
                      " "
                    "")
                  value)))))
        (`(multiplier ,value)
         ;; Multipliers attach directly to previous token without space
         (setq result (concat result value)))
        (`(divisor ,value)
         ;; Divisors attach directly to previous token without space
         (setq result (concat result value)))
        (`(whitespace ,value) (setq result (concat result value)))
        (`(bracket (,open . ,content))
         (setq result
               (concat
                result
                (if (and prev-token
                         (not
                          (tidal-parser--whitespace-token-p
                           prev-token))
                         (not
                          (tidal-parser--comma-token-p prev-token)))
                    " "
                  "")
                (string open)
                (tidal-parser-tokens-to-string content)
                (string (tidal-parser--matching-bracket open)))))
        (_ (setq result (concat result ""))))
      (setq prev-token token))
    result))

(defun tidal-parser--whitespace-token-p (token)
  "Return t if TOKEN is a whitespace token."
  (eq (car token) 'whitespace))

(defun tidal-parser--word-token-p (token)
  "Return t if TOKEN is a word token."
  (eq (car token) 'word))

(defun tidal-parser--comma-token-p (token)
  "Return t if TOKEN is a comma symbol."
  (and (eq (car token) 'symbol) (string= (cadr token) ",")))

;;; Context Detection

(defun tidal-parser-get-context-at-point ()
  "Find the pattern context at point.
Returns a plist with :start, :end, :content, and :type."
  (save-excursion
    (let (start
          end
          context-type)
      ;; Find quoted string boundary
      (setq start (save-excursion (re-search-backward "\"" nil t)))
      (when start
        (setq end
              (save-excursion
                (goto-char (1+ start))
                (re-search-forward "\"" nil t))))

      (when (and start end (>= (point) start) (<= (point) end))
        (setq context-type 'string)
        (let ((content-start (1+ start))
              (content-end (1- end)))

          ;; Check for nested bracket contexts
          (dolist (brackets
                   '((?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\( ?\))))
            (let* ((open-char (car brackets))
                   (close-char (cadr brackets))
                   (inner-start
                    (save-excursion
                      (re-search-backward
                       (regexp-quote (string open-char))
                       content-start t)))
                   (inner-end
                    (and inner-start
                         (save-excursion
                           (goto-char (1+ inner-start))
                           (re-search-forward (regexp-quote
                                               (string close-char))
                                              content-end t)))))
              (when (and inner-start
                         inner-end
                         (>= (point) inner-start)
                         (<= (point) inner-end))
                (setq
                 content-start (1+ inner-start)
                 content-end (1- inner-end)
                 context-type 'bracket))))

          (list
           :start content-start
           :end content-end
           :content
           (buffer-substring-no-properties content-start content-end)
           :type context-type))))))

;;; Pattern Manipulation Utilities

(defun tidal-parser-rotate (tokens step)
  "Rotate TOKENS by STEP positions."
  (let* ((rotatable (tidal-parser--filter-rotatable tokens))
         (rotated (tidal-parser--rotate-list rotatable step))
         (result (tidal-parser--replace-rotatable tokens rotated)))
    result))

(defun tidal-parser--filter-rotatable (tokens)
  "Extract rotatable elements from TOKENS (words and brackets)."
  (cl-remove-if-not
   (lambda (token)
     (pcase (car token)
       ('word t)
       ('bracket t)
       (_ nil)))
   tokens))

(defun tidal-parser--replace-rotatable (tokens new-rotatable)
  "Replace rotatable elements in TOKENS with NEW-ROTATABLE."
  (let ((rotatable-list new-rotatable)
        (result '()))
    (dolist (token tokens)
      (pcase token
        ((or `(word ,_) `(bracket ,_))
         (when rotatable-list
           (push (car rotatable-list) result)
           (setq rotatable-list (cdr rotatable-list))))
        (_ (push token result))))
    (reverse result)))

(defun tidal-parser--rotate-list (lst step)
  "Rotate list LST by STEP positions."
  (let* ((len (length lst)))
    (if (zerop len)
        lst
      (let ((actual-steps (mod step len)))
        (append
         (nthcdr (- len actual-steps) lst)
         (butlast lst actual-steps))))))

;;; Test Functions

(defun tidal-parser-test (string)
  "Test the parser on STRING and display results."
  (interactive "sTest pattern: ")
  (let ((tokens (tidal-parser-parse string)))
    (with-current-buffer (get-buffer-create "*Tidal Parser Test*")
      (erase-buffer)
      (insert "Original: " string "\n\n")
      (insert "Parsed tokens:\n")
      (dolist (token tokens)
        (insert (format "  %S\n" token)))
      (insert
       "\nRegenerated: " (tidal-parser-tokens-to-string tokens) "\n")
      (insert
       "\nOriginal == Regenerated: "
       (if (string= string (tidal-parser-tokens-to-string tokens))
           "YES"
         "NO")
       "\n")
      (display-buffer (current-buffer)))))

(provide 'tidal-parser)

;;; tidal-parser.el ends here
