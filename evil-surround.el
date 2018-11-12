;;; evil-surround.el --- emulate surround.vim from Vim

;; Copyright (C) 2010 - 2017 Tim Harper

;; Licensed under the same terms as Emacs (GPLv3)

;;
;; Author: Tim Harper <timcharper at gmail dot com>
;;      Vegard Øye <vegard_oye at hotmail dot com>
;; Current Maintainer: ninrod (github.com/ninrod)
;; Created: July 23 2011
;; Version: 0.1
;; Package-Requires: ((evil "1.2.12"))
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;; Keywords: emulation, vi, evil
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package emulates surround.vim by Tim Pope.
;; The functionality is wrapped into a minor mode. To enable
;; it globally, add the following lines to ~/.emacs:
;;
;;     (require 'evil-surround)
;;     (global-evil-surround-mode 1)
;;
;; Alternatively, you can enable evil-surround-mode along a major mode
;; by adding `turn-on-evil-surround-mode' to the mode hook.
;;
;; This package uses Evil as its vi layer. It is available from:
;;
;;     https://github.com/emacs-evil/evil

;;; Code:

(require 'evil)

(defgroup evil-surround nil
  "surround.vim for Emacs"
  :prefix "evil-surround-"
  :group 'evil)

;; make surround's `ysw' work like `cw', not `ce'
(when (boundp 'evil-change-commands)
  (add-to-list 'evil-change-commands 'evil-surround-region))

(defcustom evil-surround-pairs-alist
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    ;; (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    (?t . evil-surround-special-t)
    (?< . evil-surround-special-t)
    (?f . evil-surround-special-f)
    (?b . evil-surround-between)
    (?g . evil-surround-generic))
  "Association list of surround items.
Each item is of the form (TRIGGER . (LEFT . RIGHT)), all strings.
Alternatively, a function can be put in place of (LEFT . RIGHT).
This only affects inserting pairs, not deleting or changing them."
  :group 'evil-surround
  :type '(alist
          :key-type (character :tag "Key")
          :value-type (choice
                       (cons (string :tag "Opening") (string :tag "Closing"))
                       (function :tag "Function"))))
(make-variable-buffer-local 'evil-surround-pairs-alist)

(defcustom evil-surround-lisp-modes '(
				      cider-repl-mode
				      clojure-mode
				      clojurec-mode
				      clojurescript-mode
				      clojurex-mode
				      common-lisp-mode
				      emacs-lisp-mode
				      eshell-mode
				      geiser-repl-mode
				      inf-clojure-mode
				      inferior-emacs-lisp-mode
				      inferior-lisp-mode
				      inferior-scheme-mode
				      lisp-interaction-mode
				      lisp-mode
				      monroe-mode
				      racket-mode
				      racket-repl-mode
				      scheme-interaction-mode
				      scheme-mode
				      slime-repl-mode
				      stumpwm-mode
				      )
  "List of Lisp-related modes."
  :type '(repeat symbol)
  :group 'evil-surround)

(defcustom evil-surround-latex-modes '(latex-mode LaTeX-mode)
  "List of latex-related modes"
  :type '(repeat symbol)
  :group 'evil-surround)

(defcustom evil-surround-shell-modes '(shell-script-mode sh-mode)
  "List of shell-scripting-related modes"
  :type '(repeat symbol)
  :group 'evil-surround)

(defcustom evil-surround-rust-modes '(rust-mode)
  "List of rust modes"
  :type '(repeat symbol)
  :group 'evil-surround)

(defcustom evil-surround-operator-alist
  '((evil-change . change)
    (evil-delete . delete))
  "Association list of operators to their fundamental operation.
Each item is of the form (OPERATOR . OPERATION)."
  :group 'evil-surround
  :type '(repeat (cons (symbol :tag "Operator")
                       (symbol :tag "Operation"))))

(defvar evil-surround-read-tag-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map ">" 'exit-minibuffer)
    map)
  "Keymap used by `evil-surround-read-tag'.")

(defvar evil-surround-record-repeat nil
  "Flag to indicate we're manually recording repeat info.")

(defvar evil-surround-overlaying nil
  "Whether or not we are currently gathering data for the overlays to
  use. Useful to know if a text object wants to get input from the
  user and don't want to get asked twice when changing or deleting.")

(defvar evil-surround-overlaying-args nil)

(defun evil-surround-read-from-minibuffer (&rest args)
  (when evil-surround-record-repeat
    (evil-repeat-keystrokes 'post))
  (let ((res (apply #'read-from-minibuffer args)))
    (when evil-surround-record-repeat
      (evil-repeat-record res))
    res))

; special ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-surround-specials-f '((t . nil))
  "Defines what pair should be inserted in a certain mode when f is pressed.
This is a list on the form (((modes..) . func)..) where func will be
used in modes to create a surround pair.
Exactly one can be on the form ((t . func)), which is the default function to run.")

(defvar evil-surround-specials-t '((t . nil))
  "see `evil-surround-specials-f'")

(defmacro evil-surround-add-to (slist modes f)
  "Adds the association of modes -> f in `evil-surround-specials-t' (slist)
Example:
When t is pressed and in omg-mode or geez-mode, run random-func to find surround pair.
  (evil-surround-add-to t '(omg-mode geez-mode) #'random-func)

Same as above but sets the default one.
  (evil-surround-add-to f t #'random-func-again)"
  (let ((tmodes (gensym))
        (tf (gensym))
        (slist (intern-soft (concat "evil-surround-specials-" (symbol-name slist)))))
    `(let ((,tmodes ,modes)
           (,tf ,f))
       (if (eq ,tmodes t)
           (let ((getted (assq t ,slist)))
             (when getted
               (setf (cdr getted) ,tf)))
         (push (cons ,tmodes ,tf)
               ,slist)))))

(defun evil-surround-execute-special (arg)
  "runs the associated function in arg for the current major mode."
  (let ((x (find-if
            (lambda (x)
              (or (eq x t) (apply #'derived-mode-p x)))
            arg
            :key #'car)))
    (when x
      (funcall (cdr x)))))

(defun evil-surround-special-f ()
  "`evil-surround-execute-special' but for `evil-surround-specials-f'"
  (interactive)
  (evil-surround-execute-special evil-surround-specials-f))

(defun evil-surround-special-t ()
  "`evil-surround-execute-special' but for `evil-surround-specials-t'"
  (interactive)
  (evil-surround-execute-special evil-surround-specials-t))

(defmacro evil-surround-generic-read (leftf rightf)
  "Returns a function that reads som input from the user and puts it with `format' on leftf."
  `(lambda ()
     (let ((fname (evil-surround-read-from-minibuffer "" "")))
       (cons (format ,leftf fname) ,rightf))))

(defmacro evil-surround-generic-const (leftf rightf)
  "returns a function that just returns (cons leftf rightf)"
  `(lambda ()
     (cons ,leftf ,rightf)))

(evil-surround-add-to t t #'evil-surround-read-tag)
(evil-surround-add-to t evil-surround-rust-modes (evil-surround-generic-read "%s<" ">"))

(evil-surround-add-to f t (evil-surround-generic-read "%s(" ")"))
(evil-surround-add-to f evil-surround-lisp-modes (evil-surround-generic-read "(%s " ")"))
(evil-surround-add-to f evil-surround-latex-modes (evil-surround-generic-read "\\%s{" "}"))
(evil-surround-add-to f evil-surround-shell-modes (evil-surround-generic-const "\"$(" ")\""))

(defun evil-surround-read-tag ()
  "Read a XML tag from the minibuffer."
  (let* ((input (evil-surround-read-from-minibuffer "<" "" evil-surround-read-tag-map))
         (match (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" (or tag "") (or rest ""))
          (format "</%s>" (or tag "")))))

(defun evil-surround-valid-char-p (char)
  "Returns whether CHAR is a valid surround char or not."
  (not (memq char '(?\C-\[ ?\C-?))))

(defun evil-surround-pair (char)
  "Return the evil-surround pair of char.
This is a cons cell (LEFT . RIGHT), both strings."
  (let ((pair (assoc-default char evil-surround-pairs-alist)))
    (cond
     ((functionp pair)
      (funcall pair))

     ((consp pair)
      pair)

     (t
      (cons (format "%c" char) (format "%c" char))))))

(defun evil-surround-lookup-key (char outer)
  "Returns the textobject function from CHAR. If OUTER is non-nil,
outer text objects are searched for, if nil then inner objects
considered.

If no text objects is found, nil is returned.

This searches in `evil-operator-state-local-map',
`evil-visual-state-local-map' and either
`evil-outer-text-objects-map' or `evil-inner-text-objects-map'
depending on OUTER. "
  (let* ((map (if outer evil-outer-text-objects-map evil-inner-text-objects-map))
         (prefix (if outer "a" "i"))
         (key (kbd (format "%s %c" prefix char))))
    (or
     (and
      (evil-operator-state-p)
      (evil-lookup-key evil-operator-state-local-map key))
     (and
      (evil-visual-state-p)
      (evil-lookup-key evil-visual-state-local-map key))
     (evil-lookup-key map (kbd (string char))))))

(defun evil-surround-outer-overlay (char)
  "Return outer overlay for the delimited range represented by CHAR.
This overlay includes the delimiters.
See also `evil-surround-inner-overlay'."
  (let ((outer (evil-surround-lookup-key char t)))
    (when outer
      (setq outer (funcall outer))
      (when (evil-range-p outer)
        (evil-surround-trim-whitespace-from-range outer "[[:space:]]")
        (setq outer (make-overlay (evil-range-beginning outer)
                                  (evil-range-end outer)
                                  nil nil t))))))

(defun evil-surround-trim-whitespace-from-range (range &optional regexp)
  "Given an evil-range, trim whitespace around range by shrinking the range such that it neither begins nor ends with whitespace. Does not modify the buffer."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (while (looking-at regexp) (forward-char))
        (evil-set-range-beginning range (point))
        (goto-char (evil-range-end range))
        (while (looking-back regexp nil) (backward-char))
        (evil-set-range-end range (point))))))

(defun evil-surround-inner-overlay (char)
  "Return inner overlay for the delimited range represented by CHAR.
This overlay excludes the delimiters.
See also `evil-surround-outer-overlay'."
  (let ((inner (evil-surround-lookup-key char nil)))
    (when inner
      (setq inner (funcall inner))
      (when (evil-range-p inner)
        (when (eq (char-syntax char) ?\()
          (evil-surround-trim-whitespace-from-range inner "[[:space:]]"))
        (setq inner (make-overlay (evil-range-beginning inner)
                                  (evil-range-end inner)
                                  nil nil t))))))

(evil-define-motion evil-surround-line (count)
  "Move COUNT - 1 lines down but return exclusive character motion."
  :type exclusive
  (let ((beg (line-beginning-position)))
    (evil-line count)
    (end-of-line)
    (let ((range (evil-range beg (point) 'exclusive)))
      (evil-expand-range range)
      range)))

;;;###autoload
(defun evil-surround-delete (char &optional outer inner)
  "Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted."
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    ;; no overlays specified: create them on the basis of CHAR
    ;; and delete after use
    (let* ((outer (evil-surround-outer-overlay char))
           (evil-surround-overlaying t)
           (inner (evil-surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (evil-surround-delete char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

;;;###autoload
(defun evil-surround-change (char &optional outer inner)
  "Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `evil-surround-delete'."
  (interactive "c")
  (cond
   ((and outer inner)
    (evil-surround-delete char outer inner)
    (let ((key (read-char)))
      (evil-surround-region (overlay-start outer)
                            (overlay-end outer)
                            nil (if (evil-surround-valid-char-p key) key char))))
   (t
    (let* ((outer (evil-surround-outer-overlay char))
           (evil-surround-overlaying t)
           (inner (evil-surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (evil-surround-change char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

(defun evil-surround-interactive-setup ()
  (setq evil-inhibit-operator t)
     (list (assoc-default evil-this-operator
                          evil-surround-operator-alist)))

(defun evil-surround-setup-surround-line-operators ()
  (define-key evil-operator-shortcut-map "s" 'evil-surround-line)
  (define-key evil-operator-shortcut-map "S" 'evil-surround-line))

(defun evil-surround-column-at (pos)
  (save-excursion (goto-char pos) (current-column)))

(defun evil-surround-block (beg end char)
  "Surrounds a block selection with a character, as if `evil-surround-region'
were called on each segment in each line. This skips lines where EOL < BEG's
column."
  (let ((beg-col (evil-surround-column-at beg))
        (end-col (evil-surround-column-at end)))
    (evil-apply-on-block
     (lambda (ibeg iend)
       (unless (< (evil-surround-column-at ibeg) (min beg-col end-col))
         (evil-surround-region ibeg iend t char)))
     beg end nil)))

(defun evil-surround-call-with-repeat (callback)
  "Record keystrokes to repeat surround-region operator and it's motion.
This is necessary because `evil-yank' operator is not repeatable (:repeat nil)"
  (evil-repeat-start)
  (evil-repeat-record "y")
  (evil-repeat-record (this-command-keys))

  ;; set `this-command-keys' to the command that will be executed
  ;; interactively; as a result, `evil-this-operator' will be
  ;; correctly set to, for example, `evil-surround-region' instead of
  ;; `evil-yank' when surround has been invoked by `ys'
  (setq this-command callback)
  (let ((evil-surround-record-repeat t))
    (call-interactively callback))
  (evil-repeat-keystrokes 'post)
  (evil-repeat-stop))

;; Dispatcher function in Operator-Pending state.
;; "cs" calls `evil-surround-change', "ds" calls `evil-surround-delete',
;; and "ys" calls `evil-surround-region'.
(evil-define-command evil-surround-edit (operation)
  "Edit the surrounding delimiters represented by CHAR.
If OPERATION is `change', call `evil-surround-change'.
if OPERATION is `delete', call `evil-surround-delete'.
Otherwise call `evil-surround-region'."
  (interactive (evil-surround-interactive-setup))
  (save-excursion
    (cond
     ((eq operation 'change)
      (call-interactively 'evil-surround-change))
     ((eq operation 'delete)
      (call-interactively 'evil-surround-delete))
     (t
      (evil-surround-setup-surround-line-operators)
      (evil-surround-call-with-repeat 'evil-surround-region)))))

(evil-define-command evil-Surround-edit (operation)
  "Like evil-surround-edit, but for surrounding with additional new-lines.

It does nothing for change / delete."
  (interactive (evil-surround-interactive-setup))
  (cond
   ((eq operation 'change) nil)
   ((eq operation 'delete) nil)
   (t
    (evil-surround-setup-surround-line-operators)
    (evil-surround-call-with-repeat 'evil-Surround-region))))

(evil-define-operator evil-surround-region (beg end type char &optional force-new-line)
  "Surround BEG and END with CHAR.

When force-new-line is true, and region type is not line, the
following: (vertical bars indicate region start/end points)

   do |:thing|

Becomes this:

   do {
     :thing
   }"

  (interactive "<R>c")
  (when (evil-surround-valid-char-p char)
    (let* ((overlay (make-overlay beg end nil nil t))
           (pair (or (and (boundp 'pair) pair) (evil-surround-pair char)))
           (open (car pair))
           (close (cdr pair))
           (beg-pos (overlay-start overlay)))
      (unwind-protect
          (progn
            (goto-char beg-pos)
            (cond ((eq type 'block)
                   (evil-surround-block beg end char))

                  ((eq type 'line)
                   (setq force-new-line
                         (or force-new-line
                             ;; Force newline if not invoked from an operator, e.g. visual line mode with VS)
                             (evil-visual-state-p)
                             ;; Or on multi-line operator surrounds (like 'ysj]')
                             (/= (line-number-at-pos) (line-number-at-pos (1- end)))))

                   (back-to-indentation)
                   (setq beg-pos (point))
                   (insert open)
                   (when force-new-line (newline-and-indent))
                   (goto-char (overlay-end overlay))
                   (if force-new-line
                       (when (eobp)
                         (newline-and-indent))
                     (backward-char)
                     (evil-last-non-blank)
                     (forward-char))
                   (insert close)
                   (when (or force-new-line
                             (/= (line-number-at-pos) (line-number-at-pos beg-pos)))
                     (indent-region beg-pos (point))
                     (newline-and-indent)))

                  (force-new-line
                   (insert open)
                   (newline-and-indent)
                   (let ((pt (point)))
                     (goto-char (overlay-end overlay))
                     (newline-and-indent)
                     (insert close)
                     (indent-region pt (point))))

                  (t
                   (insert open)
                   (goto-char (overlay-end overlay))
                   (insert close)))
            (goto-char beg-pos))
        (delete-overlay overlay)))))

(evil-define-operator evil-Surround-region (beg end type char)
  "Call surround-region, toggling force-new-line"
  (interactive "<R>c")
  (evil-surround-region beg end type char t))

;;;###autoload
(define-minor-mode evil-surround-mode
  "Buffer-local minor mode to emulate surround.vim."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

;;;###autoload
(defun turn-on-evil-surround-mode ()
  "Enable evil-surround-mode in the current buffer."
  (evil-surround-mode 1))

;;;###autoload
(defun turn-off-evil-surround-mode ()
  "Disable evil-surround-mode in the current buffer."
  (evil-surround-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-surround-mode
  evil-surround-mode turn-on-evil-surround-mode
  "Global minor mode to emulate surround.vim.")

(defun evil-surround-generic-object (single inclusive count beg end type)
  (let ((text (if evil-surround-overlaying
                  evil-surround-overlaying-args
                (regexp-quote (if single
                                  (string (read-char))
                                (read-from-minibuffer "" ""))))))
    (when (not evil-surround-overlaying)
      (setq evil-surround-overlaying-args text))
    (evil-select-paren text text beg end type count inclusive)))

(evil-define-text-object evil-surround-generic-outer-text-object (count &optional beg end type)
  (evil-surround-generic-object nil t count beg end type))

(evil-define-text-object evil-surround-generic-inner-text-object (count &optional beg end type)
  (evil-surround-generic-object nil nil count beg end type))

(evil-define-text-object evil-surround-between-outer-text-object (count &optional beg end type)
  (evil-surround-generic-object t t count beg end type))

(evil-define-text-object evil-surround-between-inner-text-object (count &optional beg end type)
  (evil-surround-generic-object t nil count beg end type))

(defun evil-surround-between ()
  (let ((text (string (read-char))))
    (cons text text)))

(defun evil-surround-generic ()
  (let ((text (read-from-minibuffer "" "")))
    (cons text text)))

(evil-define-key 'operator evil-surround-mode-map "s" 'evil-surround-edit)
(evil-define-key 'operator evil-surround-mode-map "S" 'evil-Surround-edit)

(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "gS" 'evil-Surround-region)

;; (evil-define-key '(operator visual) evil-surround-mode-map (kbd "i g") 'evil-surround-generic-inner-text-object)
;; (evil-define-key '(operator visual) evil-surround-mode-map (kbd "a g") 'evil-surround-generic-outer-text-object)
;; (evil-define-key '(operator visual) evil-surround-mode-map (kbd "i b") 'evil-surround-between-inner-text-object)
;; (evil-define-key '(operator visual) evil-surround-mode-map (kbd "a b") 'evil-surround-between-outer-text-object)

(defun evil-surround-install-text-objects ()
  "Adds some textobjects to `evil-inner-text-objects-map' and `evil-outer-text-objects-map'"
  (define-key evil-inner-text-objects-map "g" 'evil-surround-generic-inner-text-object)
  (define-key evil-outer-text-objects-map "g" 'evil-surround-generic-outer-text-object)
  (define-key evil-inner-text-objects-map "b" 'evil-surround-between-inner-text-object)
  (define-key evil-outer-text-objects-map "b" 'evil-surround-between-outer-text-object))

(provide 'evil-surround)

;;; evil-surround.el ends here
