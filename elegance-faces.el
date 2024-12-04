;;; elegance-faces.el --- Faces for the Elegance theme
;; Package-Requires: ((emacs "28.1") (kolor "0.1.0"))
;;; Commentary:
;;; Code:

(defface elegance-outline-base '((t . (:weight bold))) "Base face for outline hierarchies.")

(defface elegance-background-accent-face nil "Face for background accents.")
(defface elegance-faint-face nil "Face for faint text that is barely distinguishable from the background.")

(defcustom elegance-kolor-lightness-component 'lightness
  "The component of the kolor that should be used for lightness."
  :type '(choice (const lightness) (const cie-l))
  :group 'elegance)

(defcustom elegance-dynamic-face-attributes
  '((elegance-background-accent-face nil :background
                                     (lambda (old-background)
                                       (with-eval-after-load 'kolor
                                         (elegance-contrasted 0.9))))
    (elegance-faint-face nil :foreground
                         (lambda (old-value)
                           (with-eval-after-load 'kolor
                             (elegance-contrasted 0.7 nil :foreground)))))
  "A list of dynamic face attributes `(FACE FRAME . SPEC)' that should be recomputed on theme change.

`SPEC' is a list of `(ATTRIBUTE FN ATTRIBUTE FN ...)', where
`FN' is a function that takes the old value of `ATTRIBUTE' and returns the new value,
and `ATTRIBUTE' is a face-spec attribute key like `:background'."
  :type '(alist :key-type (symbol symbol) :value-type (repeat (symbol function)))
  :group 'elegance)


(defun elegance-apply-dynamic-face-attributes ()
  "Recompute dynamic face attributes and set them."
  (cl-loop for (face frame . attrs) in elegance-dynamic-face-attributes
           do
           (when (facep face)
             ;; iterate over attrs in pairs of two
             (cl-loop for (attr fn) on attrs by #'cddr
                      do
                      (cl-check-type fn function)

                      (let ((old-value (face-attribute face attr)))
                        (ignore-errors
                          (set-face-attribute face nil attr (funcall fn old-value))))))))

;; (elegance-apply-dynamic-face-attributes)

(defvar elegance-dynamic-face-attributes-hook
  '(elegance-apply-dynamic-face-attributes)
  "Normal hook to recompute faces at appropriate times.")

(defun elegance-dynamic-face-attributes-run-hook (&rest ignored-args)
  "Run the `elegance-dynamic-face-attributes-hook'."
  (interactive)
  (run-hooks 'elegance-dynamic-face-attributes-hook))

(defun elegance-dynamic-face-attributes-enable ()
  "Add `elegance-dynamic-face-attributes-run-hook' to appropriate hooks."
  (add-hook 'enable-theme-functions #'elegance-dynamic-face-attributes-run-hook))



(defvar elegance-fix-fixed-pitch-faces
  '((org-agenda-calendar-event . org)
		(org-block . org)
		(org-block-begin-line . org)
		(org-block-end-line . org)
		(org-code . org)
		(org-date . org)
		(org-document-info-keyword . org)
		(org-done . org)
		(org-drawer . org)
		(org-formula . org)
		;;(org-habit-face . org)
	  (org-meta-line . org)
		(org-priority . org)
		(org-special-keyword . org)
		(org-table . org)
		(org-todo . org)
		(org-upcoming-deadline . org)
		(org-verbatim . org)
		(org-warning . org)
		)
  "An alist of `(faces . providing-feature)' that should be fixed to inherit from `fixed-pitch' face.

The face will be modified after `providing-feature' was `provide'd, unless it is `nil'")

(defvar elegance-variable-pitch-faces
  '(org-quote)
  "An alist of faces that should be fixed to inherit from `variable-pitch'.")


;;;###autoload
(defun elegance ()
  "Apply the Elegance tweaks."
  (elegance-fix-fixed-pitch-faces)
  (elegance-add-outline-base-face)
  (elegance-apply-dynamic-face-attributes)
  (elegance-dynamic-face-attributes-enable))

(defvar elegance-outline-faces
  '(outline-1
    outline-2
    outline-3
    outline-4
    outline-5
    outline-6
    outline-7
    outline-8)
  "Faces that should inherit from `elegance-outline-base'.")

(defun elegance-add-outline-base-face (&optional faces)
  (cl-loop for face in (or faces elegance-outline-faces)
           do
           (elegance-push-face-attribute face nil :inherit 'elegance-outline-base)))


(defun elegance-fix-fixed-pitch-faces (&optional faces)
  "Make `FACES' inherit from fixed-pitch.

If `FACES' is nil, use `elegance-fix-fixed-pitch-faces'."
  (interactive)
  (let ((faces (or faces elegance-fix-fixed-pitch-faces)))
    (elegance-push-face-attribute-faces :inherit 'fixed-pitch faces)))

;; (elegance-fix-fixed-pitch-faces)

(defun elegance-push-face-attribute-faces (attribute value faces)
  "`(elegance-push-face-attribute ATTRIBUTE VALUE)' for each face in `FACES'.

`FACES' is an alist like `elegance-fix-fixed-pitch-faces', and `with-eval-after-load' will be used accordingly."
  (interactive)
  (elegance-mapcar-with-eval-after-load
   (lambda (face)

     (elegance-push-face-attribute face nil attribute value))
   (elegance-alist-rotate-values-back faces)))


(defun elegance-mapcar-with-eval-after-load (fn feature-args-alist)
  "Call `FN' with each `ARGS' in `FEATURES-ARGS-ALIST' after the `feature' was loaded.

`FEATURES-ARGS-ALIST' is an alist of `(feature . args)', see `provide'.
`feature' can also be nil and `FN' will then be evaluated immediately."
  (interactive)
	(cl-loop for (providing-feature . args)
           in feature-args-alist
	         do
           (if (and providing-feature (not (featurep providing-feature)))
               (with-eval-after-load providing-feature
                 (apply fn (ensure-list args)))
             (apply fn (ensure-list args)))))

(defun elegance-push-face-attribute (face frame &rest specs)
  "Append SPECS to specs in FACE for FRAME. See `set-face-attribute`.

Assumes all specs are of list type, and sets a list, after applying seq-uniq to it. Usually used for :inherit."
  (when specs
	  (pcase-let ((`(,key ,value . ,tail) specs))
		  (let* ((old-value (cl-remove 'unspecified (ensure-list
                                                 (face-attribute face key))))
					   (merged-value (if old-value
                               (append (ensure-list value) old-value)
                             value)))
			  (set-face-attribute face frame key merged-value)))))

(defalias 'my-append-face-attribute 'elegance-push-face-attribute)


(defun elegance-alist-nreverse-values (alist)
  "Modify the values of `ALIST' in place to be the result of `nreverse'ing them."
  (cl-loop for value in alist
           collect (nreverse value)))

;; (elegance-alist-nreverse-values '((a 1 2 3) (b 4 5 6) (c 7 8 9)))

(defun elegance-alist-rotate-values-front (alist)
  "Modify the values of `ALIST' in place by rotating them internally so that the first element becomes the last."
  (cl-loop for value in alist
           collect (if (cdr-safe value)
                       (append (cdr value) (list (car value)))
                     (cons nil (car value)))))

;; (elegance-alist-rotate-values-front '((a 1 2 3) (b 4 5 6) (c 7 8 9)))

(defun elegance-alist-rotate-values-back (alist)
  "Modify the values of `ALIST' in place by rotating them internally so that the last element becomes the first.
If a value is a dotted pair, rotate that. If value is a single atom, return `(nil . value)'"
  (cl-loop for value in alist
           collect (cond
                    ;; cons pair where cdr is neither cons nor nil
                    ((not (listp (cdr-safe value)))
                     (cons (cdr value) (car value)))
                    ((cdr-safe value)
                     (append (last value) (butlast value)))
                    ((consp value)
                     (cl-assert (not (cdr-safe value)))
                     (cons nil (car value)))
                    (t
                     (cons nil value)))))

;; (elegance-alist-rotate-values-back '((a 1 2 3) (b 4 5 6) (c 7 8 9)))
;; (elegance-alist-rotate-values-back elegance-fix-fixed-pitch-faces)

(defun elegance-contrasted (contrast-multiplier &optional face attribute)
  "Retrieve the color of `FACE' `ATTRIBUTE' and return a contrasted version multiplied by `CONTRAST-MULTIPLIER'.

If `FACE' and `ATTRIBUTE' are nil, use the `default' face and its `:background'."
  (kolor-to-emacs
   (kolor-contrasted elegance-kolor-lightness-component contrast-multiplier)))
;; (elegance-contrasted 0.9)

(provide 'elegance-faces)

;;; elegance-faces.el ends here
