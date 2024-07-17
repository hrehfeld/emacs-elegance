;;; elegance-faces.el --- Faces for the Elegance theme
;;; Commentary:
;;; Code:

(defface elegance-outline-base '((t . (:weight bold))) "Base face for outline hierarchies.")

(defvar elegance-fix-fixed-pitch-faces
  '((org-agenda-calendar-event . org-mode)
		(org-block . org-mode)
		(org-block-begin-line . org-mode)
		(org-block-end-line . org-mode)
		(org-code . org-mode)
		(org-date . org-mode)
		(org-document-info-keyword . org-mode)
		(org-done . org-mode)
		(org-drawer . org-mode)
		(org-formula . org-mode)
		;;(org-habit-face . org-mode)
	  (org-meta-line . org-mode)
		(org-priority . org-mode)
		(org-special-keyword . org-mode)
		(org-table . org-mode)
		(org-todo . org-mode)
		(org-upcoming-deadline . org-mode)
		(org-verbatim . org-mode)
		(org-warning . org-mode)
		)
  "An alist of `(faces . providing-feature)' that should be fixed to inherit from `fixed-pitch' face.

The face will be modified after `providing-feature' was `provide'd, unless it is `nil'")

(defvar elegance-variable-pitch-faces
  '(org-quote)
  "An alist of faces that should be fixed to inherit from `variable-pitch'.")


(defun elegance ()
  "Apply the Elegance tweaks."
  (elegance-fix-fixed-pitch-faces)
  (elegance-add-outline-base-face))

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
           (if providing-feature
               (with-eval-after-load providing-feature
                 (apply fn (ensure-list args)))
             (apply fn (ensure-list args)))))

(defun elegance-push-face-attribute (face frame &rest specs)
  "Append SPECS to specs in FACE for FRAME. See `set-face-attribute`.

Assumes all specs are of list type, and sets a list, after applying seq-uniq to it. Usually used for :inherit."
  (when specs
	  (pcase-let ((`(,key ,value . ,tail) specs))
		  (let* ((old-value (ensure-list (face-attribute face key)))
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

(provide 'elegance-faces)

;;; elegance-faces.el ends here
