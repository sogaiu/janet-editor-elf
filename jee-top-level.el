;;; jee-top-level.el --- Janet top-level stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Top-level related

;; Functions here assume the left margin convention is followed.

;;; Code:

(defun jee-top-level--char-p (char)
  "Return non-nil if CHAR can start a top level container construct.

Supported top level container constructs include:

  * paren tuple            ()
  * quoted constructs      '() ~()

Thus the characters looked for in column 0 are (, ~, and '."
  (member char '(?\( ?\~ ?\')))

(defun jee-top-level--start-of ()
  "If a top level container exists before point, move to its start.

If there is no such construct, does not move point.

See `jee-top-level--char-p' for which characters determine
the start of a top level construct."
  (when (not (bobp))             ; do nothing if at beginning of buffer
    (let ((pos (point)))
      ;; XXX: added this because it seems wrong otherwise
      (beginning-of-line)
      (if (jee-top-level--char-p (char-after (point)))
          (setq pos (point))
        (let ((done nil))
          (while (not done)
            (forward-line -1)
            (cond ((jee-top-level--char-p (char-after (point)))
                   (setq pos (point))
                   (setq done t))
                  ((bobp)
                   (setq done t))))))
      (goto-char pos))))

(defun jee-top-level--before-next ()
  "If a top level container exists after point, move to right before it.

If there is no such construct, move to the end of the buffer.

See `jee-top-level--char-p' for which characters determine
the start of a top level construct."
  (when (not (eobp))
    (let ((pos (point)))
      (forward-line)
      (if (jee-top-level--char-p (char-after (point)))
          (progn ;; XXX: not necessarily valid for beginning of buffer?
            (forward-line -1)
            (setq pos (point)))
        (let ((done nil))
          (while (not done)
            (forward-line)
            (cond ((jee-top-level--char-p (char-after (point)))
                   (forward-line -1)
                   (setq pos (point))
                   (setq done t))
                  ((eobp)
                   (setq pos (point))
                   (setq done t))))))
      (goto-char pos))))

(provide 'jee-top-level)
;;; jee-top-level.el ends here
