(autoload 'vc-find-revision "vc")
(defvar vc-find-revision-no-save)
(defvar diff-vc-revisions nil
  "The VC revisions compared in the current Diff buffer, if any.")

                        ('unified
                        ('context "^[^-+#! \\]")
                        ('normal "^[^<>#\\]")
	   (revision (and other diff-vc-backend
                          (if reverse (nth 1 diff-vc-revisions)
                            (or (nth 0 diff-vc-revisions)
                                ;; When diff shows changes in working revision
                                (vc-working-revision file)))))
	   (buf (if revision
                    (let ((vc-find-revision-no-save t))
                      (vc-find-revision file revision diff-vc-backend))
                  (find-file-noselect file))))
then `diff-jump-to-old-file' is also set, for the next invocations.

Under version control, the OTHER-FILE prefix arg means jump to the old
revision of the file if point is on an old changed line, or to the new
revision of the file otherwise."
        (reverse (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
                 (diff-find-source-location other-file reverse)))
      (diff-hunk-status-msg line-offset (diff-xor reverse switched) t))))
      ('unified
      ('context