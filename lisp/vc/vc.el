;;; vc.el --- drive a version-control system from within Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1992-1998, 2000-2022 Free Software Foundation, Inc.

;; Author: FSF (see below for full credits)
;; Maintainer: emacs-devel@gnu.org
;; Keywords: vc tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Credits:

;; VC was initially designed and implemented by Eric S. Raymond
;; <esr@thyrsus.com> in 1992.  Over the years, many other people have
;; contributed substantial amounts of work to VC.  These include:
;;
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Paul Eggert <eggert@twinsun.com>
;;   Sebastian Kremer <sk@thp.uni-koeln.de>
;;   Martin Lorentzson <martinl@gnu.org>
;;   Dave Love <fx@gnu.org>
;;   Stefan Monnier <monnier@cs.yale.edu>
;;   Thien-Thi Nguyen <ttn@gnu.org>
;;   Dan Nicolaescu <dann@ics.uci.edu>
;;   J.D. Smith <jdsmith@alum.mit.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;
;; In July 2007 ESR returned and redesigned the mode to cope better
;; with modern version-control systems that do commits by fileset
;; rather than per individual file.
;;
;; If you maintain a client of the mode or customize it in your .emacs,
;; note that some backend functions which formerly took single file arguments
;; now take a list of files.  These include: register, checkin, print-log,
;; and diff.

;;; Commentary:

;; This mode is fully documented in the Emacs user's manual.
;;
;; Supported version-control systems presently include CVS, RCS, SRC,
;; GNU Subversion, Bzr, Git, Mercurial, Monotone and SCCS (or its free
;; replacement, CSSC).
;;
;; If your site uses the ChangeLog convention supported by Emacs, the
;; function `log-edit-comment-to-change-log' could prove a useful checkin hook,
;; although you might prefer to use C-c C-a (i.e. `log-edit-insert-changelog')
;; from the commit buffer instead or to set `log-edit-setup-invert'.
;;
;; When using SCCS, RCS, CVS: be careful not to do repo surgery, or
;; operations like registrations and deletions and renames, outside VC
;; while VC is running.  The support for these systems was designed
;; when disks were much slower, and the code maintains a lot of
;; internal state in order to reduce expensive operations to a
;; minimum.  Thus, if you mess with the repo while VC's back is turned,
;; VC may get seriously confused.
;;
;; When using Subversion or a later system, anything you do outside VC
;; *through the VCS tools* should safely interlock with VC
;; operations.  Under these VC does little state caching, because local
;; operations are assumed to be fast.
;;
;; The 'assumed to be fast' category includes SRC, even though it's
;; a wrapper around RCS.
;;
;; ADDING SUPPORT FOR OTHER BACKENDS
;;
;; VC can use arbitrary version control systems as a backend.  To add
;; support for a new backend named SYS, write a library vc-sys.el that
;; contains functions of the form `vc-sys-...' (note that SYS is in lower
;; case for the function and library names).  VC will use that library if
;; you put the symbol SYS somewhere into the list of
;; `vc-handled-backends'.  Then, for example, if `vc-sys-registered'
;; returns non-nil for a file, all SYS-specific versions of VC commands
;; will be available for that file.
;;
;; VC keeps some per-file information in the form of properties (see
;; vc-file-set/getprop in vc-hooks.el).  The backend-specific functions
;; do not generally need to be aware of these properties.  For example,
;; `vc-sys-working-revision' should compute the working revision and
;; return it; it should not look it up in the property, and it needn't
;; store it there either.  However, if a backend-specific function does
;; store a value in a property, that value takes precedence over any
;; value that the generic code might want to set (check for uses of
;; the macro `with-vc-properties' in vc.el).
;;
;; In the list of functions below, each identifier needs to be prepended
;; with `vc-sys-'.  Some of the functions are mandatory (marked with a
;; `*'), others are optional (`-').

;; BACKEND PROPERTIES
;;
;; * revision-granularity
;;
;;   Takes no arguments.  Returns either 'file or 'repository.  Backends
;;   that return 'file have per-file revision numbering; backends
;;   that return 'repository have per-repository revision numbering,
;;   so a revision level implicitly identifies a changeset
;;
;; - update-on-retrieve-tag
;;
;;   Takes no arguments.  Backends that return non-nil can update
;;   buffers on `vc-retrieve-tag' based on user input.  In this case
;;   user will be prompted to update buffers on `vc-retrieve-tag'.

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)
;;
;;   Return non-nil if FILE is registered in this backend.  Both this
;;   function as well as `state' should be careful to fail gracefully
;;   in the event that the backend executable is absent.  It is
;;   preferable that this function's *body* is autoloaded, that way only
;;   calling vc-registered does not cause the backend to be loaded
;;   (all the vc-FOO-registered functions are called to try to find
;;   the controlling backend for FILE).
;;
;; * state (file)
;;
;;   Return the current version control state of FILE.  For a list of
;;   possible values, see `vc-state'.  This function should do a full and
;;   reliable state computation; it is usually called immediately after
;;   C-x v v.
;;
;; - dir-status-files (dir files update-function)
;;
;;   Produce RESULT: a list of lists of the form (FILE VC-STATE EXTRA)
;;   for FILES in DIR.  If FILES is nil, report on all files in DIR.
;;   (It is OK, though possibly inefficient, to ignore the FILES argument
;;   and always report on all files in DIR.)
;;
;;   If FILES is non-nil, this function should report on all requested
;;   files, including up-to-date or ignored files.
;;
;;   EXTRA can be used for backend specific information about FILE.
;;   If a command needs to be run to compute this list, it should be
;;   run asynchronously using (current-buffer) as the buffer for the
;;   command.
;;
;;   When RESULT is computed, it should be passed back by doing:
;;   (funcall UPDATE-FUNCTION RESULT nil).  If the backend uses a
;;   process filter, hence it produces partial results, they can be
;;   passed back by doing: (funcall UPDATE-FUNCTION RESULT t) and then
;;   do a (funcall UPDATE-FUNCTION RESULT nil) when all the results
;;   have been computed.
;;
;;   To provide more backend specific functionality for `vc-dir'
;;   the following functions might be needed: `dir-extra-headers',
;;   `dir-printer', and `extra-dir-menu'.
;;
;; - dir-extra-headers (dir)
;;
;;   Return a string that will be added to the *vc-dir* buffer header.
;;
;; - dir-printer (fileinfo)
;;
;;   Pretty print the `vc-dir-fileinfo' FILEINFO.
;;   If a backend needs to show more information than the default FILE
;;   and STATE in the vc-dir listing, it can store that extra
;;   information in `vc-dir-fileinfo->extra'.  This function can be
;;   used to display that extra information in the *vc-dir* buffer.
;;
;; - status-fileinfo-extra (file)
;;
;;   Compute `vc-dir-fileinfo->extra' for FILE.
;;
;; * working-revision (file)
;;
;;   Return the working revision of FILE.  This is the revision fetched
;;   by the last checkout or update, not necessarily the same thing as the
;;   head or tip revision.  Should return "0" for a file added but not yet
;;   committed.
;;
;; * checkout-model (files)
;;
;;   Indicate whether FILES need to be "checked out" before they can be
;;   edited.  See `vc-checkout-model' for a list of possible values.
;;
;; - mode-line-string (file)
;;
;;   If provided, this function should return the VC-specific mode
;;   line string for FILE.  The returned string should have a
;;   `help-echo' property which is the text to be displayed as a
;;   tooltip when the mouse hovers over the VC entry on the mode-line.
;;   The default implementation deals well with all states that
;;   `vc-state' can return.
;;
;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo ()
;;
;;   Create an empty repository in the current directory and initialize
;;   it so VC mode can add files to it.  For file-oriented systems, this
;;   need do no more than create a subdirectory with the right name.
;;
;; * register (files &optional comment)
;;
;;   Register FILES in this backend.  Optionally, an initial
;;   description of the file, COMMENT, may be specified, but it is not
;;   guaranteed that the backend will do anything with this.  The
;;   implementation should pass the value of vc-register-switches to
;;   the backend command.  (Note: in older versions of VC, this
;;   command had an optional revision first argument that was
;;   not used; in still older ones it took a single file argument and
;;   not a list.)
;;
;; - responsible-p (file)
;;
;;   Return the directory if this backend considers itself "responsible" for
;;   FILE, which can also be a directory.  This function is used to find
;;   out what backend to use for registration of new files and for things
;;   like change log generation.  The default implementation always
;;   returns nil.
;;
;; - receive-file (file rev)
;;
;;   Let this backend "receive" a file that is already registered under
;;   another backend.  The default implementation simply calls `register'
;;   for FILE, but it can be overridden to do something more specific,
;;   e.g. keep revision numbers consistent or choose editing modes for
;;   FILE that resemble those of the other backend.
;;
;; - unregister (file)
;;
;;   Unregister FILE from this backend.  This is only needed if this
;;   backend may be used as a "more local" backend for temporary editing.
;;
;; * checkin (files comment &optional rev)
;;
;;   Commit changes in FILES to this backend.  COMMENT is used as a
;;   check-in comment.  The implementation should pass the value of
;;   vc-checkin-switches to the backend command.  The optional REV
;;   revision argument is only supported with some older VCSes, like
;;   RCS and CVS, and is otherwise silently ignored.
;;
;; - checkin-patch (patch-string comment)
;;
;;   Commit a single patch PATCH-STRING to this backend, bypassing
;;   the changes in filesets.  COMMENT is used as a check-in comment.
;;
;; * find-revision (file rev buffer)
;;
;;   Fetch revision REV of file FILE and put it into BUFFER.
;;   If REV is the empty string, fetch the head of the trunk.
;;   The implementation should pass the value of vc-checkout-switches
;;   to the backend command.
;;
;; * checkout (file &optional rev)
;;
;;   Check out revision REV of FILE into the working area.  FILE
;;   should be writable by the user and if locking is used for FILE, a
;;   lock should also be set.  If REV is non-nil, that is the revision
;;   to check out (default is the working revision).  If REV is t,
;;   that means to check out the head of the current branch; if it is
;;   the empty string, check out the head of the trunk.  The
;;   implementation should pass the value of vc-checkout-switches to
;;   the backend command.  The 'editable' argument of older VC versions
;;   is gone; all files are checked out editable.
;;
;; * revert (file &optional contents-done)
;;
;;   Revert FILE back to the working revision.  If optional
;;   arg CONTENTS-DONE is non-nil, then the contents of FILE have
;;   already been reverted from a version backup, and this function
;;   only needs to update the status of FILE within the backend.
;;   If FILE is in the `added' state it should be returned to the
;;   `unregistered' state.
;;
;; - merge-file (file &optional rev1 rev2)
;;
;;   Merge the changes between REV1 and REV2 into the current working
;;   file (for non-distributed VCS).  It is expected that with an
;;   empty first revision this will behave like the merge-news method.
;;
;; - merge-branch ()
;;
;;   Merge another branch into the current one, prompting for a
;;   location to merge from.
;;
;; - merge-news (file)
;;
;;   Merge recent changes from the current branch into FILE.
;;   (for non-distributed VCS).
;;
;; - pull (prompt)
;;
;;   Pull "upstream" changes into the current branch (for distributed
;;   VCS).  If PROMPT is non-nil, or if necessary, prompt for a
;;   location to pull from.
;;
;; - steal-lock (file &optional revision)
;;
;;   Steal any lock on the working revision of FILE, or on REVISION if
;;   that is provided.  This function is only needed if locking is
;;   used for files under this backend, and if files can indeed be
;;   locked by other users.
;;
;; - modify-change-comment (files rev comment)
;;
;;   Modify the change comments associated with the files at the
;;   given revision.  This is optional, many backends do not support it.
;;
;; - mark-resolved (files)
;;
;;   Mark conflicts as resolved.  Some VC systems need to run a
;;   command to mark conflicts as resolved.
;;
;; - find-admin-dir (file)
;;
;;   Return the administrative directory of FILE.

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;;
;;   Insert the revision log for FILES into BUFFER.
;;   If SHORTLOG is true insert a short version of the log.
;;   If LIMIT is true insert only insert LIMIT log entries.  If the
;;   backend does not support limiting the number of entries to show
;;   it should return `limit-unsupported'.
;;   If START-REVISION is given, then show the log starting from that
;;   revision ("starting" in the sense of it being the _newest_
;;   revision shown, rather than the working revision, which is normally
;;   the case).  Not all backends support this.  At present, this is
;;   only ever used with LIMIT = 1 (by vc-annotate-show-log-revision-at-line).
;;
;; * log-outgoing (buffer remote-location)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   sent when performing a push operation to REMOTE-LOCATION.
;;
;; * log-incoming (buffer remote-location)
;;
;;   Insert in BUFFER the revision log for the changes that will be
;;   received when performing a pull operation from REMOTE-LOCATION.
;;
;; - log-search (buffer pattern)
;;
;;   Search for PATTERN in the revision log and output results into BUFFER.
;;
;; - log-view-mode ()
;;
;;   Mode to use for the output of print-log.  This defaults to
;;   `log-view-mode' and is expected to be changed (if at all) to a derived
;;   mode of `log-view-mode'.
;;
;; - show-log-entry (revision)
;;
;;   If provided, search the log entry for REVISION in the current buffer,
;;   and make sure it is displayed in the buffer's window.  The default
;;   implementation of this function works for RCS-style logs.
;;
;; - comment-history (file)
;;
;;   Return a string containing all log entries that were made for FILE.
;;   This is used for transferring a file from one backend to another,
;;   retaining comment information.
;;
;; - update-changelog (files)
;;
;;   Using recent log entries, create ChangeLog entries for FILES, or for
;;   all files at or below the default-directory if FILES is nil.  The
;;   default implementation runs rcs2log, which handles RCS- and
;;   CVS-style logs.
;;
;; * diff (files &optional rev1 rev2 buffer async)
;;
;;   Insert the diff for FILE into BUFFER, or the *vc-diff* buffer if
;;   BUFFER is nil.  If ASYNC is non-nil, run asynchronously.  If REV1
;;   and REV2 are non-nil, report differences from REV1 to REV2.  If
;;   REV1 is nil, use the working revision (as found in the
;;   repository) as the older revision if REV2 is nil as well;
;;   otherwise, diff against an empty tree.  If REV2 is nil, use the
;;   current working-copy contents as the newer revision.  This
;;   function should pass the value of (vc-switches BACKEND 'diff) to
;;   the backend command.  It should return a status of either 0 (no
;;   differences found), or 1 (either non-empty diff or the diff is
;;   run asynchronously).
;;
;; - revision-completion-table (files)
;;
;;   Return a completion table for existing revisions of FILES.
;;   The default is to not use any completion table.
;;
;; - annotate-command (file buf &optional rev)
;;
;;   If this function is provided, it should produce an annotated display
;;   of FILE in BUF, relative to revision REV.  Annotation means each line
;;   of FILE displayed is prefixed with version information associated with
;;   its addition (deleted lines leave no history) and that the text of the
;;   file is fontified according to age.
;;
;; - annotate-time ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Return the time of the next line of annotation at or after point,
;;   as a floating point fractional number of days.  The helper
;;   function `vc-annotate-convert-time' may be useful for converting
;;   multi-part times as returned by `current-time' and `encode-time'
;;   to this format.  Return nil if no more lines of annotation appear
;;   in the buffer.  You can safely assume that point is placed at the
;;   beginning of each line, starting at `point-min'.  The buffer that
;;   point is placed in is the Annotate output, as defined by the
;;   relevant backend.  This function also affects how much of the line
;;   is fontified; where it leaves point is where fontification begins.
;;
;; - annotate-current-time ()
;;
;;   Only required if `annotate-command' is defined for the backend,
;;   AND you'd like the current time considered to be anything besides
;;   (vc-annotate-convert-time (current-time)) -- i.e. the current
;;   time with hours, minutes, and seconds included.  Probably safe to
;;   ignore.  Return the current time, in units of fractional days.
;;
;; - annotate-extract-revision-at-line ()
;;
;;   Only required if `annotate-command' is defined for the backend.
;;   Invoked from a buffer in vc-annotate-mode, return the revision
;;   corresponding to the current line, or nil if there is no revision
;;   corresponding to the current line.
;;   If the backend supports annotating through copies and renames,
;;   and displays a file name and a revision, then return a cons
;;   (REVISION . FILENAME).
;;
;; - region-history (file buffer lfrom lto)
;;
;;   Insert into BUFFER the history (log comments and diffs) of the content of
;;   FILE between lines LFROM and LTO.  This is typically done asynchronously.
;;
;; - region-history-mode ()
;;
;;   Major mode to use for the output of `region-history'.
;;
;; - mergebase (rev1 &optional rev2)
;;
;;   Return the common ancestor between REV1 and REV2 revisions.

;; TAG SYSTEM
;;
;; - create-tag (dir name branchp)
;;
;;   Attach the tag NAME to the state of the working copy.  This
;;   should make sure that files are up-to-date before proceeding with
;;   the action.  DIR can also be a file and if BRANCHP is specified,
;;   NAME should be created as a branch and DIR should be checked out
;;   under this new branch.  The default implementation does not
;;   support branches but does a sanity check, a tree traversal and
;;   assigns the tag to each file.
;;
;; - retrieve-tag (dir name update)
;;
;;   Retrieve the version tagged by NAME of all registered files at or below DIR.
;;   If UPDATE is non-nil, then update buffers of any files in the
;;   tag that are currently visited.  The default implementation
;;   does a sanity check whether there aren't any uncommitted changes at
;;   or below DIR, and then performs a tree walk, using the `checkout'
;;   function to retrieve the corresponding revisions.

;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;;
;;   Return non-nil if unmodified repository revisions of FILE should be
;;   backed up locally.  If this is done, VC can perform `diff' and
;;   `revert' operations itself, without calling the backend system.  The
;;   default implementation always returns nil.
;;
;; - root (file)
;;
;;   Return the root of the VC controlled hierarchy for file.
;;
;; - ignore (file &optional directory remove)
;;
;;   Ignore FILE under DIRECTORY (default is 'default-directory').
;;   FILE is a file wildcard relative to DIRECTORY.
;;   When called interactively and with a prefix argument, remove FILE
;;   from ignored files.
;;   When called from Lisp code, if DIRECTORY is non-nil, the
;;   repository to use will be deduced by DIRECTORY.
;;   The default behavior is to add or remove a line from the file
;;   returned by the `find-ignore-file' function.
;;
;; - ignore-completion-table (directory)
;;
;;   Return the completion table for files ignored by the current
;;   version control system, e.g., the entries in `.gitignore' and
;;   `.bzrignore'.  The default behavior is to read the contents of
;;   the file returned by the `find-ignore-file' function.
;;
;; - find-ignore-file (file)
;;
;;   Return the ignore file that controls FILE, e.g. `.gitignore' or
;;   `.bzrignore'.
;;
;; - previous-revision (file rev)
;;
;;   Return the revision number that precedes REV for FILE, or nil if no such
;;   revision exists.
;;
;; - next-revision (file rev)
;;
;;   Return the revision number that follows REV for FILE, or nil if no such
;;   revision exists.
;;
;; - log-edit-mode ()
;;
;;   Turn on the mode used for editing the check in log.  This
;;   defaults to `log-edit-mode'.  If changed, it should use a mode
;;   derived from `log-edit-mode'.
;;
;; - check-headers ()
;;
;;   Return non-nil if the current buffer contains any version headers.
;;
;; - delete-file (file)
;;
;;   Delete FILE and mark it as deleted in the repository.  If this
;;   function is not provided, the command `vc-delete-file' will
;;   signal an error.
;;
;; - rename-file (old new)
;;
;;   Rename file OLD to NEW, both in the working area and in the
;;   repository.  If this function is not provided, the renaming
;;   will be done by (vc-delete-file old) and (vc-register new).
;;
;; - find-file-hook ()
;;
;;   Operation called in current buffer when opening a file.  This can
;;   be used by the backend to setup some local variables it might need.
;;
;; - extra-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the Version Control menu.  The goal is to allow backends
;;   to specify extra menu items that appear in the VC menu.  This way
;;   you can provide menu entries for functionality that is specific
;;   to your backend and which does not map to any of the VC generic
;;   concepts.
;;
;; - extra-dir-menu ()
;;
;;   Return a menu keymap, the items in the keymap will appear at the
;;   end of the VC Status menu.  The goal is to allow backends to
;;   specify extra menu items that appear in the VC Status menu.  This
;;   makes it possible to provide menu entries for functionality that
;;   is specific to a backend and which does not map to any of the VC
;;   generic concepts.
;;
;; - conflicted-files (dir)
;;
;;   Return the list of files where conflict resolution is needed in
;;   the project that contains DIR.
;;   FIXME: what should it do with non-text conflicts?
;;
;; - repository-url (file-or-dir &optional remote-name)
;;
;;   Returns the URL of the repository of the current checkout
;;   containing FILE-OR-DIR.  The optional REMOTE-NAME specifies the
;;   remote (in Git parlance) whose URL is to be returned.  It has
;;   only a meaning for distributed VCS and is ignored otherwise.

;;; Changes from the pre-25.1 API:
;;
;; - INCOMPATIBLE CHANGE: The 'editable' optional argument of
;;   vc-checkout is gone.  The upper level assumes that all files are
;;   checked out editable.  This moves closer to emulating modern
;;   non-locking behavior even on very old VCSes.
;;
;; - INCOMPATIBLE CHANGE: The vc-register function and its backend
;;   implementations no longer take a first optional revision
;;   argument, since on no system since RCS has setting the initial
;;   revision been even possible, let alone sane.
;;
;; - INCOMPATIBLE CHANGE: In older versions of the API, vc-diff did
;;   not take an async-mode flag as a fourth optional argument.  (This
;;   change eliminated a particularly ugly global.)
;;
;; - INCOMPATIBLE CHANGE: The backend operation for non-distributed
;;   VCSes formerly called "merge" is now "merge-file" (to contrast
;;   with merge-branch), and does its own prompting for revisions.
;;   (This fixes a layer violation that produced bad behavior under
;;   SVN.)
;;
;; - INCOMPATIBLE CHANGE: The old fourth 'default-state' argument of
;;   dir-status-files is gone; none of the back ends actually used it.
;;
;; - dir-status is no longer a public method; it has been replaced by
;;   dir-status-files.
;;
;; - state-heuristic is no longer a public method (the CVS backend
;;   retains it as a private one).
;;
;; - the vc-mistrust-permissions configuration variable is gone; the
;;   code no longer relies on permissions except in one corner case where
;;   CVS leaves no alternative (which was not gated by this variable).  The
;;   only affected back ends were SCCS and RCS.
;;
;; - vc-stay-local-p and repository-hostname are no longer part
;;   of the public API.  The vc-cvs-stay-local configuration variable
;;   remains and only affects the CVS back end.
;;
;; - The init-revision function and the default-initial-revision
;;   variable are gone.  These haven't made sense on anything shipped
;;   since RCS, and using them was a dumb stunt even on RCS.
;;
;; - workfile-unchanged-p is no longer a public back-end method.  It
;;   was redundant with vc-state and usually implemented with a trivial
;;   call to it.  A few older back ends retain versions for internal use in
;;   their vc-state functions.
;;
;; - could-register is no longer a public method.  Only vc-cvs ever used it
;;
;;   The vc-keep-workfiles configuration variable is gone.  Used only by
;;   the RCS and SCCS backends, it was an invitation to shoot self in foot
;;   when set to the (non-default) value nil.  The original justification
;;   for it (saving disk space) is long obsolete.
;;
;; - The rollback method (implemented by RCS and SCCS only) is gone.  See
;;   the to-do note on uncommit.
;;
;; - latest-on-branch-p is no longer a public method.  It was to be used
;;   for implementing rollback.  RCS keeps its implementation (the only one)
;;   for internal use.


;;; Todo:

;;;; New Primitives:
;;
;; - uncommit: undo last checkin, leave changes in place in the workfile,
;;   stash the commit comment for re-use.
;;
;; - deal with push operations.
;;
;;;; Primitives that need changing:
;;
;; - vc-update/vc-merge should deal with VC systems that don't do
;;   update/merge on a file basis, but on a whole repository basis.
;;   vc-update and vc-merge assume the arguments are always files,
;;   they don't deal with directories.  Make sure the *vc-dir* buffer
;;   is updated after these operations.
;;   At least bzr, git and hg should benefit from this.
;;
;;;; Improved branch and tag handling:
;;
;; - Make sure the *vc-dir* buffer is updated after merge-branch operations.
;;
;; - add a generic mechanism for remembering the current branch names,
;;   display the branch name in the mode-line.  Replace
;;   vc-cvs-sticky-tag with that.
;;
;; - Add a primitives for switching to a branch (creating it if required.
;;
;; - Add the ability to list tags and branches.
;;
;;;; Unify two different versions of the amend capability
;;
;; - Some back ends (SCCS/RCS/SVN/SRC), have an amend capability that can
;;   be invoked from log-view.
;;
;; - The git backend supports amending, but in a different
;;   way (press `C-c C-e' in log-edit buffer, when making a new commit).
;;
;; - Second, `log-view-modify-change-comment' doesn't seem to support
;;   modern backends at all because `log-view-extract-comment'
;;   unconditionally calls `log-view-current-file'.  This should be easy to
;;   fix.
;;
;; - Third, doing message editing in log-view might be a natural way to go
;;   about it, but editing any but the last commit (and even it, if it's
;;   been pushed) is a dangerous operation in Git, which we shouldn't make
;;   too easy for users to perform.
;;
;;   There should be a check that the given comment is not reachable
;;   from any of the "remote" refs?
;;
;;;; Other
;;
;; - asynchronous checkin and commit, so you can keep working in other
;;   buffers while the repo operation happens.
;;
;; - Direct support for stash/shelve.
;;
;; - when a file is in `conflict' state, turn on smerge-mode.
;;
;; - figure out what to do with conflicts that are not caused by the
;;   file contents, but by metadata or other causes.  Example: File A
;;   gets renamed to B in one branch and to C in another and you merge
;;   the two branches.  Or you locally add file FOO and then pull a
;;   change that also adds a new file FOO, ...
;;
;; - make it easier to write logs.  Maybe C-x 4 a should add to the log
;;   buffer, if one is present, instead of adding to the ChangeLog.
;;
;; - When vc-next-action calls vc-checkin it could pre-fill the
;;   *vc-log* buffer with some obvious items: the list of files that
;;   were added, the list of files that were removed.  If the diff is
;;   available, maybe it could even call something like
;;   `diff-add-change-log-entries-other-window' to create a detailed
;;   skeleton for the log...
;;
;; - most vc-dir backends need more work.  They might need to
;;   provide custom headers, use the `extra' field and deal with all
;;   possible VC states.
;;
;; - add a function that calls vc-dir to `find-directory-functions'.
;;
;; - vc-diff, vc-annotate, etc. need to deal better with unregistered
;;   files.  Now that unregistered and ignored files are shown in
;;   vc-dir, it is possible that these commands are called
;;   for unregistered/ignored files.
;;
;; - vc-next-action needs work in order to work with multiple
;;   backends: `vc-state' returns the state for the default backend,
;;   not for the backend in the current *vc-dir* buffer.
;;
;; - vc-dir-kill-dir-status-process should not be specific to dir-status,
;;   it should work for other async commands done through vc-do-command
;;   as well,
;;
;; - vc-dir toolbar needs more icons.
;;
;; - The backends should avoid using `vc-file-setprop' and `vc-file-getprop'.
;;
;;; Code:

(require 'vc-hooks)
(require 'vc-dispatcher)
(require 'cl-lib)

(declare-function diff-setup-whitespace "diff-mode" ())
(declare-function diff-setup-buffer-type "diff-mode" ())

(eval-when-compile
  (require 'dired))

(declare-function dired-get-filename "dired" (&optional localp noerror))
(declare-function dired-move-to-filename "dired" (&optional err eol))
(declare-function dired-marker-regexp "dired" ())

(unless (assoc 'vc-parent-buffer minor-mode-alist)
  (setq minor-mode-alist
	(cons '(vc-parent-buffer vc-parent-buffer-name)
	      minor-mode-alist)))

;; General customization

(defgroup vc nil
  "Emacs interface to version control systems."
  :group 'tools)

(defcustom vc-checkin-switches nil
  "A string or list of strings specifying extra switches for checkin.
These are passed to the checkin program by \\[vc-checkin]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-checkout-switches nil
  "A string or list of strings specifying extra switches for checkout.
These are passed to the checkout program by \\[vc-checkout]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-register-switches nil
  "A string or list of strings; extra switches for registering a file.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string)))

(defcustom vc-diff-switches nil
  "A string or list of strings specifying switches for diff under VC.
When running diff under a given BACKEND, VC uses the first
non-nil value of `vc-BACKEND-diff-switches', `vc-diff-switches',
and `diff-switches', in that order.  Since nil means to check the
next variable in the sequence, either of the first two may use
the value t to mean no switches at all.  `vc-diff-switches'
should contain switches that are specific to version control, but
not specific to any particular backend."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1")

(defcustom vc-annotate-switches nil
  "A string or list of strings specifying switches for annotate under VC.
When running annotate under a given BACKEND, VC uses the first
non-nil value of `vc-BACKEND-annotate-switches', `vc-annotate-switches',
and `annotate-switches', in that order.  Since nil means to check the
next variable in the sequence, either of the first two may use
the value t to mean no switches at all.  `vc-annotate-switches'
should contain switches that are specific to version control, but
not specific to any particular backend.

As very few switches (if any) are used across different VC tools,
please consider using the specific `vc-BACKEND-annotate-switches'
for the backend you use."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "25.1")

(defcustom vc-log-show-limit 2000
  "Limit the number of items shown by the VC log commands.
Zero means unlimited.
Not all VC backends are able to support this feature."
  :type 'natnum)

(defcustom vc-allow-async-revert nil
  "Specifies whether the diff during \\[vc-revert] may be asynchronous.
Enabling this option means that you can confirm a revert operation even
if the local changes in the file have not been found and displayed yet."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :version "22.1")

;;;###autoload
(defcustom vc-checkout-hook nil
  "Normal hook (list of functions) run after checking out a file.
See `run-hooks'."
  :type 'hook
  :version "21.1")

;;;###autoload
(defcustom vc-checkin-hook nil
  "Normal hook (list of functions) run after commit or file checkin.
See also `log-edit-done-hook'."
  :type 'hook
  :options '(log-edit-comment-to-change-log))

;;;###autoload
(defcustom vc-before-checkin-hook nil
  "Normal hook (list of functions) run before a commit or a file checkin.
See `run-hooks'."
  :type 'hook)

(defcustom vc-retrieve-tag-hook nil
  "Normal hook (list of functions) run after retrieving a tag."
  :type 'hook
  :version "27.1")

(defcustom vc-revert-show-diff t
  "If non-nil, `vc-revert' shows a `vc-diff' buffer before querying."
  :type '(choice (const :tag "Show and bury afterwards" t)
                 (const :tag "Show and kill afterwards" kill)
                 (const :tag "Don't show" nil))
  :version "24.1")

;; Header-insertion hair

(defcustom vc-static-header-alist
  '(("\\.c\\'" .
     "\n#ifndef lint\nstatic char vcid[] = \"%s\";\n#endif /* lint */\n"))
  "Associate static header string templates with file types.
A %s in the template is replaced with the first string associated with
the file's version control type in `vc-BACKEND-header'."
  :type '(repeat (cons :format "%v"
		       (regexp :tag "File Type")
		       (string :tag "Header String"))))

(defcustom vc-comment-alist
  '((nroff-mode ".\\\"" ""))
  "Special comment delimiters for generating VC headers.
Add an entry in this list if you need to override the normal `comment-start'
and `comment-end' variables.  This will only be necessary if the mode language
is sensitive to blank lines."
  :type '(repeat (list :format "%v"
		       (symbol :tag "Mode")
		       (string :tag "Comment Start")
		       (string :tag "Comment End"))))

(defcustom vc-find-revision-no-save nil
  "If non-nil, `vc-find-revision' doesn't write the created buffer to file."
  :type 'boolean
  :version "27.1")


;; File property caching

(defun vc-clear-context ()
  "Clear all cached file properties."
  (interactive)
  (fillarray vc-file-prop-obarray 0))

(defmacro with-vc-properties (files form settings)
  "Execute FORM, then maybe set per-file properties for FILES.
If any of FILES is actually a directory, then do the same for all
buffers for files in that directory.
SETTINGS is an association list of property/value pairs.  After
executing FORM, set those properties from SETTINGS that have not yet
been updated to their corresponding values."
  (declare (debug t))
  `(let ((vc-touched-properties (list t))
	 (flist nil))
     (dolist (file ,files)
       (if (file-directory-p file)
	   (dolist (buffer (buffer-list))
	     (let ((fname (buffer-file-name buffer)))
	       (when (and fname (string-prefix-p file fname))
		 (push fname flist))))
	 (push file flist)))
     ,form
     (dolist (file flist)
       (dolist (setting ,settings)
         (let ((property (car setting)))
           (unless (memq property vc-touched-properties)
             (put (intern file vc-file-prop-obarray)
                  property (cdr setting))))))))

;;; Code for deducing what fileset and backend to assume

(defun vc-backend-for-registration (file)
  "Return a backend that can be used for registering FILE.

If no backend declares itself responsible for FILE, then FILE
must not be in a version controlled directory, so try to create a
repository, prompting for the directory and the VC backend to
use."
  (catch 'found
    ;; First try: find a responsible backend, it must be a backend
    ;; under which FILE is not yet registered and with the most
    ;; specific path to FILE.
    (let ((max 0)
          bk)
      (dolist (backend vc-handled-backends)
        (when (not (vc-call-backend backend 'registered file))
          (let* ((dir-name (vc-call-backend backend 'responsible-p file))
                 (len (and dir-name
                           (length (file-name-split
                                    (expand-file-name dir-name))))))
            (when (and len (> len max))
              (setq max len bk backend)))))
      (when bk
        (throw 'found bk)))
    ;; no responsible backend
    (let* ((possible-backends
	    (let (pos)
	      (dolist (crt vc-handled-backends)
		(when (vc-find-backend-function crt 'create-repo)
		  (push crt pos)))
	      pos))
	   (bk
	    (intern
	     ;; Read the VC backend from the user, only
	     ;; complete with the backends that have the
	     ;; 'create-repo method.
	     (completing-read
	      (format "%s is not in a version controlled directory.\nUse VC backend: " file)
	      (mapcar #'symbol-name possible-backends) nil t)))
	   (repo-dir
	    (let ((def-dir (file-name-directory file)))
	      ;; read the directory where to create the
	      ;; repository, make sure it's a parent of
	      ;; file.
	      (read-file-name
	       (format "create %s repository in: " bk)
	       default-directory def-dir t nil
	       (lambda (arg)
		 (message "arg %s" arg)
		 (and (file-directory-p arg)
		      (string-prefix-p (expand-file-name arg) def-dir)))))))
      (let ((default-directory repo-dir))
	(vc-call-backend bk 'create-repo))
      (throw 'found bk))))

;;;###autoload
(defun vc-responsible-backend (file &optional no-error)
  "Return the name of a backend system that is responsible for FILE.

If FILE is already registered, return the
backend of FILE.  If FILE is not registered, then the
first backend in `vc-handled-backends' that declares itself
responsible for FILE is returned.

Note that if FILE is a symbolic link, it will not be resolved --
the responsible backend system for the symbolic link itself will
be reported.

If NO-ERROR is nil, signal an error that no VC backend is
responsible for the given file."
  (or (and (not (file-directory-p file)) (vc-backend file))
      ;; FIXME it would be more efficient to walk up the directory tree,
      ;; stopping the first time a backend is responsible.
      ;;
      ;; First try: find a responsible backend.  If this is for registration,
      ;; it must be a backend under which FILE is not yet registered.
      (let* ((file (expand-file-name file))
             (dirs (delq nil
                         (mapcar
                          (lambda (backend)
                            (when-let ((dir (vc-call-backend
                                             backend 'responsible-p file)))
                              (cons backend dir)))
                          vc-handled-backends))))
        ;; Just a single response (or none); use it.
        (if (< (length dirs) 2)
            (caar dirs)
          ;; Several roots; we seem to have one vc inside another's
          ;; directory.  Choose the most specific.
          (caar (sort dirs (lambda (d1 d2)
                             (< (length (cdr d2)) (length (cdr d1))))))))
      (unless no-error
        (error "No VC backend is responsible for %s" file))))

(defun vc-expand-dirs (file-or-dir-list backend)
  "Expand directories in a file list specification.
Within directories, only files already under version control are noticed."
  (let ((flattened '()))
    (dolist (node file-or-dir-list)
      (when (file-directory-p node)
	(vc-file-tree-walk
	 node (lambda (f) (when (eq (vc-backend f) backend) (push f flattened)))))
      (unless (file-directory-p node) (push node flattened)))
    (nreverse flattened)))

(defvar vc-dir-backend)
(defvar log-view-vc-backend)
(defvar log-edit-vc-backend)
(defvar diff-vc-backend)
(defvar diff-vc-revisions)

(defun vc-deduce-backend ()
  (cond ((derived-mode-p 'vc-dir-mode)   vc-dir-backend)
	((derived-mode-p 'log-view-mode) log-view-vc-backend)
	((derived-mode-p 'log-edit-mode) log-edit-vc-backend)
	((derived-mode-p 'diff-mode)     diff-vc-backend)
        ;; Maybe we could even use comint-mode rather than shell-mode?
	((derived-mode-p
          'dired-mode 'shell-mode 'eshell-mode 'compilation-mode)
	 (ignore-errors (vc-responsible-backend default-directory)))
	(vc-mode (vc-backend buffer-file-name))))

(declare-function vc-dir-current-file "vc-dir" ())
(declare-function vc-dir-deduce-fileset "vc-dir" (&optional state-model-only-files))
(declare-function dired-vc-deduce-fileset "dired-aux" (&optional state-model-only-files not-state-changing))

(defun vc-deduce-fileset (&optional not-state-changing
				    allow-unregistered
				    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.
Return a list of the form:

  (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL)

where the last 3 members are optional, and must be present only if
STATE-MODEL-ONLY-FILES is non-nil.

NOT-STATE-CHANGING, if non-nil, means that the operation
requesting the fileset doesn't intend to change the VC state,
such as when printing the log or showing the diffs.

If the current buffer is in `vc-dir' or Dired mode, FILESET is the
list of marked files, or the file under point if no files are
marked.
Otherwise, if the current buffer is visiting a version-controlled
file or is an indirect buffer whose base buffer visits a
version-controlled file, FILESET is a single-file list containing
that file's name.
Otherwise, if ALLOW-UNREGISTERED is non-nil and the visited file
is unregistered, FILESET is a single-file list containing the
name of the visited file.
Otherwise, throw an error.

STATE-MODEL-ONLY-FILES, if non-nil, means that the caller needs
the FILESET-ONLY-FILES, STATE, and CHECKOUT-MODEL info, where
FILESET-ONLY-FILES means only files in similar VC states,
possible values of STATE are explained in `vc-state', and MODEL in
`vc-checkout-model'.  Otherwise, these 3 members may be omitted from
the returned list.

BEWARE: this function may change the current buffer."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (vc-deduce-fileset-1 not-state-changing
                         allow-unregistered
                         state-model-only-files)))

(defun vc-deduce-fileset-1 (not-state-changing
                            allow-unregistered
                            state-model-only-files)
  (let (backend)
    (cond
     ((derived-mode-p 'vc-dir-mode)
      (vc-dir-deduce-fileset state-model-only-files))
     ((derived-mode-p 'dired-mode)
      (dired-vc-deduce-fileset state-model-only-files not-state-changing))
     ((derived-mode-p 'diff-mode)
      (diff-vc-deduce-fileset))
     ((setq backend (vc-backend buffer-file-name))
      (if state-model-only-files
	(list backend (list buffer-file-name)
	      (list buffer-file-name)
	      (vc-state buffer-file-name)
	      (vc-checkout-model backend buffer-file-name))
	(list backend (list buffer-file-name))))
     ((and (buffer-live-p vc-parent-buffer)
           ;; FIXME: Why this test?  --Stef
           (or (buffer-file-name vc-parent-buffer)
				(with-current-buffer vc-parent-buffer
				  (or (derived-mode-p 'vc-dir-mode)
				      (derived-mode-p 'dired-mode)
				      (derived-mode-p 'diff-mode)))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
	(set-buffer vc-parent-buffer)
	(vc-deduce-fileset-1 not-state-changing allow-unregistered state-model-only-files)))
     ((and (not buffer-file-name)
	   (setq backend (vc-responsible-backend default-directory)))
      (list backend nil))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
	  (list (vc-backend-for-registration (buffer-file-name))
		(list buffer-file-name)
		(list buffer-file-name)
		(when state-model-only-files 'unregistered)
		nil)
	(list (vc-backend-for-registration (buffer-file-name))
	      (list buffer-file-name))))
     (t (error "File is not under version control")))))

(defun vc-ensure-vc-buffer ()
  "Make sure that the current buffer visits a version-controlled file."
  (cond
   ((derived-mode-p 'vc-dir-mode)
    (set-buffer (find-file-noselect (vc-dir-current-file))))
   ((derived-mode-p 'dired-mode)
    (set-buffer (find-file-noselect (dired-get-filename))))
   (t
    (while (and vc-parent-buffer
                (buffer-live-p vc-parent-buffer)
		;; Avoid infinite looping when vc-parent-buffer and
		;; current buffer are the same buffer.
 		(not (eq vc-parent-buffer (current-buffer))))
      (set-buffer vc-parent-buffer))))
  (if (not buffer-file-name)
      (error "Buffer %s is not associated with a file" (buffer-name))
    (unless (vc-backend buffer-file-name)
      (error "File %s is not under version control" buffer-file-name))))

;;; Support for the C-x v v command.
;; This is where all the single-file-oriented code from before the fileset
;; rewrite lives.

(defsubst vc-editable-p (file)
  "Return non-nil if FILE can be edited."
  (let ((backend (vc-backend file)))
    (and backend
         (or (eq (vc-checkout-model backend (list file)) 'implicit)
             (memq (vc-state file) '(edited needs-merge conflict))))))

(defun vc-compatible-state (p q)
  "Control which states can be in the same commit."
  (or
   (eq p q)
   (and (member p '(edited added removed)) (member q '(edited added removed)))))

(defun vc-read-backend (prompt &optional backends default)
  (let ((backends (or backends vc-handled-backends))
        (completion-ignore-case t))
    (intern
     (completing-read prompt (mapcar #'symbol-name backends)
                      nil 'require-match nil nil default))))

;; Here's the major entry point.

;;;###autoload
(defun vc-next-action (verbose)
  "Do the next logical version control operation on the current fileset.
This requires that all files in the current VC fileset be in the
same state.  If not, signal an error.

For merging-based version control systems:
  If every file in the VC fileset is not registered for version
   control, register the fileset (but don't commit).
  If every work file in the VC fileset is added or changed, pop
   up a *vc-log* buffer to commit the fileset.
  For a centralized version control system, if any work file in
   the VC fileset is out of date, offer to update the fileset.

For old-style locking-based version control systems, like RCS:
  If every file is not registered, register the file(s).
  If every file is registered and unlocked, check out (lock)
   the file(s) for editing.
  If every file is locked by you and has changes, pop up a
   *vc-log* buffer to check in the changes.  Leave a
   read-only copy of each changed file after checking in.
  If every file is locked by you and unchanged, unlock them.
  If every file is locked by someone else, offer to steal the lock.

When using this command to register a new file (or files), it
will automatically deduce which VC repository to register it
with, using the most specific one."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (backend (car vc-fileset))
	 (files (nth 1 vc-fileset))
         ;; (fileset-only-files (nth 2 vc-fileset))
         ;; FIXME: We used to call `vc-recompute-state' here.
         (state (nth 3 vc-fileset))
         ;; The backend should check that the checkout-model is consistent
         ;; among all the `files'.
	 (model (nth 4 vc-fileset)))

    ;; If a buffer has unsaved changes, a checkout would discard those
    ;; changes, so treat the buffer as having unlocked changes.
    (when (and (not (eq model 'implicit)) (eq state 'up-to-date))
      (dolist (file files)
        (let ((buffer (get-file-buffer file)))
          (and buffer
               (buffer-modified-p buffer)
               (setq state 'unlocked-changes)))))

    ;; Do the right thing.
    (cond
     ((eq state 'missing)
      (error "Fileset files are missing, so cannot be operated on"))
     ((eq state 'ignored)
      (error "Fileset files are ignored by the version-control system"))
     ((eq model 'patch)
      (vc-checkin files backend nil nil nil (buffer-string)))
     ((or (null state) (eq state 'unregistered))
      (cond (verbose
             (let ((backend (vc-read-backend "Backend to register to: ")))
               (vc-register (cons backend (cdr vc-fileset)))))
            (t
             (vc-register vc-fileset))))
     ;; Files are up-to-date, or need a merge and user specified a revision
     ((or (eq state 'up-to-date) (and verbose (eq state 'needs-update)))
      (cond
       (verbose
	;; Go to a different revision.
	(let* ((revision
                ;; FIXME: Provide completion.
                (read-string "Branch, revision, or backend to move to: "))
               (revision-downcase (downcase revision)))
	  (if (member
	       revision-downcase
	       (mapcar (lambda (arg) (downcase (symbol-name arg)))
                       vc-handled-backends))
	      (let ((vsym (intern-soft revision-downcase)))
		(dolist (file files) (vc-transfer-file file vsym)))
	    (dolist (file files)
              (vc-checkout file revision)))))
       ((not (eq model 'implicit))
	;; check the files out
	(dolist (file files) (vc-checkout file)))
       (t
        ;; do nothing
        (message "Fileset is up-to-date"))))
     ;; Files have local changes
     ((vc-compatible-state state 'edited)
      (let ((ready-for-commit files))
	;; CVS, SVN and bzr don't care about read-only (bug#9781).
	;; RCS does, SCCS might (someone should check...).
	(when (memq backend '(RCS SCCS))
	  ;; If files are edited but read-only, give user a chance to correct.
	  (dolist (file files)
	    ;; If committing a mix of removed and edited files, the
	    ;; fileset has state = 'edited.  Rather than checking the
	    ;; state of each individual file in the fileset, it seems
	    ;; simplest to just check if the file exists.	 Bug#9781.
	    (when (and (file-exists-p file) (not (file-writable-p file)))
	      ;; Make the file-buffer read-write.
	      (unless (y-or-n-p (format "%s is edited but read-only; make it writable and continue? " file))
		(error "Aborted"))
	      ;; Maybe we somehow lost permissions on the directory.
	      (condition-case nil
		  (set-file-modes file (logior (file-modes file) 128))
		(error (error "Unable to make file writable")))
	      (let ((visited (get-file-buffer file)))
		(when visited
		  (with-current-buffer visited
		    (read-only-mode -1)))))))
	;; Allow user to revert files with no changes
	(save-excursion
          (dolist (file files)
            (let ((visited (get-file-buffer file)))
              ;; For files with locking, if the file does not contain
              ;; any changes, just let go of the lock, i.e. revert.
              (when (and (not (eq model 'implicit))
			 (eq state 'up-to-date)
			 ;; If buffer is modified, that means the user just
			 ;; said no to saving it; in that case, don't revert,
			 ;; because the user might intend to save after
			 ;; finishing the log entry and committing.
			 (not (and visited (buffer-modified-p))))
		(vc-revert-file file)
		(setq ready-for-commit (delete file ready-for-commit))))))
	;; Remaining files need to be committed
	(if (not ready-for-commit)
	    (message "No files remain to be committed")
	  (if (not verbose)
	      (vc-checkin ready-for-commit backend)
	    (let* ((revision (read-string "New revision or backend: "))
                   (revision-downcase (downcase revision)))
	      (if (member
		   revision-downcase
		   (mapcar (lambda (arg) (downcase (symbol-name arg)))
			   vc-handled-backends))
		  (let ((vsym (intern revision-downcase)))
		    (dolist (file files) (vc-transfer-file file vsym)))
		(vc-checkin ready-for-commit backend nil nil revision)))))))
     ;; locked by somebody else (locking VCSes only)
     ((stringp state)
      ;; In the old days, we computed the revision once and used it on
      ;; the single file.  Then, for the 2007-2008 fileset rewrite, we
      ;; computed the revision once (incorrectly, using a free var) and
      ;; used it on all files.  To fix the free var bug, we can either
      ;; use `(car files)' or do what we do here: distribute the
      ;; revision computation among `files'.  Although this may be
      ;; tedious for those backends where a "revision" is a trans-file
      ;; concept, it is nonetheless correct for both those and (more
      ;; importantly) for those where "revision" is a per-file concept.
      ;; If the intersection of the former group and "locking VCSes" is
      ;; non-empty [I vaguely doubt it --ttn], we can reinstate the
      ;; pre-computation approach of yore.
      (dolist (file files)
        (vc-steal-lock
         file (if verbose
                  (read-string (format "%s revision to steal: " file))
                (vc-working-revision file))
         state)))
     ;; conflict
     ((eq state 'conflict)
      ;; FIXME: Is it really the UI we want to provide?
      ;; In my experience, the conflicted files should be marked as resolved
      ;; one-by-one when saving the file after resolving the conflicts.
      ;; I.e. stating explicitly that the conflicts are resolved is done
      ;; very rarely.
      (vc-mark-resolved backend files))
     ;; needs-update
     ((eq state 'needs-update)
      (dolist (file files)
	(if (yes-or-no-p (format
			  "%s is not up-to-date.  Get latest revision? "
			  (file-name-nondirectory file)))
	    (vc-checkout file t)
	  (when (and (not (eq model 'implicit))
		     (yes-or-no-p "Lock this revision? "))
	    (vc-checkout file)))))
     ;; needs-merge
     ((eq state 'needs-merge)
      (dolist (file files)
	(when (yes-or-no-p (format
			  "%s is not up-to-date.  Merge in changes now? "
			  (file-name-nondirectory file)))
	  (vc-maybe-resolve-conflicts
           file (vc-call-backend backend 'merge-news file)))))

     ;; unlocked-changes
     ((eq state 'unlocked-changes)
      (dolist (file files)
	(when (not (equal buffer-file-name file))
	  (find-file-other-window file))
	(if (save-window-excursion
	      (vc-diff-internal nil
				(cons (car vc-fileset) (cons (cadr vc-fileset) (list file)))
				(vc-working-revision file) nil)
	      (goto-char (point-min))
	      (let ((inhibit-read-only t))
		(insert
		 (format "Changes to %s since last lock:\n\n" file)))
	      (not (beep))
	      (yes-or-no-p (concat "File has unlocked changes.  "
				   "Claim lock retaining changes? ")))
	    (progn (vc-call-backend backend 'steal-lock file)
		   (clear-visited-file-modtime)
		   (write-file buffer-file-name)
		   (vc-mode-line file backend))
	  (if (not (yes-or-no-p
		    "Revert to checked-in revision, instead? "))
	      (error "Checkout aborted")
	    (vc-revert-buffer-internal t t)
	    (vc-checkout file)))))
     ;; Unknown fileset state
     (t
      (error "Fileset is in an unknown state %s" state)))))

(defun vc-create-repo (backend)
  "Create an empty repository in the current directory."
  (interactive (list (vc-read-backend "Create repository for: ")))
  (vc-call-backend backend 'create-repo))

;;;###autoload
(defun vc-register (&optional vc-fileset comment)
  "Register into a version control system.
If VC-FILESET is given, register the files in that fileset.
Otherwise register the current file.
If COMMENT is present, use that as an initial comment.

The version control system to use is found by cycling through the list
`vc-handled-backends'.  The first backend in that list which declares
itself responsible for the file (usually because other files in that
directory are already registered under that backend) will be used to
register the file.  If no backend declares itself responsible, the
first backend that could register the file is used."
  (interactive "P")
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
	 (files (nth 1 fileset-arg)))
    ;; We used to operate on `only-files', but VC wants to provide the
    ;; possibility to register directories rather than files only, since
    ;; many VCS allow that as well.
    (dolist (fname files)
      (let ((bname (get-file-buffer fname)))
	(unless fname
	  (setq fname buffer-file-name))
	(when (vc-call-backend backend 'registered fname)
	  (error "This file is already registered: %s" fname))
	;; Watch out for new buffers of size 0: the corresponding file
	;; does not exist yet, even though buffer-modified-p is nil.
	(when bname
	  (with-current-buffer bname
	    (when (and (not (buffer-modified-p))
		       (zerop (buffer-size))
		       (not (file-exists-p buffer-file-name)))
	      (set-buffer-modified-p t))
	    (vc-buffer-sync)))))
    (message "Registering %s... " files)
    (mapc #'vc-file-clearprops files)
    (vc-call-backend backend 'register files comment)
    (mapc
     (lambda (file)
       (vc-file-setprop file 'vc-backend backend)
       ;; FIXME: This is wrong: it should set `backup-inhibited' in all
       ;; the buffers visiting files affected by this `vc-register', not
       ;; in the current-buffer.
       ;; (unless vc-make-backup-files
       ;;   (setq-local backup-inhibited t))

       (vc-resynch-buffer file t t))
     files)
    (message "Registering %s... done" files)))

(defun vc-register-with (backend)
  "Register the current file with a specified back end."
  (interactive "SBackend: ")
  (when (not (member backend vc-handled-backends))
    (error "Unknown back end"))
  (let ((vc-handled-backends (list backend)))
    (call-interactively 'vc-register)))

;;;###autoload
(defun vc-ignore (file &optional directory remove)
  "Ignore FILE under the VCS of DIRECTORY.

Normally, FILE is a wildcard specification that matches the files
to be ignored.  When REMOVE is non-nil, remove FILE from the list
of ignored files.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

When called interactively, prompt for a FILE to ignore, unless a
prefix argument is given, in which case prompt for a file FILE to
remove from the list of ignored files."
  (interactive
   (let* ((rel-dir (vc--ignore-base-dir))
          (file (read-file-name "File to ignore: ")))
     (when (and (file-name-absolute-p file)
                (file-in-directory-p file rel-dir))
       (setq file (file-relative-name file rel-dir)))
     (list file
           rel-dir
           current-prefix-arg)))
  (let* ((directory (or directory default-directory))
	 (backend (or (vc-responsible-backend default-directory)
                      (error "Unknown backend"))))
    (vc-call-backend backend 'ignore file directory remove)))

(defun vc--ignore-base-dir ()
  (let ((backend (vc-responsible-backend default-directory)))
    (condition-case nil
        (file-name-directory
         (vc-call-backend backend 'find-ignore-file
                          default-directory))
      (vc-not-supported
       default-directory))))

(defun vc-default-ignore (backend file &optional directory remove)
  "Ignore FILE under DIRECTORY (default is `default-directory').
FILE is a wildcard specification relative to DIRECTORY.

When called from Lisp code, if DIRECTORY is non-nil, the
repository to use will be deduced by DIRECTORY.

If REMOVE is non-nil, remove FILE from ignored files instead.

Argument BACKEND is the backend to use."
  (let ((ignore
         (vc-call-backend backend
                          'find-ignore-file
                          (or directory default-directory))))
    (if remove
        (vc--remove-regexp (concat "^" (regexp-quote file) "\\(\n\\|$\\)") ignore)
      (vc--add-line file ignore))))

(defun vc-default-ignore-completion-table (backend file)
  "Return the list of ignored files under BACKEND."
  (cl-delete-if
   (lambda (str)
     ;; Commented or empty lines.
     (string-match-p "\\`\\(?:#\\|[ \t\r\n]*\\'\\)" str))
   (let ((file (vc-call-backend backend 'find-ignore-file file)))
     (and (file-exists-p file)
          (vc--read-lines file)))))

(defun vc--read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; Subroutine for `vc-git-ignore' and `vc-hg-ignore'.
(defun vc--add-line (string file)
  "Add STRING as a line to FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote string) "$") nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert string "\n")
      (save-buffer))))

(defun vc--remove-regexp (regexp file)
  "Remove all matching for REGEXP in FILE."
  (if (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match ""))
        (save-buffer))))

(defun vc-checkout (file &optional rev)
  "Retrieve a copy of the revision REV of FILE.
REV defaults to the latest revision.

After check-out, runs the normal hook `vc-checkout-hook'."
  (and (not rev)
       (vc-call make-version-backups-p file)
       (vc-up-to-date-p file)
       (vc-make-version-backup file))
  (let ((backend (vc-backend file)))
    (with-vc-properties (list file)
      (condition-case err
          (vc-call-backend backend 'checkout file rev)
        (file-error
         ;; Maybe the backend is not installed ;-(
         (when t
           (let ((buf (get-file-buffer file)))
             (when buf (with-current-buffer buf (read-only-mode -1)))))
         (signal (car err) (cdr err))))
      `((vc-state . ,(if (or (eq (vc-checkout-model backend (list file)) 'implicit)
                             nil)
			 'up-to-date
                       'edited))
        (vc-checkout-time . ,(file-attribute-modification-time
			      (file-attributes file))))))
  (vc-resynch-buffer file t t)
  (run-hooks 'vc-checkout-hook))

(defun vc-mark-resolved (backend files)
  (prog1 (with-vc-properties
	  files
	  (vc-call-backend backend 'mark-resolved files)
	  ;; FIXME: Is this TRTD?  Might not be.
	  `((vc-state . edited)))
    ;; Recompute mode lines.
    (dolist (file files)
      (vc-mode-line file backend))
    (message
     (substitute-command-keys
      "Conflicts have been resolved in %s.  \
Type \\[vc-next-action] to check in changes.")
     (if (> (length files) 1)
	 (format "%d files" (length files))
       "this file"))))

(defun vc-steal-lock (file rev owner)
  "Steal the lock on FILE."
  (let (file-description)
    (if rev
	(setq file-description (format "%s:%s" file rev))
      (setq file-description file))
    (when (not (yes-or-no-p (format "Steal the lock on %s from %s? "
				    file-description owner)))
      (error "Steal canceled"))
    (message "Stealing lock on %s..." file)
    (with-vc-properties
     (list file)
     (vc-call steal-lock file rev)
     `((vc-state . edited)))
    (vc-resynch-buffer file t t)
    (message "Stealing lock on %s...done" file)
    ;; Write mail after actually stealing, because if the stealing
    ;; goes wrong, we don't want to send any mail.
    (compose-mail owner (format "Stolen lock on %s" file-description))
    (setq default-directory (expand-file-name "~/"))
    (goto-char (point-max))
    (insert
     (format "I stole the lock on %s, " file-description)
     (current-time-string)
     ".\n")
    (message "Please explain why you stole the lock.  Type C-c C-c when done.")))

(defun vc-checkin (files backend &optional comment initial-contents rev patch-string)
  "Check in FILES. COMMENT is a comment string; if omitted, a
buffer is popped up to accept a comment.  If INITIAL-CONTENTS is
non-nil, then COMMENT is used as the initial contents of the log
entry buffer.
The optional argument REV may be a string specifying the new revision
level (only supported for some older VCSes, like RCS and CVS).
The optional argument PATCH-STRING is a string to check in as a patch.

Runs the normal hooks `vc-before-checkin-hook' and `vc-checkin-hook'."
  (run-hooks 'vc-before-checkin-hook)
  (vc-start-logentry
   files comment initial-contents
   "Enter a change comment."
   "*vc-log*"
   (lambda ()
     (vc-call-backend backend 'log-edit-mode))
   (lambda (files comment)
     (message "Checking in %s..." (vc-delistify files))
     ;; "This log message intentionally left almost blank".
     ;; RCS 5.7 gripes about white-space-only comments too.
     (or (and comment (string-match "[^\t\n ]" comment))
         (setq comment "*** empty log message ***"))
     (with-vc-properties
         files
       ;; We used to change buffers to get local value of
       ;; vc-checkin-switches, but 'the' local buffer is
       ;; not a well-defined concept for filesets.
       (progn
         (if patch-string
             (vc-call-backend backend 'checkin-patch patch-string comment)
           (vc-call-backend backend 'checkin files comment rev))
         (mapc #'vc-delete-automatic-version-backups files))
       `((vc-state . up-to-date)
         (vc-checkout-time . ,(file-attribute-modification-time
			       (file-attributes file)))
         (vc-working-revision . nil)))
     (message "Checking in %s...done" (vc-delistify files)))
   'vc-checkin-hook
   backend
   patch-string))

;;; Additional entry points for examining version histories

;; (defun vc-default-diff-tree (backend dir rev1 rev2)
;;   "List differences for all registered files at and below DIR.
;; The meaning of REV1 and REV2 is the same as for `vc-revision-diff'."
;;   ;; This implementation does an explicit tree walk, and calls
;;   ;; vc-BACKEND-diff directly for each file.  An optimization
;;   ;; would be to use `vc-diff-internal', so that diffs can be local,
;;   ;; and to call it only for files that are actually changed.
;;   ;; However, this is expensive for some backends, and so it is left
;;   ;; to backend-specific implementations.
;;   (setq default-directory dir)
;;   (vc-file-tree-walk
;;    default-directory
;;    (lambda (f)
;;      (vc-run-delayed
;;       (let ((coding-system-for-read (vc-coding-system-for-diff f)))
;;          (message "Looking at %s" f)
;;          (vc-call-backend (vc-backend f)
;;                           'diff (list f) rev1 rev2))))))

(defvar vc-coding-system-inherit-eol t
  "When non-nil, inherit the EOL format for reading Diff output from the file.

Used in `vc-coding-system-for-diff' to determine the EOL format to use
for reading Diff output for a file.  If non-nil, the EOL format is
inherited from the file itself.
Set this variable to nil if your Diff tool might use a different
EOL.  Then Emacs will auto-detect the EOL format in Diff output, which
gives better results.") ;; Cf. bug#4451.

(defun vc-coding-system-for-diff (file)
  "Return the coding system for reading diff output for FILE."
  (or coding-system-for-read
      ;; if we already have this file open,
      ;; use the buffer's coding system
      (let ((buf (find-buffer-visiting file)))
        (when buf (with-current-buffer buf
		    (if vc-coding-system-inherit-eol
			buffer-file-coding-system
		      ;; Don't inherit the EOL part of the coding-system,
		      ;; because some Diff tools may choose to use
		      ;; a different one.  bug#4451.
		      (coding-system-base buffer-file-coding-system)))))
      ;; otherwise, try to find one based on the file name
      (car (find-operation-coding-system 'insert-file-contents file))
      ;; and a final fallback
      'undecided))

(defun vc-switches (backend op)
  "Return a list of vc-BACKEND switches for operation OP.
BACKEND is a symbol such as `CVS', which will be downcased.
OP is a symbol such as `diff'.

In decreasing order of preference, return the value of:
vc-BACKEND-OP-switches (e.g. `vc-cvs-diff-switches');
vc-OP-switches (e.g. `vc-diff-switches'); or, in the case of
diff only, `diff-switches'.

If the chosen value is not a string or a list, return nil.
This is so that you may set, e.g. `vc-svn-diff-switches' to t in order
to override the value of `vc-diff-switches' and `diff-switches'."
  (let ((switches
	 (or (when backend
	       (let ((sym (vc-make-backend-sym
			   backend (intern (concat (symbol-name op)
						   "-switches")))))
		   (when (boundp sym) (symbol-value sym))))
	     (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
	       (when (boundp sym) (symbol-value sym)))
	     (cond
	      ((eq op 'diff) diff-switches)))))
    (if (stringp switches) (list switches)
      ;; If not a list, return nil.
      ;; This is so we can set vc-diff-switches to t to override
      ;; any switches in diff-switches.
      (when (listp switches) switches))))

(defun vc-shrink-buffer-window (&optional buffer)
  "Call `shrink-window-if-larger-than-buffer' only when BUFFER is visible.
BUFFER defaults to the current buffer."
  (let ((window (get-buffer-window buffer t)))
    (when window
      (shrink-window-if-larger-than-buffer window))))

(defvar vc-diff-finish-functions '(vc-shrink-buffer-window)
  "Functions run at the end of the diff command.
Each function runs in the diff output buffer without args.")

(defun vc-diff-restore-buffer (original new)
  "Restore point in buffer NEW to where it was in ORIGINAL.

This function works by updating buffer ORIGINAL with the contents
of NEW (without destroying existing markers), swapping their text
objects, and finally killing buffer ORIGINAL."
  (with-current-buffer original
    (let ((inhibit-read-only t))
      (replace-buffer-contents new)))
  (with-current-buffer new
    (buffer-swap-text original))
  (kill-buffer original))

(defun vc-diff-finish (buffer messages &optional oldbuf)
  ;; The empty sync output case has already been handled, so the only
  ;; possibility of an empty output is for an async process.
  (when (buffer-live-p buffer)
    (let ((emptyp (zerop (buffer-size buffer))))
      (with-current-buffer buffer
	(and messages emptyp
	     (let ((inhibit-read-only t))
	       (insert (cdr messages) ".\n")
	       (message "%s" (cdr messages))))
	(diff-setup-whitespace)
	(diff-setup-buffer-type)
        ;; `oldbuf' is the buffer that used to show this diff.  Make
        ;; sure that we restore point in it if it's given.
	(if oldbuf
            (vc-diff-restore-buffer oldbuf buffer)
          (goto-char (point-min)))
	(run-hooks 'vc-diff-finish-functions))
      (when (and messages (not emptyp))
	(message "%sdone" (car messages))))))

(defvar vc-diff-added-files nil
  "If non-nil, diff added files by comparing them to /dev/null.")

(defvar vc-patch-string nil)

(defun vc-diff-patch-string (patch-string)
  "Report diffs to be committed from the patch.
Like `vc-diff-internal' but uses PATCH-STRING to display
in the output buffer."
  (let ((buffer "*vc-diff*"))
    (vc-setup-buffer buffer)
    (let ((buffer-undo-list t)
          (inhibit-read-only t))
      (insert patch-string))
    (setq buffer-read-only t)
    (diff-mode)
    (setq-local diff-vc-backend (vc-responsible-backend default-directory))
    (setq-local revert-buffer-function
                (lambda (_ _) (vc-diff-patch-string patch-string)))
    (setq-local vc-patch-string patch-string)
    (pop-to-buffer (current-buffer))
    (vc-run-delayed (vc-diff-finish (current-buffer) nil))))

(defun vc-diff-internal (async vc-fileset rev1 rev2 &optional verbose buffer)
  "Report diffs between two revisions of a fileset.
Output goes to the buffer BUFFER, which defaults to *vc-diff*.
BUFFER, if non-nil, should be a buffer or a buffer name.
Return t if the buffer had changes, nil otherwise."
  (unless buffer
    (setq buffer "*vc-diff*"))
  (let* ((files (cadr vc-fileset))
	 (messages (cons (format "Finding changes in %s..."
                                 (vc-delistify files))
                         (format "No changes between %s and %s"
                                 (or rev1 "working revision")
                                 (or rev2 "workfile"))))
	 ;; Set coding system based on the first file.  It's a kluge,
	 ;; but the only way to set it for each file included would
	 ;; be to call the back end separately for each file.
	 (coding-system-for-read
	  (if files (vc-coding-system-for-diff (car files)) 'undecided))
         (orig-diff-buffer-clone
          (if revert-buffer-in-progress-p
              (clone-buffer
               (generate-new-buffer-name " *vc-diff-clone*") nil))))
    ;; On MS-Windows and MS-DOS, Diff is likely to produce DOS-style
    ;; EOLs, which will look ugly if (car files) happens to have Unix
    ;; EOLs.
    (if (memq system-type '(windows-nt ms-dos))
	(setq coding-system-for-read
	      (coding-system-change-eol-conversion coding-system-for-read
						   'dos)))
    (vc-setup-buffer buffer)
    (message "%s" (car messages))
    ;; Many backends don't handle well the case of a file that has been
    ;; added but not yet committed to the repo (notably CVS and Subversion).
    ;; Do that work here so the backends don't have to futz with it.  --ESR
    ;;
    ;; Actually most backends (including CVS) have options to control the
    ;; behavior since which one is better depends on the user and on the
    ;; situation).  Worse yet: this code does not handle the case where
    ;; `file' is a directory which contains added files.
    ;; I made it conditional on vc-diff-added-files but it should probably
    ;; just be removed (or copied/moved to specific backends).  --Stef.
    (when vc-diff-added-files
      (let ((filtered '())
	    process-file-side-effects)
        (dolist (file files)
          (if (or (file-directory-p file)
                  (not (string= (vc-working-revision file) "0")))
              (push file filtered)
            ;; This file is added but not yet committed;
            ;; there is no repository version to diff against.
            (if (or rev1 rev2)
                (error "No revisions of %s exist" file)
              ;; We regard this as "changed".
              ;; Diff it against /dev/null.
              (apply #'vc-do-command buffer
                     (if async 'async 1) "diff" file
                     (append (vc-switches nil 'diff) `(,(null-device)))))))
        (setq files (nreverse filtered))))
    (vc-call-backend (car vc-fileset) 'diff files rev1 rev2 buffer async)
    (set-buffer buffer)
    ;; Make the *vc-diff* buffer read only, the diff-mode key
    ;; bindings are nicer for read only buffers. pcl-cvs does the
    ;; same thing.
    (setq buffer-read-only t)
    (diff-mode)
    (setq-local diff-vc-backend (car vc-fileset))
    (setq-local diff-vc-revisions (list rev1 rev2))
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (vc-diff-internal async vc-fileset rev1 rev2 verbose)))
    (if (and (zerop (buffer-size))
             (not (get-buffer-process (current-buffer))))
        ;; Treat this case specially so as not to pop the buffer.
        (progn
          (message "%s" (cdr messages))
          nil)
      ;; Display the buffer, but at the end because it can change point.
      (pop-to-buffer (current-buffer))
      ;; The diff process may finish early, so call `vc-diff-finish'
      ;; after `pop-to-buffer'; the former assumes the diff buffer is
      ;; shown in some window.
      (let ((buf (current-buffer)))
        (vc-run-delayed (vc-diff-finish buf (when verbose messages)
                                        orig-diff-buffer-clone)))
      ;; In the async case, we return t even if there are no differences
      ;; because we don't know that yet.
      t)))

(defvar vc-revision-history nil
  "History for `vc-read-revision'.")

(defun vc-read-revision (prompt &optional files backend default initial-input)
  (cond
   ((null files)
    (let ((vc-fileset (vc-deduce-fileset t))) ;FIXME: why t?  --Stef
      (setq files (cadr vc-fileset))
      (setq backend (car vc-fileset))))
   ((null backend) (setq backend (vc-backend (car files)))))
  (let ((completion-table
         (vc-call-backend backend 'revision-completion-table files)))
    (if completion-table
        (completing-read prompt completion-table
                         nil nil initial-input 'vc-revision-history default)
      (read-string prompt initial-input nil default))))

(defun vc-diff-build-argument-list-internal (&optional fileset)
  "Build argument list for calling internal diff functions."
  (let* ((vc-fileset (or fileset (vc-deduce-fileset t))) ;FIXME: why t?  --Stef
         (files (cadr vc-fileset))
         (backend (car vc-fileset))
         (first (car files))
         (rev1-default nil)
         ) ;; (rev2-default nil)
    (cond
     ;; someday we may be able to do revision completion on non-singleton
     ;; filesets, but not yet.
     ((/= (length files) 1)
      nil)
     ;; if it's a directory, don't supply any revision default
     ((file-directory-p first)
      nil)
     ;; if the file is not up-to-date, use working revision as older revision
     ((not (vc-up-to-date-p first))
      (setq rev1-default (vc-working-revision first)))
     ;; if the file is not locked, use last revision and current source as defaults
     (t
      (setq rev1-default (ignore-errors ;If `previous-revision' doesn't work.
                           (vc-call-backend backend 'previous-revision first
                                            (vc-working-revision first))))
      (when (string= rev1-default "") (setq rev1-default nil))))
    ;; construct argument list
    (let* ((rev1-prompt (format-prompt "Older revision" rev1-default))
           (rev2-prompt (format-prompt "Newer revision"
                                       ;; (or rev2-default
                                       "current source"))
           (rev1 (vc-read-revision rev1-prompt files backend rev1-default))
           (rev2 (vc-read-revision rev2-prompt files backend nil))) ;; rev2-default
      (when (string= rev1 "") (setq rev1 nil))
      (when (string= rev2 "") (setq rev2 nil))
      (list files rev1 rev2))))

;;;###autoload
(defun vc-version-diff (_files rev1 rev2)
  "Report diffs between revisions REV1 and REV2 in the repository history.
This compares two revisions of the current fileset.
If REV1 is nil, it defaults to the current revision, i.e. revision
of the last commit.
If REV2 is nil, it defaults to the work tree, i.e. the current
state of each file in the fileset."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  ;; Yes, it's painful to call (vc-deduce-fileset) again.  Alas, the
  ;; placement rules for (interactive) don't actually leave us a choice.
  (vc-diff-internal t (vc-deduce-fileset t) rev1 rev2
		    (called-interactively-p 'interactive)))

;;;###autoload
(defun vc-root-version-diff (_files rev1 rev2)
  "Report diffs between REV1 and REV2 revisions of the whole tree."
  (interactive
   (vc-diff-build-argument-list-internal
    (or (ignore-errors (vc-deduce-fileset t))
        (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
          (list backend (list (vc-call-backend backend 'root default-directory)))))))
  ;; This is a mix of `vc-root-diff' and `vc-version-diff'
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  (let ((backend (vc-deduce-backend))
        (default-directory default-directory)
        rootdir)
    (if backend
        (setq rootdir (vc-call-backend backend 'root default-directory))
      (setq rootdir (read-directory-name "Directory for VC root-diff: "))
      (setq backend (vc-responsible-backend rootdir))
      (if backend
          (setq default-directory rootdir)
        (error "Directory is not version controlled")))
    (let ((default-directory rootdir))
      (vc-diff-internal
       t (list backend (list rootdir)) rev1 rev2
       (called-interactively-p 'interactive)))))

(defun vc-maybe-buffer-sync (not-urgent)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (when buffer-file-name (vc-buffer-sync not-urgent))))

;;;###autoload
(defun vc-diff (&optional historic not-urgent fileset)
  "Display diffs between file revisions.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer.  The optional argument FILESET can override the
deduced fileset."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-diff)
    (vc-maybe-buffer-sync not-urgent)
    (let ((fileset (or fileset (vc-deduce-fileset t))))
      (vc-buffer-sync-fileset fileset not-urgent)
      (vc-diff-internal t fileset nil nil
			(called-interactively-p 'interactive)))))

(defun vc-buffer-sync-fileset (fileset not-urgent)
  (dolist (filename (cadr fileset))
    (when-let ((buffer (find-buffer-visiting filename)))
      (with-current-buffer buffer
	(vc-buffer-sync not-urgent)))))

;;;###autoload
(defun vc-diff-mergebase (_files rev1 rev2)
  "Report diffs between the merge base of REV1 and REV2 revisions.
The merge base is a common ancestor between REV1 and REV2 revisions."
  (interactive
   (vc-diff-build-argument-list-internal
    (or (ignore-errors (vc-deduce-fileset t))
        (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
          (list backend (list (vc-call-backend backend 'root default-directory)))))))
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))
  (let ((backend (vc-deduce-backend))
        (default-directory default-directory)
        rootdir)
    (if backend
        (setq rootdir (vc-call-backend backend 'root default-directory))
      (setq rootdir (read-directory-name "Directory for VC root-diff: "))
      (setq backend (vc-responsible-backend rootdir))
      (if backend
          (setq default-directory rootdir)
        (error "Directory is not version controlled")))
    (let ((default-directory rootdir)
          (rev1 (vc-call-backend backend 'mergebase rev1 rev2)))
      (vc-diff-internal
       t (list backend (list rootdir)) rev1 rev2
       (called-interactively-p 'interactive)))))

(declare-function ediff-load-version-control "ediff" (&optional silent))
(declare-function ediff-vc-internal "ediff-vers"
                  (rev1 rev2 &optional startup-hooks))

;;;###autoload
(defun vc-version-ediff (files rev1 rev2)
  "Show differences between REV1 and REV2 of FILES using ediff.
This compares two revisions of the files in FILES.  Currently,
only a single file's revisions can be compared, i.e. FILES can
specify only one file name.
If REV1 is nil, it defaults to the current revision, i.e. revision
of the last commit.
If REV2 is nil, it defaults to the work tree, i.e. the current
state of each file in FILES."
  (interactive (vc-diff-build-argument-list-internal))
  ;; All that was just so we could do argument completion!
  (when (and (not rev1) rev2)
    (error "Not a valid revision range"))

  (message "%s" (format "Finding changes in %s..." (vc-delistify files)))

  ;; Functions ediff-(vc|rcs)-internal use "" instead of nil.
  (when (null rev1) (setq rev1 ""))
  (when (null rev2) (setq rev2 ""))

  (cond
   ;; FIXME We only support running ediff on one file for now.
   ;; We could spin off an ediff session per file in the file set.
   ((= (length files) 1)
    (require 'ediff)
    (ediff-load-version-control)  ; loads ediff-vers
    (find-file (car files))             ;FIXME: find-file from Elisp is bad.
    (ediff-vc-internal rev1 rev2 nil))
   (t
    (error "More than one file is not supported"))))

;;;###autoload
(defun vc-ediff (historic &optional not-urgent)
  "Display diffs between file revisions using ediff.
Normally this compares the currently selected fileset with their
working revisions.  With a prefix argument HISTORIC, it reads two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      (call-interactively 'vc-version-ediff)
    (vc-maybe-buffer-sync not-urgent)
    (vc-version-ediff (cadr (vc-deduce-fileset t)) nil nil)))

;;;###autoload
(defun vc-root-diff (historic &optional not-urgent)
  "Display diffs between VC-controlled whole tree revisions.
Normally, this compares the tree corresponding to the current
fileset with the working revision.
With a prefix argument HISTORIC, prompt for two revision
designators specifying which revisions to compare.

The optional argument NOT-URGENT non-nil means it is ok to say no to
saving the buffer."
  (interactive (list current-prefix-arg t))
  (if historic
      ;; We want the diff for the VC root dir.
      (call-interactively 'vc-root-version-diff)
    (vc-maybe-buffer-sync not-urgent)
    (let ((backend (vc-deduce-backend))
	  (default-directory default-directory)
	  rootdir working-revision)
      (if backend
	  (setq rootdir (vc-call-backend backend 'root default-directory))
	(setq rootdir (read-directory-name "Directory for VC root-diff: "))
	(setq backend (vc-responsible-backend rootdir))
	(if backend
	    (setq default-directory rootdir)
	  (error "Directory is not version controlled")))
      (setq working-revision (vc-working-revision rootdir))
      ;; VC diff for the root directory produces output that is
      ;; relative to it.  Bind default-directory to the root directory
      ;; here, this way the *vc-diff* buffer is setup correctly, so
      ;; relative file names work.
      (let ((default-directory rootdir))
        (vc-diff-internal
         t (list backend (list rootdir) working-revision) nil nil
         (called-interactively-p 'interactive))))))

;;;###autoload
(defun vc-root-dir ()
  "Return the root directory for the current VC tree.
Return nil if the root directory cannot be identified."
  (let ((backend (vc-deduce-backend)))
    (if backend
        (condition-case err
            (vc-call-backend backend 'root default-directory)
          (vc-not-supported
           (unless (eq (cadr err) 'root)
             (signal (car err) (cdr err)))
           nil)))))

;;;###autoload
(defun vc-revision-other-window (rev)
  "Visit revision REV of the current file in another window.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  (interactive
   (with-current-buffer (or (buffer-base-buffer) (current-buffer))
     (vc-ensure-vc-buffer)
     (list
      (vc-read-revision (format-prompt "Revision to visit" "working revision")
                        (list buffer-file-name)))))
  (set-buffer (or (buffer-base-buffer) (current-buffer)))
  (vc-ensure-vc-buffer)
  (let* ((file buffer-file-name)
	 (revision (if (string-equal rev "")
		       (vc-working-revision file)
		     rev)))
    (switch-to-buffer-other-window (vc-find-revision file revision))))

(defun vc-find-revision (file revision &optional backend)
  "Read REVISION of FILE into a buffer and return the buffer.
Use BACKEND as the VC backend if specified."
  (if vc-find-revision-no-save
      (vc-find-revision-no-save file revision backend)
    (vc-find-revision-save file revision backend)))

(defun vc-find-revision-save (file revision &optional backend)
  "Read REVISION of FILE into a buffer and return the buffer.
Saves the buffer to the file."
  (let ((automatic-backup (vc-version-backup-file-name file revision))
	(filebuf (or (get-file-buffer file) (current-buffer)))
        (filename (vc-version-backup-file-name file revision 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup filename nil)
	(message "Checking out %s..." filename)
	(with-current-buffer filebuf
	  (let ((failed t))
	    (unwind-protect
		(let ((coding-system-for-read 'no-conversion))
		  (with-temp-file filename
		    (let ((outbuf (current-buffer)))
                      ;; We will read the backend's output with no
                      ;; conversions, so we should also save the
                      ;; temporary file with no encoding conversions.
                      (setq buffer-file-coding-system 'no-conversion)
		      ;; Change buffer to get local value of
		      ;; vc-checkout-switches.
		      (with-current-buffer filebuf
			(if backend
			    (vc-call-backend backend 'find-revision file revision outbuf)
			  (vc-call find-revision file revision outbuf)))))
		  (setq failed nil))
	      (when (and failed (file-exists-p filename))
		(delete-file filename))))
	  (vc-mode-line file))
	(message "Checking out %s...done" filename)))
    (let ((result-buf (find-file-noselect filename)))
      (with-current-buffer result-buf
	;; Set the parent buffer so that things like
	;; C-x v g, C-x v l, ... etc work.
        (setq-local vc-parent-buffer filebuf))
      result-buf)))

(defun vc-find-revision-no-save (file revision &optional backend buffer)
  "Read REVISION of FILE into BUFFER and return the buffer.
If BUFFER omitted or nil, this function creates a new buffer and sets
`buffer-file-name' to the name constructed from the file name and the
revision number.
Unlike `vc-find-revision-save', doesn't save the buffer to the file."
  (let* ((buffer (when (buffer-live-p buffer) buffer))
         (filebuf (or buffer (get-file-buffer file) (current-buffer)))
         (filename (unless buffer (vc-version-backup-file-name file revision 'manual))))
    (unless (and (not buffer)
                 (or (get-file-buffer filename)
                     (file-exists-p filename)))
      (with-current-buffer filebuf
	(let ((failed t))
	  (unwind-protect
	      (with-current-buffer (or buffer (create-file-buffer filename))
                (unless buffer (setq buffer-file-name filename))
		(let ((outbuf (current-buffer)))
		  (with-current-buffer filebuf
		    (if backend
			(vc-call-backend backend 'find-revision file revision outbuf)
		      (vc-call find-revision file revision outbuf))))
                (decode-coding-inserted-region (point-min) (point-max) file)
                (after-insert-file-set-coding (- (point-max) (point-min)))
                (goto-char (point-min))
                (if buffer
                    ;; For non-interactive, skip any questions
                    (let ((enable-local-variables :safe) ;; to find `mode:'
                          (buffer-file-name file))
                      ;; Don't run hooks that might assume buffer-file-name
                      ;; really associates buffer with a file (bug#39190).
                      (ignore-errors (delay-mode-hooks (set-auto-mode))))
                  (normal-mode))
	        (set-buffer-modified-p nil)
                (setq buffer-read-only t))
		(setq failed nil)
	    (when (and failed (unless buffer (get-file-buffer filename)))
	      (with-current-buffer (get-file-buffer filename)
		(set-buffer-modified-p nil))
	      (kill-buffer (get-file-buffer filename)))))))
    (let ((result-buf (or buffer
                          (get-file-buffer filename)
                          (find-file-noselect filename))))
      (with-current-buffer result-buf
        (setq-local vc-parent-buffer filebuf))
      result-buf)))

;; Header-insertion code

;;;###autoload
(defun vc-insert-headers ()
  "Insert headers into a file for use with a version control system.
Headers desired are inserted at point, and are pulled from
the variable `vc-BACKEND-header'."
  (interactive)
  (vc-ensure-vc-buffer)
  (save-excursion
    (save-restriction
      (widen)
      (when (or (not (vc-check-headers))
		(y-or-n-p "Version headers already exist.  Insert another set? "))
	(let* ((delims (cdr (assq major-mode vc-comment-alist)))
	       (comment-start-vc (or (car delims) comment-start "#"))
	       (comment-end-vc (or (car (cdr delims)) comment-end ""))
	       (hdsym (vc-make-backend-sym (vc-backend buffer-file-name)
					   'header))
	       (hdstrings (and (boundp hdsym) (symbol-value hdsym))))
	  (dolist (s hdstrings)
	    (insert comment-start-vc "\t" s "\t"
		    comment-end-vc "\n"))
	  (when vc-static-header-alist
	    (dolist (f vc-static-header-alist)
	      (when (string-match (car f) buffer-file-name)
		(insert (format (cdr f) (car hdstrings)))))))))))

(defun vc-modify-change-comment (files rev oldcomment)
  "Edit the comment associated with the given files and revision."
  ;; Less of a kluge than it looks like; log-view mode only passes
  ;; this function a singleton list.  Arguments left in this form in
  ;; case the more general operation ever becomes meaningful.
  (let ((backend (vc-responsible-backend (car files))))
    (vc-start-logentry
     files oldcomment t
     "Enter a replacement change comment."
     "*vc-log*"
     (lambda () (vc-call-backend backend 'log-edit-mode))
     (lambda (files comment)
       (vc-call-backend backend
                        'modify-change-comment files rev comment)))))

;;;###autoload
(defun vc-merge ()
  "Perform a version control merge operation.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"merge\"
operation to incorporate changes from another branch onto the
current branch, prompting for an argument list.

On a non-distributed version control system, this merges changes
between two revisions into the current fileset.  This asks for
two revisions to merge from in the minibuffer.  If the first
revision is a branch number, then merge all changes from that
branch.  If the first revision is empty, merge the most recent
changes from the current branch."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a branch-merge operation is defined, use it.
     ((vc-find-backend-function backend 'merge-branch)
      (vc-call-backend backend 'merge-branch))
     ;; Otherwise, do a per-file merge.
     ((vc-find-backend-function backend 'merge-file)
      (vc-buffer-sync)
      (dolist (file files)
	(let* ((state (vc-state file))
	       status)
	  (cond
	   ((stringp state)	;; Locking VCses only
	    (error "File %s is locked by %s" file state))
	   ((not (vc-editable-p file))
	    (vc-checkout file t)))
	  (setq status (vc-call-backend backend 'merge-file file))
	  (vc-maybe-resolve-conflicts file status "WORKFILE" "MERGE SOURCE"))))
     (t
      (error "Sorry, merging is not implemented for %s" backend)))))

(defun vc-maybe-resolve-conflicts (file status &optional _name-A _name-B)
  (vc-resynch-buffer file t (not (buffer-modified-p)))
  (if (zerop status) (message "Merge successful")
    (smerge-mode 1)
    (message "File contains conflicts.")))

;;;###autoload
(defun vc-message-unresolved-conflicts (filename)
  "Display a message indicating unresolved conflicts in FILENAME."
  ;; This enables all VC backends to give a standard, recognizable
  ;; conflict message that indicates which file is conflicted.
  (message "There are unresolved conflicts in %s" filename))

;;;###autoload
(defalias 'vc-resolve-conflicts 'smerge-ediff)

;; TODO: This is OK but maybe we could integrate it better.
;; E.g. it could be run semi-automatically (via a prompt?) when saving a file
;; that was conflicted (i.e. upon mark-resolved).
;; FIXME: should we add an "other-window" version?  Or maybe we should
;; hook it inside find-file so it automatically works for
;; find-file-other-window as well.  E.g. find-file could use a new
;; `default-next-file' variable for its default file (M-n), and
;; we could then set it upon mark-resolve, so C-x C-s C-x C-f M-n would
;; automatically offer the next conflicted file.
;;;###autoload
(defun vc-find-conflicted-file ()
  "Visit the next conflicted file in the current project."
  (interactive)
  (let* ((backend (or (if buffer-file-name (vc-backend buffer-file-name))
                      (vc-responsible-backend default-directory)
                      (error "No VC backend")))
         (root (vc-root-dir))
         (files (vc-call-backend backend
                                 'conflicted-files (or root default-directory))))
    ;; Don't try and visit the current file.
    (if (equal (car files) buffer-file-name) (pop files))
    (if (null files)
        (message "No more conflicted files")
      (find-file (pop files))
      (message "%s more conflicted files after this one"
               (if files (length files) "No")))))

;; Named-configuration entry points

(defun vc-tag-precondition (dir)
  "Scan the tree below DIR, looking for files not up-to-date.
If any file is not up-to-date, return the name of the first such file.
\(This means, neither tag creation nor retrieval is allowed.)
If one or more of the files are currently visited, return `visited'.
Otherwise, return nil."
  (let ((status nil))
    (catch 'vc-locked-example
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (if (not (vc-up-to-date-p f)) (throw 'vc-locked-example f)
	   (when (get-file-buffer f) (setq status 'visited)))))
      status)))

;;;###autoload
(defun vc-create-tag (dir name branchp)
  "Descending recursively from DIR, make a tag called NAME.
For each registered file, the working revision becomes part of
the named configuration.  If the prefix argument BRANCHP is
given, the tag is made as a new branch and the files are
checked out in that new branch."
  (interactive
   (let ((granularity
	  (vc-call-backend (vc-responsible-backend default-directory)
			   'revision-granularity)))
     (list
      (if (eq granularity 'repository)
	  ;; For VC's that do not work at file level, it's pointless
	  ;; to ask for a directory, branches are created at repository level.
	  default-directory
	(read-directory-name "Directory: " default-directory default-directory t))
      (read-string (if current-prefix-arg "New branch name: " "New tag name: ")
                   nil 'vc-revision-history)
      current-prefix-arg)))
  (message "Making %s... " (if branchp "branch" "tag"))
  (when (file-directory-p dir) (setq dir (file-name-as-directory dir)))
  (vc-call-backend (vc-responsible-backend dir)
		   'create-tag dir name branchp)
  (vc-resynch-buffer dir t t t)
  (message "Making %s... done" (if branchp "branch" "tag")))

;;;###autoload
(defun vc-retrieve-tag (dir name)
  "For each file in or below DIR, retrieve their tagged version NAME.
NAME can name a branch, in which case this command will switch to the
named branch in the directory DIR.
Interactively, prompt for DIR only for VCS that works at file level;
otherwise use the repository root of the current buffer.
If NAME is empty, it refers to the latest revisions of the current branch.
If locking is used for the files in DIR, then there must not be any
locked files at or below DIR (but if NAME is empty, locked files are
allowed and simply skipped).
This function runs the hook `vc-retrieve-tag-hook' when finished."
  (interactive
   (let* ((granularity
           (vc-call-backend (vc-responsible-backend default-directory)
                            'revision-granularity))
          (dir
           (if (eq granularity 'repository)
               ;; For VC's that do not work at file level, it's pointless
               ;; to ask for a directory, branches are created at repository level.
               ;; XXX: Either we call expand-file-name here, or use
               ;; file-in-directory-p inside vc-resynch-buffers-in-directory.
               (expand-file-name (vc-root-dir))
             (read-directory-name "Directory: " default-directory nil t))))
     (list
      dir
      (vc-read-revision (format-prompt "Tag name to retrieve" "latest revisions")
                        (list dir)
                        (vc-responsible-backend dir)))))
  (let* ((backend (vc-responsible-backend dir))
         (update (when (vc-call-backend backend 'update-on-retrieve-tag)
                   (yes-or-no-p "Update any affected buffers? ")))
	 (msg (if (or (not name) (string= name ""))
		  (format "Updating %s... " (abbreviate-file-name dir))
	        (format "Retrieving tag %s into %s... "
		        name (abbreviate-file-name dir)))))
    (message "%s" msg)
    (vc-call-backend backend 'retrieve-tag dir name update)
    (vc-resynch-buffer dir t t t)
    (run-hooks 'vc-retrieve-tag-hook)
    (message "%s" (concat msg "done"))))


;; Miscellaneous other entry points

;; FIXME: this should be a defcustom
;; FIXME: maybe add another choice:
;; `root-directory' (or somesuch), which would mean show a short log
;; for the root directory.
(defvar vc-log-short-style '(directory)
  "Whether or not to show a short log.
If it contains `directory', show a short log if the fileset
contains a directory.
If it contains `file', show short logs for files.
Not all VC backends support short logs!")

(defvar log-view-vc-fileset)
(defvar log-view-message-re)

(defun vc-print-log-setup-buttons (working-revision is-start-revision limit pl-return)
  "Insert at the end of the current buffer buttons to show more log entries.
In the new log, leave point at WORKING-REVISION (if non-nil).
LIMIT is the number of entries currently shown.
Does nothing if IS-START-REVISION is non-nil, or if LIMIT is nil,
or if PL-RETURN is `limit-unsupported'."
  (when (and limit (not (eq 'limit-unsupported pl-return))
	     (not is-start-revision))
    (let ((entries 0))
      (goto-char (point-min))
      (while (re-search-forward log-view-message-re nil t)
        (cl-incf entries))
      ;; If we got fewer entries than we asked for, then displaying
      ;; the "more" buttons isn't useful.
      (when (>= entries limit)
        (goto-char (point-max))
        (insert "\n")
        (insert-text-button
         "Show 2X entries"
         'action (lambda (&rest _ignore)
                   (vc-print-log-internal
                    log-view-vc-backend log-view-vc-fileset
                    working-revision nil (* 2 limit)))
         'help-echo
         "Show the log again, and double the number of log entries shown")
        (insert "    ")
        (insert-text-button
         "Show unlimited entries"
         'action (lambda (&rest _ignore)
                   (vc-print-log-internal
                    log-view-vc-backend log-view-vc-fileset
                    working-revision nil nil))
         'help-echo "Show the log again, including all entries")
        (insert "\n")))))

(defun vc-print-log-internal (backend files working-revision
                                      &optional is-start-revision limit type)
  "For specified BACKEND and FILES, show the VC log.
Leave point at WORKING-REVISION, if it is non-nil.
If IS-START-REVISION is non-nil, start the log from WORKING-REVISION
\(not all backends support this); i.e., show only WORKING-REVISION and
earlier revisions.  Show up to LIMIT entries (non-nil means unlimited)."
  ;; As of 2013/04 the only thing that passes IS-START-REVISION non-nil
  ;; is vc-annotate-show-log-revision-at-line, which sets LIMIT = 1.

  ;; Don't switch to the output buffer before running the command,
  ;; so that any buffer-local settings in the vc-controlled
  ;; buffer can be accessed by the command.
  (let* ((dir-present (cl-some #'file-directory-p files))
         (shortlog (not (null (memq (if dir-present 'directory 'file)
                                    vc-log-short-style))))
	(buffer-name "*vc-change-log*")
         (type (or type (if shortlog 'short 'long))))
      (vc-log-internal-common
       backend buffer-name files type
       (lambda (bk buf _type-arg files-arg)
	 (vc-call-backend bk 'print-log files-arg buf shortlog
                          (when is-start-revision working-revision) limit))
       (lambda (_bk _files-arg ret)
         (save-excursion
           (vc-print-log-setup-buttons working-revision
                                       is-start-revision limit ret)))
       ;; When it's nil, point really shouldn't move (bug#15322).
       (when working-revision
         (lambda (bk)
           (vc-call-backend bk 'show-log-entry working-revision)))
       (lambda (_ignore-auto _noconfirm)
	 (vc-print-log-internal backend files working-revision
                              is-start-revision limit type)))))

(defvar vc-log-view-type nil
  "Set this to differentiate the different types of logs.")
(put 'vc-log-view-type 'permanent-local t)
(defvar vc-sentinel-movepoint)

(defvar vc-log-finish-functions '(vc-shrink-buffer-window)
  "Functions run at the end of the log command.
Each function runs in the log output buffer without args.")

(defun vc-log-internal-common (backend
			       buffer-name
			       files
			       type
			       backend-func
			       setup-buttons-func
			       goto-location-func
			       rev-buff-func)
  (let (retval (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq-local vc-log-view-type type))
    (setq retval (funcall backend-func backend buffer-name type files))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	;; log-view-mode used to be called with inhibit-read-only bound
	;; to t, so let's keep doing it, just in case.
	(vc-call-backend backend
                         (if (and (eq type 'with-diff)
                                  (vc-find-backend-function
                                   backend 'region-history-mode))
                             'region-history-mode
                           'log-view-mode))
        (setq-local log-view-vc-backend backend)
        (setq-local log-view-vc-fileset files)
        (setq-local revert-buffer-function rev-buff-func)))
    ;; Display after setting up major-mode, so display-buffer-alist can know
    ;; the major-mode.
    (pop-to-buffer buffer)
    (vc-run-delayed
     (let ((inhibit-read-only t))
       (funcall setup-buttons-func backend files retval)
       (when goto-location-func
         (funcall goto-location-func backend)
         (setq vc-sentinel-movepoint (point)))
       (set-buffer-modified-p nil)
       (run-hooks 'vc-log-finish-functions)))))

(defun vc-incoming-outgoing-internal (backend remote-location buffer-name type)
  (vc-log-internal-common
   backend buffer-name nil type
   (lambda (bk buf type-arg _files)
     (vc-call-backend bk type-arg buf remote-location))
   (lambda (_bk _files-arg _ret) nil)
   nil ;; Don't move point.
   (lambda (_ignore-auto _noconfirm)
     (vc-incoming-outgoing-internal backend remote-location buffer-name type))))

;;;###autoload
(defun vc-print-log (&optional working-revision limit)
  "List the change log of the current fileset in a window.
If WORKING-REVISION is non-nil, leave point at that revision.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.

When called interactively with a prefix argument, prompt for
WORKING-REVISION and LIMIT."
  (interactive
   (cond
    (current-prefix-arg
     (let ((rev (read-from-minibuffer "Leave point at revision (default: last revision): " nil
				      nil nil nil))
	   (lim (string-to-number
		 (read-from-minibuffer
		  "Limit display (unlimited: 0): "
		  (format "%s" vc-log-show-limit)
		  nil nil nil))))
       (when (string= rev "") (setq rev nil))
       (when (<= lim 0) (setq lim nil))
       (list rev lim)))
    (t
     (list nil (when (> vc-log-show-limit 0) vc-log-show-limit)))))
  (let* ((vc-fileset (vc-deduce-fileset t)) ;FIXME: Why t? --Stef
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset))
;;	 (working-revision (or working-revision (vc-working-revision (car files))))
         )
    (vc-print-log-internal backend files working-revision nil limit)))

;;;###autoload
(defun vc-print-root-log (&optional limit revision)
  "List the revision history for the current VC controlled tree in a window.
If LIMIT is non-nil, it should be a number specifying the maximum
number of revisions to show; the default is `vc-log-show-limit'.
When called interactively with a prefix argument, prompt for LIMIT.
When the prefix argument is a number, use it as LIMIT.
A special case is when the prefix argument is 1: in this case
the command asks for the ID of a revision, and shows that revision
with its diffs (if the underlying VCS supports that)."
  (interactive
   (cond
    ((eq current-prefix-arg 1)
     (let* ((default (thing-at-point 'word t))
	    (revision (read-string (format-prompt "Revision to show" default)
		                   nil nil default)))
       (list 1 revision)))
    ((numberp current-prefix-arg)
     (list current-prefix-arg))
    (current-prefix-arg
     (let ((lim (string-to-number
		 (read-from-minibuffer
		  "Limit display (unlimited: 0): "
		  (format "%s" vc-log-show-limit)
		  nil nil nil))))
       (when (<= lim 0) (setq lim nil))
       (list lim)))
    (t
     (list (when (> vc-log-show-limit 0) vc-log-show-limit)))))
  (let* ((backend (vc-deduce-backend))
	 (default-directory default-directory)
	 (with-diff (and (eq limit 1) revision))
	 (vc-log-short-style (unless with-diff vc-log-short-style))
	 rootdir)
    (if backend
	(setq rootdir (vc-call-backend backend 'root default-directory))
      (setq rootdir (read-directory-name "Directory for VC revision log: "))
      (setq backend (vc-responsible-backend rootdir))
      (unless backend
        (error "Directory is not version controlled")))
    (setq default-directory rootdir)
    (vc-print-log-internal backend (list rootdir) revision revision limit
                           (when with-diff 'with-diff))
    ;; We're looking at the root, so displaying " from <some-file>" in
    ;; the mode line isn't helpful.
    (setq vc-parent-buffer-name nil)))

;;;###autoload
(defun vc-print-branch-log (branch)
  "Show the change log for BRANCH root in a window."
  (interactive
   (list
    (vc-read-revision "Branch to log: ")))
  (when (equal branch "")
    (error "No branch specified"))
  (let* ((backend (vc-responsible-backend default-directory))
         (rootdir (vc-call-backend backend 'root default-directory)))
    (vc-print-log-internal backend
                           (list rootdir) branch t
                           (when (> vc-log-show-limit 0) vc-log-show-limit))))

;;;###autoload
(defun vc-log-incoming (&optional remote-location)
  "Show log of changes that will be received with pull from REMOTE-LOCATION.
When called interactively with a prefix argument, prompt for REMOTE-LOCATION."
  (interactive
   (when current-prefix-arg
     (list (read-string "Remote location (empty for default): "))))
  (let ((backend (vc-deduce-backend)))
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend (or remote-location "")
                                   "*vc-incoming*" 'log-incoming)))

;;;###autoload
(defun vc-log-outgoing (&optional remote-location)
  "Show log of changes that will be sent with a push operation to REMOTE-LOCATION.
When called interactively with a prefix argument, prompt for REMOTE-LOCATION."
  (interactive
   (when current-prefix-arg
     (list (read-string "Remote location (empty for default): "))))
  (let ((backend (vc-deduce-backend)))
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend (or remote-location "")
                                   "*vc-outgoing*" 'log-outgoing)))

;;;###autoload
(defun vc-log-search (pattern)
  "Search the log of changes for PATTERN.

PATTERN is usually interpreted as a regular expression.  However, its
exact semantics is up to the backend's log search command; some can
only match fixed strings.

Display all entries that match log messages in long format.
With a prefix argument, ask for a command to run that will output
log entries."
  (interactive (list (unless current-prefix-arg
                       (read-regexp "Search log with pattern: "))))
  (let ((backend (vc-deduce-backend)))
    (unless backend
      (error "Buffer is not version controlled"))
    (vc-incoming-outgoing-internal backend pattern
                                   "*vc-search-log*" 'log-search)))

;;;###autoload
(defun vc-log-mergebase (_files rev1 rev2)
  "Show a log of changes between the merge base of REV1 and REV2 revisions.
The merge base is a common ancestor between REV1 and REV2 revisions."
  (interactive
   (vc-diff-build-argument-list-internal
    (or (ignore-errors (vc-deduce-fileset t))
        (let ((backend (or (vc-deduce-backend) (vc-responsible-backend default-directory))))
          (list backend (list (vc-call-backend backend 'root default-directory)))))))
  (let ((backend (vc-deduce-backend))
	(default-directory default-directory)
	rootdir)
    (if backend
	(setq rootdir (vc-call-backend backend 'root default-directory))
      (setq rootdir (read-directory-name "Directory for VC root-log: "))
      (setq backend (vc-responsible-backend rootdir))
      (unless backend
        (error "Directory is not version controlled")))
    (setq default-directory rootdir)
    (setq rev1 (vc-call-backend backend 'mergebase rev1 rev2))
    (vc-print-log-internal backend (list rootdir) rev1 t (or rev2 ""))))

;;;###autoload
(defun vc-region-history (from to)
  "Show the history of the region between FROM and TO.

If called interactively, show the history between point and
mark."
  (interactive "r")
  (let* ((lfrom (line-number-at-pos from t))
         (lto   (line-number-at-pos (1- to) t))
         (file buffer-file-name)
         (backend (vc-backend file))
         (buf (get-buffer-create "*VC-history*")))
    (unless backend
      (error "Buffer is not version controlled"))
    (with-current-buffer buf
      (setq-local vc-log-view-type 'long))
    (vc-call region-history file buf lfrom lto)
    (with-current-buffer buf
      (vc-call-backend backend 'region-history-mode)
      (setq-local log-view-vc-backend backend)
      (setq-local log-view-vc-fileset (list file))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (with-current-buffer buf
                      (let ((inhibit-read-only t)) (erase-buffer)))
                    (vc-call region-history file buf lfrom lto))))
    (display-buffer buf)))

;;;###autoload
(defun vc-revert ()
  "Revert working copies of the selected fileset to their repository contents.
This asks for confirmation if the buffer contents are not identical
to the working revision (except for keyword expansion)."
  (interactive)
  (let* ((vc-fileset (vc-deduce-fileset))
	 (files (cadr vc-fileset))
	 (queried nil)
	 diff-buffer)
    ;; If any of the files is visited by the current buffer, make sure
    ;; buffer is saved.  If the user says `no', abort since we cannot
    ;; show the changes and ask for confirmation to discard them.
    (when (or (not files) (memq (buffer-file-name) files))
      (vc-buffer-sync nil))
    (dolist (file files)
      (let ((buf (get-file-buffer file)))
	(when (and buf (buffer-modified-p buf))
	  (error "Please kill or save all modified buffers before reverting")))
      (when (vc-up-to-date-p file)
	(if (yes-or-no-p (format "%s seems up-to-date.  Revert anyway? " file))
	    (setq queried t)
	  (error "Revert canceled"))))
    (unwind-protect
	(when (if vc-revert-show-diff
		  (progn
		    (setq diff-buffer (generate-new-buffer "*vc-diff*"))
		    (vc-diff-internal vc-allow-async-revert vc-fileset
				      nil nil nil diff-buffer))
		;; Avoid querying the user again.
		(null queried))
	  (unless (yes-or-no-p
		   (format "Discard changes in %s? "
			   (let ((str (vc-delistify files))
				 (nfiles (length files)))
			     (if (< (length str) 50)
				 str
			       (format "%d file%s" nfiles
				       (if (= nfiles 1) "" "s"))))))
	    (error "Revert canceled")))
      (when diff-buffer
	(quit-windows-on diff-buffer (eq vc-revert-show-diff 'kill))))
    (dolist (file files)
      (message "Reverting %s..." (vc-delistify files))
      (vc-revert-file file)
      (message "Reverting %s...done" (vc-delistify files)))))

;;;###autoload
(defun vc-pull (&optional arg)
  "Update the current fileset or branch.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"pull\"
operation to update the current branch, prompting for an argument
list if required.  Optional prefix ARG forces a prompt for the VCS
command to run.

On a non-distributed version control system, update the current
fileset to the tip revisions.  For each unchanged and unlocked
file, this simply replaces the work file with the latest revision
on its branch.  If the file contains changes, any changes in the
tip revision are merged into the working file."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset))
	 (files (cadr vc-fileset)))
    (cond
     ;; If a pull operation is defined, use it.
     ((vc-find-backend-function backend 'pull)
      (vc-call-backend backend 'pull arg))
     ;; If VCS has `merge-news' functionality (CVS and SVN), use it.
     ((vc-find-backend-function backend 'merge-news)
      (save-some-buffers ; save buffers visiting files
       nil (lambda ()
	     (and (buffer-modified-p)
		  (let ((file (buffer-file-name)))
		    (and file (member file files))))))
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file t)
	  (vc-maybe-resolve-conflicts
	   file (vc-call-backend backend 'merge-news file)))))
     ;; For a locking VCS, check out each file.
     ((eq (vc-checkout-model backend files) 'locking)
      (dolist (file files)
	(if (vc-up-to-date-p file)
	    (vc-checkout file t))))
     (t
      (error "VC update is unsupported for `%s'" backend)))))

;;;###autoload
(defalias 'vc-update 'vc-pull)

;;;###autoload
(defun vc-push (&optional arg)
  "Push the current branch.
You must be visiting a version controlled file, or in a `vc-dir' buffer.
On a distributed version control system, this runs a \"push\"
operation on the current branch, prompting for the precise command
if required.  Optional prefix ARG non-nil forces a prompt for the
VCS command to run.

On a non-distributed version control system, this signals an error.
It also signals an error in a Bazaar bound branch."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset t))
	 (backend (car vc-fileset)))
;;;	 (files (cadr vc-fileset)))
    (if (vc-find-backend-function backend 'push)
        (vc-call-backend backend 'push arg)
      (user-error "VC push is unsupported for `%s'" backend))))

(defun vc-version-backup-file (file &optional rev)
  "Return name of backup file for revision REV of FILE.
If version backups should be used for FILE, and there exists
such a backup for REV or the working revision of file, return
its name; otherwise return nil."
  (when (vc-call make-version-backups-p file)
    (let ((backup-file (vc-version-backup-file-name file rev)))
      (if (file-exists-p backup-file)
          backup-file
        ;; there is no automatic backup, but maybe the user made one manually
        (setq backup-file (vc-version-backup-file-name file rev 'manual))
        (when (file-exists-p backup-file)
	  backup-file)))))

(defun vc-revert-file (file)
  "Revert FILE back to the repository working revision it was based on."
  (with-vc-properties
   (list file)
   (let ((backup-file (vc-version-backup-file file)))
     (when backup-file
       (copy-file backup-file file 'ok-if-already-exists)
       (vc-delete-automatic-version-backups file))
     (vc-call revert file backup-file))
   `((vc-state . up-to-date)
     (vc-checkout-time . ,(file-attribute-modification-time
			   (file-attributes file)))))
  (vc-resynch-buffer file t t))

;;;###autoload
(defun vc-switch-backend (file backend)
  "Make BACKEND the current version control system for FILE.
FILE must already be registered in BACKEND.  The change is not
permanent, only for the current session.  This function only changes
VC's perspective on FILE, it does not register or unregister it.
By default, this command cycles through the registered backends.
To get a prompt, use a prefix argument."
  (declare (obsolete nil "28.1"))
  (interactive
   (list
    (or buffer-file-name
        (error "There is no version-controlled file in this buffer"))
    (let ((crt-bk (vc-backend buffer-file-name))
	  (backends nil))
      (unless crt-bk
        (error "File %s is not under version control" buffer-file-name))
      ;; Find the registered backends.
      (dolist (crt vc-handled-backends)
	(when (and (vc-call-backend crt 'registered buffer-file-name)
		   (not (eq crt-bk crt)))
	  (push crt backends)))
      ;; Find the next backend.
      (let ((def (car backends))
	    (others backends))
	(cond
	 ((null others) (error "No other backend to switch to"))
	 (current-prefix-arg
          (vc-read-backend "Switch to backend: " backends (symbol-name def)))
	 (t def))))))
  (unless (eq backend (vc-backend file))
    (vc-file-clearprops file)
    (vc-file-setprop file 'vc-backend backend)
    ;; Force recomputation of the state
    (unless (vc-call-backend backend 'registered file)
      (vc-file-clearprops file)
      (error "%s is not registered in %s" file backend))
    (vc-mode-line file)))

;;;###autoload
(defun vc-transfer-file (file new-backend)
  "Transfer FILE to another version control system NEW-BACKEND.
If NEW-BACKEND has a higher precedence than FILE's current backend
\(i.e.  it comes earlier in `vc-handled-backends'), then register FILE in
NEW-BACKEND, using the revision number from the current backend as the
base level.  If NEW-BACKEND has a lower precedence than the current
backend, then commit all changes that were made under the current
backend to NEW-BACKEND, and unregister FILE from the current backend.
\(If FILE is not yet registered under NEW-BACKEND, register it.)"
  (let* ((old-backend (vc-backend file))
	 (edited (memq (vc-state file) '(edited needs-merge)))
	 (registered (vc-call-backend new-backend 'registered file))
	 (move
	  (and registered    ; Never move if not registered in new-backend yet.
	       ;; move if new-backend comes later in vc-handled-backends
	       (or (memq new-backend (memq old-backend vc-handled-backends))
		   (y-or-n-p "Final transfer? "))))
	 (comment nil))
    (when (eq old-backend new-backend)
      (error "%s is the current backend of %s" new-backend file))
    (if registered
	(set-file-modes file (logior (file-modes file) 128))
      ;; `registered' might have switched under us.
      (with-suppressed-warnings ((obsolete vc-switch-backend))
        (vc-switch-backend file old-backend))
      (let* ((rev (vc-working-revision file))
	     (modified-file (and edited (make-temp-file file)))
	     (unmodified-file (and modified-file (vc-version-backup-file file))))
	;; Go back to the base unmodified file.
	(unwind-protect
	    (progn
	      (when modified-file
		(copy-file file modified-file 'ok-if-already-exists)
		;; If we have a local copy of the unmodified file, handle that
		;; here and not in vc-revert-file because we don't want to
		;; delete that copy -- it is still useful for OLD-BACKEND.
		(if unmodified-file
		    (copy-file unmodified-file file
			       'ok-if-already-exists 'keep-date)
		  (when (y-or-n-p "Get base revision from repository? ")
		    (vc-revert-file file))))
	      (vc-call-backend new-backend 'receive-file file rev))
	  (when modified-file
            (with-suppressed-warnings ((obsolete vc-switch-backend))
              (vc-switch-backend file new-backend))
	    (unless (eq (vc-checkout-model new-backend (list file)) 'implicit)
	      (vc-checkout file))
	    (rename-file modified-file file 'ok-if-already-exists)
	    (vc-file-setprop file 'vc-checkout-time nil)))))
    (when move
      (with-suppressed-warnings ((obsolete vc-switch-backend))
        (vc-switch-backend file old-backend))
      (setq comment (vc-call-backend old-backend 'comment-history file))
      (vc-call-backend old-backend 'unregister file))
    (with-suppressed-warnings ((obsolete vc-switch-backend))
      (vc-switch-backend file new-backend))
    (when (or move edited)
      (vc-file-setprop file 'vc-state 'edited)
      (vc-mode-line file new-backend)
      (vc-checkin file new-backend comment (stringp comment)))))

;;;###autoload
(defun vc-delete-file (file)
  "Delete file and mark it as such in the version control system.
If called interactively, read FILE, defaulting to the current
buffer's file name if it's under version control."
  (interactive (list (read-file-name "VC delete file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name)
                                     t)))
  (setq file (expand-file-name file))
  (let ((buf (get-file-buffer file))
        (backend (vc-backend file)))
    (unless backend
      (error "File %s is not under version control"
             (file-name-nondirectory file)))
    (unless (vc-find-backend-function backend 'delete-file)
      (error "Deleting files under %s is not supported in VC" backend))
    (when (and buf (buffer-modified-p buf))
      (error "Please save or undo your changes before deleting %s" file))
    (let ((state (vc-state file)))
      (when (eq state 'edited)
        (error "Please commit or undo your changes before deleting %s" file))
      (when (eq state 'conflict)
        (error "Please resolve the conflicts before deleting %s" file)))
    (unless (y-or-n-p (format "Really want to delete %s? "
			      (file-name-nondirectory file)))
      (error "Abort!"))
    (unless (or (file-directory-p file) (null make-backup-files)
                (not (file-exists-p file)))
      (with-current-buffer (or buf (find-file-noselect file))
	(let ((backup-inhibited nil))
	  (backup-buffer))))
    ;; Bind `default-directory' so that the command that the backend
    ;; runs to remove the file is invoked in the correct context.
    (let ((default-directory (file-name-directory file)))
      (vc-call-backend backend 'delete-file file))
    ;; If the backend hasn't deleted the file itself, let's do it for him.
    (when (file-exists-p file) (delete-file file))
    ;; Forget what VC knew about the file.
    (vc-file-clearprops file)
    ;; Make sure the buffer is deleted and the *vc-dir* buffers are
    ;; updated after this.
    (vc-resynch-buffer file nil t)))

;;;###autoload
(defun vc-rename-file (old new)
  "Rename file OLD to NEW in both work area and repository.
If called interactively, read OLD and NEW, defaulting OLD to the
current buffer's file name if it's under version control."
  (interactive (list (read-file-name "VC rename file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name) t)
                     (read-file-name "Rename to: ")))
  ;; in CL I would have said (setq new (merge-pathnames new old))
  (let ((old-base (file-name-nondirectory old)))
    (when (and (not (string= "" old-base))
               (string= "" (file-name-nondirectory new)))
      (setq new (concat new old-base))))
  (let ((oldbuf (get-file-buffer old)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (error "Please save files before moving them"))
    (when (get-file-buffer new)
      (error "Already editing new file name"))
    (when (file-exists-p new)
      (error "New file already exists"))
    (let ((state (vc-state old)))
      (unless (memq state '(up-to-date edited added))
	(error "Please %s files before moving them"
	       (if (stringp state) "check in" "update"))))
    (vc-call rename-file old new)
    (vc-file-clearprops old)
    (vc-file-clearprops new)
    ;; Move the actual file (unless the backend did it already)
    (when (file-exists-p old) (rename-file old new))
    ;; ?? Renaming a file might change its contents due to keyword expansion.
    ;; We should really check out a new copy if the old copy was precisely equal
    ;; to some checked-in revision.  However, testing for this is tricky....
    (when oldbuf
      (with-current-buffer oldbuf
	(let ((buffer-read-only buffer-read-only))
	  (set-visited-file-name new))
	(vc-mode-line new (vc-backend new))
	(set-buffer-modified-p nil)))))

;;;###autoload
(defun vc-update-change-log (&rest args)
  "Find change log file and add entries from recent version control logs.
Normally, find log entries for all registered files in the default
directory.

With prefix arg of \\[universal-argument], only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any ARGS are assumed to be filenames for which
log entries should be gathered."
  (interactive
   (cond ((consp current-prefix-arg)	;C-u
	  (list buffer-file-name))
	 (current-prefix-arg		;Numeric argument.
	  (let ((files nil))
            (dolist (buffer (buffer-list))
	      (let ((file (buffer-file-name buffer)))
                (and file (vc-backend file)
                     (setq files (cons file files)))))
	    files))
	 (t
          ;; Don't supply any filenames to backend; this means
          ;; it should find all relevant files relative to
          ;; the default-directory.
	  nil)))
  (vc-call-backend (vc-responsible-backend default-directory)
                   'update-changelog args))

(defun vc-default-responsible-p (_backend _file)
  "Indicate whether BACKEND is responsible for FILE.
The default is to return nil always."
  nil)

(defun vc-default-find-revision (backend file rev buffer)
  "Provide the new `find-revision' op based on the old `checkout' op.
This is only for compatibility with old backends.  They should be updated
to provide the `find-revision' operation instead."
  (let ((tmpfile (make-temp-file (expand-file-name file))))
    (unwind-protect
	(progn
	  (vc-call-backend backend 'checkout file nil rev tmpfile)
	  (with-current-buffer buffer
	    (insert-file-contents-literally tmpfile)))
      (delete-file tmpfile))))

(defun vc-default-rename-file (_backend old new)
  (condition-case nil
      (add-name-to-file old new)
    (error (rename-file old new)))
  (vc-delete-file old)
  (with-current-buffer (find-file-noselect new)
    (vc-register)))

(defalias 'vc-default-check-headers 'ignore)

(declare-function log-edit-mode "log-edit" ())

(defun vc-default-log-edit-mode (_backend) (log-edit-mode))

(defun vc-default-log-view-mode (_backend) (log-view-mode))

(defun vc-default-show-log-entry (_backend rev)
  (with-no-warnings
   (log-view-goto-rev rev)))

(defun vc-default-comment-history (backend file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (when (vc-find-backend-function backend 'print-log)
    (with-current-buffer "*vc*"
      (vc-call-backend backend 'print-log (list file))
      (buffer-string))))

(defun vc-default-receive-file (backend file rev)
  "Let BACKEND receive FILE from another version control system."
  (vc-call-backend backend 'register (list file) rev ""))

(defun vc-default-update-on-retrieve-tag (_backend)
  "Prompt for update buffers on `vc-retrieve-tag'."
  t)

(defun vc-default-retrieve-tag (backend dir name update)
  (if (string= name "")
      (progn
        (vc-file-tree-walk
         dir
         (lambda (f) (and
		 (vc-up-to-date-p f)
		 (vc-error-occurred
		  (vc-call-backend backend 'checkout f nil "")
		  (when update (vc-resynch-buffer f t t)))))))
    (let ((result (vc-tag-precondition dir)))
      (if (stringp result)
          (error "File %s is locked" result)
        (setq update (and (eq result 'visited) update))
        (vc-file-tree-walk
         dir
         (lambda (f) (vc-error-occurred
		 (vc-call-backend backend 'checkout f nil name)
		 (when update (vc-resynch-buffer f t t)))))))))

(defun vc-default-revert (backend file contents-done)
  (unless contents-done
    (let ((rev (vc-working-revision file))
          (file-buffer (or (get-file-buffer file) (current-buffer))))
      (message "Checking out %s..." file)
      (let ((failed t)
            (backup-name (car (find-backup-file-name file))))
        (when backup-name
          (copy-file file backup-name 'ok-if-already-exists 'keep-date)
          (unless (file-writable-p file)
            (set-file-modes file (logior (file-modes file) 128))))
        (unwind-protect
            (let ((coding-system-for-read 'no-conversion)
                  (coding-system-for-write 'no-conversion))
              (with-temp-file file
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to get local value of vc-checkout-switches.
                  (with-current-buffer file-buffer
                    (let ((default-directory (file-name-directory file)))
                      (vc-call-backend backend 'find-revision
                                       file rev outbuf)))))
              (setq failed nil))
          (when backup-name
            (if failed
                (rename-file backup-name file 'ok-if-already-exists)
              (and (not vc-make-backup-files) (delete-file backup-name))))))
      (message "Checking out %s...done" file))))

(defalias 'vc-default-revision-completion-table 'ignore)
(defalias 'vc-default-mark-resolved 'ignore)

(defun vc-default-dir-status-files (_backend _dir files update-function)
  (funcall update-function
           (mapcar (lambda (file) (list file 'up-to-date)) files)))

(defun vc-check-headers ()
  "Check if the current file has any headers in it."
  (interactive)
  (vc-call-backend (vc-backend buffer-file-name) 'check-headers))



;; These things should probably be generally available
(defun vc-file-tree-walk (dirname func &rest args)
  "Walk recursively through DIRNAME.
Invoke FUNC f ARGS on each VC-managed file f underneath it."
  (vc-file-tree-walk-internal (expand-file-name dirname) func args)
  (message "Traversing directory %s...done" dirname))

(defun vc-file-tree-walk-internal (file func args)
  (if (not (file-directory-p file))
      (when (vc-backend file) (apply func file args))
    (message "Traversing directory %s..." (abbreviate-file-name file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (lambda (f) (or
               (string-equal f ".")
               (string-equal f "..")
               (member f vc-directory-exclusion-list)
               (let ((dirf (expand-file-name f dir)))
                 (or
                  (file-symlink-p dirf) ;; Avoid possible loops.
                  (vc-file-tree-walk-internal dirf func args)))))
       (directory-files dir)))))

(provide 'vc)

;;; vc.el ends here
