;;; unpackaged.el --- Useful yet unsubstantial Emacs Lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience
;; URL: https://github.com/alphapapa/unpackaged.el
;; Package-Requires: ((emacs "25.1") (dash "2.13") (s "1.10.0") (org "9.0") (use-package "2.4"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A collection of useful Emacs Lisp code that isn't substantial
;; enough to be packaged.  This code will be maintained here so that
;; it can be updated and improved over time.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)

(require 'dash)
(require 's)
(require 'use-package)

;;; Faces, fonts

(defvar lorem-ipsum-text)

(defun unpackaged/font-compare (text fonts)
  "Compare TEXT displayed in FONTS.
If TEXT is nil, use `lorem-ipsum' text.  FONTS is a list of font
family strings and/or font specs.

Interactively, prompt for TEXT, using `lorem-ipsum' if left
empty, and select FONTS with `x-select-font' (select an
already-selected font to end font selection)."
  (interactive (list (pcase (read-string "Text: ")
                       ("" nil)
                       (else else))
                     (cl-loop for font = (x-select-font)
                              ;; HACK: `x-select-font' calls quit() when the Cancel button is
                              ;; pressed, so to avoid quit()'ing, we signal in-band by selecting a
                              ;; font that has already been selected.
                              while (not (member font fonts))
                              collect font into fonts
                              finally return fonts)))
  (setq text (or text (s-word-wrap 80 (s-join " " (progn
                                                    (require 'lorem-ipsum)
                                                    (seq-random-elt lorem-ipsum-text))))))
  (with-current-buffer (get-buffer-create "*Font Compare*")
    (erase-buffer)
    (--each fonts
      (let ((family (cl-typecase it
                      (font (symbol-name (font-get it :family)))
                      (string it))))
        (insert family ": "
                (propertize text
                            'face (list :family family))
                "\n\n")))
    (pop-to-buffer (current-buffer))))

;;; ibuffer

(defun unpackaged/ibuffer-toggle-all-filter-groups ()
  "Toggle all filter groups."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ibuffer-forward-filter-group)
    (let ((start (point)))
      (forward-char)
      (while (not (<= (point) start))
        (ibuffer-toggle-filter-group)
        (ibuffer-forward-filter-group)))))

(defun unpackaged/ibuffer-filter-group-move-down ()
  "Move filter group at point down."
  (interactive)
  (unpackaged/ibuffer-filter-group-move 'down))

(defun unpackaged/ibuffer-filter-group-move-up ()
  "Move filter group at point up."
  (interactive)
  (unpackaged/ibuffer-filter-group-move 'up))

(defun unpackaged/ibuffer-filter-group-move (direction)
  "Move filter group at point in DIRECTION, either `up' or `down'."
  (ibuffer-kill-line)
  (pcase-exhaustive direction
    ('down (ibuffer-forward-filter-group))
    ('up (ibuffer-backward-filter-group)))
  (ibuffer-yank))

;;; Misc

(cl-defun unpackaged/mpris-track (&optional player)
  "Return the artist, album, and title of the track playing in MPRIS-supporting player.
Returns a string in format \"ARTIST - ALBUM: TITLE [PLAYER]\".  If no track is
playing, returns nil.  If more than one player is playing, uses
the first one found in DBus.

If PLAYER is non-nil, include the name of the player in the
output string."
  (require 'dbus)
  (when-let* ((mpris-services (--select (string-prefix-p "org.mpris.MediaPlayer2." it)
                                        (dbus-list-known-names :session)))
              (playing-service (--first (string= "Playing"
                                                 (dbus-get-property :session it
                                                                    "/org/mpris/MediaPlayer2"
                                                                    "org.mpris.MediaPlayer2.Player"
                                                                    "PlaybackStatus"))
                                        mpris-services))
              (player-name (dbus-get-property :session playing-service
                                              "/org/mpris/MediaPlayer2"
                                              "org.mpris.MediaPlayer2"
                                              "Identity"))
              (metadata (dbus-get-property :session playing-service
                                           "/org/mpris/MediaPlayer2"
                                           "org.mpris.MediaPlayer2.Player"
                                           "Metadata")))
    ;; `-let' makes it easy to get the actual strings out of the nested lists of lists of strings.
    (-let (((&alist "xesam:artist" ((artists))
                    "xesam:album" ((album))
                    "xesam:title" ((title)))
            metadata))
      (format "%s - %s: %s%s" (s-join ", " artists) album title
              (if player
                  (format " [%s]" player-name)
                "")))))

;;; Org

(defun unpackaged/org-agenda-current-subtree-or-region (only-todos)
  "Display an agenda view for the current subtree or region.
  With prefix, display only TODO-keyword items."
  (interactive "P")
  (let ((starting-point (point))
        header)
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
                             (current-buffer))
      (if (use-region-p)
          (progn
            (setq header "Region")
            (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
            (setq org-agenda-restrict (current-buffer))
            (move-marker org-agenda-restrict-begin (region-beginning))
            (move-marker org-agenda-restrict-end
                         (save-excursion
                           ;; If point is at beginning of line, include
                           ;; heading on that line by moving forward 1.
                           (goto-char (1+ (region-end)))
                           (org-end-of-subtree))))
        ;; No region; restrict to subtree.
        (save-excursion
          (save-restriction
            ;; In case the command was called from an indirect buffer, set point
            ;; in the base buffer to the same position while setting restriction.
            (widen)
            (goto-char starting-point)
            (setq header "Subtree")
            (org-agenda-set-restriction-lock))))
      ;; NOTE: Unlike other agenda commands, binding `org-agenda-sorting-strategy'
      ;; around `org-search-view' seems to have no effect.
      (let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
            (org-agenda-overriding-header header))
        (org-search-view (if only-todos t nil) "*"))
      (org-agenda-remove-restriction-lock t)
      (message nil))))

(defface unpackaged/org-agenda-preview
  '((t (:background "black")))
  "Face for Org Agenda previews."
  :group 'org)

(defun unpackaged/org-agenda-toggle-preview ()
  "Toggle overlay of current item in agenda."
  (interactive)
  (if-let* ((overlay (ov-in 'unpackaged/org-agenda-preview t (line-end-position) (line-end-position))))
      ;; Hide existing preview
      (ov-reset overlay)
    ;; Show preview
    (let* ((entry-contents (--> (org-agenda-with-point-at-orig-entry
                                 nil (buffer-substring (save-excursion
                                                         (unpackaged/org-forward-to-entry-content t)
                                                         (point))
                                                       (org-entry-end-position)))
                                s-trim
                                (concat "\n" it "\n"))))
      (add-face-text-property 0 (length entry-contents)
                              'unpackaged/org-agenda-preview nil entry-contents)
      (ov (line-end-position) (line-end-position)
          'unpackaged/org-agenda-preview t
          'before-string entry-contents))))

(defun unpackaged/org-forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
           for pos = (pcase element
                       (`(headline . ,_) (org-element-property :contents-begin element))
                       (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
           while pos
           do (goto-char pos)))

(defun unpackaged/elisp-to-org ()
  "Convert elisp code in region to Org syntax and put in kill-ring.
Extracts and converts docstring to Org text, and places code in
source block."
  (interactive)
  (let* ((raw (->> (buffer-substring (region-beginning) (region-end))
                   (replace-regexp-in-string (rx bol) "  ")
                   (replace-regexp-in-string (rx bol (1+ blank) eol) "")))
         (sexp (read raw))
         (docstring (--when-let (-first #'stringp sexp)
                      (unpackaged/docstring-to-org it))))
    (kill-new (concat docstring (when docstring "\n\n")
                      "#+BEGIN_SRC elisp" "\n"
                      raw "\n"
                      "#+END_SRC"))))

(defun unpackaged/docstring-to-org (docstring)
  "Return DOCSTRING as formatted Org text.

Interactively, get text from region, and kill formatted Org text
to kill-ring."
  (interactive (list (buffer-substring (region-beginning) (region-end))))
  (cl-macrolet ((string-buffer--> (string &rest forms)
                                  `(with-temp-buffer
                                     (insert ,string)
                                     ,@(cl-loop for form in forms
                                                collect `(goto-char (point-min))
                                                collect form)
                                     (buffer-string))))
    (--> (string-buffer--> docstring
                           (unpackaged/caps-to-code (point-min) (point-max))
                           (unpackaged/symbol-quotes-to-org-code (point-min) (point-max))
                           (unfill-region (point-min) (point-max))
                           (while (re-search-forward (rx bol (group (1+ blank))) nil t)
                             (replace-match "" t t nil 1))
                           (when (looking-at "\"")
                             (delete-char 1))
                           (when (progn
                                   (goto-char (point-max))
                                   (looking-back "\"" nil))
                             (delete-char -1)))
         (if (called-interactively-p 'interactive)
             (progn
               (message it)
               (kill-new it))
           it))))

(defun unpackaged/caps-to-code (beg end)
  "Convert all-caps words in region to Org code emphasis."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward (rx (or space bol)
                                      (group (1+ upper))
                                      (or space eol (char punct)))
                                  nil t)
          (setf (buffer-substring (match-beginning 1) (match-end 1))
                (concat "~" (match-string 1) "~"))
          (goto-char (match-end 0)))))))

(defun unpackaged/symbol-quotes-to-org-code (beg end)
  "Change Emacs `symbol' quotes to Org =symbol= quotes in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (while (re-search-forward (rx (or "`" "‘") (group (1+ (or "-" word))) "'") nil t)
        (replace-match (concat "~" (match-string 1) "~") t)))))

(defun unpackaged/org-attach-download (url)
  "Download file at URL and attach with `org-attach'.
Interactively, look for URL at point, in X clipboard, and in
kill-ring, prompting if not found.  With prefix, prompt for URL."
  (interactive (list (if current-prefix-arg
                         (read-string "URL: ")
                       (or (org-element-property :raw-link (org-element-context))
                           (org-web-tools--get-first-url)
                           (read-string "URL: ")))))
  (when (yes-or-no-p (concat "Attach file at URL: " url))
    (let* ((temp-dir (make-temp-file "org-attach-download-" 'dir))
           (basename (file-name-nondirectory (directory-file-name url)))
           (local-path (expand-file-name basename temp-dir))
           size)
      (unwind-protect
          (progn
            (url-copy-file url local-path 'ok-if-exists 'keep-time)
            (setq size (file-size-human-readable
                        (file-attribute-size
                         (file-attributes local-path))))
            (org-attach-attach local-path nil 'mv)
            (message "Attached %s (%s)" url size))
        (delete-directory temp-dir)))))

(defun unpackaged/org-fix-blank-lines (prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer.  Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers.  You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))

(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (progn
        (advice-add #'org-export-new-title-reference :override #'unpackaged/org-export-new-title-reference)
        (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference))
    (advice-remove #'org-export-new-title-reference #'unpackaged/org-export-new-title-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference
                           (substring-no-properties (org-element-property :raw-value datum))
                           cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (title cache)
  "Return new reference for title that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((ref (url-hexify-string title)))
      (while (--any (equal ref (car it))
                    cache)
        (inc-suffixf ref))
      ref)))

(defmacro unpackaged/def-org-maybe-surround (&rest keys)
  "Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'."
  `(progn
     ,@(cl-loop for key in keys
                for name = (intern (concat "unpackaged/org-maybe-surround-" key))
                for docstring = (format "If region is active, surround it with \"%s\", otherwise call `org-self-insert-command'." key)
                collect `(defun ,name ()
                           ,docstring
                           (interactive)
                           (if (region-active-p)
                               (let ((beg (region-beginning))
                                     (end (region-end)))
                                 (save-excursion
                                   (goto-char end)
                                   (insert ,key)
                                   (goto-char beg)
                                   (insert ,key)))
                             (call-interactively #'org-self-insert-command)))
                collect `(define-key org-mode-map (kbd ,key) #',name))))

(unpackaged/def-org-maybe-surround "~" "=" "*" "/" "+")

(defun unpackaged/org-refile-to-datetree-using-ts-in-entry (which-ts file &optional subtree-p)
  "Refile current entry to datetree in FILE using timestamp found in entry.
WHICH should be `earliest' or `latest'. If SUBTREE-P is non-nil,
search whole subtree."
  (interactive (list (intern (completing-read "Which timestamp? " '(earliest latest)))
                     (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))
                     current-prefix-arg))
  (require 'ts)
  (let* ((sorter (pcase which-ts
                   ('earliest #'ts<)
                   ('latest #'ts>)))
         (tss (unpackaged/org-timestamps-in-entry subtree-p))
         (ts (car (sort tss sorter)))
         (date (list (ts-month ts) (ts-day ts) (ts-year ts))))
    (unpackaged/org-refile-to-datetree file :date date)))

(defun unpackaged/org-timestamps-in-entry (&optional subtree-p)
  "Return timestamp objects for all Org timestamps in entry.
 If SUBTREE-P is non-nil (interactively, with prefix), search
 whole subtree."
  (interactive (list current-prefix-arg))
  (save-excursion
    (let* ((beg (org-entry-beginning-position))
           (end (if subtree-p
                    (org-end-of-subtree)
                  (org-entry-end-position))))
      (goto-char beg)
      (cl-loop while (re-search-forward org-tsr-regexp-both end t)
               for ts = (save-excursion
                          (goto-char (match-beginning 0))
                          (org-element-timestamp-parser))
               collect (ts-parse-org ts)))))

(cl-defun unpackaged/org-refile-to-datetree (file &key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE."
  (interactive (list (read-file-name "File: " (concat org-directory "/") nil 'mustmatch nil
                                     (lambda (filename)
                                       (string-suffix-p ".org" filename)))))
  ;; If org-datetree isn't loaded, it will cut the tree but not file
  ;; it anywhere, losing data. I don't know why
  ;; org-datetree-file-entry-under is in a separate package, not
  ;; loaded with the rest of org-mode.
  (require 'org-datetree)
  (unless entry
    (org-cut-subtree))
  ;; Using a condition-case to be extra careful. In case the refile
  ;; fails in any way, put cut subtree back.
  (condition-case err
      (with-current-buffer (or (org-find-base-buffer-visiting file)
                               (find-file-noselect file))
        (org-datetree-file-entry-under (or entry (car kill-ring)) date)
        (save-buffer))
    (error (unless entry
             (org-paste-subtree))
           (message "Unable to refile! %s" err))))

(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

(defun unpackaged/org-return-dwim (&optional default)
  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
      (org-return)
    (cond
     ;; Act depending on context around point.

     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
     ;; followed.

     ;; ((eq 'link (car (org-element-context)))
     ;;  ;; Link: Open it.
     ;;  (org-open-at-point-global))

     ((org-at-heading-p)
      ;; Heading: Move to position after entry content.
      ;; NOTE: This is probably the most interesting feature of this function.
      (let ((heading-start (org-entry-beginning-position)))
        (goto-char (org-entry-end-position))
        (cond ((and (org-at-heading-p)
                    (= heading-start (org-entry-beginning-position)))
               ;; Entry ends on its heading; add newline after
               (end-of-line)
               (insert "\n\n"))
              (t
               ;; Entry ends after its heading; back up
               (forward-line -1)
               (end-of-line)
               (when (org-at-heading-p)
                 ;; At the same heading
                 (forward-line)
                 (insert "\n")
                 (forward-line -1))
               (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                 (insert "\n"))
               (forward-line -1)))))

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return))
            (t
             ;; Non-empty row: call `org-return'.
             (org-return))))
     (t
      ;; All other cases: call `org-return'.
      (org-return)))))

(add-hook 'org-mode-hook 'unpackaged/org-mark-read-only)

(defun unpackaged/org-next-heading-tagged (tag)
  "Move to beginning of next heading tagged with TAG and return point, or return nil if none found."
  (when (re-search-forward (rx-to-string `(seq bol (1+ "*") (1+ blank) (optional (1+ not-newline) (1+ blank))
                                               ;; Beginning of tags
                                               ":"
                                               ;; Possible other tags
                                               (0+ (seq (1+ (not (any ":" blank))) ":") )
                                               ;; The tag that matters
                                               ,tag ":"))
                           nil 'noerror)
    (goto-char (match-beginning 0))))

(defun unpackaged/org-mark-read-only ()
  "Mark all entries in the buffer tagged \"read_only\" with read-only text properties."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (unpackaged/org-next-heading-tagged "read_only")
     (add-text-properties (point) (org-end-of-subtree t)
                          '(read-only t)))))

(defun unpackaged/org-remove-read-only ()
  "Remove read-only text properties from Org entries tagged \"read_only\" in current buffer."
  (let ((inhibit-read-only t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (unpackaged/org-next-heading-tagged "read_only")
       (remove-text-properties (point) (org-end-of-subtree t)
                               '(read-only t))))))

(defun unpackaged/org-sort-multi (keys)
    "Call `org-sort-entries' with multiple sorting methods specified in KEYS."
    ;; Message copied from `org-sort-entries'.
    (interactive (list (read-string "Sort by: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
         [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
         A/N/P/R/O/F/T/S/D/C/K means reversed: ")))
    (seq-do (lambda (key)
              (org-sort-entries nil key))
            (nreverse keys)))

;;; Packages

(defun unpackaged/package-delete-all-versions (name &optional force)
  "Delete all versions of package named NAME.
NAME may be a string or symbol."
  ;; Copied from `package-delete'.
  (let* ((package-name (cl-typecase name
                         (string (intern name))
                         (symbol name)))
         (user-packages-list (->> package-alist
                                  ;; Just to be safe, we ignore built-ins.
                                  (-select (-not #'package-built-in-p))))
         (matching-versions (--select (eql (car it) package-name) user-packages-list)))
    ;; Safety checks.
    (cl-loop for (symbol first-desc . rest) in matching-versions
             do (progn
                  (unless force
                    (when-let* ((dependent (package--used-elsewhere-p first-desc)))
                      (error "Package `%s' depends on `%s'" (package-desc-name dependent) package-name)))
                  (unless (string-prefix-p (file-name-as-directory (expand-file-name package-user-dir))
                                           (expand-file-name (package-desc-dir first-desc)))
                    (error "Package `%s' is a system package"))))
    ;; Checks passed: delete packages.
    (cl-loop for (symbol . descs) in matching-versions
             do (--each descs
                  (package-delete it force)))))

(defun unpackaged/quelpa-use-package-upgrade ()
  "Eval the current `use-package' form with `quelpa-upgrade-p' true.
Deletes the package first to remove obsolete versions."
  (interactive)
  (save-excursion
    (if (or (looking-at (rx "(use-package "))
            (let ((limit (save-excursion
                           (or (re-search-backward (rx bol "("))
                               (point-min)))))
              ;; Don't go past previous top-level form
              (re-search-backward (rx "(use-package ") limit t)))
        (progn
          (pcase-let* ((`(use-package ,package-name . ,rest) (read (current-buffer))))
            (cl-assert package-name nil "Can't determine package name")
            (cl-assert (cl-loop for sexp in rest
                                thereis (eql sexp :quelpa))
                       nil "`:quelpa' form not found")
            (unpackaged/package-delete-all-versions package-name 'force))
          (let ((quelpa-upgrade-p t))
            (call-interactively #'eval-defun)))
      (user-error "Not in a `use-package' form"))))

(use-package package
  :bind (:map package-menu-mode-map
              ("t" . #'unpackaged/package-menu-upgrade-package))
  :config
  (defun unpackaged/package-menu-upgrade-package ()
    "Mark current package for upgrading (i.e. also mark obsolete version for deletion.)"
    (interactive)
    (when-let ((upgrades (package-menu--find-upgrades))
               (description (tabulated-list-get-id))
               (name (package-desc-name description))
               (upgradable (cdr (assq name upgrades))))
      ;; Package is upgradable
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((current-description (tabulated-list-get-id))
                 (current-name (package-desc-name current-description)))
            (when (equal current-name name)
              (cond ((equal description current-description)
                     (package-menu-mark-install)
                     (forward-line -1))
                    (t (package-menu-mark-delete)))))
          (forward-line 1))))))

;;; Programming

(defvar unpackaged/flex-fill-paragraph-column nil
  "Last fill column used in command `unpackaged/flex-fill-paragraph'.")

(defun unpackaged/flex-fill-paragraph (&optional unfill)
  "Fill paragraph, incrementing fill column each time this command is repeated.
When the command is called for the first time in a sequence,
unfill to the default `fill-column'.  With prefix, unfill
completely.  This command does not modify the stored value of
`fill-column'."
  (interactive "P")
  (let ((fill-column
         (cond (unfill (setf unpackaged/flex-fill-paragraph-column nil)
                       most-positive-fixnum)
               (t (setf unpackaged/flex-fill-paragraph-column
                        (if (equal last-command this-command)
                            (1+ (or unpackaged/flex-fill-paragraph-column
                                    fill-column))
                          fill-column))))))
    (fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun unpackaged/iedit-scoped (orig-fn)
  "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
  (interactive)
  (pcase-exhaustive current-prefix-arg
    ('nil (funcall orig-fn '(0)))
    ('(4) (funcall orig-fn))))

(advice-add #'iedit-mode :around #'unpackaged/iedit-scoped)

(defun unpackaged/iedit-or-flyspell ()
  "Toggle `iedit-mode' or correct previous misspelling with `flyspell', depending on context.

With point in code or when `iedit-mode' is already active, toggle
`iedit-mode'.  With point in a comment or string, and when
`iedit-mode' is not already active, auto-correct previous
misspelled word with `flyspell'.  Call this command a second time
to choose a different correction."
  (interactive)
  (if (or iedit-mode
          (and (derived-mode-p 'prog-mode)
               (not (or (nth 4 (syntax-ppss))
                        (nth 3 (syntax-ppss))))))
      ;; prog-mode is active and point is in a comment, string, or
      ;; already in iedit-mode
      (call-interactively #'iedit-mode)
    ;; Not prog-mode or not in comment or string
    (if (not (equal flyspell-previous-command this-command))
        ;; FIXME: This mostly works, but if there are two words on the
        ;; same line that are misspelled, it doesn't work quite right
        ;; when correcting the earlier word after correcting the later
        ;; one

        ;; First correction; autocorrect
        (call-interactively 'flyspell-auto-correct-previous-word)
      ;; First correction was not wanted; use popup to choose
      (progn
        (save-excursion
          (undo))                       ; This doesn't move point, which I think may be the problem.
        (flyspell-region (line-beginning-position) (line-end-position))
        (call-interactively 'flyspell-correct-previous-word-generic)))))

(defun unpackaged/sort-sexps (beg end)
  "Sort sexps in region.
Comments stay with the code below."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))

;;; Regular expressions

(defun unpackaged/query-replace-rx (&rest _)
  "Call `query-replace-regexp', reading regexp in `rx' syntax.
Automatically wraps in parens and adds `seq' to the beginning of
the form."
  (interactive)
  (cl-letf (((symbol-function #'query-replace-read-from) (lambda (&rest _)
                                                           (--> (read-string "rx form: ")
                                                                (concat "'(seq " it ")")
                                                                (read it)
                                                                (cadr it)
                                                                (rx-to-string it)))))
    (call-interactively #'query-replace-regexp)))

;;; Version control

(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

(defun unpackaged/magit-log--add-date-headers (&rest _ignore)
  "Add date headers to Magit log buffers."
  (when (derived-mode-p 'magit-log-mode)
    (save-excursion
      (ov-clear 'date-header t)
      (goto-char (point-min))
      (cl-loop with last-age
               for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                                        car
                                        (overlay-get it 'before-string)
                                        (get-text-property 0 'display it)
                                        cadr
                                        (s-match (rx (group (1+ digit) ; number
                                                            " "
                                                            (1+ (not blank))) ; unit
                                                     (1+ blank) eos)
                                                 it)
                                        cadr)
               do (when (and this-age
                             (not (equal this-age last-age)))
                    (ov (line-beginning-position) (line-beginning-position)
                        'after-string (propertize (concat " " this-age "\n")
                                                  'face 'magit-section-heading)
                        'date-header t)
                    (setq last-age this-age))
               do (forward-line 1)
               until (eobp)))))

(define-minor-mode unpackaged/magit-log-date-headers-mode
  "Display date/time headers in `magit-log' buffers."
  :global t
  (if magit-log-date-headers-mode
      (progn
        ;; Enable mode
        (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
        (advice-add #'magit-mode-setup :after #'unpackaged/magit-log--add-date-headers))
    ;; Disable mode
    (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
    (advice-remove #'magit-mode-setup #'unpackaged/magit-log--add-date-headers)))

(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;;; Web

(cl-defun unpackaged/feed-for-url (url &key (prefer 'atom) (all nil))
  "Return feed URL for web page at URL.
Interactively, insert the URL at point.  PREFER may be
`atom' (the default) or `rss'.  When ALL is non-nil, return all
feed URLs of all types; otherwise, return only one feed URL,
preferring the preferred type."
  (interactive (list (org-web-tools--get-first-url)))
  (require 'esxml-query)
  (require 'org-web-tools)
  (cl-flet ((feed-p (type)
                    ;; Return t if TYPE appears to be an RSS/ATOM feed
                    (string-match-p (rx "application/" (or "rss" "atom") "+xml")
                                    type)))
    (let* ((preferred-type (format "application/%s+xml" (symbol-name prefer)))
           (html (org-web-tools--get-url url))
           (dom (with-temp-buffer
                  (insert html)
                  (libxml-parse-html-region (point-min) (point-max))))
           (potential-feeds (esxml-query-all "link[rel=alternate]" dom))
           (return (if all
                       ;; Return all URLs
                       (cl-loop for (tag attrs) in potential-feeds
                                when (feed-p (alist-get 'type attrs))
                                collect (url-expand-file-name (alist-get 'href attrs) url))
                     (or
                      ;; Return the first URL of preferred type
                      (cl-loop for (tag attrs) in potential-feeds
                               when (equal preferred-type (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))
                      ;; Return the first URL of non-preferred type
                      (cl-loop for (tag attrs) in potential-feeds
                               when (feed-p (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))))))
      (if (called-interactively-p)
          (insert (if (listp return)
                      (s-join " " return)
                    return))
        return))))

(provide 'unpackaged)

;;; unpackaged.el ends here
