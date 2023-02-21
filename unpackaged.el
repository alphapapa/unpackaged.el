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

;;;; Customization

(defgroup unpackaged nil
  "Options for `unpackaged'."
  :group 'convenience)

;;; Faces, fonts

(require 'seq)

(defvar lorem-ipsum-text)

;;;###autoload
(defun unpackaged/font-compare (text fonts)
  "Compare TEXT displayed in FONTS.
If TEXT is nil, use `lorem-ipsum' text.  FONTS is a list of font
family strings and/or font specs.

Interactively, prompt for TEXT, using `lorem-ipsum' if left
empty, and select FONTS with `x-select-font', pressing Cancel to
stop selecting fonts."
  (interactive (list (pcase (read-string "Text: ")
                       ("" nil)
                       (else else))
                     ;; `x-select-font' calls quit() when Cancel is pressed, so we use
                     ;; `inhibit-quit', `with-local-quit', and `quit-flag' to avoid that.
                     (let ((inhibit-quit t))
                       (cl-loop for font = (with-local-quit
                                             (x-select-font))
                                while font
                                collect font into fonts
                                finally do (setf quit-flag nil)
                                finally return fonts))))
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

(require 'ibuffer)
(require 'ibuf-ext)

;;;###autoload
(defun unpackaged/ibuffer-toggle-all-filter-groups (toggle-empty)
  "Toggle all filter groups.
With prefix, toggle `ibuffer-show-empty-filter-groups'."
  (interactive "P")
  (if toggle-empty
      (progn
        (setf ibuffer-show-empty-filter-groups (not ibuffer-show-empty-filter-groups))
        (ibuffer-update nil))
    (save-excursion
      (goto-char (point-min))
      (ibuffer-forward-filter-group)
      (let ((start (point)))
        (forward-char)
        (while (not (<= (point) start))
          (ibuffer-toggle-filter-group)
          (ibuffer-forward-filter-group))))))

;;;###autoload
(defun unpackaged/ibuffer-filter-group-move-down ()
  "Move filter group at point down."
  (interactive)
  (unpackaged/ibuffer-filter-group-move 'down))

;;;###autoload
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

;;; Customization

(defun unpackaged/custom-toggle-all-more-hide ()
  "Toggle all \"More/Hide\" widgets in current buffer."
  (interactive)
  (widget-map-buttons (lambda (widget _)
                        (pcase (widget-get widget :off)
                          ("More" (widget-apply-action widget)))
                        nil)))

(use-package cus-edit
  :general
  (:keymaps 'custom-field-keymap
            "C-c C-c" (defun unpackaged/custom-set-at-point ()
                        "Set current value of widget at point."
                        (interactive)
                        (cl-labels ((find-widget (widget property)
                                                 (if (widget-get widget property)
                                                     widget
                                                   (find-widget (widget-get widget :parent) property))))
                          (when-let* ((widget (find-widget (widget-at) :custom-set)))
                            (when (eq (widget-get widget :custom-state) 'modified)
                              (widget-apply widget :custom-set)))))))

;;;###autoload
(defun unpackaged/customize-theme-faces (theme &rest faces)
  "Customize THEME with FACES.
Advises `enable-theme' with a function that customizes FACES when
THEME is enabled.  If THEME is already enabled, also applies
faces immediately.  Calls `custom-theme-set-faces', which see."
  (declare (indent defun))
  (when (member theme custom-enabled-themes)
    ;; Theme already enabled: apply faces now.
    (let ((custom--inhibit-theme-enable nil))
      (apply #'custom-theme-set-faces theme faces)))
  (let ((fn-name (intern (concat "unpackaged/enable-theme-advice-for-" (symbol-name theme)))))
    ;; Apply advice for next time theme is enabled.
    (fset fn-name
          (lambda (enabled-theme)
            (when (eq enabled-theme theme)
              (let ((custom--inhibit-theme-enable nil))
                (apply #'custom-theme-set-faces theme faces)))))
    (advice-remove #'enable-theme fn-name)
    (advice-add #'enable-theme :after fn-name)))

;;; Elfeed

(defvar elfeed-search-filter)

(cl-defmacro unpackaged/elfeed-search-view-hydra-define (name body views)
  "Define a pretty hydra named NAME with BODY and VIEWS.
VIEWS is a plist: in it, each property is a string which becomes
a column header in the hydra, and each value is a list of lists
in this format: (KEY COMPONENT &optional LABEL).

The KEY is a key sequence passed to `kbd', like \"s\" or \"S
TAB\".  The COMPONENT is an Elfeed filter component, which may
begin with \"+\" or \"=\", and in which spaces are automatically
escaped as required by Elfeed.  The LABEL, if present, is a
string displayed next to the KEY; if absent, COMPONENT is
displayed.

In the resulting hydra, when KEY is pressed, the COMPONENT is
toggled in `elfeed-search-filter'.  It is toggled between three
states: normal, inverse, and absent.  For example, the component
\"+tag\" cycles between three states in the filter: \"+tag\",
\"-tag\", and \"\".  The appropriate inverse prefix is used
according to the component's prefix (i.e. for \"=\", the inverse
is \"~\", and for \"\" (a plain regexp), \"!\" is used).

These special components may be used to read choices from the
Elfeed database with completion and toggle them:

  :complete-age   Completes and sets the age token.
  :complete-feed  Completes and toggles a feed token.
  :complete-tag   Completes and toggles a tag token.
  nil             Sets default filter.

A complete example:

  (unpackaged/elfeed-search-view-hydra-define my/elfeed-search-view-hydra
    (:foreign-keys warn)
    (\"Views\"
     ((\"@\" :complete-age \"Date\")
      (\"d\" nil))
     \"Status\"
     ((\"su\" \"+unread\"))
     \"Feed\"
     ((\"f TAB\" :complete-feed \"Choose\")
      (\"fE\" \"=Planet Emacslife\" \"Planet Emacslife\"))
     \"Tags\"
     ((\"t TAB\" :complete-tag \"Choose\")
      (\"te\" \"+Emacs\"))
     \"\"
     ((\"tn\" \"+news\"))))"
  (declare (indent defun))
  (cl-labels ((escape-spaces (string)
                             ;; Return STRING with spaces escaped with "\s-".  Necessary
                             ;; because Elfeed treats all literal spaces as separating tokens.
                             (replace-regexp-in-string (rx space) "\\s-" string t t)))
    (let* ((completion-fns
            (list (cons :complete-age
                        (lambda ()
                          (interactive)
                          (save-match-data
                            (let* ((date-regexp (rx (group (or bos blank) "@" (1+ digit) (1+ (not blank)))))
                                   (date-tag (when (string-match date-regexp elfeed-search-filter)
                                               (match-string 1 elfeed-search-filter))))
                              (elfeed-search-set-filter
                               (replace-regexp-in-string date-regexp (read-string "Date: " date-tag)
                                                         elfeed-search-filter t t))))))
                  (cons :complete-feed
                        '(concat "=" (replace-regexp-in-string
                                      (rx space) "\\s-"
                                      (->> (hash-table-values elfeed-db-feeds)
                                           (--map (elfeed-meta it :title))
                                           (completing-read "Feed: ")
                                           regexp-quote) t t)))
                  (cons :complete-tag
                        '(concat "+" (completing-read "Tag: " (elfeed-db-get-all-tags))))))
           (body (append '(:title elfeed-search-filter :color pink :hint t :quit-key "q")
                         body))
           (heads (cl-loop for (heading views) on views by #'cddr
                           collect heading
                           collect (cl-loop for (key component label) in views
                                            collect
                                            `(,key
                                              ,(cl-typecase component
                                                 ((and function (not null))
                                                  ;; I don't understand why nil matches
                                                  ;; (or lambda function), but it does,
                                                  ;; so we have to account for it.  See
                                                  ;; (info-lookup-symbol 'cl-typep).
                                                  `(funcall ,component))
                                                 (string
                                                  `(elfeed-search-set-filter
                                                    (unpackaged/elfeed-search-filter-toggle-component
                                                     elfeed-search-filter ,(escape-spaces component))))
                                                 (otherwise
                                                  `(elfeed-search-set-filter
                                                    ,(when component
                                                       `(unpackaged/elfeed-search-filter-toggle-component
                                                         elfeed-search-filter ,component)))))
                                              ,(or label component "Default"))))))
      ;; I am so glad I discovered `cl-sublis'.  I tried several variations of `cl-labels' and
      ;; `cl-macrolet' and `cl-symbol-macrolet', but this is the only way that has worked.
      (setf heads (cl-sublis completion-fns heads))
      `(pretty-hydra-define ,name ,body
         ,heads))))

(cl-defun unpackaged/elfeed-search-filter-toggle-component (string component)
  "Return STRING (which should be `elfeed-search-filter') having toggled COMPONENT.
Tries to intelligently handle components based on their prefix:
+tag, =feed, regexp."
  (save-match-data
    (cl-labels ((toggle (component +prefix -prefix string)
                        (let ((+pat (rx-to-string `(seq (or bos blank)
                                                        (group ,+prefix ,component)
                                                        (or eos blank))))
                              (-pat (rx-to-string `(seq (group (or bos (1+ blank)) ,-prefix ,component)
                                                        (or eos blank)))))
                          ;; TODO: In newer Emacs versions, the `rx' pattern `literal'
                          ;; evaluates at runtime in `pcase' expressions.
                          (pcase string
                            ((pred (string-match +pat)) (rm (concat -prefix component) string))
                            ((pred (string-match -pat)) (rm "" string))
                            (_ (concat string " " +prefix component)))))
                (rm (new string) (replace-match new t t string 1)))
      (pcase component
        ((rx bos "+" (group (1+ anything)))
         (toggle (match-string 1 component) "+" "-" string))
        ((rx bos "=" (group (1+ anything)))
         (toggle (match-string 1 component) "=" "~" string))
        (_ (toggle component "" "!" string))))))

;;; Misc

(defmacro unpackaged/define-chooser (name &rest choices)
  "Define a chooser command NAME offering CHOICES.
Each of CHOICES should be a list, the first of which is the
choice's name, and the rest of which is its body forms."
  (declare (indent defun))
  ;; Avoid redefining existing, non-chooser functions.
  (cl-assert (or (not (fboundp name))
                 (get name :unpackaged/define-chooser)))
  (let* ((choice-names (mapcar #'car choices))
         (choice-list (--map (cons (car it) `(lambda (&rest args)
                                               ,@(cdr it)))
                             choices))
         (prompt (format "Choose %s: " name))
         (docstring (concat "Choose between: " (s-join ", " choice-names))))
    `(progn
       (defun ,name ()
         ,docstring
         (interactive)
         (let* ((choice-name (completing-read ,prompt ',choice-names)))
           (funcall (alist-get choice-name ',choice-list nil nil #'equal))))
       (put ',name :unpackaged/define-chooser t))))

(defcustom unpackaged/lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
  :type '(repeat regexp))

;;;###autoload
(cl-defun unpackaged/lorem-ipsum-overlay (&key replace-p use-map-p)
  "Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays.  Useful for taking
screenshots without revealing buffer contents.

If REPLACE-P is non-nil (interactively, with prefix and prompt),
replace buffer contents rather than overlaying them.  When a
buffer is very large and would have so many overlays that
performance would be prohibitively slow, you may replace the
buffer contents instead.  (Of course, be careful about saving the
buffer after replacing its contents.)

If USE-MAP-P is non-nil (interactively, with prefix and prompt),
all instances of a real word are replaced with the same word;
otherwise, each instance of a real word is replaced with a random
word (further obscuring the text).

Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid.  Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid.  For example, in an Org buffer, a line that starts
with:

  #+TITLE: unpackaged.el

could be matched against the exclude regexp (in `rx' syntax):

  (rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))

And the line would be overlaid like:

  #+TITLE: parturient.et"
  (interactive (when current-prefix-arg
                 (list :replace-p (yes-or-no-p "Replace contents (or just overlay)? ")
                       :use-map-p (yes-or-no-p "Map words (or be completely random)? "))))
  (require 'lorem-ipsum)
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :lorem-ipsum-overlay))
        ;; Remove overlays.
        (dolist (ov ovs)
          (when (overlay-get ov :lorem-ipsum-overlay)
            (delete-overlay ov)))
      ;; Add overlays.
      (let ((lorem-ipsum-words (--> lorem-ipsum-text
                                    (-flatten it) (apply #'concat it)
                                    (split-string it (rx (or space punct)) 'omit-nulls)))
            (case-fold-search nil)
            (map (make-hash-table :test #'equal)))
        (cl-labels ((overlay-group (group)
                                   (let* ((beg (match-beginning group))
                                          (end (match-end group))
                                          (replacement-word (if use-map-p
                                                                (lorem-word* (match-string-no-properties group))
                                                              (lorem-word (match-string-no-properties group))))
                                          (ov (make-overlay beg end)))
                                     (when replacement-word
                                       (overlay-put ov :lorem-ipsum-overlay t)
                                       (overlay-put ov 'display replacement-word))))
                    (replace-group (group)
                                   (let* ((beg (match-beginning group))
                                          (end (match-end group))
                                          (replacement-word (if use-map-p
                                                                (lorem-word* (match-string-no-properties group))
                                                              (lorem-word (match-string-no-properties group)))))
                                     (when replacement-word
                                       (setf (buffer-substring beg end) replacement-word))))
                    (lorem-word (word)
                                (if-let* ((matches (lorem-matches (length word))))
                                    (apply-case word (downcase (seq-random-elt matches)))
                                  ;; Word too long: compose one.
                                  (apply-case word (downcase (compose-word (length word))))))
                    (lorem-word* (word)
                                 (or (gethash word map)
                                     (puthash word
                                              (if-let ((matches (lorem-matches (length word))))
                                                  (apply-case word (downcase (seq-random-elt matches)))
                                                ;; Word too long: compose one.
                                                (apply-case word (downcase (compose-word (length word)))))
                                              map)))
                    (lorem-matches (length &optional (comparator #'=))
                                   (cl-loop for liw in lorem-ipsum-words
                                            when (funcall comparator (length liw) length)
                                            collect liw))
                    (apply-case (source target)
                                (cl-loop for sc across-ref source
                                         for tc across-ref target
                                         when (not (string-match-p (rx lower) (char-to-string sc)))
                                         do (setf tc (string-to-char (upcase (char-to-string tc)))))
                                target)
                    (compose-word (length)
                                  (cl-loop while (> length 0)
                                           for word = (seq-random-elt (lorem-matches length #'<=))
                                           concat word
                                           do (cl-decf length (length word)))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (rx (group (1+ (or bol bos blank (not alpha)))
                                                 (0+ (not (any alpha blank)))
                                                 (group (1+ alpha))
                                                 (0+ (not (any alpha blank)))))
                                      nil t)
              (unless (cl-member (match-string-no-properties 0) unpackaged/lorem-ipsum-overlay-exclude
                                 :test (lambda (string regexp)
                                         (string-match-p regexp string)))
                (if replace-p
                    (replace-group 2)
                  (overlay-group 2)))
              (goto-char (match-end 2)))))))))

(eval-when-compile
  (require 'dbus))

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

(defvar org-agenda-overriding-header)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)

;;;###autoload
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

(defun unpackaged/org-agenda-olp (outline-path &optional file only-todos)
  "Show an agenda restricted to subtree at OUTLINE-PATH.
FILE may be a filename to search in, or nil to look in the
current buffer.  If ONLY-TODOS is non-nil, show only to-do
items. OUTLINE-PATH is a list of strings which are outline
headings.  See function `org-find-olp'."
  (when file
    (push file outline-path))
  (let ((marker (org-find-olp outline-path (not file))))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (unpackaged/org-agenda-current-subtree-or-region only-todos)))))

(defface unpackaged/org-agenda-preview
  '((t (:background "black")))
  "Face for Org Agenda previews."
  :group 'org)

;;;###autoload
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

;;;###autoload
(cl-defun unpackaged/package-org-docs (&optional (package (unpackaged/buffer-provides)))
  "Return documentation about PACKAGE as an Org string.
Interactively, place on kill ring."
  (interactive)
  (let* ((commands (--map (cons it (if (documentation it)
                                       (unpackaged/docstring-to-org (documentation it))
                                     "Undocumented."))
                          (-sort (-on #'string< #'symbol-name)
                                 (unpackaged/package-commands package))))
         (functions (seq-difference (--map (cons it (if (documentation it)
                                                        (unpackaged/docstring-to-org (documentation it))
                                                      "Undocumented."))
                                           (-sort (-on #'string< #'symbol-name)
                                                  (unpackaged/package-functions package)))
                                    commands))
         (commands-string (when commands
                            (->> commands
                                 (--map (format "+  ~%s%s~ :: %s"
                                                (car it)
                                                (--when-let (documentation (car it))
                                                  (concat " (" (unpackaged/docstring-function-args it) ")"))
                                                (cdr it)))
                                 (s-join "\n")
                                 (format "* Commands\n\n%s"))))
         (functions-string (when functions
                             (->> functions
                                  (--map (format "+  ~%s%s~ :: %s"
                                                 (car it)
                                                 (--when-let (documentation (car it))
                                                   (concat " (" (unpackaged/docstring-function-args it) ")"))
                                                 (cdr it)))
                                  (s-join "\n")
                                  (format "* Functions\n\n%s"))))
         (string (s-join "\n\n" (list commands-string functions-string))))
    (if (called-interactively-p 'any)
        (progn
          (kill-new string)
          (message "Documentation stored in kill ring"))
      string)))

(cl-defun unpackaged/package-commands (&optional (package (unpackaged/buffer-provides)))
  "Return list of command symbols in PACKAGE, or current buffer's package."
  (let* ((functions (unpackaged/package-functions package)))
    (-select #'commandp functions)))

(cl-defun unpackaged/package-functions (&optional (package (unpackaged/buffer-provides)))
  "Return list of functions defined in PACKAGE, or current buffer's package."
  (let* ((prefix (symbol-name package))
         (symbols))
    (mapatoms (lambda (symbol)
                (when (string-prefix-p prefix (symbol-name symbol))
                  (push symbol symbols))))
    (->> symbols
         (-select #'fboundp)
         (--select (not (string-suffix-p "--cmacro" (symbol-name it)))))))

(cl-defun unpackaged/buffer-provides (&optional (buffer (current-buffer)))
  "Return symbol that Emacs package in BUFFER provides."
  ;; I couldn't find an existing function that does this, but this is simple enough.
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (re-search-backward (rx bol "(provide '" (group (1+ (not (any ")")))) ")"))
      (intern (match-string 1)))))

;;;###autoload
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

;;;###autoload
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
                           (progn
                             ;; Remove end-of-string function argument list
                             (goto-char (point-max))
                             (when (re-search-backward (rx "\n\n" "(fn " (group (1+ not-newline)) ")" eos) nil t)
                               (replace-match "" t t)))
                           (unpackaged/caps-to-code (point-min) (point-max))
                           (unpackaged/symbol-quotes-to-org-code (point-min) (point-max))
                           (unfill-region (point-min) (point-max))
                           (while (re-search-forward (rx bol (group (1+ blank))) nil t)
                             (replace-match "" t t nil 1))
                           (while (re-search-forward "\n" nil t)
                             (replace-match "\n   " t t))
                           (when (looking-at "\"")
                             (delete-char 1))
                           (when (progn
                                   (goto-char (point-max))
                                   (looking-back "\"" nil))
                             (delete-char -1))
                           (while (re-search-forward (rx bol (group (>= 2 " ")) (group (1+ (not space)) (1+ not-newline))) nil t)
                             ;; Indented code samples, by two or more spaces
                             (replace-match (concat (match-string 1) "~" (match-string 2) "~"))))
         (s-trim it)
         (if (called-interactively-p 'interactive)
             (progn
               (message it)
               (kill-new it))
           it))))

(defun unpackaged/docstring-function-args (docstring)
  "Return function args parsed from DOCSTRING.
DOCSTRING should be like one returned by function
`documentation', which typically has function arguments on the
last line."
  (when (string-match (rx "\n\n" "(fn " (group (1+ not-newline)) ")" eos) docstring)
    (match-string 1 docstring)))

;;;###autoload
(defun unpackaged/caps-to-code (beg end)
  "Convert all-caps words in region to Org code emphasis."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward (rx (or space bol)
                                      (group (1+ (or upper "-")))
                                      (or space eol (char punct)))
                                  nil t)
          (setf (buffer-substring (match-beginning 1) (match-end 1))
                (concat "~" (match-string 1) "~"))
          (goto-char (match-end 0)))))))

;;;###autoload
(defun unpackaged/symbol-quotes-to-org-code (beg end)
  "Change Emacs `symbol' quotes to Org =symbol= quotes in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (while (re-search-forward (rx (or "`" "‘") (group (1+ (or word (syntax symbol)))) (or "’" "'")) nil t)
        (replace-match (concat "~" (match-string 1) "~") t)))))

;;;###autoload
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

;;;###autoload
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
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
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
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

(eval-when-compile
  (require 'easy-mmode)
  (require 'ox))

(use-package ox
  :config
  (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
    :global t
    (if unpackaged/org-export-html-with-useful-ids-mode
        (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
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
                            (unpackaged/org-export-new-title-reference datum cache))
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

  (defun unpackaged/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
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
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
        (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
        ref))))

;;;###autoload
(define-minor-mode unpackaged/org-table-face-mode
  "Apply `org-table' face family to all text in Org tables.
Useful for forcibly applying the face to portions of table data
that might have a different face, which could affect alignment."
  :global nil
  (let ((keywords '((unpackaged/org-table-face-matcher 0 'org-table))))
    (if unpackaged/org-table-face-mode
        (font-lock-add-keywords nil keywords 'append)
      (font-lock-remove-keywords nil keywords))
    (font-lock-flush)))

(cl-defun unpackaged/org-table-face-matcher
    (limit &optional (face `(:family ,(face-attribute 'org-table :family))))
  "Apply FACE to entire Org tables.
A `font-lock-keywords' function that searches up to LIMIT."
  (cl-flet* ((find-face (face &optional limit not)
                        ;; Return next position up to LIMIT that has FACE, or doesn't if NOT.
                        (cl-loop with prev-pos
                                 with pos = (point)
                                 while (not (eobp))
                                 do (setf pos (next-single-property-change pos 'face nil limit))
                                 while (and pos (not (equal pos prev-pos)))
                                 for face-at = (get-text-property pos 'face)
                                 for face-matches-p = (or (eq face-at face)
                                                          (when (listp face-at)
                                                            (member face face-at)))
                                 when (or (and not (not face-matches-p))
                                          face-matches-p)
                                 return pos
                                 do (setf prev-pos pos)))
             (apply-face-from (pos face)
                              (unless (eobp)
                                (let* ((property-at-start (get-text-property pos 'face))
                                       (table-face-start (if (or (eq property-at-start 'org-table)
                                                                 (when (listp property-at-start)
                                                                   (member 'org-table property-at-start)))
                                                             (point)
                                                           (find-face 'org-table limit)))
                                       table-face-end)
                                  (when table-face-start
                                    (goto-char table-face-start)
                                    (setf table-face-end (line-end-position))
                                    (add-face-text-property table-face-start table-face-end face)
                                    (goto-char table-face-end))))))
    (cl-loop with applied-p
             for applied = (apply-face-from (point) face)
             when applied
             do (setf applied-p t)
             while applied
             finally return applied-p)))

(defun unpackaged/org-outline-numbers (&optional remove-p)
  "Add outline number overlays to the current buffer.
When REMOVE-P is non-nil (interactively, with prefix), remove
them.  Overlays are not automatically updated when the outline
structure changes."
  ;; NOTE: This does not necessarily play nicely with org-indent-mode
  ;; or org-bullets, but it probably wouldn't be too hard to fix that.
  (interactive (list current-prefix-arg))
  (cl-labels ((heading-number ()
               (or (when-let ((num (previous-sibling-number)))
                     (1+ num))
                   1))
              (previous-sibling-number ()
               (save-excursion
                 (let ((pos (point)))
                   (org-backward-heading-same-level 1)
                   (when (/= pos (point))
                     (heading-number)))))
              (number-list ()
               (let ((ancestor-numbers (save-excursion
                                         (cl-loop while (org-up-heading-safe)
                                                  collect (heading-number)))))
                 (nreverse (cons (heading-number) ancestor-numbers))))
              (add-overlay ()
               (let* ((ov-length (org-current-level))
                      (ov (make-overlay (point) (+ (point) ov-length)))
                      (ov-string (concat (mapconcat #'number-to-string (number-list) ".")
                                         ".")))
                 (overlay-put ov 'org-outline-numbers t)
                 (overlay-put ov 'display ov-string))))
    (remove-overlays nil nil 'org-outline-numbers t)
    (unless remove-p
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (org-before-first-heading-p)
         (outline-next-heading))
       (cl-loop do (add-overlay)
                while (outline-next-heading))))))

;;;###autoload
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

(require 'org)

(require 'ts)

;;;###autoload
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

;;;###autoload
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
               collect (ts-parse-org (match-string 0))))))

;;;###autoload
(cl-defun unpackaged/org-refile-to-datetree (file &key (date (calendar-current-date)) entry)
  "Refile ENTRY or current node to entry for DATE in datetree in FILE.
DATE should be a list of (MONTH DAY YEAR) integers, e.g. as
returned by `calendar-current-date'."
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
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

;;;###autoload
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
               ;; FIXME: looking-back is supposed to be called with more arguments.
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

  ;;;###autoload
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
  (interactive)
  (let ((inhibit-read-only t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (unpackaged/org-next-heading-tagged "read_only")
       (remove-text-properties (point) (org-end-of-subtree t)
                               '(read-only t))))))

;;;###autoload
(defun unpackaged/org-sort-multi ()
  "Call `org-sort' until \\[keyboard-quit] is pressed."
  (interactive)
  ;; Not sure if `with-local-quit' is necessary, but probably a good
  ;; idea in case of recursive edit.
  (with-local-quit
    (cl-loop while (call-interactively #'org-sort))))

;;; Packages

(require 'package)

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
                    (error "Package `%s' is a system package" symbol))))
    ;; Checks passed: delete packages.
    (cl-loop for (_symbol . descs) in matching-versions
             do (--each descs
                  (package-delete it force)))))

(defun unpackaged/reload-package (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
  (interactive
   (list (intern (completing-read "Package: "
                                  (mapcar #'car package-alist) nil t))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (message "Reloaded: %s" (mapconcat #'symbol-name package-features " ")))))

(defvar quelpa-upgrade-p)

;;;###autoload
(cl-defun unpackaged/quelpa-use-package-upgrade (&key (reloadp t))
  "Eval the current `use-package' form with `quelpa-upgrade-p' true.
Delete the package first to remove obsolete versions.  When
RELOADP is non-nil, reload the package's features after upgrade
using `unpackaged/reload-package'; otherwise (interactively, with
prefix), leave old features loaded."
  (interactive (list :reloadp (not current-prefix-arg)))
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
            (cl-assert (memq :quelpa rest) nil "`:quelpa' form not found")
            (unpackaged/package-delete-all-versions package-name 'force)
            (let ((quelpa-upgrade-p t))
              (call-interactively #'eval-defun))
            (when reloadp
              (unpackaged/reload-package package-name))))
      (user-error "Not in a `use-package' form"))))

(use-package package
  :bind (:map package-menu-mode-map
              ("t" . #'unpackaged/package-menu-upgrade-package))
  :config
  ;; I think the `use-package' form takes care of autoloading here.
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

(defun unpackaged/compile-defun-debug ()
  "Compile and evaluate the current top-level form, displaying compilation warnings.
Calls `compile-defun' with `byte-compile-debug' non-nil."
  (interactive)
  (let ((byte-compile-debug t))
    (call-interactively #'compile-defun)))

(defvar unpackaged/flex-fill-paragraph-column nil
  "Last fill column used in command `unpackaged/flex-fill-paragraph'.")

;;;###autoload
(defun unpackaged/flex-fill-paragraph (&optional fewer-lines unfill)
  "Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only
bound around calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the
default `fill-column'.

When called repeatedly, increase `fill-column' until filling
changes.

With one universal prefix, increase `fill-column' until the
number of lines is reduced.  With two, unfill completely."
  (interactive "P")
  (let* ((fewer-lines (or fewer-lines (equal current-prefix-arg '(4))))
         (unfill (or unfill (equal current-prefix-arg '(16))))
         (fill-column
          (cond (unfill (setf unpackaged/flex-fill-paragraph-column nil)
                        most-positive-fixnum)
                (t (setf unpackaged/flex-fill-paragraph-column
                         (if (equal last-command this-command)
                             (or (unpackaged/flex-fill-paragraph--next-fill-column fewer-lines)
                                 fill-column)
                           fill-column))))))
    (fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun unpackaged/flex-fill-paragraph--next-fill-column (&optional fewer-lines)
  "Return next `fill-column' value.
If FEWER-LINES is non-nil, reduce the number of lines in the
buffer, otherwise just change the current paragraph."
  ;; This works well, but because of all the temp buffers, sometimes when called
  ;; in rapid succession, it can cause GC, which can be noticeable.  It would be
  ;; nice to avoid that.  Note that this has primarily been tested on
  ;; `emacs-lisp-mode'; hopefully it works well in other modes.
  (let* ((point (point))
         (source-buffer (current-buffer))
         (mode major-mode)
         (fill-column (or unpackaged/flex-fill-paragraph-column fill-column))
         (old-fill-column fill-column)
         (hash (unless fewer-lines
                 (buffer-hash)))
         (original-num-lines (when fewer-lines
                               (line-number-at-pos (point-max)))))
    (with-temp-buffer
      (delay-mode-hooks
        (funcall mode))
      (insert-buffer-substring source-buffer)
      (goto-char point)
      (cl-loop while (and (fill-paragraph)
                          (if fewer-lines
                              (= original-num-lines (line-number-at-pos (point-max)))
                            (string= hash (buffer-hash))))
               ;; If filling doesn't change after 100 iterations, abort by returning nil.
               if (> (- fill-column old-fill-column) 100)
               return nil
               else do (cl-incf fill-column)
               finally return fill-column))))

;;;###autoload
(defun unpackaged/iedit-scoped (orig-fn)
  "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
  (interactive)
  (pcase-exhaustive current-prefix-arg
    ('nil (funcall orig-fn '(0)))
    ('(4) (funcall orig-fn))))

(advice-add #'iedit-mode :around #'unpackaged/iedit-scoped)

(defvar flyspell-previous-command)

;;;###autoload
(defun unpackaged/iedit-or-flyspell ()
  "Toggle `iedit-mode' or correct previous misspelling with `flyspell', depending on context.

With point in code or when `iedit-mode' is already active, toggle
`iedit-mode'.  With point in a comment or string, and when
`iedit-mode' is not already active, auto-correct previous
misspelled word with `flyspell'.  Call this command a second time
to choose a different correction."
  (interactive)
  (if (or (bound-and-true-p iedit-mode)
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
          (undo)) ; This doesn't move point, which I think may be the problem.
        (flyspell-region (line-beginning-position) (line-end-position))
        (call-interactively 'flyspell-correct-previous-word-generic)))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
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
  (if unpackaged/magit-log-date-headers-mode
      (progn
        ;; Enable mode
        (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
        (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
    ;; Disable mode
    (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
    (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))

;;;###autoload
(defun unpackaged/magit-save-buffer-show-status ()
  "Save buffer and show its changes in `magit-status'."
  (interactive)
  (save-buffer)
  (unpackaged/magit-status))

(require 'hydra)

(use-package smerge-mode
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

(defun unpackaged/imenu-eww-headings ()
  "Return alist of HTML headings in current EWW buffer for Imenu.
Suitable for `imenu-create-index-function'."
  (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (cl-loop for next-pos = (next-single-property-change (point) 'face)
                 while next-pos
                 do (goto-char next-pos)
                 for face = (get-text-property (point) 'face)
                 when (cl-typecase face
                        (list (cl-intersection face faces))
                        (symbol (member face faces)))
                 collect (cons (buffer-substring (point-at-bol) (point-at-eol)) (point))
                 and do (forward-line 1))))))

(add-hook 'eww-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'unpackaged/imenu-eww-headings)))

(eval-when-compile
  (require 'esxml-query))

;;;###autoload
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
                       (cl-loop for (_tag attrs) in potential-feeds
                                when (feed-p (alist-get 'type attrs))
                                collect (url-expand-file-name (alist-get 'href attrs) url))
                     (or
                      ;; Return the first URL of preferred type
                      (cl-loop for (_tag attrs) in potential-feeds
                               when (equal preferred-type (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))
                      ;; Return the first URL of non-preferred type
                      (cl-loop for (_tag attrs) in potential-feeds
                               when (feed-p (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))))))
      (if (called-interactively-p 'interactive)
          (insert (if (listp return)
                      (s-join " " return)
                    return))
        return))))

(provide 'unpackaged)

;;; unpackaged.el ends here
