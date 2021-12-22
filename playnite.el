;;; playnite.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Henrique Romano
;;
;; Author: Pedro Henrique Romano <https://github.com/phr>
;; Maintainer: Pedro Henrique Romano <mail@pedroromano.org>
;; Created: November 27, 2021
;; Modified: November 27, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/phr/playnite
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'org)
(require 'org-roam)
(require 'hltb)

(cl-defstruct playnite-game
  name source playtime is-installed game-id id)

(defun playnite-load-csv (filename)
  (interactive "ffilename: ")
  (let ((result nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (forward-line 2)
      (while (not (eobp))
        (let* ((start (point))
               (_ (end-of-line))
               (end (point))
               (line (split-string-and-unquote (buffer-substring start end) ","))
               (game (make-playnite-game :name (nth 0 line)
                                         :source (nth 1 line)
                                         :playtime (string-to-number (nth 2 line))
                                         :is-installed (equal (nth 3 line) "True")
                                         :game-id (nth 4 line)
                                         :id (nth 5 line))))
          (push game result))
        (forward-line)
        (beginning-of-line)))
    result))

(defun playnite-org-make-link (game)
  (let ((name (playnite-game-name game)))
    (if (equal (playnite-game-source game) "Steam")
        (format "[[steam:%s][%s]]" (playnite-game-game-id game) name)
      (format "[[playnite:%s][%s]]" (playnite-game-id game) name))))

(defun playnite-org-insert (game)
  (if-let ((id (hltb-org-roam-find-id (playnite-game-name game))))
      (error "Game already present")
    (with-temp-buffer
      (insert (format "* TODO %s\n" (playnite-org-make-link game)))
      (write-file
       (expand-file-name
        (format "%s-%s.org"
                (format-time-string "%Y%m%d%H%M%S")
                (org-roam-node-slug
                 (org-roam-node-create :title (playnite-game-name game))))
        org-roam-directory))
      (org-id-get-create)
      (org-set-property "CATEGORY" "games")
      (condition-case _
          (hltb-org-fill-properties)
        (error (message "%s not found in HLTB" (playnite-game-name game))))
      (save-buffer)
      (message "Imported %s" (playnite-game-name game)))))

(defun playnite-org-import-csv (csv &optional limit offset)
  (let ((games (playnite-load-csv csv)))
    (when offset (setq games (-drop offset games)))
    (when limit (setq games (-take limit games)))
    (dolist (game games)
      (message "Importing %s" (playnite-game-name game))
      (condition-case err
          (playnite-org-insert game)
        (error (message (error-message-string err))))
      (org-roam-db-sync))))

(provide 'playnite)
;;; playnite.el ends here
