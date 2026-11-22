;;; skg-picker.el --- Minibuffer pickers for a skgrepo or a rung  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; The minibuffer prompts that ask the user for a skgrepo or a rung
;;; of the skgrepo restriction, with S-left/S-right cycling
;;; ('skg--completing-read-with-cycle', which other prompts use too):
;;; 'skg--prompt-for-owned-repo' (a new node's home, an import's
;;; target), 'skg--prompt-for-repo-change' (where C-? runs
;;; 'skg-view-repo-list') and 'skg--prompt-for-repo-set' (used by
;;; 'skg-restrict-repo-set', C-c v l, in 'skg-request-repo-sets.el').
;;; Reading the config they offer is 'skg-config.el's. Its Neovim
;;; twin is 'nvim/lua/skg/picker.lua'.
;;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'skg-config)

(defun skg--completing-read-with-cycle
    (prompt collection &optional predicate require-match
            initial-input hist def inherit-input-method cycle-values
            extra-minibuffer-setup)
  "Call `completing-read' with S-left/S-right cycling through CYCLE-VALUES.
The usual `completing-read' arguments keep their ordinary meaning.
EXTRA-MINIBUFFER-SETUP, if non-nil, runs inside the minibuffer setup
hook after the cycle bindings are installed."
  (let ((cycle (lambda (dir)
                 (lambda ()
                   (interactive)
                   (when cycle-values
                     (let* ((cur (minibuffer-contents))
                            (idx (or (cl-position cur cycle-values
                                                  :test #'string=)
                                     (and initial-input
                                          (cl-position initial-input
                                                       cycle-values
                                                       :test #'string=))
                                     0))
                            (new (nth (mod (+ idx dir)
                                           (length cycle-values))
                                      cycle-values)))
                       (delete-minibuffer-contents)
                       (insert new)))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when cycle-values
            (local-set-key (kbd "S-<left>")  (funcall cycle -1))
            (local-set-key (kbd "S-<right>") (funcall cycle  1)))
          (when extra-minibuffer-setup
            (funcall extra-minibuffer-setup)))
      (completing-read
       prompt collection predicate require-match initial-input hist def
       inherit-input-method))))

(defun skg--prompt-for-owned-repo (&optional prompt)
  "Prompt the user to choose an owned repo, with S-left/S-right cycling.
PROMPT defaults to \"Repo: \".
If there is only one owned repo, return it without prompting."
  (let ((owned-skgrepos (skg--owned-repos)))
    (if (= (length owned-skgrepos) 1)
        (car owned-skgrepos)
      (skg--completing-read-with-cycle
       (or prompt "Repo: ") owned-skgrepos nil t nil nil nil nil owned-skgrepos))))

(defun skg--prompt-for-repo-change (current-skgrepo)
  "Prompt for a repo to replace CURRENT-REPO.
S-left/S-right cycle through owned repos, and C-? displays every
configured repo with its path.  The user may also type a repo
name directly."
  (let ((owned-skgrepos (skg--owned-repos))
        (repo-names (skg--repo-names)))
    (skg--completing-read-with-cycle
     "Repo (S-left/right cycle; press C-? for a list of repos): "
     repo-names nil nil current-skgrepo nil nil nil owned-skgrepos
     (lambda ()
       (local-set-key (kbd "C-?") #'skg-view-repo-list)))))

(defconst skg--foreign-repos-choices
  '(("foreign: shown" . foreign-shown)
    ("foreign: hidden" . foreign-hidden))
  "The skgrepo restriction picker's foreign-repos choices: label . choice.")

(defun skg--prompt-for-repo-set (&optional offer-foreign-repos)
  "Prompt for the skgrepo restriction, with completion and S-left/S-right cycling.
The skgrepo restriction's rung is the most private owned repo to
show: choosing one shows it and every more public owned repo.  Each
choice is shown by its name, the most private one with \" (no
restriction)\" appended, the default.  Returns the chosen repo's name.
When OFFER-FOREIGN-REPOS is non-nil and the config has a foreign
repo, the choices also include \"foreign: shown\" and \"foreign:
hidden\", which return the symbol `foreign-shown' or
`foreign-hidden': foreign repos are shown or hidden apart from the
rung."
  (let* ((names (skg--repo-set-names))
         (rung-labels (mapcar #'skg--skgrepo-restriction-label names))
         (foreign-choices (when (and offer-foreign-repos
                                     (skg--foreign-repo-names))
                            skg--foreign-repos-choices))
         (labels (append rung-labels (mapcar #'car foreign-choices))))
    (unless names
      (user-error "No owned skg repos found"))
    (let ((label (skg--completing-read-with-cycle
                  (if foreign-choices
                      "Most private repo to show, or foreign: shown/hidden (S-left/right cycle): "
                    "Most private repo to show (S-left/right cycle): ")
                  labels nil t nil nil (car (last rung-labels)) nil labels)))
      (or (cdr (assoc label foreign-choices))
          (nth (cl-position label rung-labels :test #'string=) names)))))

(defun skg-view-repo-list ()
  "Display an org buffer listing configured repos and their paths."
  (interactive)
  (let ((repo-paths (skg--repo-paths)))
    (unless repo-paths
      (user-error "No skg repos found"))
    (let ((buffer (get-buffer-create "*skg-repos*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (repo-path repo-paths)
            (insert (format "* %s\n%s\n"
                            (car repo-path)
                            (cdr repo-path)))))
        (org-mode)
        (goto-char (point-min)))
      (display-buffer buffer))))

(provide 'skg-picker)
