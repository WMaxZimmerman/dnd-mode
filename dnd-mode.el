;;; package --- dnd
;;; Commentary:
;;; This is the first draft of function to utilize when playing DnD

;;; Code:
(require 'yasnippet)
(require 'org)

;; === Variables ===
(setq dnd-srd-dir "~/org-dnd-srd/")
(setq dnd-snippet-dir "~/dnd-mode/snippets")
(setq dnd-org-capture-templates `(("i" "Inbox" entry  (file "inbox.org")
                                   ,(concat "* TODO %?\n"
                                            "/Entered on/ %U"))
                                  ("n" "Note" entry  (file "notes.org")
                                   ,(concat "* %?\n"
                                            "/Entered on/ %U"))))


;; === Functions ===
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun dnd-load-srd-agenda ()
  "Loads the DnD SRD files in the directory defined by 'dnd-srd-dir' into the org agenda"
  (setq org-agenda-files (mapcar (lambda (path) (concat dnd-srd-dir path))
                                 (read-lines (concat dnd-srd-dir ".agenda-index")))))

(defun dnd-select-session-target ()
  "Sets the org agenda/capture to a the specified directory"
  (interactive)
  (setq dir (read-directory-name "dir:"))
  (progn (dnd-load-srd-agenda)
         (setq org-agenda-files (append org-agenda-files (mapcar (lambda (path) (concat dir path))
                                                                 (read-lines (concat dir ".agenda-index")))))
         (setq org-directory (concat dir "org/"))
         (setq org-capture-templates dnd-org-capture-templates)))


(defun rtd ()
  "Sometimes you just got to roll the dice. This function currently expects the patter of '1d4 + 0' (the spaces are needed and you must have an add, sorry)"
  (interactive)

  (setq dnd-text (buffer-substring (region-beginning) (region-end)))
  (setq dnd-text-values (split-string dnd-text " "))
  (setq dnd-dice-text (nth 0 dnd-text-values))
  (setq dnd-mod-text (nth 2 dnd-text-values))
  
  (setq dnd-values (split-string dnd-dice-text "d"))
  (setq dnd-count (string-to-number (nth 0 dnd-values)))
  (setq dnd-limit (string-to-number (nth 1 dnd-values)))

  (setq dnd-total 0)
  (let ((dnd-x 0))
    (while (< dnd-x dnd-count)
      (setq dnd-rand (random dnd-limit))
      (setq dnd-roll (+ dnd-rand 1))
      (setq dnd-total (+ dnd-total dnd-roll))
      (message "Dice %d was %d" (+ 1 dnd-x) dnd-roll)
      (setq dnd-x (+ dnd-x 1))))

  (message "Mod was %s" dnd-mod-text)
  (if dnd-mod-text
      (setq dnd-total (+ dnd-total (string-to-number dnd-mod-text)))
    (nil))

  (message "You rolled %d" dnd-total))


(defun calc-dnd-mod (score)
  "Calculates the modifier of the given DND ability score"
  (message "input is: %d" score)
  (floor (- (/ score 2) 5)))


(defun calc-dnd-pb (pb check)
  "Calculates the Proficiency Bonus to use based on the check and value provided"
  (if (string= check "X")
      pb
    (if (string= check "XX")
      (* 2 pb)
      0)))


(defun dnd-output-ability-constants (table)
  "Outputs constants for the Ability Modifiers"
  (setq consts (mapcar (lambda (row) (format "#+CONSTANTS: %s=%s\n" (nth 0 row) (nth 1 row))) table))
  (while consts
    (if (string= "#+CONSTANTS: Allowed=27\n" (car consts))
        (setq consts (cdr consts))
      (if (string= "#+CONSTANTS: =\n" (car consts))
          (setq consts (cdr consts))
        (progn (princ (car consts))
               (setq consts (cdr consts)))))))


(defun calc-dnd-point-buy-cost (score)
  "Calculates the point buy value of the given ability score"
  (setq base-cost (- (string-to-number score) 8))
  (setq cost 0)
  (if (> base-cost 5)
      (if (> base-cost 6)
          (setq cost (+ base-cost 2))
        (setq cost (+ base-cost 1)))
    (setq cost base-cost))
  (if (> cost 9)
      "Value Too High"
    (if (< cost 0)
        "Value Too Low"
      cost)))


(defun dnd-eval-charsheet ()
  "Evaluates the dnd character sheet that you are currently in"
  (interactive)
  
  (setq starting-point (point))
  (outline-show-all)
  (goto-char (point-min))

  (search-forward "BEGIN_SRC")
  (org-babel-execute-src-block)
  (search-forward ":results:")
  (next-line)
  (backward-word)

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "elisp")))  ;don't ask for ditaa
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
  
  (org-ctrl-c-ctrl-c)

  (unwind-protect
      (while (string= "1" "1")
        (search-forward "TBLFM")
        (org-ctrl-c-ctrl-c))
    (progn 
      (goto-char (point-min))
      (search-forward "* Constants")
      (outline-hide-leaves)
      (goto-char starting-point))))


;; ==== Minor Mode ===
(define-minor-mode dnd-mode
  "Toggles global dnd-mode"
  nil
  :global t
  :lighter " dnd"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c r") 'rtd)
            (define-key map (kbd "C-c e") 'dnd-eval-charsheet)
            (define-key map (kbd "C-c s") 'dnd-select-session-target)
            map)
  
  (if dnd-mode
      (message "dnd-mode activated!")
    (message "dnd-mode deactivated!")))

(add-hook 'dnd-mode-hook (lambda () (message "Dnd Mode did thing")))

(add-hook 'dnd-mode-on-hook (lambda () (progn
                                            (setq dnd-prev-org-capture-templates org-capture-templates)
                                            (setq dnd-prev-org-directory org-directory)
                                            (setq dnd-prev-org-agenda-files org-agenda-files)
                                            (setq dnd-prev-yas-snippet-dirs yas-snippet-dirs)

                                            (dnd-load-srd-agenda)
                                            (setq yas-snippet-dirs (append yas-snippet-dirs '(dnd-snippet-dir)))
                                            (yas-reload-all)
                                            (message "Dnd Mode is On!"))))

(add-hook 'dnd-mode-off-hook (lambda () (progn 
                                            (setq org-capture-templates dnd-prev-org-capture-templates)
                                            (setq org-directory dnd-prev-org-directory)
                                            (setq org-agenda-files dnd-prev-org-agenda-files)
                                            (setq yas-snippet-dirs dnd-prev-yas-snippet-dirs)
                                            (yas-reload-all)

                                            (message "Dnd Mode is Off!"))))

(provide 'dnd)
;;; dnd.el ends here
