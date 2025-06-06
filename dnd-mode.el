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
(defun dnd-read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun dnd-load-srd-agenda ()
  "Loads the DnD SRD files in the directory defined by 'dnd-srd-dir' into the org agenda"
  (setq org-agenda-files (mapcar (lambda (path) (concat dnd-srd-dir path))
                                 (dnd-read-lines (concat dnd-srd-dir ".agenda-index")))))

(defun dnd-select-session-target ()
  "Sets the org agenda/capture to a the specified directory"
  (interactive)
  (setq dir (read-directory-name "dir:"))
  (progn (dnd-load-srd-agenda)
         (setq org-agenda-files (append org-agenda-files (mapcar (lambda (path) (concat dir path))
                                                                 (dnd-read-lines (concat dir ".agenda-index")))))
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


(defun dnd-calc-mod (score)
  "Calculates the modifier of the given DND ability score"
  (message "input is: %d" score)
  (floor (- (/ score 2) 5)))


(defun dnd-calc-pb (pb check)
  "Calculates the Proficiency Bonus to use based on the check and value provided"
  (if (string= check "X")
      pb
    (if (string= check "XX")
      (* 2 pb)
      0)))


(defun dnd-calc-offhand-mod (mod check)
  "Calculates the Proficiency Bonus to use based on the check and value provided"
  (if (string= check "X")
      0
    mod))

(defun dnd-calc-bonus-dmg (bonus)
  "Calculates the Proficiency Bonus to use based on the check and value provided"
  (if (string= bonus "-")
      ""
    (concat " + " bonus)))

(defun dnd-reset-based-on-rest (sr lr used sr_refill lr_refill)
  "Calculates the Proficiency Bonus to use based on the check and value provided"
  (if (string= sr "X")
      (if (string= sr_refill "X")
          0
        (if (string= sr_refill "-")
            used
          (- (string-to-number used) (string-to-number sr_refill))))
    (if (string= lr "X")
        (if (string= lr_refill "X")
            0
          (if (string= lr_refill "-")
              (string-to-number used)
            (- used (string-to-number lr_refill))))
      used)))

(defun dnd-index (object list)
  "Return the index of OBJECT (string or symbol) in LIST, comparing string representations."
  (let ((index 0)
        (needle (format "%s" object)))
    (catch 'found
      (dolist (el list)
        (let ((el-str (format "%s" el)))
          (message "Comparing %S to %S" needle el-str)
          (when (string= needle el-str)
            (message "Match found at %d" index)
            (throw 'found index)))
        (setq index (1+ index)))
      ;; only runs if no match
      (message "No match found")
      nil)))

(defun dnd-calc-size-mod (size)
  "Outputs constants for the Ability Modifiers"
  (if (string= size "Tiny") 0.5
    (if (string= size "Small") 1
      (if (string= size "Medium") 1
        (if (string= size "Large") 2
          (if (string= size "Huge") 3
            (if (string= size "Gargantuan") 4 1)))))))

(defun dnd-calc-carry-capacity (STR size bonus)
  "Outputs constants for the Ability Modifiers"
  (setq carryMod 7.5)
  (setq sizeScale (+ (truncate (dnd-calc-size-mod size)) bonus))
  (setq iterator 0)
  (while (< iterator sizeScale)
    (setq carryMod (* carryMod 2))
    (setq iterator (1+ iterator)))
  (* STR carryMod))

(defun dnd-calc-drag-capacity (STR size bonus)
  "Outputs constants for the Ability Modifiers"
  (setq carryMod 15)
  (setq sizeScale (+ (truncate (dnd-calc-size-mod size)) bonus))
  (setq iterator 0)
  (while (< iterator sizeScale)
    (setq carryMod (* carryMod 2))
    (setq iterator (1+ iterator)))
  (* STR carryMod))

(defun dnd-calc-hp (CON hitDie level)
  "Outputs constants for the Ability Modifiers"
  (setq conMod (dnd-calc-mod CON))
  (setq hp (+ hitDie conMod))
  (setq iterator 1)
  (while (< iterator level)
    (setq hp (+ hp (+ (ceiling (/ hitDie 2)) conMod)))
    (setq iterator (1+ iterator)))
  (format "%s" hp))

(defun dnd-half-die (die)
  "Outputs constants for the Ability Modifiers"
  (setq halvedDie (/ die 2))
  (if (= (% halvedDie 2) 0)
      halvedDie
    (- halvedDie 1)
    ))

(defun dnd-calc-dice (die count size)
  "Outputs constants for the Ability Modifiers"
  (setq sizeMod (dnd-calc-size-mod size))
  (if (>= sizeMod 1)
      (progn
        (setq diceCount (* (string-to-number count) sizeMod))
        (format "%sd%s" diceCount die))
    (format "%sd%s" count (dnd-half-die (string-to-number die)))))

(defun dnd-calc-unarmored-defense (dex con)
  "Outputs constants for the Ability Modifiers"
  (+ 10 (dnd-calc-mod dex) (dnd-calc-mod con)))

(defun dnd-calc-spell-save-dc (ability prof has_dc)
  "Outputs constants for the Ability Modifiers"
  (if (string= has_dc "X")
      (+ prof (dnd-calc-mod ability) 8)
    (format "%s" "-")))

(defun dnd-calc-ac (armor dex con bonus shield)
  "Outputs constants for the Ability Modifiers"
  (setq mod (dnd-calc-mod dex))
  (setq conMod (dnd-calc-mod con))
  (setq armorBaseAc (dnd-get-armor-base-ac armor))
  (setq armorType (dnd-get-armor-type armor))
  (if (string= armorType "light") (+ armorBaseAc mod bonus shield)
    (if (string= armorType "medium") (+ armorBaseAc (if (> mod 2) 2 mod) bonus shield)
      (if (string= armorType "heavy") (+ armorBaseAc bonus shield) 
        (if (string= armorType "unarmored") (+ armorBaseAc mod conMod bonus shield) (+ armorBaseAc mod bonus shield))))))

(defun dnd-get-armor-type (armor)
  "Outputs constants for the Ability Modifiers"
  (if (string= armor "Padded") "light"
    (if (string= armor "Leather") "light"
      (if (string= armor "Studded Leather") "light"
        (if (string= armor "Hide") "medium"
          (if (string= armor "Chain Shirt") "medium"
            (if (string= armor "Scale Mail") "medium"
              (if (string= armor "Spiked Armor") "medium"
                (if (string= armor "Breastplate") "medium"
                  (if (string= armor "Halfplate") "medium"
                    (if (string= armor "Ring Mail") "heavy"
                      (if (string= armor "Chain Mail") "heavy"
                        (if (string= armor "Splint") "heavy"
                          (if (string= armor "Plate") "heavy" 
                            (if (string= armor "Unarmored") "unarmored" "none")))))))))))))))

(defun dnd-get-armor-base-ac (armor)
  "Outputs constants for the Ability Modifiers"
  (if (string= armor "Padded") 11
    (if (string= armor "Leather") 11
      (if (string= armor "Studded Leather") 12
        (if (string= armor "Hide") 12
          (if (string= armor "Chain Shirt") 13
            (if (string= armor "Scale Mail") 14
              (if (string= armor "Spiked Armor") 14
                (if (string= armor "Breastplate") 14
                  (if (string= armor "Halfplate") 15
                    (if (string= armor "Ring Mail") 14
                      (if (string= armor "Chain Mail") 16
                        (if (string= armor "Splint") 17
                          (if (string= armor "Plate") 18 10))))))))))))))

(defun dnd-get-stat (ability)
  "Outputs constants for the Ability Modifiers"
  (setq values (org-table-get-remote-range "stats" "@2$1..@>$>"))
  (setq abilities (mapcar (lambda (field) (org-no-properties field)) (org-table-get-remote-range "stats" "@1$1..@1$>")))
  (setq abilityIndex (dnd-index ability abilities))
  (setq value (nth abilityIndex values))
  (if (string-match-p "^-?\\(?:\\(?:\\(?:0\\|[1-9][0-9]*\\)?[.][0-9]+\\)\\|\\(?:0\\|[1-9][0-9]*\\)\\)\\(?:e-?\\(?:0\\|[1-9][0-9]*\\)?\\)?$" (format "%s" value))
      (string-to-number (format "%s" value))
    (format "%s" value)))


(defun dnd-output-ability-constants (table)
  "Outputs constants for the Ability Modifiers"
  (setq consts (mapcar (lambda (row) (format "#+CONSTANTS: %s=%s\n" (nth 0 row) (if (string= (format "%s" (nth 1 row)) "X") (format "\"%s\"" (nth 1 row)) (if (string= (format "%s" (nth 1 row)) "-") (format "\"%s\"" (nth 1 row)) (nth 1 row))))) table))
  (while consts
    (if (string= "#+CONSTANTS: Allowed=27\n" (car consts))
        (setq consts (cdr consts))
      (if (string= "#+CONSTANTS: =\n" (car consts))
          (setq consts (cdr consts))
        (progn (princ (car consts))
               (setq consts (cdr consts)))))))


(defun dnd-calc-point-buy-cost (score)
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

  ;; (search-forward "BEGIN_SRC")
  ;; (org-babel-execute-src-block)
  ;; (search-forward ":results:")
  ;; (next-line)
  ;; (backward-word)

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "elisp")))  ;don't ask for ditaa
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
  
  ;;(org-ctrl-c-ctrl-c)

  (unwind-protect
      (while (string= "1" "1")
        (search-forward "TBLFM")
        (org-ctrl-c-ctrl-c))
    (progn 
      (goto-char (point-min))
      ;; (search-forward "* Constants")
      ;; (outline-hide-leaves)
      (goto-char starting-point))))


;; ==== Minor Mode ===
(define-minor-mode dnd-mode
  "Toggles global dnd-mode"
  nil
  :global t
  :lighter " dnd"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c r") 'rtd)
            (define-key map (kbd "C-c e") 'org-table-recalculate-buffer-tables)
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
