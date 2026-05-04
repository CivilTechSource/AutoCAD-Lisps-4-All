;; ============================================================
;;  Pips - Calculate Pipe Slope v1.0.lsp
;;  Calculates the slope of a pipe between two invert levels.
;;
;;  Usage : Type PIPESLOPE at the AutoCAD command line.
;;
;;  Workflow:
;;    1. Pick Point 1 (upstream / higher end)
;;    2. Enter Invert Level at Point 1
;;    3. Pick Point 2 (downstream / lower end)
;;    4. Enter Invert Level at Point 2
;;    5. Results are printed to the command line and written
;;       as a MTEXT object placed near the mid-point of the pipe.
;;
;;  Outputs:
;;    - Horizontal Distance  (drawing units)
;;    - Fall                 (IL1 - IL2, in metres or survey units)
;;    - Slope                1 : X  (e.g. 1:80.0)
;;    - Slope %              (e.g. 1.25%)
;;    - Slope degrees        (e.g. 0.72°)
;;
;;  Notes:
;;    - A negative Fall (IL2 > IL1) means the pipe rises toward
;;      Point 2 and will be flagged as a warning.
;;    - Zero fall is caught and reported without dividing by zero.
;;    - The routine uses the 2-D horizontal distance between the
;;      two picked points, ignoring any Z difference in the snap.
;;
;;  Author  : AutoCAD Lisps 4 All  (MIT Licence)
;;  Version : 1.0
;; ============================================================

(defun C:Pips ( / pt1 pt2 il1 il2 dx dy dist fall slope pct deg midpt txt_height msg)

  ;; ----- Helper: round a real number to N decimal places -----
  (defun rnd (val n / factor)
    (setq factor (expt 10.0 n))
    (/ (float (fix (+ (* val factor) (if (>= val 0) 0.5 -0.5)))) factor)
  )

  (princ "\n--- PIPE SLOPE CALCULATOR ---")

  ;; ---- Step 1: Pick Point 1 ----
  (setq pt1 (getpoint "\nPick Point 1 (Upstream / Start): "))
  (if (null pt1)
    (progn (princ "\n[!] No point selected. Command cancelled.") (exit))
  )

  ;; ---- Step 2: Invert Level at Point 1 ----
  (setq il1 (getreal "\nEnter Invert Level at Point 1: "))
  (if (null il1)
    (progn (princ "\n[!] No invert level entered. Command cancelled.") (exit))
  )

  ;; ---- Step 3: Pick Point 2 ----
  (setq pt2 (getpoint "\nPick Point 2 (Downstream / End): "))
  (if (null pt2)
    (progn (princ "\n[!] No point selected. Command cancelled.") (exit))
  )

  ;; ---- Step 4: Invert Level at Point 2 ----
  (setq il2 (getreal "\nEnter Invert Level at Point 2: "))
  (if (null il2)
    (progn (princ "\n[!] No invert level entered. Command cancelled.") (exit))
  )

  ;; ---- Step 5: Calculations ----

  ;; Horizontal distance (2D, ignoring any Z from snap)
  (setq dx   (- (car  pt2) (car  pt1))
        dy   (- (cadr pt2) (cadr pt1))
        dist (sqrt (+ (* dx dx) (* dy dy)))
  )

  ;; Fall = IL1 - IL2  (positive when pipe falls toward Point 2)
  (setq fall (- il1 il2))

  ;; ---- Step 6: Output ----
  (princ "\n")
  (princ "\n========================================")
  (princ "\n         PIPE SLOPE RESULTS             ")
  (princ "\n========================================")
  (princ (strcat "\n  Point 1 IL    : " (rtos il1   2 3) " m"))
  (princ (strcat "\n  Point 2 IL    : " (rtos il2   2 3) " m"))
  (princ (strcat "\n  Horiz. Dist.  : " (rtos dist  2 3) " m"))
  (princ (strcat "\n  Fall (IL drop): " (rtos fall  2 3) " m"))

  (cond

    ;; Zero distance — cannot calculate
    ((< dist 0.0001)
     (princ "\n  [!] WARNING: Points are coincident — cannot calculate slope.")
     (princ "\n========================================\n")
     (exit)
    )

    ;; Zero fall — level pipe
    ((equal fall 0.0 0.0001)
     (princ "\n  Slope         : LEVEL (0% grade)")
     (princ "\n  [i] No fall between the two invert levels.")
    )

    ;; Rising pipe — warn the user
    ((< fall 0.0)
     (setq slope (/ dist (abs fall))
           pct   (* (/ (abs fall) dist) 100.0)
           deg   (* (atan (/ (abs fall) dist)) (/ 180.0 pi))
     )
     (princ (strcat "\n  Slope         : 1 : " (rtos slope 2 2)))
     (princ (strcat "\n  Slope %%       : " (rtos pct 2 2) "%%"))
     (princ (strcat "\n  Slope (deg)   : " (rtos deg 2 2) " deg"))
     (princ "\n  [!] WARNING: Pipe RISES toward Point 2 (adverse grade).")
    )

    ;; Normal falling pipe
    (T
     (setq slope (/ dist fall)
           pct   (* (/ fall dist) 100.0)
           deg   (* (atan (/ fall dist)) (/ 180.0 pi))
     )
     (princ (strcat "\n  Slope         : 1 : " (rtos slope 2 2)))
     (princ (strcat "\n  Slope %%       : " (rtos pct 2 2) "%%"))
     (princ (strcat "\n  Slope (deg)   : " (rtos deg 2 2) " deg"))
    )

  ) ; end cond

  (princ "\n========================================\n")

  ;; ---- Step 7: Place a label near the pipe mid-point ----
  ;; Text height: 2.5 drawing units — adjust to your drawing scale if needed.
  (setq txt_height 2.5)

  (setq midpt (list (/ (+ (car pt1)  (car pt2))  2.0)
                    (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                    0.0))

  ;; Build the label string
  (cond
    ((< dist 0.0001)
     (setq msg "COINCIDENT POINTS")
    )
    ((equal fall 0.0 0.0001)
     (setq msg (strcat
       "PIPE SLOPE\n"
       "IL1=" (rtos il1 2 3) "m  IL2=" (rtos il2 2 3) "m\n"
       "L="   (rtos dist 2 3) "m  Fall=0.000m\n"
       "Slope: LEVEL"
     ))
    )
    (T
     (setq msg (strcat
       "PIPE SLOPE\n"
       "IL1=" (rtos il1 2 3) "m  IL2=" (rtos il2 2 3) "m\n"
       "L="   (rtos dist 2 3) "m  Fall=" (rtos (abs fall) 2 3) "m\n"
       "1:" (rtos (/ dist (abs fall)) 2 2)
       "  (" (rtos (* (/ (abs fall) dist) 100.0) 2 2) "%)"
       (if (< fall 0.0) "  [ADVERSE GRADE]" "")
     ))
    )
  )

  ;; Create MTEXT entity
  (entmake
    (list
      (cons 0  "MTEXT")
      (cons 100 "AcDbEntity")
      (cons 8  "PIPE_SLOPE")           ; layer name — creates layer if needed
      (cons 100 "AcDbMText")
      (cons 10 midpt)                  ; insertion point
      (cons 40 txt_height)             ; text height
      (cons 41 (* txt_height 40.0))    ; reference width (40× height)
      (cons 71 5)                      ; attachment: middle-centre
      (cons 72 5)                      ; drawing direction: by style
      (cons 1  msg)                    ; text content
    )
  )

  (princ (strcat "[✓] Label placed on layer \"PIPE_SLOPE\" near pipe mid-point."))
  (princ)

) ; end defun

(princ "\n[✓] PipeSlope.lsp loaded.  Type PIPESLOPE to run.\n")
(princ)