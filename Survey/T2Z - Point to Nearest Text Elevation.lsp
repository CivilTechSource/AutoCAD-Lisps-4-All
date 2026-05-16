;;-----------------------------------------------------------------
;; TextToZ Auditor - Version 1.1
;;-----------------------------------------------------------------
;; EDITABLE SETTINGS:
(setq T2Z_TextHeight 0.5)      ;; Height of the temporary check text
(setq T2Z_LayerName  "T2Z-CHECK") ;; Layer for the temporary text
(setq T2Z_Color      4)        ;; Color for the text (4 = Cyan)
;;-----------------------------------------------------------------

(defun c:T2Z (/ ssPoints ssText i j ptEnt ptData ptXY textEnt textData 
                    textXY textStr textVal dist minDist nearestVal sh res)
  (vl-load-com)
  (setvar "CMDECHO" 0)

  ;; 1. INITIAL POPUP PRE-CHECK
  (setq sh (vlax-create-object "WScript.Shell"))
  (setq res (vlax-invoke-method sh 'Popup 
            "This will update Point/Block Z-values based on the nearest text.\n\nContinue?" 
            0 "Text To Z Auditor" 33))
  (vlax-release-object sh)

  (if (= res 1) ;; 1 = OK
    (progn
      ;; 2. SELECTION
      (princ "\nStep 1: Select Points or Blocks to update...")
      (if (setq ssPoints (ssget '((0 . "POINT,INSERT"))))
        (progn
          (princ "\nStep 2: Scanning drawing for Text/MText...")
          (setq ssText (ssget "_X" '((0 . "TEXT,MTEXT"))))

          (if ssText
            (progn
              ;; Create the Audit Layer
              (if (not (tblsearch "LAYER" T2Z_LayerName))
                (command "._-LAYER" "_MAKE" T2Z_LayerName "_COLOR" T2Z_Color "" "")
              )

              (setq i 0)
              (princ "\nProcessing... Please wait.")
              
              (repeat (sslength ssPoints)
                (setq ptEnt  (ssname ssPoints i)
                      ptData (entget ptEnt)
                      ptXY   (list (cadr (assoc 10 ptData)) (caddr (assoc 10 ptData)))
                      minDist 1e99
                      nearestVal nil
                      j 0)

                ;; Find Nearest Text
                (repeat (sslength ssText)
                  (setq textEnt  (ssname ssText j)
                        textData (entget textEnt)
                        textXY   (list (cadr (assoc 10 textData)) (caddr (assoc 10 textData)))
                        dist     (distance ptXY textXY))

                  (if (< dist minDist)
                    (progn
                      (setq minDist dist
                            textStr (cdr (assoc 1 textData))
                            nearestVal (atof textStr)))
                  )
                  (setq j (1+ j))
                )

                ;; Apply Z-Value and Create Audit Text
                (if (and nearestVal)
                  (progn
                    ;; Update the Point/Block
                    (setq ptData (subst (list 10 (car ptXY) (cadr ptXY) nearestVal) (assoc 10 ptData) ptData))
                    (entmod ptData)

                    ;; Create the "Audit" Text showing the new Z
                    (entmake (list
                               '(0 . "TEXT")
                               (cons 10 (list (+ (car ptXY) 0.2) (+ (cadr ptXY) 0.2) nearestVal)) ;; Offset slightly
                               (cons 40 T2Z_TextHeight)
                               (cons 1  (strcat "Z:" (rtos nearestVal 2 2))) ;; Format: Z:12.50
                               (cons 8  T2Z_LayerName)
                               (cons 62 T2Z_Color)
                             ))
                  )
                )
                (setq i (1+ i))
              )
              
              (setvar "CLAYER" "0")
              (princ (strcat "\nSuccess! " (itoa i) " points updated. Check layer: " T2Z_LayerName))
            )
            (princ "\nError: No text found in drawing.")
          )
        )
        (princ "\nError: No points or blocks selected.")
      )
    )
    (princ "\nOperation cancelled.")
  )
  
  (setvar "CMDECHO" 1)
  (princ)
)