(vl-load-com)
(defun C:VisForward (/ Layer1_Name Layer1-Color Layer1-Ltype ForwardVisDistance inc js SelPline SelPline_Length Counter SPoint EPoint)
  (setq Layer1_Name "Insert Layer Name Here"  ;Set Layer Name Here
        Layer1-Color "210"                    ;Set Color by Number or Name
        Layer1-Ltype "Continuous"             ;Set Linetype Name Here
  )
  
	;Set the Layer for the New Polylines
  (if (not (tblsearch "LAYER" Layer1_Name)) 
    (command "-LAYER" "_M" Layer1_Name "_C" Layer1-Color Layer1_Name "" ""))
  (command "clayer" "" Layer1_Name "")
  
  ; Predefined variables
  (setq ForwardVisDistance 15.0)    ; Default forward visibility distance
  (setq inc 2.0)        ; Default increment/resolution
  
  ; Prompt user to accept/change defaults
  (initget "Yes No")
  (if (= (getkword (strcat "\nUse default settings? [Yes/No] <Yes>: ")) "No")
    (progn
      (setq ForwardVisDistance (getdist (strcat "\nEnter default visibility distance <" (rtos ForwardVisDistance 2 2) ">: ")))
      (setq inc (getdist (strcat "\nEnter default increment/resolution <" (rtos inc 2 2) ">: ")))
    )
  )
  
  (princ (strcat"\nSelect Polyline:"))
	(while
		(not
			(setq js
				(ssget "_+.:E:S" 
					(list
						(cons 0 "*POLYLINE,ARC,SPLINE")
						(cons 67 (if (eq (getvar "CVPORT") 2) 0 1))
						(cons 410 (if (eq (getvar "CVPORT") 2) "Model" (getvar "CTAB")))
						(cons -4 "<NOT")
							(cons -4 "&") (cons 70 112)
						(cons -4 "NOT>")
					)
				)
			)
		)
	)
  
  ;----- Assign the Selected Polyline to a VLA-Object and get its Length and other properties
	(setq
		SelPline (vlax-ename->vla-object (ssname js 0))
		SelPline_Length (vlax-curve-getDistAtParam SelPline (vlax-curve-getEndParam SelPline))
    Counter 0.0
    SPoint nil
    EPoint nil
  )
  
  ;----- Check Forward Visi Distance and Increment against the Length of the Selected Polyline
  (while (>= ForwardVisDistance SelPline_Length)
    (if (>= ForwardVisDistance SelPline_Length) (princ "\nDistance execeded length of Selected Polyline"))
  )
  (while (>= inc SelPline_Length)
    (if (>= inc SelPline_Length) (princ "\nDistance execeded length of Selected Polyline"))
  )
  
  ;----- Function to create Forward Visibility Polylines
	(while (< Counter SelPline_Length)
		(setq EPoint (vlax-curve-getPointAtDist SelPline (+ Counter ForwardVisDistance)))
    (setq SPoint (vlax-curve-getPointAtDist SelPline Counter))
    (entmake
      (append
        '(
          (0 . "LWPOLYLINE")
          (100 . "AcDbEntity")
          (67 . 0)
          (410 . "Model")
          (8 . "Visibility Forward 1.5m x X")
          (6 . "ByLayer")
          (370 . -2)
          (100 . "AcDbPolyline")
          (90 . 2)
        )
        (list (cons 10 SPoint))
        (list (cons 10 EPoint))
        '((210 0.0 0.0 1.0))
      )
    )
   
    ;(setq SPoint (vlax-curve-getPointAtDist SelPline (+ Counter inc)))
    (setq Counter (+ Counter inc))
	)
    (command "clayer" "" "0" "")
  
)