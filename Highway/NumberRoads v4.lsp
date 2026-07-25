;;; =========================================================================
;;; USER CONFIGURATION - EDIT THESE VARIABLES EASILY
;;; =========================================================================
(setq road-text-height 2.5)          ; How tall the road number text should be
(setq road-text-layer "Road-Labels") ; The layer where the text numbers will go
(setq road-text-color 2)             ; Color index for the text numbers (2 = Yellow)

(setq road-default-prefix "Road")    ; The default name if you just press Enter

(setq error-circle-radius 5.0)       ; How big the warning circles should be
(setq error-circle-layer "Unnamed-Roads") ; Layer for roads that were skipped
(setq error-circle-color 1)          ; Color for warning circles (1 = Red)
;;; =========================================================================

(vl-load-com) ; Loads advanced geometry functions for handling polylines

;;; MAIN COMMAND
(defun c:NumberRoads ( / road-ss user-prefix first-road start-point i entity)
  
  ;; Reset our trackers
  (setq global:road-counter 1)
  (setq global:all-roads '())
  (setq global:visited-list '())
  
  ;; 1. Select the entire road network at once
  (princ "\nSelect all polylines and lines that make up the road network: ")
  (setq road-ss (ssget '((0 . "LINE,*POLYLINE"))))
  
  (if road-ss
    (progn
      ;; Convert the selection set into a clean list of entities for the algorithm
      (setq i 0)
      (while (setq entity (ssname road-ss i))
        (setq global:all-roads (cons entity global:all-roads))
        (setq i (1+ i))
      )
      
      ;; 2. Ask the user for the naming prefix
      (setq user-prefix (getstring t (strcat "\nEnter road prefix <" road-default-prefix ">: ")))
      ;; If the user just hit Enter (blank), use the default configuration prefix
      (if (= user-prefix "")
        (setq global:road-prefix road-default-prefix)
        (setq global:road-prefix user-prefix)
      )
      
      ;; 3. Select the starting road
      (setq first-road (car (entsel "\nSelect the FIRST road (from your selection): ")))
      
      ;; 4. Select the driving direction start point
      (setq start-point (getpoint "\nClick near the START point of the first road: "))
      
      ;; Run the numbering drive if everything was selected properly
      (if (and first-road start-point)
        (progn
          ;; Start the main driving loop
          (explore-road-logic first-road start-point)
          
          ;; SCAN FOR UNNAMED ROADS: Look for any lines left behind
          (foreach entity global:all-roads
            (if (not (member entity global:visited-list))
              (action-mark-unnamed entity)
            )
          )
          
          (princ "\nRoad numbering complete! Skipped roads have been marked with red circles.")
        )
        (princ "\nMissing first road or start point selection.")
      )
    )
    (princ "\nNo roads were selected.")
  )
  (princ)
)

;;; CORE LOGIC FUNCTION
(defun explore-road-logic (current-road current-start-pt / road-num intersections intersection-info next-road next-pt)
  (setq road-num global:road-counter)
  (setq global:visited-list (cons current-road global:visited-list))
  
  ;; Place the rotated, plan-readable MTEXT label on the current road
  (action-label-road current-road road-num)
  
  ;; Scan and find intersections only from our approved network list
  (setq intersections (helper-find-and-sort-intersections current-road current-start-pt))
  
  (foreach intersection-info intersections
    (setq next-pt (car intersection-info))
    (setq next-road (cdr intersection-info))
    
    (if (not (member next-road global:visited-list))
      (progn
        (setq global:road-counter (+ global:road-counter 1))
        (explore-road-logic next-road next-pt)
      )
    )
  )
)

;;; MTEXT CREATION FUNCTION (WITH ROTATION & READABILITY LOGIC)
(defun action-label-road (road-entity number / mid-param mid-pt deriv ang readable-ang)
  ;; 1. Find the physical middle point of the line or polyline
  (setq mid-param (/ (vlax-curve-getEndParam road-entity) 2.0))
  (setq mid-pt (vlax-curve-getPointAtParam road-entity mid-param))
  
  ;; 2. Determine the direction (tangent) of the road at that midpoint
  (setq deriv (vlax-curve-getFirstDeriv road-entity mid-param))
  (setq ang (angle '(0 0 0) deriv)) ; Convert vector to angle in radians

  ;; 3. PLAN READABLE LOGIC
  ;; Standard drafting rule: Text should read left-to-right, bottom-to-top.
  ;; In radians: angles between PI/2 (90deg) and 3*PI/2 (270deg) are "upside down".
  (if (and (> ang (/ pi 2.0)) (<= ang (* pi 1.5)))
    (setq readable-ang (+ ang pi)) ; Flip 180 degrees if necessary
    (setq readable-ang ang)         ; Keep original angle otherwise
  )

  ;; 4. Create the MTEXT object using the custom settings and calculated angle
  (entmake (list
    '(0 . "MTEXT")
    '(100 . "AcDbEntity")
    (cons 8 road-text-layer)   ; Applies your layer
    (cons 62 road-text-color)  ; Applies your color
    '(100 . "AcDbMText")
    (cons 10 mid-pt)           ; Places text at the middle of the road
    (cons 40 road-text-height) ; Applies your text size
    (cons 50 readable-ang)     ; <--- APPLIES CALCULATED READABLE ROTATION
    '(71 . 5)                  ; Forces "Middle Center" alignment
    (cons 1 (strcat global:road-prefix " " (itoa number))) ; Text content
  ))
)

;;; WARNING CIRCLE FUNCTION
(defun action-mark-unnamed (road-entity / mid-param mid-pt)
  (setq mid-param (/ (vlax-curve-getEndParam road-entity) 2.0))
  (setq mid-pt (vlax-curve-getPointAtParam road-entity mid-param))
  (entmake (list
    '(0 . "CIRCLE")
    '(100 . "AcDbEntity")
    (cons 8 error-circle-layer)
    (cons 62 error-circle-color)
    '(100 . "AcDbCircle")
    (cons 10 mid-pt)
    (cons 40 error-circle-radius)
  ))
)

;;; GEOMETRY ENGINE
(defun helper-find-and-sort-intersections (road-entity start-pt / road-vla intersections-list current-dist-start other-vla int-points pt dist rel-dist abs-rel-dist)
  (setq road-vla (vlax-ename->vla-object road-entity))
  (setq current-dist-start (vlax-curve-getDistAtPoint road-entity start-pt))
  (if (not current-dist-start) (setq current-dist-start 0.0))
  (setq intersections-list '())
  (foreach other-entity global:all-roads
    (if (and (not (eq road-entity other-entity))
             (not (member other-entity global:visited-list)))
      (progn
        (setq other-vla (vlax-ename->vla-object other-entity))
        (setq int-points (vlax-invoke road-vla 'IntersectWith other-vla 0))
        (if int-points
          (while int-points
            (setq pt (list (car int-points) (cadr int-points) (caddr int-points)))
            (setq int-points (cdddr int-points))
            (if (vlax-curve-getDistAtPoint road-entity pt)
              (progn
                (setq dist (vlax-curve-getDistAtPoint road-entity pt))
                (setq abs-rel-dist (abs (- dist current-dist-start)))
                (if (> abs-rel-dist 0.01)
                  (setq intersections-list (cons (list abs-rel-dist pt other-entity) intersections-list))
                )
              )
            )
          )
        )
      )
    )
  )
  (setq intersections-list (vl-sort intersections-list '(lambda (a b) (< (car a) (car b)))))
  (mapcar '(lambda (x) (cons (cadr x) (caddr x))) intersections-list)
)