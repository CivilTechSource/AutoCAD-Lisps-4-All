(defun c:PL2Swale ( / *error* pl-ent obj bw depth slope halfBW sideOffset totalOffset 
                   pStart pEnd lineR lineL rStart rEnd lStart lEnd cap1 cap2 oR oL
                   swaleBottom swaleBottomObj currentLayer topSwale)

  (vl-load-com)

  (defun *error* (msg)
    (setvar "CMDECHO" 1)
    (princ)
  )

  
  ;; 1. Inputs
  (setq pl-ent (car (entsel "\nSelect swale centerline polyline: ")))
  (if (not pl-ent) (exit))

  (setq bw    (getreal "\nEnter bottom width: ")
        depth (getreal "\nEnter depth: ")
        slope (getreal "\nEnter side slope (H:1): "))

  (setq halfBW      (/ bw 2.0)
        sideOffset  (* depth slope)
        totalOffset (+ halfBW sideOffset))

  (setq obj (vlax-ename->vla-object pl-ent))
  (setq currentLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; 2. Create the Bottom Offsets
  (setq lineR (vlax-invoke obj 'Offset halfBW))
  (setq lineL (vlax-invoke obj 'Offset (- halfBW)))

  (setq oR (car lineR)
        oL (car lineL))

  ;; Set bottom offsets to current layer
  (vla-put-Layer oR currentLayer)
  (vla-put-Layer oL currentLayer)

  ;; 3. Get Endpoints
  (setq rStart (vlax-curve-getStartPoint oR)
        rEnd   (vlax-curve-getEndPoint   oR)
        lStart (vlax-curve-getStartPoint oL)
        lEnd   (vlax-curve-getEndPoint   oL)
        pStart (vlax-curve-getStartPoint obj)
        pEnd   (vlax-curve-getEndPoint   obj))

  ;; 4. Draw the Caps and Join (corrected to face outward)
  ;; Start Cap - reversed order
  (command "_.PLINE" "_none" lStart "_A" "_CE" "_none" pStart "_none" rStart "")
  (setq cap1 (entlast))

  ;; End Cap - reversed order  
  (command "_.PLINE" "_none" rEnd "_A" "_CE" "_none" pEnd "_none" lEnd "")
  (setq cap2 (entlast))

  ;; 5. Join to create the "Pill" (swale bottom)
  (command "_.PEDIT" (vlax-vla-object->ename oR) "_J" cap1 (vlax-vla-object->ename oL) cap2 "" "")
  (setq swaleBottom (entlast))

  ;; 6. Create Top Bank Offsets from the joined swale bottom
  (setq swaleBottomObj (vlax-ename->vla-object swaleBottom))
  (setq topSwale (vlax-invoke swaleBottomObj 'Offset sideOffset))

  ;; Set top offsets to current layer
  (vla-put-Layer (car topSwale) currentLayer)

  ;; 7. Reverse the top swale polylines
  (command "_.REVERSE" (vlax-vla-object->ename (car topSwale)) "")


  (setvar "CMDECHO" 1)
  (princ "\nSwale created with outward caps and top banks on current layer.")
  (princ)
)