;;;----------------------------------------------------------------------------
;;; GULLY.LSP
;;;
;;; Channel-line-aware block inserter for stormwater gullies / catchpits.
;;;
;;; Commands:
;;;   GULLY        - insert gullies, snapping to a nearby channel line
;;;   GULLYSETUP   - configure block name / layer names / radius / scale
;;;                  for your own drawing standards (run this first time,
;;;                  or any time you switch to a project with different
;;;                  layer names)
;;;
;;; BEHAVIOUR
;;;   1. Prompts for an insertion point (same as -INSERT would).
;;;   2. Searches for the nearest LINE / LWPOLYLINE / (2D or 3D)POLYLINE /
;;;      ARC on the configured "channel line" layer, within
;;;      *gully-search-radius* drawing units of the picked point.
;;;      - Searches entities directly in the current space (model/paper).
;;;      - ALSO searches one level inside any block or xref reference
;;;        (so a channel line that lives inside an attached xref is found
;;;        too), correctly transforming the nested geometry into world
;;;        coordinates.
;;;   3. If a channel line is found within range:
;;;        - The block is ROTATED so it is PERPENDICULAR to the line's
;;;          bearing at the closest point.
;;;        - The block is then shifted along that perpendicular direction
;;;          so that the EDGE of the block's own geometry (the edge on the
;;;          side the user actually clicked) sits exactly ON the line,
;;;          rather than the insertion point itself sitting on the line.
;;;        - Z is always forced to 0, even if the channel line has
;;;          elevation.
;;;   4. If no channel line is found nearby, the block is inserted at the
;;;      picked point with rotation 0 and default scale (old -INSERT
;;;      behaviour).
;;;
;;; CONFIGURATION
;;;   This routine doesn't assume any particular office CAD standard.
;;;   Block name and layer names are set from the generic defaults below,
;;;   but every drawing/office is different, so:
;;;     - Run GULLYSETUP once per session (or per project) to interactively
;;;       set the block name, channel-line layer, gully layer, search
;;;       radius and insertion scale - just press Enter to keep a default.
;;;     - Or edit the DEFAULT CONFIG block below to hard-code your own
;;;       office standards permanently.
;;;   Settings chosen via GULLYSETUP last for the current AutoCAD session.
;;;
;;; ASSUMPTIONS
;;;   - Drawings are flat / plan view: current UCS = WCS, extrusion = (0,0,1).
;;;   - The Gully block is inserted at uniform scale (X=Y=Z).
;;;   - Xref / block nesting is a MAXIMUM of one level deep - i.e. the
;;;     channel line lives directly in the xref, not in a block that is
;;;     itself nested inside that xref.
;;;
;;; NOTE: entities that live inside an attached xref frequently report
;;; their Layer property as "XREFNAME|<layer name>" rather than the bare
;;; layer name. This is handled automatically.
;;;
;;; Author  : AutoCAD Lisps 4 All  (MIT Licence)
;;;----------------------------------------------------------------------------

(vl-load-com)

;; =========================== DEFAULT CONFIG =================================
;; These are just starting points - change them here for a permanent default,
;; or override them per-session with the GULLYSETUP command (see above).
(setq *gully-block-name*    "Gully")            ;; block to insert
(setq *gully-target-layer*  "Channel-Line")      ;; layer to snap to
(setq *gully-gully-layer*   "Gully")             ;; layer to insert onto
(setq *gully-reset-layer*   "0")                 ;; layer to return to on exit
(setq *gully-search-radius* 1.0)      ;; drawing units. If your dwg units are
                                       ;; millimetres, change this to 500.0
(setq *gully-scale*         1.0)
(setq *gully-stack-gap*     0.0)   ;; extra spacing between stacked gullies
(setq *gully-configured*    nil)   ;; becomes T once GULLYSETUP has been run
;; =============================================================================


;; ------------------------------------------------------------- basic utils -
(defun gu:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun gu:curspace ()
  (vla-get-Block (vla-get-ActiveLayout (gu:doc))))

(defun gu:dist2d (p1 p2)
  (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))

(defun gu:normalize2d (v / m)
  (setq m (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))
  (if (< m 1e-9)
    '(1.0 0.0)
    (list (/ (car v) m) (/ (cadr v) m))))

(defun gu:layer-match-p (layerName target / ln tg)
  (setq ln (strcase layerName) tg (strcase target))
  (or (= ln tg) (wcmatch ln (strcat "*|" tg))))

;; Convert whatever vla-getboundingbox handed back (variant-wrapped
;; safearray, raw safearray, or already a list) into a plain (x y z) list.
(defun gu:corner->list (v / r)
  (setq r (vl-catch-all-apply
            '(lambda () (vlax-safearray->list (vlax-variant-value v)))))
  (if (not (vl-catch-all-error-p r))
    r
    (progn
      (setq r (vl-catch-all-apply '(lambda () (vlax-safearray->list v))))
      (if (not (vl-catch-all-error-p r))
        r
        (if (and (listp v) (numberp (car v))) v nil)))))

;; Safe bounding box: returns (minlist maxlist) in WCS, or nil on failure.
;; vla-getboundingbox writes its two corners through BY-REFERENCE output
;; args. Those bindings only survive if the symbols are bound in the same
;; scope the call executes in, so we MUST call it inside a lambda rather
;; than pass 'lo 'hi inside a data list to vl-catch-all-apply.
(defun gu:safe-bbox (obj / lo hi res loL hiL)
  (setq res (vl-catch-all-apply
              '(lambda () (vla-getboundingbox obj 'lo 'hi))))
  (if (or (vl-catch-all-error-p res) (null lo) (null hi))
    nil
    (progn
      (setq loL (gu:corner->list lo)
            hiL (gu:corner->list hi))
      (if (and loL hiL) (list loL hiL) nil))))

(defun gu:ok-curve-type-p (obj / nm)
  (setq nm (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
  (if (vl-catch-all-error-p nm)
    nil
    (member nm '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline" "AcDbArc"))))

;; ---- block-reference transform helpers (local block space <-> world) ------
(defun gu:local->world (localPt insPt rot xsc ysc basePt / dx dy sx sy)
  (setq dx (- (car localPt) (car basePt))
        dy (- (cadr localPt) (cadr basePt)))
  (setq sx (* dx xsc) sy (* dy ysc))
  (list
    (+ (car insPt)  (- (* sx (cos rot)) (* sy (sin rot))))
    (+ (cadr insPt) (+ (* sx (sin rot)) (* sy (cos rot))))
    0.0))

(defun gu:world->local (worldPt insPt rot xsc ysc basePt / dx dy cs sn lx ly)
  (setq dx (- (car worldPt) (car insPt))
        dy (- (cadr worldPt) (cadr insPt)))
  (setq cs (cos rot) sn (sin rot))
  (setq lx (+ (* dx cs) (* dy sn))
        ly (+ (* (- dx) sn) (* dy cs)))
  (if (equal xsc 0.0 1e-9) (setq xsc 1.0))
  (if (equal ysc 0.0 1e-9) (setq ysc 1.0))
  (list (+ (/ lx xsc) (car basePt))
        (+ (/ ly ysc) (cadr basePt))
        0.0))

(defun gu:local-vec->world (localVec rot xsc ysc / vx vy)
  (setq vx (* (car localVec) xsc) vy (* (cadr localVec) ysc))
  (list
    (- (* vx (cos rot)) (* vy (sin rot)))
    (+ (* vx (sin rot)) (* vy (cos rot)))
    0.0))

;; ---------------------------------------------------- generic closest-point-
;; Returns (closestPt tangentVec) in the SAME coordinate space as obj/qPt,
;; or nil if it fails / obj is not a curve.
(defun gu:closest-on-curve (obj qPt / cp par tan)
  (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj qPt)))
  (if (vl-catch-all-error-p cp)
    nil
    (progn
      (setq par (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list obj cp)))
      (if (vl-catch-all-error-p par)
        nil
        (progn
          (setq tan (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list obj par)))
          (if (vl-catch-all-error-p tan)
            nil
            (list cp tan)))))))

;; --------------------------------------------------------- top-level search-
;; entities directly in the current space, on the target layer
(defun gu:search-toplevel (pt radius / ss n i ent obj hit d best)
  (setq best nil)
  (setq ss (ssget "_X" (list '(0 . "LINE,LWPOLYLINE,POLYLINE,ARC")
                              (cons 8 *gully-target-layer*))))
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq hit (gu:closest-on-curve obj pt))
        (if hit
          (progn
            (setq d (gu:dist2d pt (car hit)))
            (if (and (<= d radius) (or (null best) (< d (car best))))
              (setq best (list d (car hit) (cadr hit))))))
        (setq i (1+ i)))))
  best)

;; ------------------------------------------------------------ nested search-
;; one level inside block / xref references
(defun gu:pt-near-box-p (pt bmin bmax radius)
  (and (>= (car pt)  (- (car bmin)  radius))
       (<= (car pt)  (+ (car bmax)  radius))
       (>= (cadr pt) (- (cadr bmin) radius))
       (<= (cadr pt) (+ (cadr bmax) radius))))

(defun gu:search-nested (pt radius / ss n i insEnt insObj bbr bmn bmx
                          blkName blkObj basePt insPt rot xsc ysc
                          localQ e lyr hit localCP worldCP worldTan d best)
  (setq best nil)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq insEnt (ssname ss i))
        (setq insObj (vlax-ename->vla-object insEnt))
        (setq bbr (gu:safe-bbox insObj))
        (if bbr
          (progn
            (setq bmn (car bbr))
            (setq bmx (cadr bbr))
            (if (gu:pt-near-box-p pt bmn bmx radius)
              (progn
                (setq blkName (vl-catch-all-apply 'vla-get-EffectiveName (list insObj)))
                (if (vl-catch-all-error-p blkName)
                  (setq blkName (vl-catch-all-apply 'vla-get-Name (list insObj))))
                (setq blkObj (vl-catch-all-apply 'vla-item
                               (list (vla-get-Blocks (gu:doc)) blkName)))
                (if (not (vl-catch-all-error-p blkObj))
                  (progn
                    (setq insPt (gu:corner->list (vla-get-InsertionPoint insObj)))
                    (setq rot (vla-get-Rotation insObj))
                    (setq xsc (vla-get-XScaleFactor insObj))
                    (setq ysc (vla-get-YScaleFactor insObj))
                    (setq basePt (vl-catch-all-apply 'vla-get-Origin (list blkObj)))
                    (if (vl-catch-all-error-p basePt)
                      (setq basePt '(0.0 0.0 0.0))
                      (setq basePt (gu:corner->list basePt)))
                    (if (null basePt) (setq basePt '(0.0 0.0 0.0)))
                    (setq localQ (gu:world->local pt insPt rot xsc ysc basePt))
                    (vlax-for e blkObj
                      (setq lyr (vl-catch-all-apply 'vla-get-Layer (list e)))
                      (if (and (not (vl-catch-all-error-p lyr))
                               (gu:layer-match-p lyr *gully-target-layer*)
                               (gu:ok-curve-type-p e))
                        (progn
                          (setq hit (gu:closest-on-curve e localQ))
                          (if hit
                            (progn
                              (setq localCP (car hit))
                              (setq worldCP (gu:local->world localCP insPt rot xsc ysc basePt))
                              (setq d (gu:dist2d pt worldCP))
                              (if (and (<= d radius) (or (null best) (< d (car best))))
                                (progn
                                  (setq worldTan (gu:local-vec->world (cadr hit) rot xsc ysc))
                                  (setq best (list d worldCP worldTan)))))))))))))))
        (setq i (1+ i)))))
  best)

;; --------------------------------------------------------- block local bbox-
;; Minimum local-X extent of the whole "Gully" block definition, i.e. how
;; far the block's geometry extends past its insertion point in the
;; direction that will become "perpendicular to the channel line" once
;; rotated. Used to work out the edge-snap offset.
(defun gu:block-local-xmin (blkname / blkObj e bbr mn mx xmin)
  (setq xmin nil)
  (setq blkObj (vl-catch-all-apply 'vla-item (list (vla-get-Blocks (gu:doc)) blkname)))
  (if (vl-catch-all-error-p blkObj)
    0.0
    (progn
      (vlax-for e blkObj
        (setq bbr (gu:safe-bbox e))
        (if bbr
          (progn
            (setq mn (car bbr))
            (if (or (null xmin) (< (car mn) xmin)) (setq xmin (car mn))))))
      (if xmin xmin 0.0))))

;; Local-space bounding box of the whole block definition:
;; returns (xmin ymin xmax ymax) or nil.
(defun gu:block-local-bbox (blkname / blkObj e bbr mn mx x0 y0 x1 y1)
  (setq blkObj (vl-catch-all-apply 'vla-item (list (vla-get-Blocks (gu:doc)) blkname)))
  (if (vl-catch-all-error-p blkObj)
    nil
    (progn
      (vlax-for e blkObj
        (setq bbr (gu:safe-bbox e))
        (if bbr
          (progn
            (setq mn (car bbr) mx (cadr bbr))
            (if (or (null x0) (< (car mn)  x0)) (setq x0 (car mn)))
            (if (or (null y0) (< (cadr mn) y0)) (setq y0 (cadr mn)))
            (if (or (null x1) (> (car mx)  x1)) (setq x1 (car mx)))
            (if (or (null y1) (> (cadr mx) y1)) (setq y1 (cadr mx))))))
      (if x0 (list x0 y0 x1 y1) nil))))

;; Collect insertion points of existing Gully block refs in the current
;; space (world coords), optionally only those within maxdist of nearPt.
(defun gu:gather-gullies (nearPt maxdist / pts e nm ip)
  (setq pts '())
  (vlax-for e (gu:curspace)
    (if (= (vl-catch-all-apply 'vla-get-ObjectName (list e)) "AcDbBlockReference")
      (progn
        (setq nm (vl-catch-all-apply 'vla-get-EffectiveName (list e)))
        (if (vl-catch-all-error-p nm)
          (setq nm (vl-catch-all-apply 'vla-get-Name (list e))))
        (if (and (= (type nm) 'STR)
                 (= (strcase nm) (strcase *gully-block-name*)))
          (progn
            (setq ip (gu:corner->list (vla-get-InsertionPoint e)))
            (if (and ip (or (null nearPt) (null maxdist)
                            (<= (gu:dist2d ip nearPt) maxdist)))
              (setq pts (cons ip pts))))))))
  pts)

;; ordered step sequence 0, 1, -1, 2, -2, ... up to maxk
(defun gu:kseq (maxk / lst k)
  (setq lst (list 0) k 1)
  (while (<= k maxk)
    (setq lst (append lst (list k (- k))))
    (setq k (1+ k)))
  lst)

(defun gu:slot-free-p (cand existing thresh / free)
  (setq free T)
  (foreach g existing
    (if (< (gu:dist2d cand g) thresh) (setq free nil)))
  free)

;; Starting from idealPt, step along tvec by `pitch` until a slot is found
;; that clears all existing gullies (nearest free slot, both directions).
(defun gu:free-slot (idealPt tvec pitch existing thresh / chosen cand)
  (if (or (null existing) (<= pitch 1e-6))
    idealPt
    (progn
      (setq chosen nil)
      (foreach k (gu:kseq 100)
        (if (null chosen)
          (progn
            (setq cand (list (+ (car idealPt)  (* k pitch (car tvec)))
                             (+ (cadr idealPt) (* k pitch (cadr tvec)))
                             0.0))
            (if (gu:slot-free-p cand existing thresh)
              (setq chosen cand)))))
      (if chosen chosen idealPt))))

;; -------------------------------------------------------------- inserters -
(defun gu:do-insert-cmd (pt rot / rotStr)
  (setq rotStr (angtos rot))
  (command "_.-insert" *gully-block-name* pt *gully-scale* rotStr))

(defun gu:do-insert (pt rot / space res)
  (if (tblsearch "BLOCK" *gully-block-name*)
    (progn
      (setq space (gu:curspace))
      (setq res (vl-catch-all-apply 'vla-InsertBlock
                  (list space (vlax-3d-point pt) *gully-block-name*
                        *gully-scale* *gully-scale* *gully-scale* rot)))
      (if (vl-catch-all-error-p res)
        (gu:do-insert-cmd pt rot)
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list res *gully-gully-layer*))
          (vla-Update res))))
    (gu:do-insert-cmd pt rot)))

;; ----------------------------------------------------------------- main --
;; place a single gully at picked point pt (already Z=0'd)
(defun gu:place-one (pt / res1 res2 best d closestPt tanVec tvec nvec dp
                      ang finalPt bbox xmin gh pitch idealPt existing thresh)
  (setq res1 (gu:search-toplevel pt *gully-search-radius*))
  (setq res2 (gu:search-nested   pt *gully-search-radius*))

  (setq best
    (cond
      ((and res1 res2) (if (< (car res1) (car res2)) res1 res2))
      (res1 res1)
      (res2 res2)
      (T nil)))

  (if best
    (progn
      (setq d         (car best))
      (setq closestPt (cadr best))
      (setq tanVec    (caddr best))

      (setq tvec (gu:normalize2d (list (car tanVec) (cadr tanVec))))
      (setq nvec (list (- (cadr tvec)) (car tvec)))

      ;; orient nvec toward the side the user actually picked
      (setq dp (+ (* (- (car pt) (car closestPt)) (car nvec))
                  (* (- (cadr pt) (cadr closestPt)) (cadr nvec))))
      (if (< dp 0.0) (setq nvec (list (- (car nvec)) (- (cadr nvec)))))

      (setq ang (atan (cadr nvec) (car nvec)))
      (setq closestPt (list (car closestPt) (cadr closestPt) 0.0))

      ;; block extents (scaled): xmin -> edge-snap offset, gh -> stack pitch
      (setq bbox (gu:block-local-bbox *gully-block-name*))
      (if bbox
        (setq xmin (* (car bbox) *gully-scale*)
              gh   (* (- (cadddr bbox) (cadr bbox)) *gully-scale*))
        (setq xmin (* (gu:block-local-xmin *gully-block-name*) *gully-scale*)
              gh   0.0))

      ;; ideal edge-snapped insertion point
      (setq idealPt
        (list
          (+ (car closestPt)  (* (- xmin) (car nvec)))
          (+ (cadr closestPt) (* (- xmin) (cadr nvec)))
          0.0))

      ;; if an existing gully occupies that spot, step along the line
      (setq pitch  (+ gh *gully-stack-gap*))
      (setq thresh (* pitch 0.9))
      (setq existing (gu:gather-gullies idealPt (* pitch 120.0)))
      (setq finalPt  (gu:free-slot idealPt tvec pitch existing thresh))

      (gu:do-insert finalPt ang)
      (if (equal finalPt idealPt 1e-6)
        (princ (strcat "\nGully snapped to channel line (" (rtos d 2 2)
                        " units from pick point)."))
        (princ "\nGully snapped to channel line and offset to avoid existing gully.")))
    (progn
      (gu:do-insert pt 0.0)
      (princ "\nNo channel line found within radius - inserted with default rotation.")))
  (princ))

;; --------------------------------------------------- interactive settings -
;; Prompt for a string, showing/accepting the current value as default.
(defun gu:ask (prompt default / input)
  (setq input (getstring T (strcat prompt " <" default ">: ")))
  (if (or (null input) (= (strcase input) "")) default input))

;; Prompt for a positive real number, showing/accepting the current value.
(defun gu:ask-real (prompt default / input)
  (setq input (getreal (strcat prompt " <" (rtos default 2 3) ">: ")))
  (if (null input) default input))

(defun c:GULLYSETUP ( / )
  (princ "\n--- GULLY SETUP ---")
  (princ "\nPress Enter at any prompt to keep the current value shown in <>.")
  (setq *gully-block-name*
    (gu:ask "\nBlock name to insert" *gully-block-name*))
  (setq *gully-target-layer*
    (gu:ask "\nChannel-line layer to snap to" *gully-target-layer*))
  (setq *gully-gully-layer*
    (gu:ask "\nLayer to insert the gully onto" *gully-gully-layer*))
  (setq *gully-reset-layer*
    (gu:ask "\nLayer to return to when GULLY finishes" *gully-reset-layer*))
  (setq *gully-search-radius*
    (gu:ask-real "\nSearch radius (drawing units)" *gully-search-radius*))
  (setq *gully-scale*
    (gu:ask-real "\nInsertion scale" *gully-scale*))
  (setq *gully-stack-gap*
    (gu:ask-real "\nExtra gap between stacked gullies" *gully-stack-gap*))

  (if (not (tblsearch "BLOCK" *gully-block-name*))
    (princ (strcat "\n[!] NOTE: block \"" *gully-block-name*
                    "\" is not yet defined in this drawing - "
                    "insert/define it before running GULLY.")))
  (if (not (tblsearch "LAYER" *gully-target-layer*))
    (princ (strcat "\n[i] NOTE: layer \"" *gully-target-layer*
                    "\" does not exist yet in this drawing.")))

  (setq *gully-configured* T)
  (princ "\nGULLY settings updated for this session. Type GULLY to run.")
  (princ))

(defun c:GULLY (/ *error* oldlyr pt)

  ;; ensure the target insert layer exists; select it as current
  (defun *error* (msg)
    (setvar "CLAYER" *gully-reset-layer*)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nGULLY error: " msg)))
    (princ))

  ;; First-time-per-session sanity check: if the block or channel layer
  ;; can't be found and the user hasn't run GULLYSETUP yet, point them at it
  ;; rather than silently failing or inserting on the wrong layer.
  (if (and (not *gully-configured*)
           (or (not (tblsearch "BLOCK" *gully-block-name*))
               (not (tblsearch "LAYER" *gully-target-layer*))))
    (progn
      (princ "\n[i] GULLY is using default settings that may not match this drawing.")
      (princ (strcat "\n    Block: \"" *gully-block-name*
                      "\"  Channel layer: \"" *gully-target-layer* "\""))
      (princ "\n    Run GULLYSETUP to change these, or continue to use the defaults.")))

  (setq oldlyr (getvar "CLAYER"))
  (if (tblsearch "LAYER" *gully-gully-layer*)
    (setvar "CLAYER" *gully-gully-layer*)
    (princ (strcat "\nWARNING: layer \"" *gully-gully-layer*
                    "\" not found - inserting on current layer.")))

  (princ "\nGULLY: pick insertion points. Press Enter or Esc to finish.")
  (while (setq pt (getpoint "\nSpecify Gully insertion point <exit>: "))
    (gu:place-one (list (car pt) (cadr pt) 0.0)))

  ;; finished normally -> drop back to reset layer
  (setvar "CLAYER" *gully-reset-layer*)
  (princ "\nGULLY finished.")
  (princ))

(princ "\nGULLY.LSP loaded - type GULLY to insert a channel-aware gully block,")
(princ "\nor GULLYSETUP to match it to your own block/layer names.")
(princ)