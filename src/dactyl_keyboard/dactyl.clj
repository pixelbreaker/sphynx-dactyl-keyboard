(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)
(def trackpad true)
(def bottom-row false)

(def column-curvature (deg2rad 17))                         ; 15       ; curvature of the columns
(def row-curvature (deg2rad 6))                             ; 5        ; curvature of the rows
(def centerrow (- nrows 2.5))                               ; controls front-back tilt
(def centercol 3)                                           ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (deg2rad 14))                            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))

(def controller-type "rpi-pico") ; elite-c, pro-micro

;; (defn column-offset [column] (cond
;;                                (= column 2) [0 5 -3]
;;                                (= column 3) [0 0 -0.5]
;;                                (>= column 4) [0 -10 6]
;;                                :else [0 0 0]))
(defn column-offset [column] (cond
                               (= column 0) [0 -0.3 -0.2] ;inner
                               (= column 1) [0 0.2 0] ;index
                               (= column 2) [0 3 -3] ;middle
                               (= column 3) [0 -0.5 -0.5] ;ring
                               (= column 4) [0 -11.5 2] ;pinky 1
                                ;; (>= column 5) [0 -8 2] ;pinky mods
                               :else [0 0 0]))

(def thumb-offsets [12 -5.4 -2])

(def keyboard-z-offset 9)                                   ; controls overall height; original=9 with centercol=3; use 16 for centercol=2
(def bottom-height 3)                                    ; plexiglass plate or printed plate
(def extra-width 1.1)                                       ; extra space between the base of keys; original= 2
(def extra-height -1.2)                                      ; original= 0.5

(def wall-z-offset -1)                                      ; -5                ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 1)

(def wall-thickness 2)                                      ; wall thickness parameter; originally 5

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? false)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 13.38)                                   ;; Was 14.1, then 14.25
(def keyswitch-width 13.38)
(def plate-thickness 2)
(def keyswitch-below-plate (- 3.7 plate-thickness))           ; approx space needed below keyswitch

(def sa-profile-key-height 12.8)

(def side-nub-thickness 4)
(def retention-tab-thickness 1.32)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-height (+ keyswitch-height 3.7))
(def mount-width (+ keyswitch-width 3.7))
(def side-tab-width 10.8)

;for the bottom
(def filled-plate
  (->> (cube mount-height mount-width plate-thickness)
       (translate [0 0 (/ plate-thickness 2)])))
(def single-plate
  (let [top-wall (->> (cube mount-width 1.55 plate-thickness)
                      (translate [0
                                  (+ 1 (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.55 mount-height plate-thickness)
                       (translate [(+ 1 (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ pi 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 side-tab-width retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2)) 0 (/ retention-tab-hole-thickness 2)]))
        top-nub-quad (union top-nub
                            (rotate (deg2rad 90) [0 0 1] top-nub)
                            (rotate (deg2rad 180) [0 0 1] top-nub)
                            (rotate (deg2rad 270) [0 0 1] top-nub))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     top-nub-quad)))

;amoeba is 16 mm high
(def switch-bottom
  (translate [0 0 (/ keyswitch-below-plate -2)] (cube 17 17 keyswitch-below-plate)))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1   (let [bl2 (/ 17.5 2)
                       m (/ 16.5 2)
                       key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.5]))
                                     (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 3]))
                                    ;;  (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                    ;;       (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                    ;;       (translate [0 0 12]))
                                     )]
                   (->> key-cap
                        (translate [0 0 (+ 2 plate-thickness)])
                        (color [0.3 0.3 0.3 0.6])))
             2   (let [bl2 sa-length
                       bw2 (/ 18.25 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [0.3 0.3 0.3 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ column-curvature 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ row-curvature 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin row-curvature)))))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* row-curvature (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn (* column-curvature (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn (* column-curvature (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))]

    (->> (case column-style
           :orthographic placed-shape-ortho
           placed-shape)
         (rotate-y-fn tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle) (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle) 0 (Math/sin angle)]
    [0 1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn rotate-around-z [angle position]
  (mmul
   [[(Math/cos angle) (- (Math/sin angle)) 0]
    [(Math/sin angle) (Math/cos angle) 0]
    [0 0 1]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(defn key-places [shape]
  (apply union
         (for [column columns
               row rows
               :when (or (and (.contains [2 3] column) (not= bottom-row false))
                         (not= row lastrow))]
           (->> shape
                (key-place column row)))))
(def key-holes
  (key-places single-plate))
(def key-fills
  (key-places filled-plate))
(def key-space-below
  (key-places switch-bottom))
(def caps
  (key-places (sa-cap 1)))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

; posts are located at the inside corners of the key plates.
; the 'web' is the fill between key plates.
;

(def web-thickness 2)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; fat web post for very steep angles between thumb and finger clusters
; this ensures the walls stay somewhat thicker
(def fat-post-size 1.3)
(def fat-web-post (->> (cube fat-post-size fat-post-size web-thickness)
                       (translate [0 0 (+ (/ web-thickness -2)
                                          plate-thickness)])))

(def fat-post-adj (/ fat-post-size 2))
(def fat-web-post-tr (translate [(- (/ mount-width 2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-tl (translate [(+ (/ mount-width -2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-bl (translate [(+ (/ mount-width -2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-br (translate [(- (/ mount-width 2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn piramid-hulls [top & shapes]
  (apply union
         (map (partial apply hull top)
              (partition 2 1 shapes))))

(def connectors
  (apply union
         (concat
           ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

           ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

           ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-place [rot move shape]
  (->> shape
       (rotate (deg2rad (nth rot 0)) [1 0 0])
       (rotate (deg2rad (nth rot 1)) [0 1 0])
       (rotate (deg2rad (nth rot 2)) [0 0 1])               ; original 10
       (translate thumborigin)
       (translate move)))

; convexer
(defn thumb-r-place [shape] (thumb-place [14 -37 10] [-15 -10 3] shape)) ; right
(defn thumb-m-place [shape] (thumb-place [10 -21 22] [-33.9 -15.8 -7] shape)) ; middle
(defn thumb-l-place [shape] (thumb-place [8 -3 33] [-53.6 -26.1 -11.5] shape)) ; left

(defn thumb-layout [shape]
  (union
   (thumb-r-place shape)
   (thumb-m-place shape)
   (thumb-l-place shape)))

(defn debug [shape]
  (color [0.5 0.5 0.5 0.5] shape))

(def thumbcaps (thumb-layout (sa-cap 1)))
(def thumb (thumb-layout single-plate))
(def thumb-fill (thumb-layout filled-plate))
(def thumb-space-below (thumb-layout switch-bottom))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) 0]) ; base
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset]) ; key
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset]) ; key + wall

(def thumb-connectors
  (union
   (triangle-hulls                                         ; top two
    (thumb-m-place web-post-tr)
    (thumb-m-place web-post-br)
    (thumb-r-place web-post-tl)
    (thumb-r-place web-post-bl))
   (triangle-hulls                                         ; top two
    (thumb-m-place web-post-tl)
    (thumb-l-place web-post-tr)
    (thumb-m-place web-post-bl)
    (thumb-l-place web-post-br)
    (thumb-m-place web-post-bl))
   (if bottom-row (triangle-hulls                                         ; top two to the main keyboard, starting on the left
                   (key-place 2 lastrow web-post-br)
                   (key-place 3 lastrow web-post-bl)
                   (key-place 2 lastrow web-post-tr)
                   (key-place 3 lastrow web-post-tl)
                   (key-place 3 cornerrow web-post-bl)
                   (key-place 3 lastrow web-post-tr)
                   (key-place 3 cornerrow web-post-br)
                   (key-place 4 cornerrow web-post-bl)))
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 cornerrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl))
   (if bottom-row
     (triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 3 lastrow web-post-br)
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl)))
   (hull                                                   ; between thumb m and top key
    (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
    (thumb-m-place web-post-tr)
    (thumb-m-place web-post-tl))
   (piramid-hulls                                          ; top ridge thumb side
    (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
    (key-place 0 cornerrow (translate (wall-locate2 -1 0) web-post-bl))
    (key-place 0 cornerrow web-post-bl)
    (thumb-r-place web-post-tl)
    (thumb-m-place web-post-tr)
    (key-place 0 cornerrow (translate (wall-locate3 -1 0) web-post-bl)))
   (triangle-hulls
    (key-place 0 cornerrow web-post-bl)
    (key-place 0 cornerrow web-post-br)
    (thumb-r-place web-post-tl))
   (triangle-hulls
    (key-place 0 cornerrow web-post-br)
    (key-place 1 cornerrow web-post-bl)
    (thumb-r-place web-post-tl)
    (thumb-r-place web-post-tr))
   (triangle-hulls
    (key-place 1 cornerrow web-post-bl)
    (key-place 1 cornerrow web-post-br)
    (thumb-r-place web-post-tr))
   (if bottom-row
     (union
      (triangle-hulls
       (key-place 2 lastrow web-post-tl)
       (thumb-r-place web-post-tr)
       (key-place 2 lastrow web-post-bl)
       (thumb-r-place web-post-br))
      (triangle-hulls
       (thumb-r-place web-post-br)
       (key-place 2 lastrow web-post-bl)
       (key-place 3 lastrow web-post-bl)
       (key-place 2 lastrow web-post-br)))
     (union
      (triangle-hulls
       (thumb-r-place web-post-tr)
       (key-place 2 cornerrow web-post-bl)
       (key-place 1 cornerrow web-post-br)
       (key-place 2 cornerrow web-post-br)
       (key-place 3 cornerrow web-post-bl)
       (thumb-r-place web-post-br)
       (thumb-r-place web-post-tr))
      (triangle-hulls
       (thumb-r-place web-post-tr)
       (key-place 1 cornerrow web-post-br)
       (key-place 1 cornerrow web-post-br)
       (key-place 3 cornerrow web-post-bl)
       (thumb-r-place web-post-tr))))))

; dx1, dy1, dx2, dy2 = direction of the wall. '1' for front, '-1' for back, '0' for 'not in this direction'.
; place1, place2 = function that places an object at a location, typically refers to the center of a key position.
; post1, post2 = the shape that should be rendered
(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(defn key-corner [x y loc]
  (case loc
    :tl (key-wall-brace x y 0 1 web-post-tl x y -1 0 web-post-tl)
    :tr (key-wall-brace x y 0 1 web-post-tr x y 1 0 web-post-tr)
    :bl (key-wall-brace x y 0 -1 web-post-bl x y -1 0 web-post-bl)
    :br (key-wall-brace x y 0 -1 web-post-br x y 1 0 web-post-br)))

(def right-wall
  (union (key-corner lastcol 0 :tr)
         (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
         (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
         (key-corner lastcol cornerrow :br)))

(def case-walls
  (union
   right-wall
    ; back wall
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x 0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
    ;; ; left wall
   (for [y (range 0 lastrow)] (key-wall-brace 0 y -1 (if (= y 0) 1 0) web-post-tl 0 y -1 0 web-post-bl))
   (for [y (range 1 lastrow)] (key-wall-brace 0 (dec y) -1 0 web-post-bl 0 y -1 0 web-post-tl))
    ;; ; left-back-corner
   (key-wall-brace 0 0 0 1 web-post-tl 0 0 -1 1 web-post-tl)
   (wall-brace (partial key-place 0 cornerrow) -1 0 web-post-bl thumb-m-place -0.5 1 web-post-tl)
    ;; ; front wall
   (if bottom-row
     (union
      (key-wall-brace 3 lastrow 0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
      (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 0.5 -1 web-post-bl))
     (union
      (key-wall-brace 3 cornerrow 0 -1 web-post-bl 3 cornerrow 0 -1 web-post-br)
      (key-wall-brace 3 cornerrow 0 -1 web-post-br 4 cornerrow 0 -1 web-post-bl)))
   (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x cornerrow 0 -1 web-post-br)) ; TODO fix extra wall
   (for [x (range (if bottom-row 5 4) ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))

   (if bottom-row
     (wall-brace thumb-r-place 0 -1 web-post-br (partial key-place 3 lastrow) 0 -1 web-post-bl)
     (wall-brace thumb-r-place 0 -1 web-post-br (partial key-place 3 cornerrow) 0 -1 web-post-bl))

    ; thumb walls
   (wall-brace thumb-r-place 0 -1 web-post-br thumb-r-place 0 -1 web-post-bl)
   (wall-brace thumb-m-place 0 -1 web-post-br thumb-m-place 0 -1 web-post-bl)
   (wall-brace thumb-l-place 0 -1 web-post-br thumb-l-place 0 -1 web-post-bl)
   (wall-brace thumb-l-place 0 1 web-post-tr thumb-l-place 0 1 web-post-tl)
   (wall-brace thumb-l-place -1 0 web-post-tl thumb-l-place -1 0 web-post-bl)
    ; thumb corners
   (wall-brace thumb-l-place -1 0 web-post-bl thumb-l-place 0 -1 web-post-bl)
   (wall-brace thumb-l-place -1 0 web-post-tl thumb-l-place 0 1 web-post-tl)
    ; thumb tweeners
   (wall-brace thumb-r-place 0 -1 web-post-bl thumb-m-place 0 -1 web-post-br)
   (wall-brace thumb-m-place 0 -1 web-post-bl thumb-l-place 0 -1 web-post-br)
   (wall-brace thumb-m-place 0 1 web-post-tl thumb-l-place 0 1 web-post-tr)
   (wall-brace thumb-l-place -1 0 web-post-bl thumb-l-place -1 0 web-post-tl)))

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (->> (binding [*fn* 30]
         (cylinder [bottom-radius top-radius] height))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [position (key-position column row [0 0 0])]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 2 0 bottom-radius top-radius height [0 4.8 bottom-height]) ; top middle
         (screw-insert 0 1 bottom-radius top-radius height [-5 -12 bottom-height]) ; left side
         (screw-insert lastcol 0 bottom-radius top-radius height [-10 6 bottom-height]) ; top right
         (if bottom-row
           (screw-insert 0 lastrow bottom-radius top-radius height [-12 -7 bottom-height]) ;thumb
           (screw-insert 0 lastrow bottom-radius top-radius height [-10.5 -8.5 bottom-height]) ;thumb         
           )
         (screw-insert (- lastcol 2) lastrow bottom-radius top-radius height [5.5 7 bottom-height]) ; bottom front
        ;;  (screw-insert (- lastcol 0) lastrow bottom-radius top-radius height [6 13 bottom-height]) ; bottom right 2
         (if bottom-row ;bottom middle
           (screw-insert 2 (+ lastrow 1) bottom-radius top-radius height [0 6.5 bottom-height])
           (screw-insert 2 (+ cornerrow 1) bottom-radius top-radius height [-7 -12 bottom-height]))))

; Hole Depth Y: 4.4
(def screw-insert-height 4)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.8) (+ screw-insert-top-radius 1.8) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes (screw-insert-all-shapes 1.7 1.7 350))

(def plate-stops
  (union
   (screw-insert lastcol cornerrow 2 2 1.8 [8.5 -8.5 bottom-height]) ; bottom right
   (screw-insert 0 lastrow 2 2 1.8 [-22 -32 bottom-height]) ; thumb
   (screw-insert lastcol 0 2 2 1.8 [8.5 7.5 bottom-height]) ; top right
   ))

(defn plate-feet-place [radius z]
  (union
   (screw-insert lastcol cornerrow radius radius bottom-height [4 -3.5 z]) ; bottom right
   (screw-insert 0 lastrow radius radius bottom-height [-21 -25 z]) ; thumb
   (screw-insert lastcol 0 radius radius bottom-height [4 2.5 z]) ; top right
   (screw-insert 0 0 radius radius bottom-height [0 5 z]) ; usb holder
   ))

(def holder-depth 20)
(def holder-rad (/ 42 2))
(def holder-rad2 6)
(def holder-back-offset (- holder-rad 6))
(def holder-ring-depth 6)
(def holder-diff-offset 3)
(def quality-fn 60)

(def trackpad-holder-body (->> (difference
                                (hull
                                 (translate [0 0 3] (binding [*fn* (+ quality-fn 60)] (cylinder holder-rad holder-ring-depth)))
                                 (translate [holder-back-offset -4 holder-depth] (binding [*fn* (+ quality-fn 60)] (sphere holder-rad2)))
                                 (translate [(- holder-back-offset 1) 8 holder-depth] (binding [*fn* (+ quality-fn 60)] (sphere holder-rad2))))
                                (hull
                                 (translate [0 0 2] (binding [*fn* quality-fn] (cylinder (- holder-rad holder-diff-offset) holder-ring-depth)))
                                 (translate [holder-back-offset -4 holder-depth] (binding [*fn* quality-fn] (sphere (- holder-rad2 holder-diff-offset))))
                                 (translate [(- holder-back-offset 1) 8 holder-depth] (binding [*fn* quality-fn] (sphere (- holder-rad2 holder-diff-offset))))))
                               (rotate (deg2rad 90) [1 0 0])))

(defn trackpad-holder-main [flip] (union
                                   (->> (mirror [(if flip -1 0) 0 0] (import "../things/cirque-40-flat.stl"))
                                        (rotate (deg2rad 270) [1 0 0])
                                        (rotate (deg2rad 166) [0 1 0]))
                                   trackpad-holder-body))

(defn trackpad-pos [shape]
  (->> shape
       (rotate (deg2rad 149) [1 0 0])
       (rotate (deg2rad -8) [0 1 0])
       (rotate (deg2rad -1) [0 0 1])
       (translate thumborigin)
       (translate [-63 3 12])))

(defn trackpad-holder [flip] (trackpad-pos (trackpad-holder-main flip)))

(def trackpad-holder-cutaway (union
                              (->>
                               (hull
                                (translate [0 0 1] (binding [*fn* 100] (cylinder (- holder-rad 0.5) (+ holder-ring-depth 0.5))))
                                (translate [holder-back-offset -4 holder-depth] (binding [*fn* 30] (sphere (- holder-rad2 0.5))))
                                (translate [(- holder-back-offset 1) 8 holder-depth] (binding [*fn* 30] (sphere (- holder-rad2 0.5)))))
                               (rotate (deg2rad 90) [1 0 0]))
                              (->>
                               (cylinder (- holder-rad 0.8) 5)
                               (rotate (deg2rad 90) [1 0 0])
                               (translate [0 4 0]))))

(def trackpad-holder-cutaway (trackpad-pos trackpad-holder-cutaway))

(def trackpad-holder-cutaway2 (->>
                               (cylinder (+ holder-rad 4) 5)
                               (rotate (deg2rad 90) [1 0 0])
                               (translate [0 2 0])))
(def trackpad-holder-cutaway2 (trackpad-pos trackpad-holder-cutaway2))

(def usb-holder (mirror [0 0 0]
                        (import (cond
                                  (= controller-type "rpi-pico") "../things/holder rpi-pico.stl"
                                  (= controller-type "elite-c") "../things/holder elite-c.stl"
                                  :else "../things/holder pro-micro.stl"))))

(def usb-holder (translate (cond
                             (= controller-type "rpi-pico") [-41 41 bottom-height]
                             :else [-40.8 41.5 bottom-height]) usb-holder))
(def usb-holder-space
  (translate [0 0 (/ (+  bottom-height 8.2) 2)]
             (extrude-linear {:height (+ bottom-height 15.8) :twist 0 :convexity 0}
                             (offset 0.1
                                     (project usb-holder)))))

(spit "things/usb-holder.scad" (write-scad usb-holder))

(def model-outline
  (project
   (union
    key-fills
    connectors
    thumb-fill
    thumb-connectors
    case-walls)))

(defn model-side [flip]
  (difference
   (union
    key-holes
    connectors
    thumb
      ;; (debug usb-holder)
      ;; (debug trackpad-holder-cutaway)
      ;; (debug key-space-below)
    (if trackpad
      (difference (union thumb-connectors) trackpad-holder-cutaway)
      thumb-connectors)
    (if trackpad
      (difference (trackpad-holder flip)
                  (difference
                   (union
                    (rotate (deg2rad 1) [0 0 1]
                            (translate [-37 0 -12] (cube 50 100 100))))
                   trackpad-holder-cutaway2)
                  key-space-below))
    (difference (union case-walls
                       screw-insert-outers
                       plate-stops)
                usb-holder-space
                (if trackpad trackpad-holder-cutaway)
                screw-insert-holes
                key-space-below
                thumb-space-below))
   (translate [0 0 -20] (cube 350 350 40))))

(spit "things/right.scad"
      (write-scad (model-side false)))

(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] (model-side true))))

(spit "things/both.scad"
      (write-scad
       (rotate (deg2rad 8) [0 0 1] (translate [110 0 0] (model-side false)))
       (rotate (deg2rad -8) [0 0 1] (translate [-110 0 0] (mirror [-1 0 0] (model-side true))))))

(spit "things/test.scad"
      (write-scad
       (union
        (model-side false)
        caps
        thumbcaps
        (debug key-space-below)
        (debug thumb-space-below)
        (debug usb-holder)
            ;; (debug (difference
            ;;   bottom-plate
            ;;   (union
            ;;     bottom-wall-usb-holder
            ;;     key-space-below
            ;;     thumb-space-below
            ;;     bottom-screw-holes-head
            ;;     bottom-screw-holes-top
            ;;     )))
        )))

(spit "things/thumb.scad"
      (write-scad
       (difference
        (union
         thumb
         thumb-connectors
         thumbcaps)
        (translate [0 0 -20] (cube 350 350 40)))))

(spit "things/keyhole.scad"
      (write-scad
       single-plate))

(def wall-shape
  (cut
   (translate [0 0 -0.1]
              (union case-walls
                     screw-insert-outers))))

(def wall-shape (cut (translate [0 0 -0.1] (difference case-walls usb-holder-space))))

(def bottom-height-half (/ bottom-height 2))

(def bottom-plate-shape (difference model-outline
                                    (offset 0.2 wall-shape)))

(def bottom-plate
  (extrude-linear {:height bottom-height :twist 0 :convexity 0 :center false}
                  bottom-plate-shape))
;; (def bottom-wall
;;   (translate [0 0 bottom-height-half] (extrude-linear {:height bottom-height :twist 0 :convexity 0} wall-shape)))

(def bottom-wall-usb-holder
  (translate [0 0 bottom-height]
             (extrude-linear {:height bottom-height-half :twist 0 :convexity 0}
                             (offset 3))))

(def screw-head-height 1.5)
(def bottom-screw-holes-head
  (translate [0 0 (- bottom-height)] (screw-insert-all-shapes 2.8 1 screw-head-height)))

(def bottom-screw-holes-top
  (translate [0 0 (- bottom-height)]
             (screw-insert-all-shapes 1.5 1.5 bottom-height)))

;; (spit "things/trackpad-holder.scad"
;;   (write-scad (trackpad-holder false))
;; )

(spit "things/right-plate-print.scad"
      (write-scad
       (difference
        bottom-plate
        (union
         bottom-wall-usb-holder
         key-space-below
         thumb-space-below
         bottom-screw-holes-head
         bottom-screw-holes-top
         (plate-feet-place 4 (- (- bottom-height 2)))))))

(spit "things/right-plate-cut.scad"
      (write-scad
       (cut
        (translate [0 0 (+ bottom-height 1)] ;biggest cutout on top
                   (difference
                    (union
                     bottom-plate)
                    (union
                     bottom-wall-usb-holder
                     (screw-insert-all-shapes 1 1 50)))))))

(def voronoi-model (difference
                    (union
                     (intersection
                      (union
                       (translate [0 0 (- bottom-height)] (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.8) (+ screw-insert-bottom-radius 1.8) bottom-height))
                       (plate-feet-place 5 0))
                      bottom-plate)

                     (extrude-linear {:height bottom-height :twist 0 :convexity 0 :center false}
                                     (difference bottom-plate-shape ; voronoi base plate
                                                 (difference
                                                  (offset -3 bottom-plate-shape)
                                                  (translate [-120 -50 0] (scale [0.14 0.14 1] (import "../things/voronoi.dxf")))))))
                    (union
        ;;  bottom-wall-usb-holder
                     bottom-screw-holes-head
                     bottom-screw-holes-top
                     (plate-feet-place 4 (- (- bottom-height 2))))))

(spit "things/voronoi-plate-print.scad"
      (write-scad voronoi-model))

(spit "things/voronoi-plate-cut.scad"
      (write-scad
       (cut
        (translate [0 0 (- 1 bottom-height 1)] ;biggest cutout on top
                   voronoi-model))))

