(require '[clojure.string :as str])
(require '[clojure.java.io :as jio])

(load-file "quat.clj")

(def IW 1024)
(def IH 798)
(def oc [(+ 0.5 (/ IW -2)) (+ 0.5 (/ IH -2)) (/ IW 2)])
(def tilt 0.6)

(def sky-color [63 126 226])
(def plane-white [255 255 255])
(def plane-black [0 0 0])
(def error-color [255 0 0])

(def oo [0 -1.5 -0.6])
(def rr 0.6)

(declare camera-model find-color hit-sphere? hit-plane? touch-sphere? trace-reflection plane-texture)

(def pix
  (for [j (range IH)
        k (range IW)]
    (camera-model j k)))

(defn camera-model [j k]
  (let [cw [0 0 -2.0]
        qq [(- 1 (* tilt tilt)) (* tilt tilt) 0 0]
        vv (rotate qq (vsum [k j 0] oc))]
    (find-color cw vv 1)))

(defn find-color [cw vv iter]
  (or (hit-sphere? cw vv iter)
      (hit-plane? cw vv)
      sky-color))

(defn hit-sphere? [cw vv iter]
  (let [dd (touch-sphere? cw vv)]
    (if dd
      (trace-reflection cw dd vv iter)
      nil)))

(defn trace-reflection [cw dd vv iter]
  (let [newcw (vsum cw (vscale dd (normalze vv)))
        nn (vsub newcw oo)]
    (if (< 0 iter)
        (find-color newcw (reflect vv nn) (- iter 1))
        error-color)))

(defn hit-plane? [cw vv]
  (let [t (/ (* -1 (get cw 2)) (get vv 2))]
    (plane-texture cw vv t)))

(defn plane-texture [cw vv t]
  (if (< t 0)
    nil
    (let [px (+ (get cw 0) (* t (get vv 0)))
          py (+ (get cw 1) (* t (get vv 1)))]
       (if (> (norm [px py 0]) 50)
         nil
         (if (or (and (< (mod px 1.0) 0.5) (< (mod py 1.0) 0.5)) 
                 (and (> (mod px 1.0) 0.5) (> (mod py 1.0) 0.5)))
             (map int (vscale (/ 0.001 (* t t)) plane-white))
             (map int (vscale (/ 0.001 (* t t)) plane-black)))))))

(defn touch-sphere? [cw vv]
  (let [ll (normalze vv)
        oc (vsub cw oo)
        bb (dot ll oc)
        cc (- (dot oc oc) (* rr rr))
        delt (- (* bb bb) cc)    
        dd (- (* -1 bb) (Math/sqrt delt))]
    (if (and (> delt 0)
             (> 0 (+ (get cw 2) (* (get ll 2) dd)))
             (< 0 dd))
      dd
      nil)))

;; (def pix
;;   (for [j (range IH)
;;         k (range IW)]
;;     (let [r
;;           (+ 1e-7 (* 0.2 (Math/sqrt (+ (* k k) (* j j)))))]
;;       (int (* 255  (+ 0.5 ( * 0.5 (/ (Math/sin ( * (* 0.5 Math/PI) r))
;;                                      (Math/sqrt r)))))))))
    
;; output netbpm file
(defn write-to-file [file-name contents]
  (with-open [wtr (jio/writer file-name)]
    (dorun
     (.write wtr contents))))

(defn joined-values [vv]
  (str/join " "  vv))

(def p6-header ["P3" (str IW " " IH) "255"])

(def final-file-rows
  (conj p6-header
        (joined-values (flatten pix))
        ""))

(def final-file-contents
  (str/join "\n" final-file-rows))

(write-to-file "out.pnm" final-file-contents)
