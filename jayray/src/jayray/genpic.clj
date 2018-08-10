(ns jayray.genpic
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.java.io :as jio])

;; (load-file "quat.clj")
(use 'jayray.quat)

(def IW 1024)
(def IH 798)
(def oc [(+ 0.5 (/ IW -2)) (+ 0.5 (/ IH -2)) (/ IW 2)])
(def tilt 0.6)

(def sky-color [63 126 226])
(def plane-white [255 255 255])
(def plane-black [0 0 0])
(def error-color [255 0 0])

(def oo1 [0 -1.5 -0.6])
(def oo2 [1.2 -1.5 -0.6])
(def oo3 [-1.2 -1.5 -0.6])
(def rr 0.6)

(declare camera-model find-color hit-sphere? hit-plane? touch-sphere? trace-reflection plane-texture smallest-head? hit-sky vec-from-pixel)

(def pix
  (for [j (range IH)
        k (range IW)]
    (camera-model j k)))

(defn camera-model [j k]
  (let [[cw vv] (vec-from-pixel j k)]
    (find-color cw vv)))

(defn vec-from-pixel [j k]
  (let [cw [0 0 -2.0]
        qq [(- 1 (* tilt tilt)) (* tilt tilt) 0 0]
        vv (rotate qq (vsum [k j 0] oc))]
    [cw vv]))

(defn find-color [cw vv]
  (let [tests [(hit-sphere? cw vv oo1)
               (hit-sphere? cw vv oo2)
               (hit-sphere? cw vv oo3)
               (hit-plane? cw vv)]
        valid-tests (filter identity tests)
        the-hit (smallest-head? valid-tests)]
    (if (nil? the-hit)
      sky-color
      (the-hit))))

(defn hit-sky [] sky-color)

(defn smallest-head? [pairs]
  (if (empty? pairs)
    nil
    (second (apply min-key (cons first pairs)))))

(defn hit-sphere? [cw vv oo]
  (let [dd (touch-sphere? cw vv oo)]
    (if dd
      [dd (fn [] (trace-reflection cw dd vv oo))]
      nil)))

(defn trace-reflection [cw dd vv oo]
  (let [newcw (vsum cw (vscale dd (normalze vv)))
        nn (vsub newcw oo)]
     (find-color newcw (reflect vv nn))))

(defn hit-plane? [cw vv]
  (let [t (/ (* -1 (get cw 2)) (get vv 2))
        yaya (plane-texture cw vv t)]
    (if yaya
      [1000 (fn [] yaya)]
      nil)))

(defn plane-texture [cw vv t]
  (if (< t 0)
    nil
    (let [px (+ (get cw 0) (* t (get vv 0)))
          py (+ (get cw 1) (* t (get vv 1)))]
       (if (> (norm [px py 0]) 50)
         nil
         (if (or (and (< (mod px 0.4) 0.2) (< (mod py 0.4) 0.2)) 
                 (and (> (mod px 0.4) 0.2) (> (mod py 0.4) 0.2)))
             (map int (vscale (/ 0.001 (* t t)) plane-white))
             (map int (vscale (/ 0.001 (* t t)) plane-black)))))))

(defn touch-sphere? [cw vv oo]
  (let [ll (normalze vv)
        oc (vsub cw oo)
        bb (dot ll oc)
        cc (- (dot oc oc) (* rr rr))
        delt (- (* bb bb) cc)    
        dd (- (* -1 bb) (Math/sqrt delt))]
    (if (and (> delt 0)
             (> 0 (+ (get cw 2) (* (get ll 2) dd)))
             (< 0.01 dd))
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

; (write-to-file "out.pnm" final-file-contents)
