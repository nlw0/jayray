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

(declare camera-model find-color hit-sphere? hit-plane? touch-sphere? trace-reflection plane-texture smallest-head? hit-sky vec-from-pixel
         make-ray)

(def pix
  (for [j (range IH)
        k (range IW)]
    (camera-model j k)))

(defn camera-model [j k]
  (let [ray (vec-from-pixel j k)]
    (find-color ray)))

(defn vec-from-pixel [j k]
  (let [cw [0 0 -2.0]
        qq [(- 1 (* tilt tilt)) (* tilt tilt) 0 0]
        vv (rotate qq (vsum [k j 0] oc))]
    (make-ray cw vv)))

(defn make-ray [cw vv] {:orig cw :dir vv})

(defn find-color [ray]
  (let [tests [(hit-sphere? ray oo1)
               (hit-sphere? ray oo2)
               (hit-sphere? ray oo3)
               (hit-plane? ray)]
        valid-tests (filter identity tests)
        the-hit (smallest-head? valid-tests)]
    (if (nil? the-hit)
      sky-color
      (the-hit))))

(defn hit-sky [] sky-color)

(defn smallest-head? [pairs]
  (if (empty? pairs)
    nil
    (:func (apply min-key (cons :dist pairs)))))

(defn hit-sphere? [ray oo]
  (let [dd (touch-sphere? ray oo)]
    (if dd
      ;; [dd (fn [] (trace-reflection cw dd vv oo))]
      {:dist dd :func (fn [] (trace-reflection ray dd oo))}
      nil)))

(defn trace-reflection [ray dd oo]
  (let [newcw (vsum (:orig ray) (vscale dd (normalze (:dir ray))))
        nn (vsub newcw oo)]
    (find-color (make-ray newcw (reflect (:dir ray) nn)))))

(defn hit-plane? [ray]
  (let [t (/ (* -1 (get (:orig ray) 2))
             (get (:dir ray) 2))
        yaya (plane-texture ray t)]
    (if yaya
      {:dist 1000 :func (fn [] yaya)}
      nil)))

(defn plane-texture [ray t]
  (if (< t 0)
    nil
    (let [px (+ (get (:orig ray) 0) (* t (get (:dir ray) 0)))
          py (+ (get (:orig ray) 1) (* t (get (:dir ray) 1)))]
       (if (> (norm [px py 0]) 50)
         nil
         (if (or (and (< (mod px 0.4) 0.2) (< (mod py 0.4) 0.2))
                 (and (> (mod px 0.4) 0.2) (> (mod py 0.4) 0.2)))
             (map int (vscale (/ 0.001 (* t t)) plane-white))
             (map int (vscale (/ 0.001 (* t t)) plane-black)))))))

(defn touch-sphere? [ray oo]
  (let [ll (normalze (:dir ray))
        oc (vsub (:orig ray) oo)
        bb (dot ll oc)
        cc (- (dot oc oc) (* rr rr))
        delt (- (* bb bb) cc)
        dd (- (* -1 bb) (Math/sqrt delt))]
    (if (and (> delt 0)
             (> 0 (+ (get (:orig ray) 2) (* (get ll 2) dd)))
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
