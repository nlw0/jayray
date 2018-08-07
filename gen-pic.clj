
(require '[clojure.string :as str])
(require '[clojure.java.io :as jio])

(load-file "quat.clj")

(def IW 1024)
(def IH 798)
(def oc [(+ 0.5 (/ IW -2)) (+ 0.5 (/ IH -2)) (/ IW 2)])
(def tilt 0.6)

(defn touch-sphere? [cw vv]
  (let [
        oo [0 -1.5 -0.6]
        rr 0.6
        ll (normalze vv)
        oc (vsub oo cw)
        aa 1
        bb (* 2 (dot ll oc))
        cc (- (dot oc oc) (* rr rr))
        delt (- (* bb bb) (* 4 aa cc))]
    (def dd (/ (- bb (Math/sqrt delt)) 2))
    (and (> delt 0)
         (> 0 (+ (get cw 2) (* (get ll 2) dd)))
         )))
    

(defn camera-model [j k]
  (let [cw [0 0 -2.0]
        qq [(- 1 (* tilt tilt)) (* tilt tilt) 0 0]
        vv (rotate qq (vsum [k j 0] oc))
        t (/ (* -1 (get cw 2)) (get vv 2))]
    
    (if (< t 0)
      128
      (let [px (+ (get cw 0) (* t(get vv 0)))
            py (+ (get cw 1) (* t(get vv 1)))]
        (if (touch-sphere? cw vv)
          200
          (if (or (and (< (mod px 1.0) 0.5) (< (mod py 1.0) 0.5)) 
                  (and (> (mod px 1.0) 0.5) (> (mod py 1.0) 0.5)))
            255
            0))))))


;; (def pix
;;   (for [j (range IH)
;;         k (range IW)]
;;     (let [r
;;           (+ 1e-7 (* 0.2 (Math/sqrt (+ (* k k) (* j j)))))]
;;       (int (* 255  (+ 0.5 ( * 0.5 (/ (Math/sin ( * (* 0.5 Math/PI) r))
;;                                      (Math/sqrt r)))))))))

(def pix
  (for [j (range IH)
        k (range IW)]
    (camera-model j k)
    ))

;; output netbpm file
(defn write-to-file [file-name contents]
  (with-open [wtr (jio/writer file-name)]
    (dorun
     (.write wtr contents))))

(defn joined-values [vv]
  (str/join " "  vv))

(def p6-header ["P2" (str IW " " IH) "255"])

(def final-file-rows
  (conj p6-header
        (joined-values pix)
        ""))

(def final-file-contents
  (str/join "\n" final-file-rows))

(write-to-file "out.pnm" final-file-contents)
