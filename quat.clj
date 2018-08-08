(declare norm normalze dot vscale vsum vsub)

(defn qrot [a b c d]

  ;; [[(a * a + b * b - c * c - d * d), (2 * b * c - 2 * a * d), (2 * b * d + 2 * a * c)],
  ;;  [(2 * b * c + 2 * a * d), (a * a - b * b + c * c - d * d), (2 * c * d - 2 * a * b)],
  ;;  [(2 * b * d - 2 * a * c), (2 * c * d + 2 * a * b), (a * a - b * b - c * c + d * d)]]

  (let [aa (* a a)        ab (* a b)        ac (* a c)        ad (* a d)        bb (* b b)
        bc (* b c)        bd (* b d)        cc (* c c)        cd (* c d)        dd (* d d)]    
    [[(- (+ aa bb) cc dd),      (* 2 (- bc ad)),      (* 2 (+ bd ac))],     
     [(* 2 (+ bc ad)),      (- (+ aa cc) bb dd),      (* 2 (- cd ab))],
     [(* 2 (- bd ac)),      (* 2 (+ cd ab)),      (- (+ aa dd) bb cc)]]))
    
(defn rotate [qq vec]
  (let [qr (apply qrot qq)
        qx (get qr 0)
        qy (get qr 1)
        qz (get qr 2)]
    [(dot qx vec)
     (dot qy vec)
     (dot qz vec)]))
  
(defn reflect[dd nn]
  (vsub dd (vscale (* 2 (dot dd nn))
                   (normalze nn))))

(defn normalze [vv]
  (vec (map #(/ % (norm vv)) vv)))       

(defn norm [va]
  (Math/sqrt (dot va va)))

(defn dot [va vb]
  (+ (* (get va 0) (get vb 0))
     (* (get va 1) (get vb 1))
     (* (get va 2) (get vb 2))))

(defn vsum [va vb]
  [(+ (get va 0) (get vb 0))
   (+ (get va 1) (get vb 1))
   (+ (get va 2) (get vb 2))])

(defn vsub [va vb]
  [(- (get va 0) (get vb 0))
   (- (get va 1) (get vb 1))
   (- (get va 2) (get vb 2))])

(defn vscale [s vv]
  (vec (map #(* s %) vv)))
    
(def myq [(- 1 (* 0.1 0.1))          
          (* 0.1 0.1)
          0 0])
  
(def myv [0.0 1.0 0.0])

;; (println myq)

;; (println (apply qrot myq))

;; (println (rotate myq myv))
