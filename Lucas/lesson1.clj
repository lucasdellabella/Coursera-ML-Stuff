;; All about higher order functions =)
;; Linear Regression with multiple variables

(ns mlpractice.lesson1)

;innefficient because iterate holds onto old maps, should use a different function?
(defn generate-data-set
  "Generates a map of a vector x of feature values 
   to its y value for linear regression"
  [data-point-num & coefficients]
  (nth (iterate
        #(let [n (count coefficients)
               ;;x-vec:: (1, x1, x2, ..., xn  
               x-vec (->> (fn [] Math/random)
                          (repeatedly (dec n))
                          (cons 1))] 
           (->> (map * x-vec coefficients)
                (apply +)
                (assoc %1 x-vec))) {}) data-point-num))

;NOTE: The first element of the feature vec should be 1 so coefficient sub 0 is independent of any x
(defn hypothesis-fn
  "Builds the hypothesis function h(x) using a vector theta of coefficients"
  [param-vec]
  (fn h
    [feature-vec]
    (apply + (map * param-vec feature-vec))))

(defn cost-summation
  "The function that finds the error between our hypothesis function
   and our actual y value, and adds it to an aggregate"
  [hypothesis agg data]
  (let [x (first data)
        y (second data)]
    (+ agg (- (hypothesis x) y))))

;;Difference between inside and outside let?
(defn cost-fn
  "Builds the cost function J(th0, th1) for some amount of data"
  [data]
  (let [coefficient (->> data (count) (* 2) (/ 1))]
    (fn cost
      [param-vec]
      (let [hypothesis (hypothesis-fn param-vec)]
        (-> (partial cost-summation hypothesis)
            (reduce 0 data)
            (Math/pow 2)
            (* coefficient))))))

(take 10 (iterate #(int (/ % 2.0)) 321))

(defn int->bin [x]
  (loop [num x
         bin ""]
    (let [new-bin (str bin (mod num 2))
          half (int (/ num 2.0))]
      (if (#{0 1} half)
        (str new-bin (mod half 2))
        (recur half
               new-bin)))))
