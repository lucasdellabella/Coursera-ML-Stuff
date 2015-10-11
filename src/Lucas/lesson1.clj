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
               x-vec (repeatedly n (fn [] (Math/random)))]
           (assoc %1 x-vec (apply + (map * x-vec coefficients))))
        {})
       data-point-num))

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
