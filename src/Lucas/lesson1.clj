;; All about higher order functions =)
(ns mlpractice.lesson1)

(defn generate-data-set
  "Generates a map of :x y values for linear regression"
  [constant slope num]
  (nth (iterate
        #(let [x (Math/random)]
           (assoc %1 x (+ constant (* slope x)))) {}) num))

(defn hypothesis-fn
  "Builds the hypothesis function h(x) using th0 and th1"
  [theta0 theta1]
  (fn h
    [x]
    (+ theta0 (* theta1 x))))

(defn cost-summation
  "The function that gets applied to every entry in every data set"
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
      [th0 th1]
      (let [hypothesis (hypothesis-fn th0 th1)] 
        (* coefficient
           (reduce (partial cost-summation hypothesis) 0 data))))))
