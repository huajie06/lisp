

;; *ns* in REPL will show the namespace
  

(ns clojure.examples.hello
   (:gen-class))
(defn Example []
   (println (str "Hello World"))
   (println (+ 1 2)))
(Example)

;; (ns clojure.examples.hello
;;    (:gen-class))
;; (require "clojure.java.io")
;; (defn Example []
;;    (.exists (file "Example.txt")))
;; (Example)

(defn Test []
  (println '(1, 2, 3)))

(Test)
 


(println \e)
(println "he")



(println (/ (+ 7 5 (* 3 4)) 10))


;; (println (+ 3 4 (* 4 5)))

;; (println (/ 10 3))
