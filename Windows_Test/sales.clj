
;; Files to be placed in this path :(System/getProperty "user.dir") 

(def Custmap (atom (sorted-map)))
(def Prodmap (atom (sorted-map)))
(def Salesmap(atom (sorted-map)))
(def Costmap (atom (sorted-map)))
(def Restmap (atom (sorted-map)))
(def ItemMap  (atom (sorted-map)))

;; reading cust.txt
(defn readCustList[]
   (def custlist_var (slurp "cust.txt"))
    (def custlist (clojure.string/split-lines custlist_var))
     (doseq [item custlist] 
           (def value (clojure.string/split item #"\|"))
           (swap! Custmap assoc (Integer/parseInt(first value)) (rest value ) )
           )
   )  

;;reading prod.txt
(defn readProdList []
  (def prodlist_var (slurp "prod.txt"))
   (def prodlist (clojure.string/split-lines prodlist_var))
     (doseq [item prodlist] 
           (def value (clojure.string/split item #"\|"))
           (swap! Prodmap assoc (Integer/parseInt(first value)) (rest value) )
           )
  )

;; readiing sales.txt
(defn readSalesList []
  (def saleslist_var (slurp "sales.txt"))
   (def saleslist (clojure.string/split-lines saleslist_var))
     (doseq [item saleslist] 
       (def value (clojure.string/split item #"\|"))
        (swap! Salesmap assoc (Integer/parseInt(first value)) 
               [(first(get @Custmap (Integer/parseInt(get value 1) )))
                    (first(get @Prodmap (Integer/parseInt(get value 2) ))) 
                        (last value)
               ] 
        )     
      )
  )


(defn generate[]
  (doseq [[prodkey prodval] (map vector (keys @Prodmap) ( vals @Prodmap))]
        (swap! Costmap assoc (first prodval)
               (nth prodval 1)               )    
       )
    ;;(println @Costmap)
    ;;(println @Custmap)
    ;;(println @Prodmap)
    ;;(println @Costmap)
    ;;(println @Salesmap)
    
  )

;; Task 4 implementation
(defn customerSales[]
    (doseq [[skey sval] (map vector (keys @Salesmap) ( vals @Salesmap))]
        (swap! Restmap assoc (first sval)  [0]  )    
       )
    (doseq [[salek salev] (map vector (keys @Salesmap) ( vals @Salesmap))]
        ;; getting values from salesmap username item nos
        (def Uname (get salev 0))
        (def item (get salev 1))
        (def nos (Float/parseFloat(get salev 2)))
        ;;getting exp for the user current iteration
        (doseq [[costkey costval] (map vector (keys @Costmap) ( vals @Costmap))]
          (def Costval (Float/parseFloat  costval))
            (if (= costkey item)
              (def exp (* nos Costval)))
         )
         (doseq [[restkey restval] (map vector (keys @Restmap) ( vals @Restmap))]
            (if (= Uname restkey)
                  (swap! Restmap assoc (first salev)  (conj restval exp) ))
         )  
      )
  )


;; Task 1  
(defn printCustList[]
  (println "Customer Table")
    (doseq [[k v] @Custmap] 
        (println (str k ":" v)))
      (println "\n*** End of Task ***\n")
     
    )

;; Task 2
(defn printProdList[]
  (println "Product Table")
    (doseq [[k v] @Prodmap] 
       (println (str k ":" v)))
       (println "\n*** End of Task ***\n")
    
    )

;; Task 3
(defn printSalesList[]
  (println "Sales Table")
    (doseq [[k v] (map vector ( keys @Salesmap) (vals @Salesmap))] 
        (println (str k ":" v)))
        (println "\n*** End of Task ***\n")
    )

;; Task 4
(defn customer_Sales[]
  (customerSales)
  ;;(print @Restmap)
  (println "Enter the Customer name (case sensitive) :  ")
  (def input(read-line))
  (println "Selected Customer : " input)  
  (def flag "0.00")
  (doseq [[restkey restval] (map vector (keys @Restmap) ( vals @Restmap))]
             ;;(println input restkey)
             (if (= input restkey)
             ;;(println "\n" restkey ": $"(reduce + restval )) 
             (def flag (reduce + restval ))
             ) 
         )
  (println "\n" input ": $" (format "%.2f" flag)) 
  (println "\n*** End of Task ***\n")
       
    )

;;Task 5
(defn productsCount[]
  
  (doseq [[Pkey Pval] (map vector (keys @Prodmap) ( vals @Prodmap))]
        (swap! ItemMap assoc (first Pval)  [0]  )    
       )
  
  (doseq [[salek salev] (map vector (keys @Salesmap) ( vals @Salesmap))]
        ;; getting values from salesmap username item nos
        (def Sitem (get salev 1))
        (def Snos (Integer/parseInt(get salev 2)))
         (doseq [[Ikey Ival] (map vector (keys @ItemMap) ( vals @ItemMap))]
           (if (= Sitem Ikey)
            (swap! ItemMap assoc Sitem (conj Ival Snos))
            )
         )  
      )
  ;;(print @ItemMap)
  (println "Enter the Product name (case sensitive) : ")
  (def product(read-line))
  (println "Selected Product : " product) 
  (def flagy "0")
  (doseq [[Itemkey Itemval] (map vector (keys @ItemMap) ( vals @ItemMap))]
            ;; (println product Itemkey)
             (if (= product Itemkey)
              (def flagy  (reduce + Itemval ))
             ;;(println product ": "(reduce + Itemval ))
                      ) 
         )
  (println product ": " flagy)
  (println "\n*** End of Task ***\n")
  
)
;;Task 6
(defn exit_end[]
  (println "\nGood Bye")
  (System/exit 0)
  )

;; main menu 
(defn Welcome []
  
  (println "\n*** Sales Menu ***")
  (println "------------------")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit \n")
  (println "Enter an option (1-6) ?:")

  (let [input(read-line)]
  (print "Choosen option : ")
    (println input "\n")
    (case input
     "1" (printCustList)
     "2" (printProdList)
     "3" (printSalesList)
     "4" (customer_Sales)
     "5" (productsCount)
     "6" (exit_end)
    )
  )
  (Welcome)
)

(defn main[]
 ;; Reading all text files into list and Map
  (readCustList)
  (readProdList)
  (readSalesList)
  (generate)
	;;Printing Main Menu
  (Welcome)
 )

;;main function
(main)



