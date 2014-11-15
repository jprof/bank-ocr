(ns bank-ocr.core
  (:gen-class))

(def TOP-PRIMES [2  3  5])
(def MID-PRIMES [7  11 13])
(def BOT-PRIMES [17 19 23])
(def NUMBERS {(* 3 7 13 17 19 23)    0
              (* 13 23)              1
              (* 3 11 13 17 19)      2
              (* 3 11 13 19 23)      3
              (* 7 11 13 23)         4
              (* 3 7 11 19 23)       5
              (* 3 7 11 17 19 23)    6
              (* 3 13 23)            7
              (* 3 7 11 13 17 19 23) 8
              (* 3 7 11 13 19 23)    9})

(defn segment-parser
  "Returns a function that parses a three character segment representing the
   top, middle, or bottom line of an input value. If an unexpected character
   is encountered, the returned value will be less than 0.
   char-values -> values to map to the | or _ if they are present in the
                  correct location in the character sequence."
  [char-values]
  (fn [chrs]
    (reduce *
            (map-indexed 
              (fn [i chr]
                (cond
                  (or
                    (and
                      (= chr \|)
                      (or
                        (= i 0)
                        (= i 2)))
                    (and
                      (= chr \_)
                      (= i 1))) (nth char-values i)
                  (= chr \space) 1
                  :else -1))
              chrs))))

(def top-parser (segment-parser TOP-PRIMES))
(def mid-parser (segment-parser MID-PRIMES))
(def bot-parser (segment-parser BOT-PRIMES))

(defn parse-account-number
  "Parses the input format into a seq containing the account number. The blankline
   following the three lines of spaces, |, and _ is expected to be included in the
   lines arg despite it being ignored."
  [lines]
  (map #(NUMBERS %)
       (apply map *
              (map (fn [parse-fn line]
                     (map parse-fn (partition 3 line)))
                   [top-parser mid-parser bot-parser]
                   lines))))

(defn check-account-number
  "Compute a checksum on the account number, returning true if account number
   is valid and false otherwise."
  [acct-number]
  (->> acct-number
      reverse
      (map-indexed (fn [i v] (* (inc i) v)))
      (reduce +)
      ((fn [v] (mod v 11)))
      (= 0)))

(defn parse-file
  "Read in an input file with account numbers encoded as |, _ and spaces. Parse the
   numeric values from this input, returning a seq of account numbers of the format:
   (1 2 3 4 5 6 7 8 9)."
  [fname]
  (let [lines (-> (slurp fname) (clojure.string/split #"\n"))]
    (map parse-account-number (partition 4 lines))))

(defn write-file
  "Write an output file containing the numeric values of the bank-ocr input format.
   account-numbers -> output from the parse-file function"
  [fname account-numbers]
  (let [str-account-numbers (apply str (map #(str (apply str %) "\n") account-numbers))
        out-fname (str fname ".out")]
    (spit out-fname str-account-numbers)))

(defn parse-and-write-file
  "Takes the path to the input file, processes that file, and outputs the results to
   $fname.out"
  [fname]
  (let [results (parse-file fname)]
    (write-file fname results)))

(defn -main
  [filename & args]
  (parse-and-write-file filename))
