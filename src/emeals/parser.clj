(ns emeals.parser
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell])
  (:gen-class))

(def meal-parser
  (insta/parser "meal = header <'\n'> names nl (times nl)? ingredients dashes ingredients nl instructions dashes instructions
                 <ws> = <#'\\s+'>
                 <nl> = <#'(\\s*\n)+'>
                 <digit> = #'[0-9]'
                 <number> = #'[0-9]+'
                 <fraction> = '¼' | '½' | '¾' | '⅛' | '⅓'
                 <dashes> = nl? <'---' '-'+ nl>

                 header = <'Meal'> ws <digit> (<' '> flag)?
                 <flag> = #'[A-Za-z \\-]+'

                 names = (name | nl)+
                 <name> = #'([\\p{L}\\- \",&])+'

                 times = <'Prep Cook Total'> nl time ws time ws time
                 minutes = (number 'm')
                 hours = (number 'h')
                 time = minutes | hours | hours ws minutes

                 ingredients = ingredient <sep> ingredients / ingredient
                 <sep> = nl | ', ' | (',' ws)
                 ingredient = normal_ingredient | special_ingredient
                 <normal_ingredient> = amount ((ws unit ws description) / (ws description))
                 <special_ingredient> = ('Zest of ' | 'Zest and juice of ') ((digit | fraction) ' ')? description
                 amount = (digit | '.' | '/' | fraction)+
                 unit = singular_unit <'s'?>
                 <singular_unit> = 'lb' | 'teaspoon' | 'tablespoon' | 'cup' | 'oz' | 'clove'
                 description = description_words
                 <description_words> = description_word | description_word ','? (' ' | nl) description_words
                 <description_word> = ((digit | fraction) '-' #'[\\p{L}\\-]*') | sub_unit | paren_words | #'[\\p{L}\\-*]+'
                 <paren_words> = '(' #'[\\p{L}\\- \n,]+' ')'
                 <sub_unit> = ('(' ('about ')? (digit | fraction)+ #'[\\p{L}\\- ]+' ')') | (('of ' | 'or ') (digit | fraction)+)

                 instructions = #'(?!Zest (and juice )?of)\\p{Lu}' {instruction}
                 <instruction> = #'.+(?=\n)' nl
                 "))

(defn assemble-instructions [instr]
  (let [instructions (rest instr)]
    (string/join "\n"
                 (string/split (string/join " " (cons (str (first instructions)
                                                           (second instructions))
                                                      (drop 2 instructions)))
                               #"\. "))))

(defn flatten-ingredients [ingreds]
  (if (empty? ingreds)
    nil
    (let [ingredient (second ingreds)
          ingredients (first (rest (rest ingreds)))]
      (cons ingredient
            (flatten-ingredients ingredients)))))

(def fraction-escape
  {\½ " 1/2"
   \¼ " 1/4"
   \¾ " 3/4"
   \⅛ " 1/8"
   \⅓ " 1/3"})

(defn remove-fractions [key ingred]
  (if-let [value (key ingred)]
    (assoc ingred
           key
           (string/trim (string/escape value fraction-escape)))
    ingred))

(defn add-to-key [ing-map key value]
  (assoc ing-map
         key
         (if (key ing-map)
           (str (key ing-map) value)
           value)))

(defn normalize-ingredient [ingred]
  (let [attributes (rest ingred)]
    (remove-fractions
      :description
      (remove-fractions
        :amount
        (reduce (fn [acc entry]
                  (if (string? entry)
                    (add-to-key acc :description entry)
                    (let [[attr-key & values] entry]
                      (add-to-key acc attr-key (string/join values)))))
                {}
                attributes)))))

(defn assemble-ingredients [ingreds]
  (map normalize-ingredient
       (flatten-ingredients ingreds)))

(defn assemble-time [time]
  (string/join " "
               (map #(string/join (rest %)) (rest time))))

(defn assemble-times [times]
  (if times
    (let [times (rest times)]
      {:prep (assemble-time (nth times 0))
       :cook (assemble-time (nth times 1))
       :total (assemble-time (nth times 2))})
    {}))

(defn assemble-two-line-names [names]
  names)

(defn assemble-three-line-names [names]
  (if (> (count (first names)) (count (second names)))
    [(string/join " " (take 2 names))
     (nth names 2)]
    [(first names)
     (string/join " " (rest names))]))

(defn assemble-four-or-more-line-names [names]
  [(string/join " " (take 2 names))
   (string/join " " (drop 2 names))])

(defn assemble-names [names]
  (let [names (rest names)]
    (case (count names)
      2 (assemble-two-line-names names)
      3 (assemble-three-line-names names)
      (assemble-four-or-more-line-names names))))

(defn assemble-header [header]
  (rest header))

(defn mapify [coll]
  (reduce #(assoc %1 (first %2) %2)
          {}
          coll))

(defn assemble-meal [meal]
  (if (map? meal)
    [:failures (with-out-str (clojure.pprint/pprint meal))]
    (let [meal-map (mapify (rest meal))
          flags (assemble-header (:header meal-map))
          names (assemble-names (:names meal-map))
          entree-name (first names)
          side-name (second names)
          times (assemble-times (:times meal-map))
          ingredients (map assemble-ingredients (filter #(= (first %) :ingredients) (rest meal)))
          entree-ingredients (first ingredients)
          side-ingredients (second ingredients)
          instructions (map assemble-instructions (filter #(= (first %) :instructions) (rest meal)))
          entree-instructions (first instructions)
          side-instructions (second instructions)]
      [:successes
       {:flags flags
        :times times
        :entree {:name entree-name
                 :ingredients entree-ingredients
                 :instructions entree-instructions}
        :side {:name side-name
               :ingredients side-ingredients
               :instructions side-instructions}}])))

(defn copy-to-tempfile [menu]
  (let [tempfile (java.io.File/createTempFile "menu" ".pdf")
        in-file (io/file menu)]
    (io/copy in-file tempfile)
    tempfile))

(defn pdf-to-text-path []
  (string/trim (:out (shell/sh "which" "pdftotext"))))

(defn convert-to-text [pdf]
  (let [pdf-path (.getAbsolutePath pdf)]
    (shell/sh (pdf-to-text-path) "-raw" "-enc" "UTF-8" pdf-path)
    (io/file (string/replace pdf-path #"\.pdf$" ".txt"))))

(defn find-meals [lines]
  (loop [lines lines
         complete-menus []
         partial-menu []]
    (if (empty? lines)
      complete-menus
      (let [line (first lines)]
        (if-let [[_ meal-num] (re-find #"Meal (\d+)" line)]
          (cond (< (read-string meal-num) (count complete-menus)) complete-menus
                (empty? partial-menu) (recur (rest lines)
                                             complete-menus
                                             [line])
                :else (recur lines
                             (conj complete-menus partial-menu)
                             []))
          (if (re-find #"^Copyright" line)
            (recur (rest lines)
                   (conj complete-menus (take (- (count partial-menu) 2) partial-menu))
                   [])
            (recur (rest lines)
                   complete-menus
                   (if (empty? partial-menu) partial-menu (conj partial-menu line)))))))))

(defn extract-meals [menu-text]
  (with-open [menu (io/reader menu-text)]
    (map #(str (string/join "\n" %) "\n") (find-meals (line-seq menu)))))

(defn meals-from-file [file]
  (extract-meals (convert-to-text (copy-to-tempfile file))))

(defn parse-meals [meals]
  (reduce (fn [assembled-meals next-meal]
            (let [[indicator result] (assemble-meal (meal-parser next-meal))]
              (assoc assembled-meals
                     indicator
                     (conj (indicator assembled-meals) result))))
          {:failures [] :successes []}
          meals))

(def sample-meal
"Meal 7
Spicy Sausage and Egg
Scramble
Oregano Roasted Zucchini
1½ lb beef stew meat
½ teaspoon kosher salt, ½ teaspoon
pepper
14 oz can diced tomatoes
1 cup organic beef broth
1 teaspoon minced fresh oregano
---------------------------------------------------------------
1 lb fresh zucchini, trimmed and thinly sliced
1 tablespoon chopped fresh oregano
1 tablespoon olive oil
¼ teaspoon kosher salt, ¼ teaspoon pepper
Heat a nonstick skillet over medium-high heat.
Add sausage to pan; cook 6 minutes or until
done. Drain on a paper towel-lined plate,
reserving 1 tablespoon drippings in pan. Add
onion to pan over medium heat. Cook 3
minutes or until lightly browned. Add eggs; cook
2 minutes, stirring constantly. Stir in sausage,
salt and pepper; cook until eggs are thickened
and set. Note: Remaining pork sausage can be
frozen for up to 2 months.
---------------------------------------------------------------
Preheat oven to 425 degrees. Toss zucchini,
oregano, oil, salt and pepper in a large roasting
pan. Bake 20 minutes or until just tender.
")

;(json/write-str (assemble-meal (meal-parser sample-meal)))
;(assemble-meal [:meal
                ;[:header]
                ;[:names "Spicy Sausage and Egg" "Scramble" "Oregano Roasted Zucchini"]
                ;[:times [:time [:minutes "10" "m"]] [:time [:hours "4" "h"]] [:time [:hours "4" "h"] [:minutes "10" "m"]]]
                ;[:ingredients [:ingredient [:amount "1" "½"] [:unit "lb"] [:description "beef stew meat"]]
                 ;[:ingredients [:ingredient [:amount "½"] [:unit "teaspoon"] [:description "kosher salt"]]
                  ;[:ingredients [:ingredient [:amount "½"] [:unit "teaspoon"] [:description "pepper"]]
                   ;[:ingredients [:ingredient [:amount "1" "4"] [:unit "oz"] [:description "can diced tomatoes"]]
                    ;[:ingredients [:ingredient [:amount "1"] [:unit "cup"] [:description "organic beef broth"]]
                     ;[:ingredients [:ingredient [:amount "1"] [:unit "teaspoon"] [:description "minced fresh oregano"]]]]]]]]
                ;[:ingredients [:ingredient [:amount "1"] [:unit "lb"] [:description "fresh zucchini" "," "trimmed and thinly sliced"]]
                 ;[:ingredients [:ingredient [:amount "1"] [:unit "tablespoon"] [:description "chopped fresh oregano"]]
                  ;[:ingredients [:ingredient [:amount "1"] [:unit "tablespoon"] [:description "olive oil"]]
                   ;[:ingredients [:ingredient [:amount "¼"] [:unit "teaspoon"] [:description "kosher salt"]]
                    ;[:ingredients [:ingredient [:amount "¼"] [:unit "teaspoon"] [:description "pepper"]]]]]]]
                ;[:instructions "H" "eat a nonstick skillet over medium-high heat." "Add sausage to pan; cook 6 minutes or until"
                 ;"done. Drain on a paper towel-lined plate," "reserving 1 tablespoon drippings in pan. Add" "onion to pan over medium heat. Cook 3"
                 ;"minutes or until lightly browned. Add eggs; cook" "2 minutes, stirring constantly. Stir in sausage,"
                 ;"salt and pepper; cook until eggs are thickened" "and set. Note: Remaining pork sausage can be" "frozen for up to 2 months."]
                ;[:instructions "P" "reheat oven to 425 degrees. Toss zucchini," "oregano, oil, salt and pepper in a large roasting"
                 ;"pan. Bake 20 minutes or until just tender."]])

(defn -main [& args]
  (let [parsed (parse-meals (meals-from-file (first args)))]
    (try
      (json/pprint parsed :escape-slash false)
      (finally
        (flush)
        (shutdown-agents)))))
