(ns clj-chopshop.parse
  (:require [clojure.java.io :as io])
  (:import [java.util.regex Pattern Matcher]))

;; ...now you have two problems...

(def ^:private QUOTE "\"")
(def ^:private NON-QUOTE "[^\"]")
(def ^:private NO-BS "(?<!\\\\)")
(def ^:private LEADING-BS (str NO-BS "\\\\"))
(def ^:private PAIRED-BS "(?:\\\\\\\\)")
(def ^:private ODD-BS (str LEADING-BS PAIRED-BS "*"))
(def ^:private STRING
  (str  NO-BS QUOTE "(?:" NON-QUOTE "|" "(?:(?:" ODD-BS ")" QUOTE "))*" NO-BS QUOTE))

;; We are very generous.
;; The tokenizer is designed to keep on chuggin'.
;;
;; This means that we accept things as symbols or numbers which are
;; not symbols or numbers. By using the Clojure reader to realize
;; their values, they'll be fully tested later. Because we're
;; generous, however, we can keep parsing the whole file and present
;; all errors at once.
(def ^:private NOT-NON-SYM "[^\\s,(){}\\[\\]\\\"\\'#]") ; TODO
(def ^:private NUMBER (str "[-+]?\\d" NOT-NON-SYM "*"))
(def ^:private SYMBOL (str NOT-NON-SYM "+"))

;; Order matters. Some patterns are subsets of other patterns.
(def pattern-list
  [[:atom "whitespace"     "(?m)\\s+(?-m)"]
   [:atom "semicomment"    ";.*"]
   [:atom "shebangcomment" "#!.*"]
   [:atom "comma"          ","]
   [:atom "char"           "\\\\."]
   [:atom "string"         STRING]
   [:atom "regex"          (str "#" STRING)]
   [:atom "number"         NUMBER]

   [:pfix "removal"       "#_"]
   [:pfix "symbolquote"   "'"]
   [:pfix "syntaxquote"   "`"]
   [:pfix "varquote"      "#'"]
   [:pfix "deref"         "@"]
   [:pfix "unquote"       "~"]
   [:pfix "unquotesplice" "~@"]
   [:pfix "eval"          "#="]

   [:meta "metadata"      "\\^"]

   [:coll "listliteral"   "\\("]
   [:coll "vectorliteral" "\\["]
   [:coll "mapliteral"    "\\{"]
   [:coll "setliteral"    "#\\{"]
   [:coll "fnliteral"     "#\\("]

   [:tail "closeparen"  "\\)"]
   [:tail "closecurly"  "\\}"]
   [:tail "closesquare" "\\]"]

   ;; Symbol like things...
   [:pfix "constructor" (str "#" SYMBOL)]
   [:atom "keyword"     (str ":" SYMBOL)]
   [:atom "symbol"      SYMBOL]
   ])

(def full-pattern
  (->>
   (for [[t k v] pattern-list]
     (str "(?<" k ">" v ")"))
   (flatten)
   (interpose "|")
   (apply str)))

(defn run [t]
  (let [^Pattern p (Pattern/compile full-pattern)
        ^Matcher m (.matcher p t)]
    (while (.find m)
      (doseq [[t k v] pattern-list]
        (when-let [x (.group m k)]
          (prn {:type t :name k :start (.start m) :end (.end m) :token x}))))))

;; Self test
(def s (slurp (io/resource "clj_chopshop/parse.clj")))
;; Test forms
(def n 3.1415)
(def N 3/1415)
