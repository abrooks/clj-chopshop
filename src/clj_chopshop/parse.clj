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
  [;; atoms...
   "whitespace"     "(?m)\\s+(?-m)"
   "semicomment"    ";.*"
   "shebangcomment" "#!.*"
   "comma"          ","
   "char"           "\\\\."
   "string"         STRING
   "regex"          (str "#" STRING)
   "number"         NUMBER

   ;; prefixes...
   "removal"       "#_"
   "symbolquote"   "'"
   "syntaxquote"   "`"
   "varquote"      "#`"
   "deref"         "@"
   "metadata"      "\\^"
   "unquote"       "~"
   "unquotesplice" "~@"
   "eval"          "#="

   ;; collections...
   "listliteral"   "\\("
   "vectorliteral" "\\["
   "mapliteral"    "\\{"
   "setliteral"    "#\\{"
   "fnliteral"     "#\\("

   ;; collection closers...
   "closeparen"  "\\)"
   "closecurly"  "\\}"
   "closesquare" "\\]"

   ;; Symbol like things...
   "keyword"     (str ":" SYMBOL)
   "constructor" (str "#" SYMBOL)
   "symbol"      SYMBOL
   ])

(def full-pattern
  (->>
   (for [[k v] (partition 2 pattern-list)]
     (str "(?<" k ">" v ")"))
   (flatten)
   (interpose "|")
   (apply str)))

(defn run [t]
  (let [^Pattern p (Pattern/compile full-pattern)
        ^Matcher m (.matcher p t)
        name-list (partition 2 pattern-list)]
    (while (.find m)
      (doseq [[k v] name-list]
        (when-let [x (.group m k)]
          (println "Found" k "pattern:" x))))))

;; Self test
(def s (slurp (io/resource "clj_chopshop/parse.clj")))
;; Test forms
(def n 3.1415)
(def N 3/1415)
