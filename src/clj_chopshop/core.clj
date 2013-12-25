(ns clj-chopshop.core
  (:require [instaparse.core :as insta]
            [clojure.walk :as walk]))

(def clj-parse
  (insta/parser "
    <CljSrc> = SrcEnt*
    <SrcEnt> = Readable | ReadIgnored
    <ReadIgnored> = Comment | Comma | Whitespace
    Comma = ','
    <Comment> = ShebangComment | ReaderRemove | SemiComment
    ShebangComment = '#!' #'.*'
    SemiComment = ';' #'.*'
    Whitespace = #'\\s+'
    ReaderRemove = '#_' Whitespace* Readable
    <Readable> = List | Set | Map | Symbol | Character (* Set ReaderPragma Vector ... *)
    Set = '#{' SrcEnt* '}'
    Character = #'\\\\.'
    Symbol =  #'[A-Za-z0-9_]+' (* TODO: Not this *)
    List = '(' SrcEnt* ')'
    Map = '{' MapPairs* '}'
    <MapPairs> = ReadIgnored? Readable ReadIgnored? Readable ReadIgnored?
"))

(defn reassemble [ptree]
  (apply str
         (for [e ptree
               :when (not (keyword? e))]
           (if (string? e)
             e
             (reassemble e)))))

(comment
  (def t "#!/bin/bash\n(reduce \\c #{a b}\\_ {(;foo bar\n){}zip zap,herp derp})")
  (clojure.pprint/pprint (clj-parse t))
  (walk/postwalk #(if (coll? %)
                    (remove (fn [x]
                              (and (coll? x)
                                   (#{:Whitespace :ShebangComment :SemiComment} (first x))))
                            %)
                    %)
                 (clj-parse t))
  )
