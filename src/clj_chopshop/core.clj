(ns clj-chopshop.core
  (:require [instaparse.core :as insta]))

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

(comment
  (clojure.pprint/pprint (clj-parse "#!/bin/bash\n(reduce {(;foo bar\n){}zip zap,herp derp})"))
  )
