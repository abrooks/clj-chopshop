(ns clj-chopshop.core
  (:require [instaparse.core :as insta]
            [clojure.walk :as walk]))

(def clj-parse
  (insta/parser "
    <CljSrc> = SrcEnt*
    <SrcEnt> = Readable | ReadIgnored

    <ReadIgnored> = Comment | Comma | Whitespace
    Comma = ','
    Whitespace = #'\\s+'

    <Comment> = ShebangComment | ReaderRemove | SemiComment
    ShebangComment = '#!' #'.*'
    SemiComment = ';' #'.*'
    ReaderRemove = '#_' Whitespace* Readable

    <Readable> = (List | Vector | Set | Map | Number
                  | Symbol | Keyword | Character | Meta
                  | String | Quotes | Unquotes | Deref
                  | ReaderDispatch | GenSym )

    (* <Number> = Long | Double | Radix *)
    Number = #'[+-]?[0-9]+(\\.[0-9]+)?'

    <SymSeg> = #'[^\\s,(){}\\[\\]\\\"\\'`#]+'
    <SymInt> = (SymSeg ('.' SymSeg)*) ('/' SymSeg ('.' SymSeg)*)?
    Symbol = SymInt
    Keyword = ':' ':'? SymInt

    GenSym = SymInt '#'

    List = '(' SrcEnt* ')'
    Vector = '[' SrcEnt* ']'
    Map = '{' MapPairs* ReadIgnored* '}'
    Set = '#{' SrcEnt* '}'
    <MapPairs> = ReadIgnored* Readable ReadIgnored* Readable

    Character = #'\\\\.'

    <BS> = '\\\\'
    <QUOTE> = '\"'
    <NON-QUOTE-BS> = #'[^\\\\\"]'
    <ESC> = BS (QUOTE | 'n' | 'r' | 't' | BS)
    String = !BS QUOTE (ESC | NON-QUOTE-BS)* QUOTE

    <ReaderDispatch> = Regex | Fn | Eval | VarQuote
    Regex = '#' String
    Fn = '#(' SrcEnt* ')'
    Eval = '#=(' SrcEnt* ')'
    VarQuote = '#\\'' <SymInt>

    <Quotes> = SymbolQuote | SyntaxQuote
    SymbolQuote = \"'\" ReadIgnored* Readable
    SyntaxQuote = '`' ReadIgnored* Readable

    <Unquotes> = UnquoteSplice | Unquote
    Unquote = '~' Readable
    UnquoteSplice = '~@' Readable

    Deref = '@' Readable

    Meta = '^' ReadIgnored* Readable
"))

(defn reassemble [ptree]
  (apply str
         (for [e ptree
               :when (not (keyword? e))]
           (if (string? e)
             e
             (reassemble e)))))

(comment
  (def t "#!/bin/bash\n(reduce \"foo\\n\" 123 12.34 \\c ^:foo ^ [1 2 3] #{a b}\\_ {(;foo bar\n){}zip zap,herp derp.ferp/derpy.derp} #())")
  (clojure.pprint/pprint (insta/transform {:String str} (clj-parse t)))
  (clojure.pprint/pprint (insta/transform {:String str} (clj-parse (slurp "src/clj_chopshop/core.clj"))))
  (walk/postwalk #(if (coll? %)
                    (remove (fn [x]
                              (and (coll? x)
                                   (#{:Whitespace :ShebangComment :SemiComment} (first x))))
                            %)
                    %)
                 (clj-parse t))
  )
