(defpackage #:cl-extensions
  (:nicknames :cl-ext)
  (:use :cl)
  (:export
   ;; macros
   #:with-gensyms
   #:with-unique-names
   #:make-gensym-list
   #:once-only
   #:parse-body
   #:parse-ordinary-lambda-list
   #:rebinding
   #:rebind
   ;; arrays
   #:copy-array
   #:make-displaced-array
   ;; conditions
   #:required-argument
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:ignore-nome-conditions
   ;; control structures
   #:if-let
   #:when-let
   #:when-let*
   #:extract-function-name
   #:switch
   #:eswitch
   #:cswitch
   #:whichever
   #:xor
   #:nth-value-or
   #:define-constant
   #:if-bind
   #:aif
   #:when-bind
   #:awhen
   #:cond-bind
   #:acond
   #:aand
   #:and-bind
   #:if2-bind
   #:aif2
   #:while*
   #:until*
   #:awhile
   #:acond2
   #:varsymp
   #:binding
   #:list-match-case
   #:let1
   ;; packages and symbols
   #:featurep
   #:ensure-symbol
   #:maybe-intern
   #:format-symbol
   #:make-keyword
   #:make-gensym
   #:symbolicate
   #:intern-concat
   ;; environment
   #:quit
   #:getenv
   #:argv
   #:def-special-environment
   #:collect-timing
   ;; evaluation
   #:eval-always
   ;; lists
   #:range
   #:map-range
   #:do-range
   #:iota
   #:map-iota
   #:plist-to-alist
   #:alist-to-plist
   #:doplist
   #:appendf
   #:nconcf
   #:unionf
   #:nunionf
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:proper-list-p
   #:proper-list
   #:lastcar
   #:make-circular-list
   #:circular-list
   #:ensure-car
   #:ensure-cons
   #:ensure-list
   #:remove-from-plist
   #:delete-from-plist
   #:mappend
   #:setp
   #:set-equal
   #:map-product
   #:flatten
   #:dolist*
   #:partition
   #:dotree
   #:push*
   #:append1
   #:length=1
   #:partitionx
   ;; sequences
   #:rotate
   #:shuffle
   #:random-elt
   #:removef
   #:deletef
   #:proper-sequence
   #:emptyp
   #:length=
   #:sequence-of-length-p
   #:copy-sequence
   #:first-elt
   #:last-elt
   #:starts-with-subseq
   #:ends-with-subseq
   #:starts-with
   #:ends-with
   #:map-combinations
   #:map-permutations
   #:map-derangements
   #:remove-keywords
   #:remf-keywords
   #:tail
   #:but-tail
   #:head
   #:but-head
   #:read-sequence*
   #:levenshtein-distance
   ;; strings
   #:+lower-case-ascii-alphabet+
   #:+upper-case-ascii-alphabet+
   #:+base64-alphabet+
   #:+alphanumeric-ascii-alphabet+
   #:+ascii-alphabet+
   #:random-string
   #:strcat
   #:strcat*
   #:join-strings
   #:fold-strings
   #:string-to-octets
   #:octets-to-string
   #:encoding-keyword-to-native
   #:string-from-array
   ;; functions
   #:ensure-function
   #:disjoin
   #:conjoin
   #:compose
   #:multiple-value-compose
   #:curry
   #:rcurry
   #:named-lambda
   #:make-reducer
   #:with-reducer
   #:make-collector
   #:make-pusher
   #:with-collector
   #:with-collectors
   ;; hash tables
   #:copy-hash-table
   #:maphash-keys
   #:maphash-values
   #:hash-table-keys
   #:hash-table-values
   #:hash-to-alist
   #:hash-to-plist
   #:alist-to-hash
   #:plist-to-hash
   #:ensure-gethash
   #:buld-hash-table
   #:make-lookup-name
   #:deflookup-table
   ;; io
   #:with-input-from-file
   #:with-output-to-file
   #:read-file-into-string
   #:write-string-into-file
   #:copy-file
   #:copy-stream
   ;; math
   #:10^
   #:parse-float
   #:mulf
   #:divf
   #:minf
   #:maxf
   #:clamp
   #:lerp
   #:mean
   #:median
   #:variance
   #:standard-deviation
   #:factorial
   #:gaussian-random
   #:binaminal-coefficient
   #:subfactorial
   #:count-permutations
   ;; oop
   #:defclass-struct
   #:generate-defclass
   #:defprint-object
   #:with-accessors*
   #:class-name-of
   ;; types
   ;; FIXME: add symbols by macrolet frob
   #:array-index
   #:array-length
   #:of-type
   #:type=
   #:coercef
   ;; optional names
   #:defalias
   #:defvaralias
   #:defmacalias
   #:fun
   #:new
   #:!
   ))

(in-package :cl-extensions)
