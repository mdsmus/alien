(defpackage #:cl-extensions
  (:nicknames :cl-ext)
  (:use :cl)
  #+:allegro
  (:shadow :copy-file
           :delete-directory-and-files)
  #+:abcl
  (:shadow :list-directory)
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
   #:print-grid
   ;; conditions
   #:required-argument
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:ignore-nome-conditions
   #:unwind-protect-case
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
   #:while*
   #:until*
   #:awhile
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
   #:defsubst
   #:defcustom
   #:mkdir
   #:rmdir
   #:default-directory
   #:run-prog
   #:pipe-output
   #:pipe-input
   #:close-pipe
   #:with-open-pipe
   #:quit
   #:getenv
   #:argv
   #:def-special-environment
   #:collect-timing
   #:arglist
   #:class-slot-list
   #:class-slot-initargs
   #:structure-slots
   #:structure-keyword-constructor
   #:structure-boa-constructors
   #:structure-copier
   #:structure-predicate
   #:sysinfo
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
   #:unflatten
   #:dolist*
   #:partition
   #:dotree
   #:push*
   #:append1
   #:length=1
   #:list-to-alist
   #:transpose-list
   #:find-anywhere
   #:find-all
   #:maybe-add
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
   #:split-sequence
   #:split-sequence-if
   #:split-sequence-if-not
   #:member-equal
   #:sort*
   ;; strings
   #:+lower-case-ascii-alphabet+
   #:+upper-case-ascii-alphabet+
   #:+base64-alphabet+
   #:+alphanumeric-ascii-alphabet+
   #:+ascii-alphabet+
   #:random-string
   #:stringify
   #:strcat
   #:strcat*
   #:join-strings
   #:fold-strings
   #:string-to-octets
   #:octets-to-string
   #:encoding-keyword-to-native
   #:string-from-array
   #:empty-string-p
   #:split-tab
   #:split-space
   #:split-newline
   #:replace-all
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
   #:funcall-if
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
   ;; io, files and pathnames
   #:with-input-from-file
   #:with-output-to-file
   #:read-file-into-string
   #:write-string-into-file
   #:copy-file
   #:copy-stream
   #:chdir
   #:with-dir
   #:copy-file
   #:copy-stream
   #:delete-directory-and-files
   #:directory-exists-p
   #:directory-pathname-p
   #:file-exists-p
   #:list-directory
   #:pathname-as-directory
   #:pathname-as-file
   #:walk-directory
   ;; math
   #:10^
   #:expt-mod
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
   #:invalid-number
   #:parse-number
   #:parse-real-number
   #:parse-positive-real-number
   #:running-average
   #:square
   #:sum
   #:between
   #:dot-product
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
   ;; iterate
   #:iterate #:iter #:display-iterate-clauses
   #:defsynonym #:dsetq #:declare-variables
   #:defmacro-clause #:defmacro-driver #:defclause-sequence
   #:initially #:after-each #:finally #:finally-protected
   #:else #:if-first-time #:first-iteration-p #:first-time-p
   #:finish #:leave #:next-iteration #:next #:terminate
   #:repeat #:for #:as #:generate #:generating #:in
   #:sum #:summing #:multiply #:multiplying
   #:maximize #:minimize #:maximizing #:minimizing #:counting
   #:always #:never #:thereis #:finding #:collect #:collecting
   #:with #:while #:until #:adjoining #:nconcing #:appending
   #:nunioning #:unioning #:reducing #:accumulate #:accumulating
   ))

(in-package :cl-extensions)
