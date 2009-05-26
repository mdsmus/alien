(split-sequence #\; "a;;b;c")
=> ("a" "" "b" "c"), 6

(split-sequence #\; "a;;b;c" :from-end t)
=> ("a" "" "b" "c"), 0

(split-sequence #\; "a;;b;c" :from-end t :count 1)
=> ("c"), 4

(split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
=> ("a" "b" "c"), 6

(split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
=> ("oo" "bar" "b"), 9
