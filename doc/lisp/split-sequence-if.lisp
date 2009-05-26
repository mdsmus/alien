(split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
=> ("" "" "r" "c" "d" "" "r" ""), 11
