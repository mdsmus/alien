(split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
=> ("ab" "a" "a" "ab" "a"), 11 
