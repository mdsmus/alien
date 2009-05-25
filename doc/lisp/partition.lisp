(partition '(1 2 3) #'oddp #'evenp) => ((1 3) (2))
(partition '(1 2 3) #'oddp t) => ((1 3) (1 2 3))
(partition '(1 2 3) #'oddp #'stringp) => ((1 3) nil)
