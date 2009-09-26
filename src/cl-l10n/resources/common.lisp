
(export '(with-indefinite-article with-definite-article))

(defun with-indefinite-article (str &key capitalize-first-letter)
  (let ((article (indefinite-article-for str)))
    (strcat (if capitalize-first-letter
                (capitalize-first-letter article)
                article)
            #\Space str)))

(defun with-definite-article (str &key capitalize-first-letter)
  (let ((article (definite-article-for str)))
    (strcat (if capitalize-first-letter
                (capitalize-first-letter article)
                article)
            #\Space str)))
