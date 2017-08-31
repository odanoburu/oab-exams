
(defun read-file (filename)
  (coerce (with-open-file (stream filename)
	    (loop for char = (read-char stream nil)
		  while char
		  collect char))
	  'string))

(defun split-exam (exam-str)
  (rest (ppcre:split "---" exam-str))) ;; rest because starts with ---

(defun exam-to-questions (exam-path)
  (split-exam (read-file exam-path)))

;; nr-questão, enunciado
"ENUM\\s*Questão\\s*(\\d+)\\s+([\\S|\\s]+?.)\\s*OPTIONS"
;;item, correta?, texto-item
"\\s*([A-E])(\:CORRECT)?\\)\\s+([\\S|\\s]+?)\\n{2,}"

(defun parse-enum (question-str)
  (ppcre:scan "ENUM\\s*Questão\\s*(\\d+)\\s+([\\S|\\s]+?.)\\s*OPTIONS"
                         question-str))

(defun parse-item (question-str)
  (ppcre:scan-to-strings "\\s*([A-E])(\:CORRECT)?\\)\\s+([\\S|\\s]+?)\\n{2,}"
              question-str))


;; http://weitz.de/cl-ppcre/#scan bug here
(defun get-str (str ix-vector)
  (assert (< (length ix-vector) 3))
  (let ((ix-list (coerce ix-vector 'list)))
    (subseq str (first ix-list) (second ix-list))))

(defun trim-str (question-str ix)
  (subseq question-str ix))

(defun parse-items (question-str &optional parsed-items)
  (multiple-value-bind (* i-end i-letter i-correct? i-text)
      (parse-item question-str)
    (if (null question-str)
        (reverse parsed-items)
        (parse-items (trim-str question-str i-end)
                     (cons (mapcar #'(lambda (ix-vector)
                                     (get-str question-str ix-vector))
                                   (list i-letter i-correct? i-text))
                           parsed-items)))))

(defun parse-question (question-str)
  (multiple-value-bind (* enum-end q-nr q-enum)
      (parse-enum question-str)
    (cons (get-str question-str q-nr)
          (cons (get-str question-str q-enum)
                (parse-items (trim-str question-str
                                       enum-end))))))

(defun parse-questions (questions &optional parsed-questions)
  (if (endp questions)
      parsed-questions
      (parse-questions (rest questions)
                       (cons (parse-question (first questions))
                             parsed-questions))))
      
