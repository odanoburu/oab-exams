(ql:quickload :cl-ppcre)

;; http://www.regular-expressions.info/captureall.html

;; questão
"ENUM\\s+Questão\\s*(\\d+)"
;; enunciado
"\\s+([\\S|\\s]+.)\\s*OPTIONS"
;; items
"\\s+([A-E])(\:CORRECT)?\\)"
;; item texto
"\\s+([\\S|\\s]+?)\\n+" ;;+? =  non greedy
;; nr-questão, enunciado, texto
"ENUM\\s*Questão\\s*(\\d+)\\s+([\\S|\\s]+?.)\\s*OPTIONS"
;;item, correta, texto-item
"\\s*([A-E])(\:CORRECT)?\\)\\s+([\\S|\\s]+?)\\n{2,}
