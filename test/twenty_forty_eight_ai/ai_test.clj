(ns twenty-forty-eight-ai.ai-test
  (:require [expectations :refer :all]
            [twenty-forty-eight-ai.ai :refer :all]))

; No arguments
(expect nil (max-number-list))
; One argument
(expect '(0) (max-number-list '(0)))
; Two empty lists
(expect nil (max-number-list nil nil))
; An empty list and a non-empty list
(expect '(0) (max-number-list nil '(0)))
; Two lists of count 1
(expect '(1) (max-number-list '(0) '(1)))
; Two equivalent lists of different sizes
(expect '(0 0) (max-number-list '(0) '(0 0)))
; Two lists of different sizes with the shorter being larger
(expect '(1) (max-number-list '(1) '(0 0)))
; Two lists of different sizes with the longer being larger
(expect '(0 1) (max-number-list '(0) '(0 1)))
; Two lists with count > 1
(expect '(0 1) (max-number-list '(0 0) '(0 1)))
; More than two lists
(expect '(1) (max-number-list nil '(0) '(1) '(0 0) '(0 1)))

; No arguments
(expect nil (sum-number-lists))
; One argument
(expect '(0) (sum-number-lists '(0)))
; Two empty lists
(expect nil (sum-number-lists nil nil))
; An empty list and a non-empty list
(expect '(0) (sum-number-lists nil '(0)))
; Two lists of count 1
(expect '(1) (sum-number-lists '(0) '(1)))
; Two equivalent lists of different sizes
(expect '(0 0) (sum-number-lists '(0) '(0 0)))
; Two lists of different sizes with the shorter being larger
(expect '(1 0) (sum-number-lists '(1) '(0 0)))
; Two lists of different sizes with the longer being larger
(expect '(0 1) (sum-number-lists '(0) '(0 1)))
; Two lists with count > 1
(expect '(0 1) (sum-number-lists '(0 0) '(0 1)))
; More than two lists
(expect '(1 1) (sum-number-lists nil '(0) '(1) '(0 0) '(0 1)))
