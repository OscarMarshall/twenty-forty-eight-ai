(ns twenty-forty-eight-ai.ai-test
  (:require [expectations :refer :all]
            [twenty-forty-eight-ai.ai :refer :all]))

(expect [2 2 0 0] (tier-score [[2 0 0 0] [0 2 0 0] [0 0 0 0] [0 0 0 0]]))
(expect [4 0 0 0] (tier-score [[2 2 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]))
(expect [2 2 0 0] (tier-score [[0 0 0 0] [2 2 0 0] [0 0 0 0] [0 0 0 0]]))
(expect [192 0 0 0] (tier-score [[128 64 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]))
(expect [192 0 0 0] (tier-score [[0 0 0 0] [0 0 0 0] [0 0 0 0] [128 64 0 0]]))
(expect [6 0 0 0] (tier-score [[2 4 2 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]))
