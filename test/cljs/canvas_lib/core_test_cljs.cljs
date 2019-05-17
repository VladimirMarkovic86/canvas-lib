(ns canvas-lib.core-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [canvas-lib.chart.core :refer [min-max-iterate-coordinates
                                           find-x-y-min-max calculate-segment-value
                                           axis-segment format-segment-value
                                           bar-min-max-iterate-coordinates
                                           find-bar-x-y-min-max bar-axis-segment]]))

(deftest test-min-max-iterate-coordinates
  (testing "Test min max iterate coordinates"
    
    (let [dot-values nil
          x-min nil
          x-max nil
          y-min nil
          y-max nil
          result (min-max-iterate-coordinates
                   dot-values
                   x-min
                   x-max
                   y-min
                   y-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [[20 20] [30 30] [40 40]]
          x-min (atom (aget
                        js/Number
                        "MAX_SAFE_INTEGER"))
          x-max (atom (aget
                        js/Number
                        "MIN_SAFE_INTEGER"))
          y-min (atom (aget
                        js/Number
                        "MAX_SAFE_INTEGER"))
          y-max (atom (aget
                        js/Number
                        "MIN_SAFE_INTEGER"))
          result (min-max-iterate-coordinates
                   dot-values
                   x-min
                   x-max
                   y-min
                   y-max)]
      (is
        (= @x-min
           20)
       )
      
      (is
        (= @x-max
           40)
       )
      (is
        (= @y-min
           20)
       )
      
      (is
        (= @y-max
           40)
       )
      
     )
    
   ))

(deftest test-find-x-y-min-max
  (testing "Test find x y min max"
    
    (let [dot-values nil
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values []
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [1 2 3]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [[1 1] [2 2] [3 3]]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (= result
           [1 3 1 3])
       )
      
     )
    
    (let [dot-values [[20 100] [40 200] [60 100]]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (= result
           [20 60 100 200])
       )
      
     )
    
   ))

(deftest test-calculate-segment-value
  (testing "Test calculate segment value"
    
    (let [lower-limit nil
          higher-limit nil
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [lower-limit 16
          higher-limit 33
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (= result
           20)
       )
      
     )
    
    (let [lower-limit 166
          higher-limit 333
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (= result
           200)
       )
      
     )
    
   ))

(deftest test-axis-segment
  (testing "Test axis segment"
    
    (let [min-value nil
          max-value nil
          width nil
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [min-value 0
          max-value 500
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]
            [100 60]
            [200 120]
            [300 180]
            [400 240]
            [500 300]])
       )
      
     )
    
    (let [min-value 200
          max-value 500
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[200 0] [300 100] [400 200] [500 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 50
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [10 60] [20 120] [30 180] [40 240] [50 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 500
          width 30
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]])
       )
      
     )
    
    (let [min-value 0
          max-value 5000
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [1000 60] [2000 120] [3000 180] [4000 240] [5000 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 5000000
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]
            [1000000 60]
            [2000000 120]
            [3000000 180]
            [4000000 240]
            [5000000 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 400
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [100 75] [200 150] [300 225] [400 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 300
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [100 100] [200 200] [300 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 200
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [50 75] [100 150] [150 225] [200 300]])
       )
      
     )
    
    (let [november-2018 (js/Date.
                          "2018-11-01")
          july-2019 (js/Date.
                      "2019-07-01")
          min-value (.getTime
                      november-2018)
          max-value (.getTime
                      july-2019)
          width 500
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[1541030400000 0]
            [1545030400000 100]
            [1549030400000 200]
            [1553030400000 300]
            [1557030400000 400]
            [1561030400000 500]])
       )
      
     )
    
   ))

(deftest test-format-segment-value
  (testing "Test format segment value"
    
    (let [segment-value nil
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [segment-value 1
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1")
       )
      
     )
    
    (let [segment-value -1
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1")
       )
      
     )
    
    (let [segment-value 1000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1K")
       )
      
     )
    
    (let [segment-value -1000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1K")
       )
      
     )
    
    (let [segment-value 1000
          segment-value-type nil
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1H")
       )
      
     )
    
    (let [segment-value -1000
          segment-value-type nil
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1H")
       )
      
     )
    
    (let [segment-value 1000000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1M")
       )
      
     )
    
    (let [segment-value -1000000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1M")
       )
      
     )
    
    (let [january-2019 (js/Date.
                         "2019-01-01")
          segment-value january-2019
          segment-value-type "date"
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "2019-01-01")
       )
      
     )
    
    (let [january-2019 (js/Date.
                         "2019-01-01")
          segment-value january-2019
          segment-value-type "date"
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "01.01.2019")
       )
      
     )
    
   ))

(deftest test-bar-min-max-iterate-coordinates
  (testing "Test bar min max iterate coordinates"
    
    (let [bar-values nil
          axis-min nil
          axis-max nil
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values []
          axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (= @axis-min
           (aget
             js/Number
             "MAX_SAFE_INTEGER"))
       )
      
      (is
        (= @axis-max
           (aget
             js/Number
             "MIN_SAFE_INTEGER"))
       )
      
     )
    
    (let [bar-values [20 50 30]
          axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (= @axis-min
           20)
       )
      
      (is
        (= @axis-max
           50)
       )
      
     )
    
   ))

(deftest test-find-bar-x-y-min-max
  (testing "Test find bar x y min max"
    
    (let [bar-values nil
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values []
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values [10 20 30]
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 (aget
                js/Number
                "MIN_SAFE_INTEGER")
            0 30])
       )
      
     )
    
    (let [bar-values [10 20 30]
          multi-bars nil
          bar-values-on-x-axis true
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 30
            0 (aget
                js/Number
                "MIN_SAFE_INTEGER")])
       )
      
     )
    
    (let [bar-values [[10 20 30]
                      [30 40 50]]
          multi-bars true
          bar-values-on-x-axis true
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 50
            0 (aget
                js/Number
                "MIN_SAFE_INTEGER")])
       )
      
     )
    
    (let [bar-values [[10 20 30]
                      [30 40 50]]
          multi-bars true
          bar-values-on-x-axis false
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 (aget
                js/Number
                "MIN_SAFE_INTEGER")
            0 50])
       )
      
     )
    
   ))

(deftest test-bar-axis-segment
  (testing "Test bar axis segment"
    
    (let [bar-labels nil
          width nil
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-labels []
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-labels ["data 1" "data 2" "data 3"]
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (= result
           [["data 1" 33]
            ["data 2" 99]
            ["data 3" 165]])
       )
      
     )
    
    (let [bar-labels ["data 1" "data 2" "data 3"
                      "data 4" "data 5"]
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (= result
           [["data 1" 20]
            ["data 2" 60]
            ["data 3" 100]
            ["data 4" 140]
            ["data 5" 180]
            ])
       )
      
     )
    
   ))

