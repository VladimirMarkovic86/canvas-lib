(ns canvas-lib.chart.core
  (:require [htmlcss-lib.core :refer [gen canvas]]
            [utils-lib.core :as utils]))

(defn min-max-iterate-coordinates
  "Iterates through coordinates"
  [dot-values
   x-min
   x-max
   y-min
   y-max]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values)
              )
             x-min
             (instance?
               Atom
               x-min)
             x-max
             (instance?
               Atom
               x-max)
             y-min
             (instance?
               Atom
               y-min)
             y-max
             (instance?
               Atom
               y-max))
    (doseq [coordinate-pair dot-values]
      (let [[x y] (when (and coordinate-pair
                             (vector?
                               coordinate-pair)
                             (= (count
                                  coordinate-pair)
                                2))
                    coordinate-pair)]
        (let [x-as-number (if (instance?
                                js/Date
                                x)
                            (.getTime
                              x)
                            x)
              y-as-number (if (instance?
                                js/Date
                                y)
                            (.getTime
                              y)
                            y)]
          (when (< x-as-number
                   @x-min)
            (reset!
              x-min
              x-as-number))
          (when (< @x-max
                   x-as-number)
            (reset!
              x-max
              x-as-number))
          (when (< y-as-number
                   @y-min)
            (reset!
              y-min
              y-as-number))
          (when (< @y-max
                   y-as-number)
            (reset!
              y-max
              y-as-number))
         ))
     ))
 )

(defn find-x-y-min-max
  "Finds minimum and maximum of x and y axis"
  [dot-values
   & [multi-line]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
         )
    (let [x-min (atom (aget
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
                        "MIN_SAFE_INTEGER"))]
      (if multi-line
        (doseq [line-vector dot-values]
          (min-max-iterate-coordinates
            line-vector
            x-min
            x-max
            y-min
            y-max))
        (min-max-iterate-coordinates
          dot-values
          x-min
          x-max
          y-min
          y-max))
      (when (and @x-min
                 @x-max
                 @y-min
                 @y-max)
        [@x-min @x-max
         @y-min @y-max]))
   ))

(defn calculate-segment-value
  "Calculates segment value"
  [lower-limit
   higher-limit]
  (when (and lower-limit
             (number?
               lower-limit)
             higher-limit
             (number?
               higher-limit))
    (let [number-of-digits (- (count
                                (str
                                  higher-limit))
                              1)
          number-base (atom 1)]
      (dotimes [i number-of-digits]
        (swap!
          number-base
          *
          10))
      (let [lower-limit-left-over (- @number-base
                                     (mod
                                       lower-limit
                                       @number-base))
            new-lower-limit (+ lower-limit
                               lower-limit-left-over)
            higher-limit-left-over (mod
                                     higher-limit
                                     @number-base)
            new-higher-limit (- higher-limit
                                higher-limit-left-over)
            generate-range (range
                             new-lower-limit
                             (inc
                               new-higher-limit)
                             @number-base)
            segment-value-i (last
                              generate-range)
            lower-limit-base (long
                               (/ lower-limit
                                  @number-base))
            higher-limit-base (long
                                (/ higher-limit
                                   @number-base))
            segment-base (long
                           (/ (+ higher-limit-base
                                 lower-limit-base
                                 1)
                              2))
            segment-value-ii (* segment-base
                                @number-base)
            result (atom segment-value-i)]
        (when (contains?
                #{1 2 5}
                segment-base)
          (reset!
            result
            segment-value-ii))
        @result))
   ))

(defn axis-segment
  "Calculate segments in pixels and round value for every segment"
  [min-value
   max-value
   width]
  (when (and min-value
             (number?
               min-value)
             max-value
             (number?
               max-value)
             width
             (number?
               width))
    (let [segment-value-length (- max-value
                                  min-value)
          lower-limit (long
                        (/ (* segment-value-length
                              50)
                           width))
          higher-limit (long
                         (/ (* segment-value-length
                               100)
                            width))
          segment-value (calculate-segment-value
                          lower-limit
                          higher-limit)
          segment-value-range (range
                                min-value
                                (inc
                                  max-value)
                                segment-value)
          segment-pixel-value (long
                                (/ width
                                   (long
                                     (/ segment-value-length
                                        segment-value))
                                 ))
          segment-pixel-value-range (range
                                      0
                                      (inc
                                        width)
                                      segment-pixel-value)
          result (atom [])
          map-result (map
                       (fn [segment-value-p
                            segment-pixel-value-p]
                         (swap!
                           result
                           conj
                           [segment-value-p
                            segment-pixel-value-p]))
                       segment-value-range
                       segment-pixel-value-range)]
      (last
        map-result))
   ))

(defn format-segment-value
  "Formats segment value
   10 -> 10
   1000 -> 1K
   1000000 -> 1M"
  [segment-value
   & [segment-value-type
      selected-language]]
  (let [result (atom "")
        segment-value-type (or segment-value-type
                               "number")]
    (when (and segment-value
               (number?
                 segment-value)
               (= segment-value-type
                  "number"))
      (let [number-of-digits (if (neg?
                                   segment-value)
                               (dec
                                 (count
                                   (str
                                     segment-value))
                                )
                               (count
                                 (str
                                   segment-value))
                              )]
        (when (< number-of-digits
                 4)
          (swap!
            result
            str
            segment-value))
        (when (< 3
                 number-of-digits
                 7)
          (let [formated-number (utils/round-decimals
                                  (double
                                    (/ segment-value
                                       1000))
                                  (if (< 0
                                         (mod segment-value
                                              1000))
                                    1
                                    0))]
            (swap!
              result
              str
              formated-number
              (if (= selected-language
                     "serbian")
                "H"
                "K"))
           ))
        (when (< 6
                 number-of-digits
                 10)
          (let [formated-number (utils/round-decimals
                                  (double
                                    (/ segment-value
                                       1000000))
                                  (if (< 0
                                         (mod segment-value
                                              1000000))
                                    1
                                    0))]
            (swap!
              result
              str
              formated-number
              "M"))
         ))
     )
    (when (and segment-value
               (string?
                 segment-value)
               (= segment-value-type
                  "string"))
      (swap!
        result
        str
        segment-value))
    (when (and segment-value
               (or (string?
                     segment-value)
                   (number?
                     segment-value))
               (= segment-value-type
                  "percentage"))
      (swap!
        result
        str
        segment-value
        "%"))
    (when (and segment-value
               (= segment-value-type
                  "date"))
      (let [current-date (js/Date.)]
        (.setTime
          current-date
          segment-value)
        (swap!
          result
          str
          (let [date-part (let [date-part (str
                                            (.getDate
                                              current-date))
                                date-part-number-of-digits (count
                                                             date-part)]
                            (if (< date-part-number-of-digits
                                   2)
                              (str
                                "0"
                                date-part)
                              date-part))
                month-part (let [month-part (str
                                              (inc
                                                (.getMonth
                                                  current-date))
                                             )
                                 month-part-number-of-digits (count
                                                               month-part)]
                             (if (< month-part-number-of-digits
                                    2)
                               (str
                                 "0"
                                 month-part)
                               month-part))
                year-part (.getFullYear
                            current-date)]
            (if (= selected-language
                   "serbian")
              (str
                date-part
                "."
                month-part
                "."
                year-part)
              (str
                year-part
                "-"
                month-part
                "-"
                date-part))
           ))
       ))
    @result))

(defn generate-polyline
  "Generates single polyline"
  [ctx
   dot-values
   calculate-x-coordinate
   x-left-offset
   height
   y-bottom-offset
   calculate-y-coordinate
   itr
   & [x-value-type
      y-value-type
      selected-language]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             x-left-offset
             (number?
               x-left-offset)
             height
             (number?
               height)
             y-bottom-offset
             (number?
               y-bottom-offset)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             itr
             (number?
               itr))
    (let [set-color (case itr
                      0 "#08c"
                      1 "orange"
                      2 "red"
                      "#08c")]
      (aset
        ctx
        "fillStyle"
        set-color)
      (aset
        ctx
        "strokeStyle"
        set-color))
    (let [dots-as-vector-a (atom [])
          line-coordinates-a (atom [])
          point-vector-a (atom [])]
      (doseq [[x y] dot-values]
        (if (= (count
                 @line-coordinates-a)
               4)
          (do
            (swap!
              dots-as-vector-a
              conj
              @line-coordinates-a)
            (let [x-start (get
                            @line-coordinates-a
                            2)
                  y-start (get
                            @line-coordinates-a
                            3)
                  x-end (+ (calculate-x-coordinate
                             x)
                           x-left-offset)
                  y-end (- height
                           y-bottom-offset
                           (calculate-y-coordinate
                             y))]
              (reset!
                line-coordinates-a
                [x-start y-start
                 x-end y-end]))
           )
          (swap!
            line-coordinates-a
            conj
            (+ (calculate-x-coordinate
                 x)
               x-left-offset)
            (- height
               y-bottom-offset
               (calculate-y-coordinate
                 y))
           ))
       )
      (swap!
        dots-as-vector-a
        conj
        @line-coordinates-a)
      (doseq [[x-start y-start
               x-end y-end] @dots-as-vector-a]
        
        (.beginPath
          ctx)
        (.moveTo
          ctx
          x-start
          y-start)
        (.lineTo
          ctx
          x-end
          y-end)
        (aset
          ctx
          "lineWidth"
          2)
        (.stroke
          ctx)
        (aset
          ctx
          "lineWidth"
          1)
        (.closePath
          ctx))
      (doseq [[x y] dot-values]
        (swap!
          point-vector-a
          conj
          [(+ (calculate-x-coordinate
               x)
             x-left-offset)
           (- height
              y-bottom-offset
              (calculate-y-coordinate
                y))
           (let [set-color (case itr
                             0 "#08c"
                             1 "orange"
                             2 "red"
                             "#08c")]
             set-color)])
       )
      @point-vector-a))
 )

(defn generate-polylines
  "Generates polylines out of line vectors"
  [ctx
   dot-values
   multi-line
   calculate-x-coordinate
   x-left-offset
   height
   y-bottom-offset
   calculate-y-coordinate
   & [x-value-type
      y-value-type
      selected-language]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             x-left-offset
             (number?
               x-left-offset)
             height
             (number?
               height)
             y-bottom-offset
             (number?
               y-bottom-offset)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate))
    (if multi-line
      (let [itr (atom 0)
            point-vector (atom [])]
        (doseq [line-vector dot-values]
          (swap!
            point-vector
            (fn [atom-value
                 new-value]
              (apply
                conj
                atom-value
                new-value))
            (generate-polyline
              ctx
              line-vector
              calculate-x-coordinate
              x-left-offset
              height
              y-bottom-offset
              calculate-y-coordinate
              @itr
              x-value-type
              y-value-type
              selected-language))
          (swap!
            itr
            inc))
        (doseq [[x y
                 set-color] @point-vector]
          (.beginPath
            ctx)
          (aset
            ctx
            "fillStyle"
            set-color)
          (.arc
            ctx
            x
            y
            5
            0
            (* (aget
                 js/Math
                 "PI")
               2))
          (.fill
            ctx)
          (.closePath
            ctx))
       )
      (let [point-vector (generate-polyline
                           ctx
                           dot-values
                           calculate-x-coordinate
                           x-left-offset
                           height
                           y-bottom-offset
                           calculate-y-coordinate
                           0
                           x-value-type
                           y-value-type
                           selected-language)]
        (doseq [[x y
                 set-color] point-vector]
          (.beginPath
            ctx)
          (aset
            ctx
            "fillStyle"
            set-color)
          (.arc
            ctx
            x
            y
            5
            0
            (* (aget
                 js/Math
                 "PI")
               2))
          (.fill
            ctx)
          (.closePath
            ctx))
       ))
   ))

(defn build-line-chart-clj-map
  "Builds line chart clojure map of html elements"
  [{dot-values :dot-values
    x-value-type :x-value-type
    y-value-type :y-value-type
    multi-line :multi-line
    {line-names :line-names
     legend-position :position} :legend
    main-title :main-title
    x-axis-title :x-axis-title
    y-axis-title :y-axis-title
    horizontal-grid-lines :horizontal-grid-lines
    vertical-grid-lines :vertical-grid-lines
    width :width
    height :height
    x-minimum :x-minimum
    y-minimum :y-minimum
    x-maximum :x-maximum
    y-maximum :y-maximum
    selected-language :selected-language}]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
         )
    (let [width (or width
                    500)
          height (or height
                     500)
          canvas-element (gen
                           (canvas
                             nil
                             {:width (str
                                       width)
                              :height (str
                                        height)})
                          )
          ctx (.getContext
                canvas-element
                "2d")
          basic-offset 50
          x-axis-title-offset (if (and y-axis-title
                                       (string?
                                         y-axis-title))
                                50
                                0)
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and line-names
                                        (vector?
                                          line-names)
                                        (not
                                          (nil?
                                            line-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          void (when (and line-names
                          (vector?
                            line-names)
                          (not
                            (empty?
                              line-names))
                      )
                 (let [itr (atom 0)
                       lines-number (count
                                      line-names)
                       top-bottom-x-start (long
                                            (/ (- width
                                                  (* lines-number
                                                     80))
                                               2))
                       top-y 40
                       bottom-y (- height
                                   30)
                       left-right-y-start (long
                                            (/ (- height
                                                  (* lines-number
                                                     30))
                                               2))
                       left-x 30
                       right-x (- width
                                  65)]
                   (doseq [line-name line-names]
                     (let [cx (if (contains?
                                    #{"top"
                                      "bottom"}
                                    legend-position)
                                (+ top-bottom-x-start
                                   (* @itr
                                      80))
                                (if (= legend-position
                                       "right")
                                  right-x
                                  (when (= legend-position
                                           "left")
                                    left-x))
                               )
                           cy (if (= legend-position
                                     "top")
                                top-y
                                (if (= legend-position
                                       "bottom")
                                  bottom-y
                                  (when (contains?
                                          #{"left"
                                            "right"}
                                          legend-position)
                                    (+ left-right-y-start
                                       (* @itr
                                          30))
                                   ))
                               )]
                       (.beginPath
                         ctx)
                       (let [set-color (case @itr
                                         0 "#08c"
                                         1 "orange"
                                         2 "red"
                                         "#08c")]
                         (aset
                           ctx
                           "fillStyle"
                           set-color))
                       (.arc
                         ctx
                         cx
                         cy
                         5
                         0
                         (* (aget
                              js/Math
                              "PI")
                            2))
                       (.fill
                         ctx)
                       (.closePath
                         ctx)
                       (.beginPath
                         ctx)
                       (aset
                         ctx
                         "fillStyle"
                         "#08c")
                       (aset
                         ctx
                         "font"
                         "16px Courier New, Courier, monospace")
                       (.fillText
                         ctx
                         line-name
                         (- cx
                            20)
                         (+ cy
                            20))
                       (.closePath
                         ctx))
                     (swap!
                       itr
                       inc))
                  ))
          x-left-offset (+ basic-offset
                           x-axis-title-offset
                           left-legend-offset)
          x-right-offset (+ basic-offset
                            right-legend-offset)
          y-top-offset (+ basic-offset
                          top-legend-offset)
          y-bottom-offset (+ basic-offset
                             bottom-legend-offset)
          x-axis-width (- width
                          (+ x-left-offset
                             x-right-offset))
          x-axis-height 1
          y-axis-height (- height
                           (+ y-top-offset
                              y-bottom-offset))
          y-axis-width 1
          [x-min x-max
           y-min y-max] (find-x-y-min-max
                          dot-values
                          multi-line)
          x-min (- x-min
                   (mod x-min
                        10))
          y-min (- y-min
                   (mod y-min
                        10))
          x-max (+ x-max
                   (- 10
                      (mod x-max
                           10))
                 )
          y-max (+ y-max
                   (- 10
                      (mod y-max
                           10))
                 )
          x-min (if (and x-minimum
                         (number?
                           x-minimum))
                  x-minimum
                  x-min)
          x-max (if (and x-maximum
                         (number?
                           x-maximum))
                  x-maximum
                  x-max)
          y-min (if (and y-minimum
                         (number?
                           y-minimum))
                  y-minimum
                  y-min)
          y-max (if (and y-maximum
                         (number?
                           y-maximum))
                  y-maximum
                  y-max)
          x-axis-segments (axis-segment
                            x-min
                            x-max
                            x-axis-width)
          void (doseq [[segment-value
                        segment-pixels] x-axis-segments]
                 (.beginPath
                   ctx)
                 (aset
                   ctx
                   "strokeStyle"
                   "#d5d5d5")
                 (.moveTo
                   ctx
                   (+ segment-pixels
                      x-left-offset)
                   (if vertical-grid-lines
                     y-top-offset
                     (- height
                        y-bottom-offset
                        5))
                  )
                 (.lineTo
                   ctx
                   (+ segment-pixels
                      x-left-offset)
                   (+ (- height
                         y-bottom-offset)
                      5))
                 (.stroke
                   ctx)
                 (.closePath
                   ctx)
                 (let [x-axis-text-x (- (+ segment-pixels
                                           x-left-offset
                                           1)
                                        (long
                                          (/ (* (count
                                                  (format-segment-value
                                                    segment-value
                                                    x-value-type
                                                    selected-language))
                                                9)
                                             2))
                                      )
                       character-count (count
                                         (format-segment-value
                                           segment-value
                                           x-value-type
                                           selected-language))
                       x-axis-text-y (+ (- height
                                           y-bottom-offset)
                                        20)]
                   (.beginPath
                     ctx)
                   (aset
                     ctx
                     "fillStyle"
                     "#d5d5d5")
                   (aset
                     ctx
                     "font"
                     "14px Courier New, Courier, monospace")
                   (aset
                     ctx
                     "textAlign"
                     "left")
                   (when (< 7
                            character-count)
                     (.rotate
                       ctx
                       (- (/ (* (aget
                                  js/Math
                                  "PI")
                                20)
                             180))
                      )
                     (let [[rotated-x
                            rotated-y] (utils/rotate-coordinates
                                         x-axis-text-x
                                         x-axis-text-y
                                         20)]
                       (.fillText
                         ctx
                         (format-segment-value
                           segment-value
                           x-value-type
                           selected-language)
                         rotated-x
                         (+ rotated-y
                            20))
                      )
                     (.rotate
                       ctx
                       (/ (* (aget
                               js/Math
                               "PI")
                             20)
                          180))
                    )
                   (when-not (< 7
                                character-count)
                     (.fillText
                       ctx
                       (format-segment-value
                         segment-value
                         x-value-type
                         selected-language)
                       x-axis-text-x
                       x-axis-text-y))
                   (.closePath
                     ctx))
                )
          y-axis-segments (axis-segment
                            y-min
                            y-max
                            y-axis-height)
          void (doseq [[segment-value
                        segment-pixels] y-axis-segments]
                 (.beginPath
                   ctx)
                 (aset
                   ctx
                   "strokeStyle"
                   "#d5d5d5")
                 (.moveTo
                   ctx
                   (- x-left-offset
                      5)
                   (- height
                      y-bottom-offset
                      segment-pixels))
                 (.lineTo
                   ctx
                   (if horizontal-grid-lines
                     (+ x-left-offset
                        x-axis-width)
                     (+ x-left-offset
                        5))
                   (- height
                      y-bottom-offset
                      segment-pixels))
                 (.stroke
                   ctx)
                 (.closePath
                   ctx)
                 (let [y-axis-text-x (- x-left-offset
                                        (long
                                          (* (count
                                               (format-segment-value
                                                 segment-value
                                                 y-value-type
                                                 selected-language))
                                             9))
                                        5)
                       character-count (count
                                         (format-segment-value
                                           segment-value
                                           y-value-type
                                           selected-language))
                       y-axis-text-y (+ (- height
                                           y-bottom-offset
                                           segment-pixels)
                                        6)]
                   (.beginPath
                     ctx)
                   (aset
                     ctx
                     "font"
                     "14px Courier New, Courier, monospace")
                   (aset
                     ctx
                     "fillStyle"
                     "#d5d5d5")
                   (aset
                     ctx
                     "textAlign"
                     "left")
                   (when (< 7
                            character-count)
                     (.rotate
                       ctx
                       (- (/ (* (aget
                                  js/Math
                                  "PI")
                                70)
                             180))
                      )
                     (let [[rotated-x
                            rotated-y] (utils/rotate-coordinates
                                         y-axis-text-x
                                         y-axis-text-y
                                         70)]
                       (.fillText
                         ctx
                         (format-segment-value
                           segment-value
                           y-value-type
                           selected-language)
                         (- rotated-x
                            15)
                         (+ rotated-y
                            70))
                      )
                     (.rotate
                       ctx
                       (/ (* (aget
                               js/Math
                               "PI")
                             70)
                          180))
                     )
                   (when-not (< 7
                                character-count)
                     (.fillText
                       ctx
                       (format-segment-value
                         segment-value
                         y-value-type
                         selected-language)
                       y-axis-text-x
                       y-axis-text-y))
                   (.closePath
                     ctx))
                )
          [x-s-min
           x-p-min] (first
                      x-axis-segments)
          [x-s-max
           x-p-max] (last
                      x-axis-segments)
          x-s-length (- x-s-max
                        x-s-min)
          x-p-length (- x-p-max
                        x-p-min)
          calculate-x-coordinate (fn [x-s-current]
                                   (- x-p-length
                                      (long
                                        (* (/ x-p-length
                                              x-s-length)
                                           (- x-s-length
                                              (- x-s-current
                                                 x-s-min))
                                         ))
                                    ))
          [y-s-min
           y-p-min] (first
                      y-axis-segments)
          [y-s-max
           y-p-max] (last
                      y-axis-segments)
          y-s-length (- y-s-max
                        y-s-min)
          y-p-length (- y-p-max
                        y-p-min)
          calculate-y-coordinate (fn [y-s-current]
                                   (- y-p-length
                                      (long
                                        (* (/ y-p-length
                                              y-s-length)
                                           (- y-s-length
                                              (- y-s-current
                                                 y-s-min))
                                         ))
                                    ))]
      (.beginPath
        ctx)
      (aset
        ctx
        "strokeStyle"
        "#d5d5d5")
      (.moveTo
        ctx
        x-left-offset
        (- height
           y-bottom-offset))
      (.lineTo
        ctx
        (- width
           x-right-offset)
        (- height
           y-bottom-offset))
      (.stroke
        ctx)
      (.closePath
        ctx)
      (.beginPath
        ctx)
      (aset
        ctx
        "strokeStyle"
        "#d5d5d5")
      (.moveTo
        ctx
        x-left-offset
        y-top-offset)
      (.lineTo
        ctx
        x-left-offset
        (- height
           y-bottom-offset))
      (.stroke
        ctx)
      (.closePath
        ctx)
      (generate-polylines
        ctx
        dot-values
        multi-line
        calculate-x-coordinate
        x-left-offset
        height
        y-bottom-offset
        calculate-y-coordinate
        x-value-type
        y-value-type
        selected-language)
      (when (and main-title
                 (string?
                   main-title))
        (.beginPath
          ctx)
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.fillText
          ctx
          main-title
          (long
            (/ width
               2))
          20)
        (.closePath
          ctx))
      (when (and x-axis-title
                 (string?
                   x-axis-title))
        (.beginPath
          ctx)
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.fillText
          ctx
          x-axis-title
          (long
            (/ width
               2))
          (- height
             bottom-legend-offset))
        (.closePath
          ctx))
      (when (and y-axis-title
                 (string?
                   y-axis-title))
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.rotate
          ctx
          (- (/ (aget
                  js/Math
                  "PI")
                2))
         )
        (.fillText
          ctx
          y-axis-title
          (- (/ height
                2))
          (+ left-legend-offset
             30))
        (.rotate
          ctx
          (/ (aget
               js/Math
               "PI")
             2))
       )
      canvas-element))
 )

(defn render-line-chart
  "Renders line chart html elements from chart map"
  [chart-configuration]
  (build-line-chart-clj-map
    chart-configuration))

(defn bar-min-max-iterate-coordinates
  "Iterates through bar values"
  [bar-values
   axis-min
   axis-max]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values)
              )
             axis-min
             (instance?
               Atom
               axis-min)
             axis-max
             (instance?
               Atom
               axis-max))
    (doseq [bar-value bar-values]
      (let [bar-value-as-number (if (instance?
                                      js/Date
                                      bar-value)
                                  (.getTime
                                    bar-value)
                                  bar-value)]
        (when (< bar-value-as-number
                 @axis-min)
          (reset!
            axis-min
            bar-value-as-number))
        (when (< @axis-max
                 bar-value-as-number)
          (reset!
            axis-max
            bar-value-as-number))
       ))
   ))

(defn find-bar-x-y-min-max
  "Finds minimum and maximum for bars and sets them for particular axis x or y"
  [bar-values
   multi-bars
   bar-values-on-x-axis
   value-type]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
         )
    (let [axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))]
      (if multi-bars
        (doseq [bar-vector bar-values]
          (bar-min-max-iterate-coordinates
            bar-vector
            axis-min
            axis-max))
        (bar-min-max-iterate-coordinates
          bar-values
          axis-min
          axis-max))
      (when @axis-max
        (if bar-values-on-x-axis
          [(if (= value-type
                  "date")
             @axis-min
             0)
           @axis-max
           0
           (aget
             js/Number
             "MIN_SAFE_INTEGER")]
          [0
           (aget
             js/Number
             "MIN_SAFE_INTEGER")
           (if (= value-type
                  "date")
             @axis-min
             0)
           @axis-max]))
     ))
 )

(defn bar-axis-segment
  "Calculate segments for bars"
  [bar-labels
   width]
  (when (and bar-labels
             (vector?
               bar-labels)
             (not
               (empty?
                 bar-labels))
             width
             (number?
               width))
    (let [bar-count (count
                      bar-labels)
          segment-pixel-value (long
                                (/ width
                                   bar-count))
          segment-pixel-value-range (range
                                      (long
                                        (/ segment-pixel-value
                                           2))
                                      (inc
                                        width)
                                      segment-pixel-value)
          result (atom [])
          map-result (map
                       (fn [segment-value-p
                            segment-pixel-value-p]
                         (swap!
                           result
                           conj
                           [segment-value-p
                            segment-pixel-value-p]))
                       bar-labels
                       segment-pixel-value-range)]
      (last
        map-result))
   ))

(defn generate-bar
  "Generates row of bars and their values"
  [ctx
   bar-value
   chart-bars
   bar-values-on-x-axis
   calculate-x-coordinate
   calculate-y-coordinate
   calculate-width
   calculate-height
   itr
   number-of-bars
   value-type
   selected-language]
  (when (and bar-value
             (vector?
               bar-value)
             (not
               (empty?
                 bar-value))
             chart-bars
             (instance?
               Atom
               chart-bars)
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             calculate-width
             (fn?
               calculate-width)
             calculate-height
             (fn?
               calculate-height)
             itr
             (number?
               itr)
             number-of-bars
             (number?
               number-of-bars))
    (let [bar-number (atom 0)]
      (doseq [value-i bar-value]
        (let [value-i (if (= value-type
                             "date")
                        (.getTime
                          value-i)
                        value-i)
              rect-width (if bar-values-on-x-axis
                           (calculate-width
                             value-i
                             (count
                               bar-value))
                           (long
                             (/ (- (calculate-width
                                     @bar-number
                                     (count
                                       bar-value))
                                   5)
                                number-of-bars))
                          )
              rect-height (if bar-values-on-x-axis
                            (long
                              (/ (- (calculate-height
                                      @bar-number
                                      (count
                                        bar-value))
                                    5)
                                 number-of-bars))
                            (calculate-height
                              value-i
                              (count
                                bar-value))
                           )
              rect-x (if bar-values-on-x-axis
                       (calculate-x-coordinate
                         value-i
                         (count
                           bar-value))
                       (+ (calculate-x-coordinate
                            @bar-number
                            (count
                              bar-value))
                          (* (long
                               (/ (- (calculate-width
                                       @bar-number
                                       (count
                                         bar-value))
                                     5)
                                  number-of-bars))
                             itr)
                          3))
              rect-y (if bar-values-on-x-axis
                       (+ (calculate-y-coordinate
                            (inc
                              @bar-number)
                            (count
                              bar-value))
                          (* (long
                               (/ (- (calculate-height
                                       @bar-number
                                       (count
                                         bar-value))
                                     5)
                                  number-of-bars))
                             itr)
                          3)
                       (calculate-y-coordinate
                         value-i
                         (count
                           bar-value))
                      )]
          (let [set-color (case itr
                            0 "#08c"
                            1 "orange"
                            2 "red"
                            "#08c")]
            (aset
              ctx
              "fillStyle"
              set-color))
          (.fillRect
            ctx
            rect-x
            rect-y
            rect-width
            rect-height))
        (swap!
          bar-number
          inc))
     ))
 )

(defn generate-bars
  "Generates canvas rectangulars as bars"
  [ctx
   bar-values
   multi-bars
   bar-values-on-x-axis
   calculate-x-coordinate
   calculate-y-coordinate
   calculate-width
   calculate-height
   value-type
   selected-language]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             calculate-width
             (fn?
               calculate-width)
             calculate-height
             (fn?
               calculate-height))
    (let [chart-bars (atom [])]
      (if multi-bars
        (let [itr (atom 0)]
          (doseq [bar-value bar-values]
            (generate-bar
              ctx
              bar-value
              chart-bars
              bar-values-on-x-axis
              calculate-x-coordinate
              calculate-y-coordinate
              calculate-width
              calculate-height
              @itr
              (count
                bar-values)
              value-type
              selected-language)
            (swap!
              itr
              inc))
         )
        (generate-bar
          ctx
          bar-values
          chart-bars
          bar-values-on-x-axis
          calculate-x-coordinate
          calculate-y-coordinate
          calculate-width
          calculate-height
          0
          1
          value-type
          selected-language)
       )
     @chart-bars))
 )

(defn build-bar-chart-clj-map
  "Builds bar chart clojure map of html elements"
  [{bar-values :bar-values
    bar-labels :bar-labels
    bar-values-on-x-axis :bar-values-on-x-axis
    value-type :value-type
    multi-bars :multi-bars
    {bar-names :bar-names
     legend-position :position} :legend
    main-title :main-title
    x-axis-title :x-axis-title
    y-axis-title :y-axis-title
    horizontal-grid-lines :horizontal-grid-lines
    vertical-grid-lines :vertical-grid-lines
    width :width
    height :height
    x-minimum :x-minimum
    y-minimum :y-minimum
    x-maximum :x-maximum
    y-maximum :y-maximum
    selected-language :selected-language}]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
         )
    (let [width (or width
                    500)
          height (or height
                     500)
          canvas-element (gen
                           (canvas
                             nil
                             {:width (str
                                       width)
                              :height (str
                                        height)})
                          )
          ctx (.getContext
                canvas-element
                "2d")
          basic-offset 50
          x-axis-title-offset (if (and y-axis-title
                                       (string?
                                         y-axis-title))
                                50
                                0)
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and bar-names
                                        (vector?
                                          bar-names)
                                        (not
                                          (nil?
                                            bar-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          bar-labels (if (and bar-labels
                              (vector?
                                bar-labels))
                       bar-labels
                       (if multi-bars
                         (let [count-labels (count
                                              (first
                                                bar-values))
                               default-bar-labels (atom [])]
                           (dotimes [i count-labels]
                             (swap!
                               default-bar-labels
                               conj
                               (str
                                 i))
                            )
                           @default-bar-labels)
                         (let [count-labels (count
                                              bar-values)
                               default-bar-labels (atom [])]
                           (dotimes [i count-labels]
                             (swap!
                               default-bar-labels
                               conj
                               (str
                                 i))
                            )
                           @default-bar-labels))
                      )
          x-left-offset (+ basic-offset
                           x-axis-title-offset
                           left-legend-offset)
          x-right-offset (+ basic-offset
                            right-legend-offset)
          y-top-offset (+ basic-offset
                          top-legend-offset)
          y-bottom-offset (+ basic-offset
                             bottom-legend-offset)
          x-axis-width (- width
                          (+ x-left-offset
                             x-right-offset))
          x-axis-height 1
          y-axis-height (- height
                           (+ y-top-offset
                              y-bottom-offset))
          y-axis-width 1
          [x-min x-max
           y-min y-max] (find-bar-x-y-min-max
                          bar-values
                          multi-bars
                          bar-values-on-x-axis
                          value-type)
          x-max (+ x-max
                   (- 10
                      (mod x-max
                           10))
                 )
          y-max (+ y-max
                   (- 10
                      (mod y-max
                           10))
                 )
          x-min (if (and x-minimum
                         (number?
                           x-minimum))
                  x-minimum
                  x-min)
          x-max (if (and x-maximum
                         (number?
                           x-maximum))
                  x-maximum
                  x-max)
          y-min (if (and y-minimum
                         (number?
                           y-minimum))
                  y-minimum
                  y-min)
          y-max (if (and y-maximum
                         (number?
                           y-maximum))
                  y-maximum
                  y-max)
          x-axis-segments (if bar-values-on-x-axis
                            (axis-segment
                              x-min
                              x-max
                              x-axis-width)
                            (bar-axis-segment
                              bar-labels
                              x-axis-width))
          itr (atom 0)
          void (doseq [[segment-value
                        segment-pixels] x-axis-segments]
                 (aset
                   ctx
                   "strokeStyle"
                   "#d5d5d5")
                 (.beginPath
                   ctx)
                 (.moveTo
                   ctx
                   (+ segment-pixels
                      x-left-offset)
                   (if vertical-grid-lines
                     y-top-offset
                     (- height
                        y-bottom-offset
                        5))
                  )
                 (.lineTo
                   ctx
                   (+ segment-pixels
                      x-left-offset)
                   (+ (- height
                         y-bottom-offset)
                      5))
                 (.closePath
                   ctx)
                 (.stroke
                   ctx)
                 (let [x-axis-text-x (- (+ segment-pixels
                                           x-left-offset
                                           1)
                                        (long
                                          (/ (* (count
                                                  (format-segment-value
                                                    segment-value
                                                    (if bar-values-on-x-axis
                                                      value-type
                                                      "string")
                                                    selected-language))
                                                9)
                                             2))
                                      )
                       character-count (count
                                         (format-segment-value
                                           segment-value
                                           (if bar-values-on-x-axis
                                             value-type
                                             "string")
                                           selected-language))
                       x-axis-text-y (+ (- height
                                           y-bottom-offset)
                                        20)]
                   (aset
                     ctx
                     "font"
                     "14px Courier New, Courier, monospace")
                   (aset
                     ctx
                     "fillStyle"
                     "#d5d5d5")
                   (aset
                     ctx
                     "textAlign"
                     "left")
                   (when (< 7
                            character-count)
                     (.rotate
                       ctx
                       (- (/ (* (aget
                                  js/Math
                                  "PI")
                                20)
                             180))
                      )
                     (let [[rotated-x
                            rotated-y] (utils/rotate-coordinates
                                         x-axis-text-x
                                         x-axis-text-y
                                         20)]
                       (.fillText
                         ctx
                         (format-segment-value
                           segment-value
                           (if bar-values-on-x-axis
                             value-type
                             "string")
                           selected-language)
                         rotated-x
                         (+ rotated-y
                            20))
                      )
                     (.rotate
                       ctx
                       (/ (* (aget
                               js/Math
                               "PI")
                             20)
                          180))
                    )
                   (when-not (< 7
                                character-count)
                     (.fillText
                       ctx
                       (format-segment-value
                         segment-value
                         (if bar-values-on-x-axis
                           value-type
                           "string")
                         selected-language)
                       x-axis-text-x
                       x-axis-text-y))
                  )
                 (swap!
                   itr
                   inc))
          y-axis-segments (if bar-values-on-x-axis
                            (bar-axis-segment
                              bar-labels
                              y-axis-height)
                            (axis-segment
                              y-min
                              y-max
                              y-axis-height))
          void (doseq [[segment-value
                        segment-pixels] y-axis-segments]
                 (aset
                   ctx
                   "fillStyle"
                   "#d5d5d5")
                 (.beginPath
                   ctx)
                 (.moveTo
                   ctx
                   (- x-left-offset
                      5)
                   (- height
                      y-bottom-offset
                      segment-pixels))
                 (.lineTo
                   ctx
                   (if horizontal-grid-lines
                     (+ x-left-offset
                        x-axis-width)
                     (+ x-left-offset
                        5))
                   (- height
                      y-bottom-offset
                      segment-pixels))
                 (.closePath
                   ctx)
                 (.stroke
                   ctx)
                 (let [y-axis-text-x (- x-left-offset
                                        (long
                                          (* (count
                                               (format-segment-value
                                                 segment-value
                                                 (if-not bar-values-on-x-axis
                                                   value-type
                                                   "string")
                                                 selected-language))
                                             9))
                                        5)
                       character-count (count
                                         (format-segment-value
                                           segment-value
                                           (if-not bar-values-on-x-axis
                                             value-type
                                             "string")
                                           selected-language))
                       y-axis-text-y (+ (- height
                                           y-bottom-offset
                                           segment-pixels)
                                        6)]
                   (aset
                     ctx
                     "font"
                     "14px Courier New, Courier, monospace")
                   (aset
                     ctx
                     "fillStyle"
                     "#d5d5d5")
                   (aset
                     ctx
                     "textAlign"
                     "left")
                   (when (< 7
                            character-count)
                     (.rotate
                       ctx
                       (- (/ (* (aget
                                  js/Math
                                  "PI")
                                70)
                             180))
                      )
                     (let [[rotated-x
                            rotated-y] (utils/rotate-coordinates
                                         y-axis-text-x
                                         y-axis-text-y
                                         70)]
                       (.fillText
                         ctx
                         (format-segment-value
                           segment-value
                           (if-not bar-values-on-x-axis
                             value-type
                             "string")
                           selected-language)
                         (- rotated-x
                            15)
                         (+ rotated-y
                            70))
                      )
                     (.rotate
                       ctx
                       (/ (* (aget
                               js/Math
                               "PI")
                             70)
                          180))
                     )
                   (when-not (< 7
                                character-count)
                     (.fillText
                       ctx
                       (format-segment-value
                         segment-value
                         (if-not bar-values-on-x-axis
                           value-type
                           "string")
                         selected-language)
                       y-axis-text-x
                       y-axis-text-y))
                  ))
          [x-s-min
           x-p-min] (first
                      x-axis-segments)
          [x-s-max
           x-p-max] (last
                      x-axis-segments)
          calculate-x-coordinate (if bar-values-on-x-axis
                                   (fn [bar-x-value
                                        bar-count]
                                     x-left-offset)
                                   (fn [bar-number
                                        bar-count]
                                     (long
                                       (+ (* (/ x-axis-width
                                                bar-count)
                                             bar-number)
                                          x-left-offset))
                                    ))
          [y-s-min
           y-p-min] (first
                      y-axis-segments)
          [y-s-max
           y-p-max] (last
                      y-axis-segments)
          y-bottom-start (- height
                            y-bottom-offset)
          calculate-y-coordinate (if bar-values-on-x-axis
                                   (fn [bar-number
                                        bar-count]
                                     (long
                                       (- y-bottom-start
                                          (* (/ y-axis-height
                                                bar-count)
                                             bar-number))
                                      ))
                                   (fn [bar-y-value
                                        bar-count]
                                     (long
                                       (- y-bottom-start
                                          (/ (* y-p-max
                                                (- bar-y-value
                                                   y-s-min))
                                             (- y-s-max
                                                y-s-min))
                                        ))
                                    ))
          calculate-width (if bar-values-on-x-axis
                            (fn [bar-value
                                 bar-count]
                              (long
                                (/ (* x-p-max
                                      (- bar-value
                                         x-s-min))
                                   (- x-s-max
                                      x-s-min))
                               ))
                            (fn [bar-number
                                 bar-count]
                              (long
                                (/ x-axis-width
                                   bar-count))
                             ))
          calculate-height (if bar-values-on-x-axis
                             (fn [bar-number
                                  bar-count]
                               (long
                                 (/ y-axis-height
                                    bar-count))
                              )
                             (fn [bar-value
                                  bar-count]
                               (long
                                 (/ (* y-p-max
                                       (- bar-value
                                          y-s-min))
                                    (- y-s-max
                                       y-s-min))
                                ))
                            )
          chart-bars (generate-bars
                       ctx
                       bar-values
                       multi-bars
                       bar-values-on-x-axis
                       calculate-x-coordinate
                       calculate-y-coordinate
                       calculate-width
                       calculate-height
                       value-type
                       selected-language)]
      (when (and bar-names
                 (vector?
                   bar-names)
                 (not
                   (empty?
                     bar-names))
             )
        (let [itr (atom 0)
              lines-number (count
                             bar-names)
              top-bottom-x-start (long
                                   (/ (- width
                                         (* lines-number
                                            80))
                                      2))
              top-y 40
              bottom-y (- height
                          30)
              left-right-y-start (long
                                   (/ (- height
                                         (* lines-number
                                            30))
                                      2))
              left-x 30
              right-x (- width
                         65)]
          (doseq [bar-name bar-names]
            (let [cx (if (contains?
                           #{"top"
                             "bottom"}
                           legend-position)
                       (+ top-bottom-x-start
                          (* @itr
                             80))
                       (if (= legend-position
                              "right")
                         right-x
                         (when (= legend-position
                                  "left")
                           left-x))
                      )
                  cy (if (= legend-position
                            "top")
                       top-y
                       (if (= legend-position
                              "bottom")
                         bottom-y
                         (when (contains?
                                 #{"left"
                                   "right"}
                                 legend-position)
                           (+ left-right-y-start
                              (* @itr
                                 30))
                          ))
                      )]
              (.beginPath
                ctx)
              (.arc
                ctx
                cx
                cy
                5
                0
                (* (aget
                     js/Math
                     "PI")
                   2))
              (let [set-color (case @itr
                                0 "#08c"
                                1 "orange"
                                2 "red"
                                "#08c")]
                (aset
                  ctx
                  "fillStyle"
                  set-color))
              (.closePath
                ctx)
              (.fill
                ctx)
              (aset
                ctx
                "font"
                "16px Courier New, Courier, monospace")
              (aset
                ctx
                "fillStyle"
                "#08c")
              (aset
                ctx
                "textAlign"
                "left")
              (.fillText
                ctx
                bar-name
                (- cx
                   20)
                (+ cy
                   20))
             )
            (swap!
              itr
              inc))
          ))
       ;; x-axis line
       (aset
         ctx
         "strokeStyle"
         "#d5d5d5")
       (.beginPath
         ctx)
       (.moveTo
         ctx
         x-left-offset
         (- height
            y-bottom-offset))
       (.lineTo
         ctx
         (- width
            x-right-offset)
         (- height
            y-bottom-offset))
       (.closePath
         ctx)
       (.stroke
         ctx)
       ;; y-axis line
       (aset
         ctx
         "strokeStyle"
         "#d5d5d5")
       (.beginPath
         ctx)
       (.moveTo
         ctx
         x-left-offset
         y-top-offset)
       (.lineTo
         ctx
         x-left-offset
         (- height
            y-bottom-offset))
       (.closePath
         ctx)
       (.stroke
         ctx)
      (when (and main-title
                 (string?
                   main-title))
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.fillText
          ctx
          main-title
          (long
            (/ width
               2))
          20))
      (when (and x-axis-title
                 (string?
                   x-axis-title))
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.fillText
          ctx
          x-axis-title
          (long
            (/ width
               2))
          (- height
             bottom-legend-offset))
       )
      (when (and y-axis-title
                 (string?
                   y-axis-title))
        (aset
          ctx
          "font"
          "16px Courier New, Courier, monospace")
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.rotate
          ctx
          (- (/ (aget
                  js/Math
                  "PI")
                2))
         )
        (.fillText
          ctx
          y-axis-title
          (- (/ height
                2))
          (+ left-legend-offset
             30))
        (.rotate
          ctx
          (/ (aget
               js/Math
               "PI")
             2))
       )
      canvas-element))
 )

(defn render-bar-chart
  "Renders bar chart html elements from chart map"
  [chart-configuration]
  (build-bar-chart-clj-map
    chart-configuration))

(defn generate-pie-slices
  "Generates pie slices in canvas context"
  [ctx
   pie-values
   radius
   cx
   cy
   width
   height
   value-type
   selected-language]
  (when (and pie-values
             (vector?
               pie-values)
             (not
               (empty?
                 pie-values))
         )
    (let [total-value (apply
                        +
                        pie-values)
          start-angle (atom 0)
          itr (atom 0)]
      (doseq [single-value pie-values]
        (let [set-color (case @itr
                          0 "#08c"
                          1 "orange"
                          2 "red"
                          "#08c")]
          (aset
            ctx
            "fillStyle"
            set-color))
        (let [end-angle (/ (* single-value
                              360)
                           total-value)
              end-angle (+ @start-angle
                           end-angle)
              quadrate-vector (utils/find-quadrate-by-angle
                                @start-angle
                                end-angle)]
          (doseq [quadrate-number quadrate-vector]
            (let [{quadrate-start-point :start
                   quadrate-middle-point :angle
                   quadrate-end-point :end
                   quadrate-start-angle :start-angle
                   quadrate-end-angle :end-angle} (utils/get-quadrate-extreme-points
                                                    quadrate-number
                                                    radius)
                  quadrate-start-angle (if (< quadrate-start-angle
                                              @start-angle)
                                         @start-angle
                                         quadrate-start-angle)
                  [x-start
                   y-start] (utils/calculate-circle-coordinates
                              radius
                              quadrate-start-angle)
                  quadrate-end-angle (if (< end-angle
                                            quadrate-end-angle)
                                       end-angle
                                       quadrate-end-angle)
                  [x-end
                   y-end] (utils/calculate-circle-coordinates
                            radius
                            quadrate-end-angle)
                  quadrate-middle-angle (+ quadrate-start-angle
                                           (long
                                             (/ (- quadrate-end-angle
                                                   quadrate-start-angle)
                                                2))
                                         )
                  [x-middle
                   y-middle] (utils/calculate-circle-coordinates
                               radius
                               quadrate-middle-angle)]
              (.beginPath
                ctx)
              (.arc
                ctx
                cx
                cy
                radius
                (utils/calculate-angle
                  quadrate-start-angle)
                (utils/calculate-angle
                  quadrate-end-angle)
                true)
              (.closePath
                ctx)
              (.fill
                ctx)
              (.beginPath
                ctx)
              (.moveTo
                ctx
                cx
                cy)
              (.lineTo
                ctx
                (+ cx
                   x-start)
                (- cy
                   y-start))
              (.lineTo
                ctx
                (+ cx
                   x-middle)
                (- cy
                   y-middle))
              (.lineTo
                ctx
                (+ cx
                   x-end)
                (- cy
                   y-end))
              (.closePath
                ctx)
              (.fill
                ctx))
           )
          (reset!
            start-angle
            end-angle))
        (swap!
          itr
          inc))
     ))
 )

(defn build-pie-chart-clj-map
  "Builds pie chart clojure map of html elements"
  [{pie-values :pie-values
    value-type :value-type
    {piece-names :piece-names
     legend-position :position} :legend
    main-title :main-title
    width :width
    height :height
    selected-language :selected-language}]
  (when (and pie-values
             (vector?
               pie-values)
             (not
               (empty?
                 pie-values))
         )
    (let [width (or width
                    500)
          height (or height
                     500)
          canvas-element (gen
                           (canvas
                             nil
                             {:width (str
                                       width)
                              :height (str
                                        height)})
                          )
          ctx (.getContext
                canvas-element
                "2d")
          basic-offset 50
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and piece-names
                                        (vector?
                                          piece-names)
                                        (not
                                          (nil?
                                            piece-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          left-offset (+ basic-offset
                         left-legend-offset)
          right-offset (+ basic-offset
                          right-legend-offset)
          x-axis-width (- width
                          left-offset
                          right-offset)
          top-offset (+ basic-offset
                        top-legend-offset)
          bottom-offset (+ basic-offset
                           bottom-legend-offset)
          y-axis-height (- height
                           top-offset
                           bottom-offset)
          radius (if (< x-axis-width
                        y-axis-height)
                   (long
                     (/ x-axis-width
                        2))
                   (long
                     (/ y-axis-height
                        2))
                  )
          cx (+ left-offset
                radius)
          cy (+ top-offset
                radius)]
      (aset
        ctx
        "font"
        "16px Courier New, Courier, monospace")
      (when (and main-title
                 (string?
                   main-title))
        (aset
          ctx
          "fillStyle"
          "#08c")
        (aset
          ctx
          "textAlign"
          "center")
        (.fillText
          ctx
          main-title
          (/ width
             2)
          20))
      (when (and piece-names
                 (vector?
                   piece-names)
                 (not
                   (empty?
                     piece-names))
              )
        (let [itr (atom 0)
              lines-number (count
                             piece-names)
              top-bottom-x-start (long
                                   (/ (- width
                                         (* lines-number
                                            80))
                                      2))
              top-y 40
              bottom-y (- height
                          30)
              left-right-y-start (long
                                   (/ (- height
                                         (* lines-number
                                            30))
                                      2))
              left-x 30
              right-x (- width
                         65)]
          (doseq [bar-name piece-names]
            (let [cx (if (contains?
                           #{"top"
                             "bottom"}
                           legend-position)
                       (+ top-bottom-x-start
                          (* @itr
                             80))
                       (if (= legend-position
                              "right")
                         right-x
                         (when (= legend-position
                                  "left")
                           left-x))
                      )
                  cy (if (= legend-position
                            "top")
                       top-y
                       (if (= legend-position
                              "bottom")
                         bottom-y
                         (when (contains?
                                 #{"left"
                                   "right"}
                                 legend-position)
                           (+ left-right-y-start
                              (* @itr
                                 30))
                          ))
                      )]
              (.beginPath
                ctx)
              (.arc
                ctx
                cx
                cy
                5
                0
                (* (aget
                     js/Math
                     "PI")
                   2))
              (let [set-color (case @itr
                                0 "#08c"
                                1 "orange"
                                2 "red"
                                "#08c")]
                (aset
                  ctx
                  "fillStyle"
                  set-color))
              (.fill
                ctx)
              (.closePath
                ctx)
              (aset
                ctx
                "fillStyle"
                "#08c")
              (aset
                ctx
                "textAlign"
                "left")
              (.fillText
                ctx
                bar-name
                (- cx
                   20)
                (+ cy
                   20))
             )
            (swap!
              itr
              inc))
          ))
      (generate-pie-slices
        ctx
        pie-values
        radius
        cx
        cy
        width
        height
        value-type
        selected-language)
      canvas-element))
 )

(defn render-pie-chart
  "Renders pie chart html elements from chart map"
  [chart-configuration]
  (build-pie-chart-clj-map
    chart-configuration))

