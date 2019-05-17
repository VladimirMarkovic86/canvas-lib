(ns canvas-lib.test-runner
  (:require [canvas-lib.core-test-cljs]
            [doo.runner :refer-macros [doo-tests doo-all-tests]]))

(enable-console-print!)

(doo-tests
  'canvas-lib.core-test-cljs)

