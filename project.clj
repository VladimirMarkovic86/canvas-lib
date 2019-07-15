(defproject org.clojars.vladimirmarkovic86/canvas-lib "0.1.5"
  :description "Canvas library"
  :url "http://github.com/VladimirMarkovic86/canvas-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.clojars.vladimirmarkovic86/htmlcss-lib "0.1.8"]
                 [org.clojars.vladimirmarkovic86/utils-lib "0.4.12"]
                 ]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/cljs"]
  
  :jar-exclusions [#"^public/"
                   #"README.md$"
                   #"LICENSE$"]

  :plugins [[lein-cljsbuild  "1.1.7"]
            [lein-doo "0.1.11"]
            ]

  :cljsbuild
    {:builds
      {:test
        {:source-paths ["src/cljs" "test/cljs"]
         :compiler     {:main canvas-lib.test-runner
                        :optimizations :whitespace
                        :output-dir "resources/public/assets/js/out/test"
                        :output-to "resources/public/assets/js/test.js"}}
       }}
 )

