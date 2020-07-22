(ns load-tester.core
  (:gen-class)
  (:refer-clojure :exclude [time])
  (:require [clojure.core.async :refer (thread <!!)]
            [org.httpkit.client :as http]
            )
  )

(defmacro time
  "Evaluates expr and returns the time (milliseconds) it took and the value of expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :ret ret#
      }
     )
  )

(defn access-url "访问 url" [url]
  (let [{:keys [status headers body error] :as resp} @(http/get url)]
    (not error)
    )
  )

(defn time-access-url "访问 url，记录响应时间" [url]
  (time (access-url url))
  )

(defn test-url "访问 url 若干次，记录每次的响应时间" [url times]
  (doall (repeatedly times #(time-access-url url)))
  )

(defn percentile "计算百分位" [coll p]
  (nth (sort coll) (int (/ (* (count coll) p) 100)))
  )

(defn -main [arg1 arg2 & args]
  (let [concurrency (Integer/valueOf arg1)
        times (Integer/valueOf arg2)
        url "https://www.baidu.com"
        results (->> (repeatedly concurrency #(<!! (thread (test-url url times))))
                     flatten
                     (map :time)
                     )
        sorted-results (sort results)
        average-time (/ (apply + results) (* concurrency times))
        ]
    (println (format "average: %sms min: %sms max: %sms" average-time (first sorted-results) (last sorted-results)))
    (println (format "95th percentile: %sms" (percentile results 95)))
    )
  )
