(ns ^:figwheel-no-load dcm-cljs.dev
  (:require
    [dcm-cljs.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
