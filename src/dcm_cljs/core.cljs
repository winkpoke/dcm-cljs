(ns dcm-cljs.core
    (:require
      [clojure.zip :as z]
      [reagent.core :as r]
      [dicomParser]
      ))


;; -------------------------
;; Dicom

{:group :element}

(set! *warn-on-infer* true)

(defn valid-tag?
  [tag]
  true)

(defn valid-uid?
  [uid]
  (re-matches #"[0-9.]*" uid))

(defn string
  [^js/dicomParser.DataSet d tag]
  (when (and (some? d) (valid-tag? tag))
    (.string d tag)))

(defn window [ds]
  (js/parseInt (string ds "x00281051")))

(defn level [ds]
  (js/parseInt (string ds "x00281050")))

(defn rows [^js/dicomParser.DataSet ds]
  (.uint16 ds "x00280010"))

(defn columns [^js/dicomParser.DataSet ds]
  (.uint16 ds "x00280011"))

(defn sop-class-uid [ds]
  (string ds "x00080016"))

(defn sop-inst-uid [ds]
  (string ds "x00080018"))

(defn study-inst-uid [ds]
  (string ds "x0020000d"))

(defn series-inst-uid [ds]
  (string ds "x0020000e"))

(defn patient-name [ds]
  (string ds "x00100010"))

(defn items [ds tag]
  (for [i (aget ds "elements" tag "items")]
    (aget i "dataSet")))

;;; imaging - functions for image based dicom dataset e.g. CT, MRI, PET, etc.

(defn winlev
  "Window/Level transform"
  [v win lev]
  (let [half (/ win 2.0)
        lower (- lev half)
        upper (+ lev half)]
    (cond
      (<= v lower) 0
      (>= v upper) 255
      :else (js/Math.round (/ (* 255 (- v lower)) win)))))

(defn pixelbuf
  ([ds]
   (let [pixel-data (aget ds "elements" "x7fe00010")
         buffer     (aget ds "byteArray" "buffer")
         offset     (aget pixel-data "dataOffset")
         length     (/ (aget pixel-data  "length") 2)]
     (js/Int16Array. buffer offset length))))

(defn ->imgarray
  [s16array & {:keys [window level width height]}]
  (let [size (* 4 width height)
        u8array (js/Uint8ClampedArray. size)]
    (loop [i 0]
      (when (< i size)
        (let [v16 (aget s16array i)
              v8  (winlev v16 window level)
              img-idx (* 4 i)]
          (aset u8array (+ 0 img-idx) v8)
          (aset u8array (+ 1 img-idx) v8)
          (aset u8array (+ 2 img-idx) v8)
          (aset u8array (+ 3 img-idx) 255))
        (recur (inc i))))
    u8array))

(defn ->image
  ([s16array & {:keys [window level width height]}]
   (let [u8array (->imgarray s16array :window window :level level :width width :height height)]
     (js/ImageData. u8array width height))))

(defn render [ds]
  (let [window     (window ds)
        level      (level ds)
        rows       (rows ds)
        columns    (columns ds)
        pixel-buf  (pixelbuf ds)]
    (js/console.log "load CT image ...")
    (->image pixel-buf :width columns :height rows :window window :level level)))


;; -------------------------
;; Structure set

(defn trans-coord 
  [p x y space]
  (list (/ (+ (first p) x) space) (/ (+ (second p) x) space))
  )

(defn draw-line-segment [ctx [w h] [x0 y0] [x1 y1]]
  
  )

(defn render-contour [cont ctx [w h]]
  (let [p0 (first cont)
        p (rest cont)]
    (set! (.-fillStyle ctx) "rgba(0, 0, 200, 0.5)")
    (set! (.-strokeStyle ctx) "rgba(255, 165, 0, 0.8)")
    (.beginPath ctx)
    (.moveTo ctx (first p0) (second p0))
    (-> (map #(.lineTo ctx (first %) (second %)) p)
        dorun
        )
    (.closePath ctx)
    (.stroke ctx)
    )
  )

;; -------------------------
;; Data

(defonce db (r/atom nil))
(defonce ct (r/atom nil))
(defonce ss (r/atom nil))

;; -------------------------
;; Handle
(defn on-file []
  (fn [evt] 
    (let [file (-> evt .-target .-files (aget 0))
          reader (js/FileReader.)]
      (letfn [(do-load [] 
                (let [array (js/Uint8Array. (.-result reader))]
                  (js/console.log "Load DICOM file" (.-name file))
                  (let [ds (.parseDicom js/dicomParser array)]
                    (reset! db ds)
                    (js/console.log ds)
                    (reset! ct (render ds))
                    (js/console.log @ct)
                    )))]
             (set! (.-onload reader) do-load)  
            )
      (.readAsArrayBuffer reader file)
      )))

;; -------------------------
;; Views 
(defn canvas [{:keys [width height render]}]
  [:canvas {:style  {:width width :height height}
            :width  width
            :height height
            :ref (fn [this]
                   (when-let [el this]
                     (set! (.-width el) width)
                     (set! (.-height el) height)
                     (when (some? render)
                       (let [w (.-clientWidth el)
                             h (.-clientHeight el)
                             ctx (.getContext el "2d")]
                         (.setTransform ctx 1 0 0 1 0 0)
                         (render ctx [w h])))))
            ;:on-click #(rf/dispatch [::events/canvas-size {:width 256 :height 256}])
            }])

(defn open-file []
  [:div 
   [:input {:type "file" :id "file" :name "file" 
            :on-change (on-file)}]
   ]
  )

(defn map-zipper [m]
  (z/zipper 
    (fn [x] (or (map? x) (map? (nth x 1))))
    (fn [x] (seq (if (map? x) x (nth x 1))))
    (fn [x children] 
      (if (map? x) 
        (into {} children) 
        (assoc x 1 (into {} children))))
    m))

(def dict {[0x0008, 0x0016] {:name "SOP Class UID" }
           [0x0020, 0x000d] {:name "Study Instance UID"}
           [0x0020, 0x0032] {:name "Image Position Patient"}
           })


(defn tag [[k v]] k)

(defn tag->str2 [[grp ele]] 
  (let [->str (fn [x]
                (let [s (.toString x 16)
                      l  4    ; string length of tag group/element 
                      padding (-> (take (- l (count s)) (repeat "0")) 
                                  clojure.string/join)]
                  (str padding s)))]
    (str "x" (->str grp) (->str ele))))


(tag->str2 [32 20])

(-> (map-zipper dict) z/down z/right z/node tag tag->str)

(defn table []
  [:div>table
   [:tr
    [:th "Name"]
    [:th "Tag"]
    [:th "Value"]]
   [:tr
    [:td "SOP Class UID"]
    [:td "0008, 0016"]
    [:td (and @db (sop-class-uid @db))]]
   [:tr
    [:td "Study Instance UID"]
    [:td "0020, 000d"]
    [:td (and @db (string @db "x0020000d"))]]
   [:tr
    [:td "Patient Name"]
    [:td "0010, 0010"]
    [:td (and @db (patient-name @db))]]
   [:tr
    [:td "Image Position Patient"]
    [:td "0020, 0032"]
    [:td (and @db (string @db "x00200032"))]]
   ]
  )

(defn home-page []
  [:div
   [open-file]
   [table]
   [canvas {:ct @ct :width 512 :height 512 :render 
           (fn [ctx [w h]]
       (set! (.-fillStyle ctx) "blue")
       (.fillRect ctx 0 0 w h)
       (js/console.log "rendering...")
       (when @ct (.putImageData ctx @ct 0 0))
       (render-contour [[100 100] [200 100] [200 200] [100 200]] ctx [w h])
       )} 
           ]
   ]
  )


;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn handle-file [evt]
  (js/console.log (-> evt (.-target) (.-files)))
  )

(defn init! []
  (js/console.log (js/document.getElementById "file"))
  (mount-root))



