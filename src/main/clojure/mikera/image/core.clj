(ns mikera.image.core
  (:require [clojure.java.io :refer [file resource]])
  (:require [clojure.string :refer [lower-case split]])
  (:require [mikera.image.colours :as col])
  (:require [mikera.image.filters :as filt])
  (:require [mikera.image.protocols :as protos])
  (:use mikera.cljutils.error) 
  (:import [java.awt.image BufferedImage BufferedImageOp]
           [javax.imageio.metadata IIOMetadata]
           [com.sun.imageio.plugins.jpeg JPEG JPEGImageWriter]
           [javax.imageio ImageTypeSpecifier]
           [org.w3c.dom Node NodeList])
  (:import [javax.imageio ImageIO IIOImage ImageWriter ImageWriteParam])
  (:import [org.imgscalr Scalr])
  (:import [mikera.gui Frames]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def subsampling-1x1 17) ; 4:4:4 subsampling
(def subsampling-2x1 33) ; 4:2:2 subsampling
(def subsampling-2x2 34) ; 4:2:0 subsampling
(def subsampling-4x1 65) ; 4:1:1 subsampling

(defn new-image
  "Creates a new BufferedImage with the specified width and height.
   Uses ARGB format by default."
  (^BufferedImage [width height]
    (new-image width height true))
  (^BufferedImage [width height alpha?]
    (if alpha?
      (BufferedImage. (int width) (int height) BufferedImage/TYPE_INT_ARGB)
      (BufferedImage. (int width) (int height) BufferedImage/TYPE_INT_RGB))))

(defn resize
  "Resizes an image to the specified width and height. If height is omitted,
  maintains the aspect ratio."
  (^BufferedImage [^BufferedImage image new-width new-height]
    (Scalr/resize image
                  org.imgscalr.Scalr$Method/BALANCED
                  org.imgscalr.Scalr$Mode/FIT_EXACT
                  (int new-width) (int new-height) nil))
  (^BufferedImage [^BufferedImage image new-width]
    (resize image new-width (/ (* new-width (.getHeight image)) (.getWidth image)))))

(defn scale-image
  "DEPRECATED: use 'resize' instead"
  (^BufferedImage [^BufferedImage image new-width new-height]
    (Scalr/resize image
                  org.imgscalr.Scalr$Method/BALANCED
                  org.imgscalr.Scalr$Mode/FIT_EXACT
                  (int new-width) (int new-height) nil)))

(defn scale
  "Scales an image by a given factor or ratio."
  (^BufferedImage [^BufferedImage image factor]
    (resize image (* (.getWidth image) factor) (* (.getHeight image) factor)))
  (^BufferedImage [^BufferedImage image width-factor height-factor]
    (resize image (* (.getWidth image) width-factor) (* (.getHeight image) height-factor))))

(defn load-image
  "Loads a BufferedImage from a string, file or a URL representing a resource
  on the classpath.

  Usage:

    (load-image \"/some/path/to/image.png\")
    ;; (require [clojure.java.io :refer [resource]])
    (load-image (resource \"some/path/to/image.png\"))"
  (^BufferedImage [resource] (protos/as-image resource)))

(defn load-image-resource
  "Loads an image from a named resource on the classpath.

   Equivalent to (load-image (clojure.java.io/resource res-path))"
  (^BufferedImage [res-path] (load-image (resource res-path))))

(defn zoom
  "Zooms into (scales) an image with a given scale factor."
  (^BufferedImage [^BufferedImage image factor]
    (scale image factor)))

(defn flip
  "Flips an image in the specified direction :horizontal or :vertical"
  (^BufferedImage [^BufferedImage image direction]
   (cond
     (= :horizontal direction)
     (Scalr/rotate image org.imgscalr.Scalr$Rotation/FLIP_HORZ nil)
     (= :vertical direction)
     (Scalr/rotate image org.imgscalr.Scalr$Rotation/FLIP_VERT nil)
     :else (error "Flip direction not valid: " direction))))

(defn rotate
  "Rotate an image clockwise by x degrees"
  (^BufferedImage [^BufferedImage image degrees]
   (let [rot (mod degrees 360)]
     (cond
	     (== rot 0)
	       image
	     (== rot 90)
	       (Scalr/rotate image org.imgscalr.Scalr$Rotation/CW_90 nil)
	     (== rot 180)
	       (Scalr/rotate image org.imgscalr.Scalr$Rotation/CW_180 nil)
	     (== rot 270)
	       (Scalr/rotate image org.imgscalr.Scalr$Rotation/CW_270 nil)
	     :else (error "Rotation amount not valid: " degrees " current supported values must be a multiple of 90")))))

(defn get-pixels
  "Gets the pixels in a BufferedImage as a primitive int[] array.
   This is often an efficient format for manipulating an image."
  (^ints [^BufferedImage image]
    (.getDataElements (.getRaster image) 0 0 (.getWidth image) (.getHeight image) nil)))

(defn set-pixels
  "Sets the pixels in a BufferedImage using a primitive int[] array.
   This is often an efficient format for manipulating an image."
  ([^BufferedImage image ^ints pixels]
    (.setDataElements (.getRaster image) 0 0 (.getWidth image) (.getHeight image) pixels)))

(defn filter-image
  "Applies a filter to a source image.
  Filter may be either a BufferedImageOp or an Imagez filter.

   Returns a new image."
  (^BufferedImage [^java.awt.image.BufferedImage image
                   filter]
  (let [filter (filt/to-image-op filter)
        dest-img (.createCompatibleDestImage filter image (.getColorModel image))]
    (.filter filter image dest-img)
    dest-img)))

(defn sub-image
  "Gets a sub-image area from an image."
  (^BufferedImage [^BufferedImage image x y w h]
    (.getSubimage image (int x) (int y) (int w) (int h))))

(defn gradient-image
  "Creates an image filled with a gradient according to the given spectrum function.
   Default is a filled gradient from left=0 to right=1."
  (^BufferedImage [spectrum-fn w h]
    (let [w (int w)
          h (int h)
          im (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
          g (.getGraphics im)]
      (dotimes [i w]
        (.setColor g (col/color (spectrum-fn (/ (double i) w))))
        (.fillRect g (int i) (int 0) (int 1) (int h)))
      im))
  (^BufferedImage [spectrum-fn]
    (gradient-image spectrum-fn 200 60)))

(defn show
  "Displays an image in a new frame.

   The frame includes simple menus for saving an image, and other handy utilities."
  ([image & {:keys [zoom title]}]
    (let [^BufferedImage image (if zoom (mikera.image.core/zoom image (double zoom)) image)
          ^String title (or title "Imagez Frame")]
      (Frames/display image title))))

(defn- ^ImageWriteParam apply-compression
  "Applies compression to the write parameter, if possible."
  [^ImageWriteParam write-param quality ext]
  (when (.canWriteCompressed write-param)
    (if (= ext "gif")
      (doto write-param
        (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
        (.setCompressionType "LZW"))
      (doto write-param
        (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
        (.setCompressionQuality quality))))
  write-param)

(defn- ^ImageWriteParam apply-progressive
  "Applies progressive encoding, if possible.

  If `progressive-flag` is `true`, turns progressive encoding on, `false`
  turns it off. Defaults to `ImageWriteParam/MODE_COPY_FROM_METADATA`, which
  is the default in ImageIO API."
  [^ImageWriteParam write-param progressive-flag]
  (when (.canWriteProgressive write-param)
    (let [mode-map {true  ImageWriteParam/MODE_DEFAULT
                    false ImageWriteParam/MODE_DISABLED}
          mode-flag (get mode-map
                         progressive-flag
                         ImageWriteParam/MODE_COPY_FROM_METADATA)]
      (doto write-param
        (.setProgressiveMode mode-flag))))
  write-param)

(defn valid-sof-marker?
  " 'SOF' marker can have:
      1 child node if the color representation is greyscale,
      3 child nodes if the color representation is YCbCr, and
      4 child nodes if the color representation is YCMK.
  This subsampling applies only to YCbCr."
  [^Node marker]
  (when (and (.equalsIgnoreCase "sof" (.getNodeName marker))
             (.hasChildNodes marker)
             (= 3 (.getLength (.getChildNodes marker))))
    true))

(defn set-subsampling
  [^Node sof-marker subsampling]
  (let [attribute-map (.getAttributes (.getFirstChild sof-marker))
        horizontal-sampling (.getNamedItem attribute-map "HsamplingFactor")
        vertical-sampling (.getNamedItem attribute-map "VsamplingFactor")]
    (.setNodeValue vertical-sampling (str (bit-and subsampling 0xf)))
    (.setNodeValue horizontal-sampling (str (bit-and (bit-shift-right subsampling 4) 0xf)))))

(defn generate-metadata-with-subsampling
  [^ImageWriter writer subsampling ^BufferedImage image]
  (when (instance? JPEGImageWriter writer )
    (let [specifier (ImageTypeSpecifier. (.getColorModel image) (.getSampleModel image))
          metadata (.getDefaultImageMetadata writer specifier nil)
          root-node (.getAsTree metadata JPEG/nativeImageMetadataFormatName)
          ; The top level root node has two children, out of which the second one will
          ; contain all the information related to image markers.
          last-child (.getLastChild root-node)]
      (when last-child
        (let [^Node markers (-> last-child
                          (.getChildNodes))]
          (loop [^Node marker (.getFirstChild markers)]
            (if (valid-sof-marker? marker)
              (do
                (set-subsampling marker subsampling)
                (.setFromTree metadata JPEG/nativeImageMetadataFormatName root-node))
              (recur (.getNextSibling marker)))))
        metadata))))


(defn save
  "Stores an image to disk.

  Accepts optional keyword arguments.

  `:quality` - decimal, between 0.0 and 1.0. Defaults to 0.8.

  `:progressive` - boolean, `true` turns progressive encoding on, `false`
  turns it off. Defaults to the default value in the ImageIO API -
  `ImageWriteParam/MODE_COPY_FROM_METADATA`. See
  [Java docs](http://docs.oracle.com/javase/7/docs/api/javax/imageio/ImageWriteParam.html).

  Examples:

    (save image \"/path/to/new/image.jpg\" :quality 1.0)
    (save image \"/path/to/new/image/jpg\" :progressive false)
    (save image \"/path/to/new/image/jpg\" :quality 0.7 :progressive true)

  Returns the path to the saved image when saved successfully."
  [^BufferedImage image path & {:keys [quality progressive subsampling]
                                  :or {quality 0.8
                                       progressive nil
                                       subsampling subsampling-1x1}}]
  (let [outfile (file path)
        ext (-> path (split #"\.") last lower-case)
        ^ImageWriter writer (.next (ImageIO/getImageWritersByFormatName ext))
        ^ImageWriteParam write-param (.getDefaultWriteParam writer)
        ^IIOMetadata metadata (generate-metadata-with-subsampling writer subsampling image)
        iioimage (IIOImage. image nil metadata)
        outstream (ImageIO/createImageOutputStream outfile)]
    (apply-compression write-param quality ext)
    (apply-progressive write-param progressive)
    (doto writer
      (.setOutput outstream)
      (.write nil iioimage write-param)
      (.dispose))
    (.close outstream)
    path))
