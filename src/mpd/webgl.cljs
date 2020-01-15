(ns mpd.webgl
  (:require [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.texture :as texture]
            [cljs-webgl.constants.texture-unit :as texture-unit]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.constants.texture-target :as texture-target]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]))


(def vertex-source
  "attribute highp vec4 position;
   attribute highp vec4 color;
   varying highp vec4 colorv;
   varying highp vec4 positionv;
   uniform mat4 projection;
   void main ( )
   {
	gl_Position = projection * position;
	gl_PointSize = 8.0;
	colorv = color;
	positionv = position;
   }")


(def fragment-source
  "varying highp vec4 colorv;
   varying highp vec4 positionv;
   void main( )
   {
	gl_FragColor = colorv;
   	if ( colorv.w == 1.0 && colorv.x < 0.8 )
   	{
       	 highp float ver = sin(positionv.y)+1.0;
       	 highp float hor = sin(positionv.x)+1.0;
       	 highp float dia = sin((positionv.x+positionv.y)/1.5)+1.0;
       	 ver = floor(ver * 2.0)/4.0;
       	 hor = floor(hor * 2.0)/4.0;
       	 dia = floor(dia * 2.0)/4.0;
       	 if ( colorv.x >= colorv.y && colorv.x >= colorv.z ) gl_FragColor.x -= ver*0.05;
       	 if ( colorv.y >= colorv.x && colorv.y >= colorv.z ) gl_FragColor.y -= hor*0.05;
	 if ( colorv.z >= colorv.x && colorv.z >= colorv.y ) gl_FragColor.z -= dia*0.2;
   	}
   }")


(defn init []
  "initialize webgl module"
  (let [context (context/get-context
                 (.getElementById js/document "main")
                 {:premultiplied-alpha false :alpha false})
    
        shader (shaders/create-program
                context
                (shaders/create-shader context shader/vertex-shader vertex-source)
                (shaders/create-shader context shader/fragment-shader fragment-source))
                
        mass_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/dynamic-draw)

        line_buffer (buffers/create-buffer
                     context
                     (ta/float32 [500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0
                                  500.0 500.0 0.0 1.0 1.0 1.0 1.0 1.0])
                     buffer-object/array-buffer
                     buffer-object/static-draw)
        
        location_pos (shaders/get-attrib-location context shader "position")
        location_col (shaders/get-attrib-location context shader "color")]
    
    {:context context
     :shader shader
     :mass_buffer mass_buffer
     :line_buffer line_buffer
     :location_pos location_pos
     :location_col location_col}))


(defn clear[ {context :context} ]
  (buffers/clear-color-buffer context 0.0 0.0 0.0 1.0))


(defn drawlines! [ {:keys [context shader line_buffer location_pos location_col] :as state} projection lines ]
  "draw lines"

  (.bindBuffer context buffer-object/array-buffer line_buffer)
  
  (.bufferData context
               buffer-object/array-buffer
               (ta/float32
                (vec
                 (flatten
                  (map
                   (fn voxelize [[tx ty]]
                     [tx ty 0.0 1.0 1.0 1.0 1.0 1.0]) lines))))
               buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count lines)
   :shader shader
   :draw-mode draw-mode/lines
   :attributes [{:buffer line_buffer
                 :location location_pos
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 0
                 :stride 32}
                {:buffer line_buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 16
                 :stride 32}]
   :uniforms [{:name "projection"
               :type :mat4
               :values projection}])

  state)


(defn drawpoints! [{:keys [context shader mass_buffer location_pos location_col] :as state} projection points]
  "draw points"
  
  (.bindBuffer context buffer-object/array-buffer mass_buffer)
  
  (.bufferData context
               buffer-object/array-buffer
               (ta/float32
                (vec
                 (flatten
                  (map
                   (fn voxelize [[tx ty]]
                     [tx ty 0.0 1.0 1.0 1.0 1.0 1.0]) points))))
               buffer-object/dynamic-draw)
  
  (buffers/draw!
   context
   :count (count points)
   :shader shader
            :draw-mode draw-mode/points               
            :attributes [{:buffer mass_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}                        {:buffer mass_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}])

  state)
