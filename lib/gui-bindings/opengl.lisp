(defpackage #:opengl
 (:use #:cffi #:cl)
 (:nicknames #:gl)
 (:export
  enum boolean bitfield byte short int sizei ubyte ushort uint float clampf
  double clampd void intptr sizeiptr char

  +version-1-1+ +version-1-2+ +version-1-3+ +version-1-4+ +version-1-5+
  +version-2-0+ +accum+ +load+ +return+ +mult+ +add+ +never+ +less+ +equal+
  +lequal+ +greater+ +notequal+ +gequal+ +always+ +current-bit+ +point-bit+
  +line-bit+ +polygon-bit+ +polygon-stipple-bit+ +pixel-mode-bit+ +lighting-bit+
  +fog-bit+ +depth-buffer-bit+ +accum-buffer-bit+ +stencil-buffer-bit+
  +viewport-bit+ +transform-bit+ +enable-bit+ +color-buffer-bit+ +hint-bit+
  +eval-bit+ +list-bit+ +texture-bit+ +scissor-bit+ +all-attrib-bits+ +points+
  +lines+ +line-loop+ +line-strip+ +triangles+ +triangle-strip+ +triangle-fan+
  +quads+ +quad-strip+ +polygon+ +zero+ +one+ +src-color+ +one-minus-src-color+
  +src-alpha+ +one-minus-src-alpha+ +dst-alpha+ +one-minus-dst-alpha+ +dst-color+
  +one-minus-dst-color+ +src-alpha-saturate+ +true+ +false+ +clip-plane0+
  +clip-plane1+ +clip-plane2+ +clip-plane3+ +clip-plane4+ +clip-plane5+ +byte+
  +unsigned-byte+ +short+ +unsigned-short+ +int+ +unsigned-int+ +float+ +2-bytes+
  +3-bytes+ +4-bytes+ +double+ +none+ +front-left+ +front-right+ +back-left+
  +back-right+ +front+ +back+ +left+ +right+ +front-and-back+ +aux0+ +aux1+
  +aux2+ +aux3+ +no-error+ +invalid-enum+ +invalid-value+ +invalid-operation+
  +stack-overflow+ +stack-underflow+ +out-of-memory+ +2d+ +3d+ +3d-color+
  +3d-color-texture+ +4d-color-texture+ +pass-through-token+ +point-token+
 +line-token+ +polygon-token+ +bitmap-token+ +draw-pixel-token+
 +copy-pixel-token+ +line-reset-token+ +exp+ +exp2+ +cw+ +ccw+ +coeff+ +order+
 +domain+ +current-color+ +current-index+ +current-normal+
 +current-texture-coords+ +current-raster-color+ +current-raster-index+
 +current-raster-texture-coords+ +current-raster-position+
 +current-raster-position-valid+ +current-raster-distance+ +point-smooth+
 +point-size+ +point-size-range+ +point-size-granularity+ +line-smooth+
 +line-width+ +line-width-range+ +line-width-granularity+ +line-stipple+
 +line-stipple-pattern+ +line-stipple-repeat+ +list-mode+ +max-list-nesting+
 +list-base+ +list-index+ +polygon-mode+ +polygon-smooth+ +polygon-stipple+
 +edge-flag+ +cull-face+ +cull-face-mode+ +front-face+ +lighting+
 +light-model-local-viewer+ +light-model-two-side+ +light-model-ambient+
 +shade-model+ +color-material-face+ +color-material-parameter+ +color-material+
 +fog+ +fog-index+ +fog-density+ +fog-start+ +fog-end+ +fog-mode+ +fog-color+
 +depth-range+ +depth-test+ +depth-writemask+ +depth-clear-value+ +depth-func+
 +accum-clear-value+ +stencil-test+ +stencil-clear-value+ +stencil-func+
 +stencil-value-mask+ +stencil-fail+ +stencil-pass-depth-fail+
 +stencil-pass-depth-pass+ +stencil-ref+ +stencil-writemask+ +matrix-mode+
 +normalize+ +viewport+ +modelview-stack-depth+ +projection-stack-depth+
 +texture-stack-depth+ +modelview-matrix+ +projection-matrix+ +texture-matrix+
 +attrib-stack-depth+ +client-attrib-stack-depth+ +alpha-test+ +alpha-test-func+
 +alpha-test-ref+ +dither+ +blend-dst+ +blend-src+ +blend+ +logic-op-mode+
 +index-logic-op+ +color-logic-op+ +aux-buffers+ +draw-buffer+ +read-buffer+
 +scissor-box+ +scissor-test+ +index-clear-value+ +index-writemask+
 +color-clear-value+ +color-writemask+ +index-mode+ +rgba-mode+ +doublebuffer+
 +stereo+ +render-mode+ +perspective-correction-hint+ +point-smooth-hint+
 +line-smooth-hint+ +polygon-smooth-hint+ +fog-hint+ +texture-gen-s+
 +texture-gen-t+ +texture-gen-r+ +texture-gen-q+ +pixel-map-i-to-i+
 +pixel-map-s-to-s+ +pixel-map-i-to-r+ +pixel-map-i-to-g+ +pixel-map-i-to-b+
 +pixel-map-i-to-a+ +pixel-map-r-to-r+ +pixel-map-g-to-g+ +pixel-map-b-to-b+
 +pixel-map-a-to-a+ +pixel-map-i-to-i-size+ +pixel-map-s-to-s-size+
 +pixel-map-i-to-r-size+ +pixel-map-i-to-g-size+ +pixel-map-i-to-b-size+
 +pixel-map-i-to-a-size+ +pixel-map-r-to-r-size+ +pixel-map-g-to-g-size+
 +pixel-map-b-to-b-size+ +pixel-map-a-to-a-size+ +unpack-swap-bytes+
 +unpack-lsb-first+ +unpack-row-length+ +unpack-skip-rows+ +unpack-skip-pixels+
 +unpack-alignment+ +pack-swap-bytes+ +pack-lsb-first+ +pack-row-length+
 +pack-skip-rows+ +pack-skip-pixels+ +pack-alignment+ +map-color+ +map-stencil+
 +index-shift+ +index-offset+ +red-scale+ +red-bias+ +zoom-x+ +zoom-y+
 +green-scale+ +green-bias+ +blue-scale+ +blue-bias+ +alpha-scale+ +alpha-bias+
 +depth-scale+ +depth-bias+ +max-eval-order+ +max-lights+ +max-clip-planes+
 +max-texture-size+ +max-pixel-map-table+ +max-attrib-stack-depth+
 +max-modelview-stack-depth+ +max-name-stack-depth+ +max-projection-stack-depth+
 +max-texture-stack-depth+ +max-viewport-dims+ +max-client-attrib-stack-depth+
 +subpixel-bits+ +index-bits+ +red-bits+ +green-bits+ +blue-bits+ +alpha-bits+
 +depth-bits+ +stencil-bits+ +accum-red-bits+ +accum-green-bits+
 +accum-blue-bits+ +accum-alpha-bits+ +name-stack-depth+ +auto-normal+
 +map1-color-4+ +map1-index+ +map1-normal+ +map1-texture-coord-1+
 +map1-texture-coord-2+ +map1-texture-coord-3+ +map1-texture-coord-4+
 +map1-vertex-3+ +map1-vertex-4+ +map2-color-4+ +map2-index+ +map2-normal+
 +map2-texture-coord-1+ +map2-texture-coord-2+ +map2-texture-coord-3+
 +map2-texture-coord-4+ +map2-vertex-3+ +map2-vertex-4+ +map1-grid-domain+
 +map1-grid-segments+ +map2-grid-domain+ +map2-grid-segments+ +texture-1d+
 +texture-2d+ +feedback-buffer-pointer+ +feedback-buffer-size+
 +feedback-buffer-type+ +selection-buffer-pointer+ +selection-buffer-size+
 +texture-width+ +texture-height+ +texture-internal-format+
 +texture-border-color+ +texture-border+ +dont-care+ +fastest+ +nicest+ +light0+
 +light1+ +light2+ +light3+ +light4+ +light5+ +light6+ +light7+ +ambient+
 +diffuse+ +specular+ +position+ +spot-direction+ +spot-exponent+ +spot-cutoff+
 +constant-attenuation+ +linear-attenuation+ +quadratic-attenuation+ +compile+
 +compile-and-execute+ +clear+ +and+ +and-reverse+ +copy+ +and-inverted+ +noop+
 +xor+ +or+ +nor+ +equiv+ +invert+ +or-reverse+ +copy-inverted+ +or-inverted+
 +nand+ +set+ +emission+ +shininess+ +ambient-and-diffuse+ +color-indexes+
 +modelview+ +projection+ +texture+ +color+ +depth+ +stencil+ +color-index+
 +stencil-index+ +depth-component+ +red+ +green+ +blue+ +alpha+ +rgb+ +rgba+
 +luminance+ +luminance-alpha+ +bitmap+ +point+ +line+ +fill+ +render+
 +feedback+ +select+ +flat+ +smooth+ +keep+ +replace+ +incr+ +decr+ +vendor+
 +renderer+ +version+ +extensions+ +s+ +t+ +r+ +q+ +modulate+ +decal+
 +texture-env-mode+ +texture-env-color+ +texture-env+ +eye-linear+
 +object-linear+ +sphere-map+ +texture-gen-mode+ +object-plane+ +eye-plane+
 +nearest+ +linear+ +nearest-mipmap-nearest+ +linear-mipmap-nearest+
 +nearest-mipmap-linear+ +linear-mipmap-linear+ +texture-mag-filter+
 +texture-min-filter+ +texture-wrap-s+ +texture-wrap-t+ +clamp+ +repeat+
 +client-pixel-store-bit+ +client-vertex-array-bit+ +client-all-attrib-bits+
 +polygon-offset-factor+ +polygon-offset-units+ +polygon-offset-point+
 +polygon-offset-line+ +polygon-offset-fill+ +alpha4+ +alpha8+ +alpha12+
 +alpha16+ +luminance4+ +luminance8+ +luminance12+ +luminance16+
 +luminance4-alpha4+ +luminance6-alpha2+ +luminance8-alpha8+
 +luminance12-alpha4+ +luminance12-alpha12+ +luminance16-alpha16+ +intensity+
 +intensity4+ +intensity8+ +intensity12+ +intensity16+ +r3-g3-b2+ +rgb4+ +rgb5+
 +rgb8+ +rgb10+ +rgb12+ +rgb16+ +rgba2+ +rgba4+ +rgb5-a1+ +rgba8+ +rgb10-a2+
 +rgba12+ +rgba16+ +texture-red-size+ +texture-green-size+ +texture-blue-size+
 +texture-alpha-size+ +texture-luminance-size+ +texture-intensity-size+
 +proxy-texture-1d+ +proxy-texture-2d+ +texture-priority+ +texture-resident+
 +texture-binding-1d+ +texture-binding-2d+ +texture-binding-3d+ +vertex-array+
 +normal-array+ +color-array+ +index-array+ +texture-coord-array+
 +edge-flag-array+ +vertex-array-size+ +vertex-array-type+ +vertex-array-stride+
 +normal-array-type+ +normal-array-stride+ +color-array-size+ +color-array-type+
 +color-array-stride+ +index-array-type+ +index-array-stride+
 +texture-coord-array-size+ +texture-coord-array-type+
 +texture-coord-array-stride+ +edge-flag-array-stride+ +vertex-array-pointer+
 +normal-array-pointer+ +color-array-pointer+ +index-array-pointer+
 +texture-coord-array-pointer+ +edge-flag-array-pointer+ +v2f+ +v3f+ +c4ub-v2f+
 +c4ub-v3f+ +c3f-v3f+ +n3f-v3f+ +c4f-n3f-v3f+ +t2f-v3f+ +t4f-v4f+ +t2f-c4ub-v3f+
 +t2f-c3f-v3f+ +t2f-n3f-v3f+ +t2f-c4f-n3f-v3f+ +t4f-c4f-n3f-v4f+ +bgr+ +bgra+
 +constant-color+ +one-minus-constant-color+ +constant-alpha+
 +one-minus-constant-alpha+ +blend-color+ +func-add+ +min+ +max+
 +blend-equation+ +blend-equation-rgb+ +blend-equation-alpha+ +func-subtract+
 +func-reverse-subtract+ +color-matrix+ +color-matrix-stack-depth+
 +max-color-matrix-stack-depth+ +post-color-matrix-red-scale+
 +post-color-matrix-green-scale+ +post-color-matrix-blue-scale+
 +post-color-matrix-alpha-scale+ +post-color-matrix-red-bias+
 +post-color-matrix-green-bias+ +post-color-matrix-blue-bias+
 +post-color-matrix-alpha-bias+ +color-table+ +post-convolution-color-table+
 +post-color-matrix-color-table+ +proxy-color-table+
 +proxy-post-convolution-color-table+ +proxy-post-color-matrix-color-table+
 +color-table-scale+ +color-table-bias+ +color-table-format+ +color-table-width+
 +color-table-red-size+ +color-table-green-size+ +color-table-blue-size+
 +color-table-alpha-size+ +color-table-luminance-size+
 +color-table-intensity-size+ +convolution-1d+ +convolution-2d+ +separable-2d+
 +convolution-border-mode+ +convolution-filter-scale+ +convolution-filter-bias+
 +reduce+ +convolution-format+ +convolution-width+ +convolution-height+
 +max-convolution-width+ +max-convolution-height+ +post-convolution-red-scale+
 +post-convolution-green-scale+ +post-convolution-blue-scale+
 +post-convolution-alpha-scale+ +post-convolution-red-bias+
 +post-convolution-green-bias+ +post-convolution-blue-bias+
 +post-convolution-alpha-bias+ +constant-border+ +replicate-border+
 +convolution-border-color+ +max-elements-vertices+ +max-elements-indices+
 +histogram+ +proxy-histogram+ +histogram-width+ +histogram-format+
 +histogram-red-size+ +histogram-green-size+ +histogram-blue-size+
 +histogram-alpha-size+ +histogram-luminance-size+ +histogram-sink+ +minmax+
 +minmax-format+ +minmax-sink+ +table-too-large+ +unsigned-byte-3-3-2+
 +unsigned-short-4-4-4-4+ +unsigned-short-5-5-5-1+ +unsigned-int-8-8-8-8+
 +unsigned-int-10-10-10-2+ +unsigned-byte-2-3-3-rev+ +unsigned-short-5-6-5+
 +unsigned-short-5-6-5-rev+ +unsigned-short-4-4-4-4-rev+
 +unsigned-short-1-5-5-5-rev+ +unsigned-int-8-8-8-8-rev+
 +unsigned-int-2-10-10-10-rev+ +rescale-normal+ +light-model-color-control+
 +single-color+ +separate-specular-color+ +pack-skip-images+ +pack-image-height+
 +unpack-skip-images+ +unpack-image-height+ +texture-3d+ +proxy-texture-3d+
 +texture-depth+ +texture-wrap-r+ +max-3d-texture-size+ +clamp-to-edge+
 +clamp-to-border+ +texture-min-lod+ +texture-max-lod+ +texture-base-level+
 +texture-max-level+ +smooth-point-size-range+ +smooth-point-size-granularity+
 +smooth-line-width-range+ +smooth-line-width-granularity+
 +aliased-point-size-range+ +aliased-line-width-range+ +texture0+ +texture1+
 +texture2+ +texture3+ +texture4+ +texture5+ +texture6+ +texture7+ +texture8+
 +texture9+ +texture10+ +texture11+ +texture12+ +texture13+ +texture14+
 +texture15+ +texture16+ +texture17+ +texture18+ +texture19+ +texture20+
 +texture21+ +texture22+ +texture23+ +texture24+ +texture25+ +texture26+
 +texture27+ +texture28+ +texture29+ +texture30+ +texture31+ +active-texture+
 +client-active-texture+ +max-texture-units+ +combine+ +combine-rgb+
 +combine-alpha+ +rgb-scale+ +add-signed+ +interpolate+ +constant+
 +primary-color+ +previous+ +subtract+ +src0-rgb+ +src1-rgb+ +src2-rgb+
 +src3-rgb+ +src4-rgb+ +src5-rgb+ +src6-rgb+ +src7-rgb+ +src0-alpha+
 +src1-alpha+ +src2-alpha+ +src3-alpha+ +src4-alpha+ +src5-alpha+ +src6-alpha+
 +src7-alpha+ +source0-rgb+ +source1-rgb+ +source2-rgb+ +source3-rgb+
 +source4-rgb+ +source5-rgb+ +source6-rgb+ +source7-rgb+ +source0-alpha+
 +source1-alpha+ +source2-alpha+ +source3-alpha+ +source4-alpha+ +source5-alpha+
 +source6-alpha+ +source7-alpha+ +operand0-rgb+ +operand1-rgb+ +operand2-rgb+
 +operand3-rgb+ +operand4-rgb+ +operand5-rgb+ +operand6-rgb+ +operand7-rgb+
 +operand0-alpha+ +operand1-alpha+ +operand2-alpha+ +operand3-alpha+
 +operand4-alpha+ +operand5-alpha+ +operand6-alpha+ +operand7-alpha+ +dot3-rgb+
 +dot3-rgba+ +transpose-modelview-matrix+ +transpose-projection-matrix+
 +transpose-texture-matrix+ +transpose-color-matrix+ +normal-map+
 +reflection-map+ +texture-cube-map+ +texture-binding-cube-map+
 +texture-cube-map-positive-x+ +texture-cube-map-negative-x+
 +texture-cube-map-positive-y+ +texture-cube-map-negative-y+
 +texture-cube-map-positive-z+ +texture-cube-map-negative-z+
 +proxy-texture-cube-map+ +max-cube-map-texture-size+ +compressed-alpha+
 +compressed-luminance+ +compressed-luminance-alpha+ +compressed-intensity+
 +compressed-rgb+ +compressed-rgba+ +texture-compression-hint+
 +texture-compressed-image-size+ +texture-compressed+
 +num-compressed-texture-formats+ +compressed-texture-formats+ +multisample+
 +sample-alpha-to-coverage+ +sample-alpha-to-one+ +sample-coverage+
 +sample-buffers+ +samples+ +sample-coverage-value+ +sample-coverage-invert+
 +multisample-bit+ +depth-component16+ +depth-component24+ +depth-component32+
 +texture-depth-size+ +depth-texture-mode+ +texture-compare-mode+
 +texture-compare-func+ +compare-r-to-texture+ +query-counter-bits+
 +current-query+ +query-result+ +query-result-available+ +samples-passed+
 +fog-coord-src+ +fog-coord+ +fragment-depth+ +current-fog-coord+
 +fog-coord-array-type+ +fog-coord-array-stride+ +fog-coord-array-pointer+
 +fog-coord-array+ +fog-coordinate-source+ +fog-coordinate+
 +current-fog-coordinate+ +fog-coordinate-array-type+
 +fog-coordinate-array-stride+ +fog-coordinate-array-pointer+
 +fog-coordinate-array+ +color-sum+ +current-secondary-color+
 +secondary-color-array-size+ +secondary-color-array-type+
 +secondary-color-array-stride+ +secondary-color-array-pointer+
 +secondary-color-array+ +point-size-min+ +point-size-max+
 +point-fade-threshold-size+ +point-distance-attenuation+ +blend-dst-rgb+
 +blend-src-rgb+ +blend-dst-alpha+ +blend-src-alpha+ +generate-mipmap+
 +generate-mipmap-hint+ +incr-wrap+ +decr-wrap+ +mirrored-repeat+
 +max-texture-lod-bias+ +texture-filter-control+ +texture-lod-bias+
 +array-buffer+ +element-array-buffer+ +array-buffer-binding+
 +element-array-buffer-binding+ +vertex-array-buffer-binding+
 +normal-array-buffer-binding+ +color-array-buffer-binding+
 +index-array-buffer-binding+ +texture-coord-array-buffer-binding+
 +edge-flag-array-buffer-binding+ +secondary-color-array-buffer-binding+
 +fog-coord-array-buffer-binding+ +weight-array-buffer-binding+
 +vertex-attrib-array-buffer-binding+ +stream-draw+ +stream-read+ +stream-copy+
 +static-draw+ +static-read+ +static-copy+ +dynamic-draw+ +dynamic-read+
 +dynamic-copy+ +read-only+ +write-only+ +read-write+ +buffer-size+
 +buffer-usage+ +buffer-access+ +buffer-mapped+ +buffer-map-pointer+
 +fog-coordinate-array-buffer-binding+ +current-program+ +shader-type+
 +delete-status+ +compile-status+ +link-status+ +validate-status+
 +info-log-length+ +attached-shaders+ +active-uniforms+
 +active-uniform-max-length+ +shader-source-length+ +float-vec2+ +float-vec3+
 +float-vec4+ +int-vec2+ +int-vec3+ +int-vec4+ +bool+ +bool-vec2+ +bool-vec3+
 +bool-vec4+ +float-mat2+ +float-mat3+ +float-mat4+ +sampler-1d+ +sampler-2d+
 +sampler-3d+ +sampler-cube+ +sampler-1d-shadow+ +sampler-2d-shadow+
 +shading-language-version+ +vertex-shader+ +max-vertex-uniform-components+
 +max-varying-floats+ +max-vertex-texture-image-units+
 +max-combined-texture-image-units+ +active-attributes+
 +active-attribute-max-length+ +fragment-shader+
 +max-fragment-uniform-components+ +fragment-shader-derivative-hint+
 +max-vertex-attribs+ +vertex-attrib-array-enabled+ +vertex-attrib-array-size+
 +vertex-attrib-array-stride+ +vertex-attrib-array-type+
 +vertex-attrib-array-normalized+ +current-vertex-attrib+
 +vertex-attrib-array-pointer+ +vertex-program-point-size+
 +vertex-program-two-side+ +max-texture-coords+ +max-texture-image-units+
 +max-draw-buffers+ +draw-buffer0+ +draw-buffer1+ +draw-buffer2+ +draw-buffer3+
 +draw-buffer4+ +draw-buffer5+ +draw-buffer6+ +draw-buffer7+ +draw-buffer8+
 +draw-buffer9+ +draw-buffer10+ +draw-buffer11+ +draw-buffer12+ +draw-buffer13+
 +draw-buffer14+ +draw-buffer15+ +point-sprite+ +coord-replace+
 +point-sprite-coord-origin+ +lower-left+ +upper-left+ +stencil-back-func+
 +stencil-back-value-mask+ +stencil-back-ref+ +stencil-back-fail+
 +stencil-back-pass-depth-fail+ +stencil-back-pass-depth-pass+
 +stencil-back-writemask+

 accum alpha-func are-textures-resident array-element begin bind-texture bitmap
 blend-color blend-equation blend-equation-separate blend-func call-list
 call-lists clear clear-accum clear-color clear-depth clear-index clear-stencil
 clip-plane color-3b color-3bv color-3d color-3dv color-3f color-3fv color-3i color-3iv
 color-3s color-3sv color-3ub color-3ubv color-3ui color-3uiv color-3us color-3usv
 color-4b color-4bv color-4d color-4dv color-4f color-4fv color-4i color-4iv color-4s
 color-4sv color-4ub color-4ubv color-4ui color-4uiv color-4us color-4usv color-mask
 color-material color-pointer color-sub-table color-table
 color-table-parameter-fv color-table-parameter-iv convolution-filter-1d
 convolution-filter-2d convolution-parameter-f convolution-parameter-fv
 convolution-parameter-i convolution-parameter-iv copy-color-sub-table
 copy-color-table copy-convolution-filter-1d copy-convolution-filter-2d
 copy-pixels copy-tex-image-1d copy-tex-image-2d copy-tex-sub-image-1d
 copy-tex-sub-image-2d copy-tex-sub-image-3d cull-face delete-lists
 delete-textures depth-func depth-mask depth-range disable disable-client-state
 draw-arrays draw-buffer draw-elements draw-pixels draw-range-elements edge-flag
 edge-flag-pointer edge-flagv enable enable-client-state end end-list
 eval-coord-1d eval-coord-1dv eval-coord-1f eval-coord-1fv eval-coord-2d
 eval-coord-2dv eval-coord-2f eval-coord-2fv eval-mesh-1 eval-mesh-2 eval-point-1
 eval-point-2 feedback-buffer finish flush fog-f fog-fv fog-i fog-iv front-face
 frustum gen-lists gen-textures get-booleanv get-clip-plane get-color-table
 get-color-table-parameter-fv get-color-table-parameter-iv get-convolution-filter
 get-convolution-parameter-fv get-convolution-parameter-iv get-doublev get-error
 get-floatv get-histogram get-histogram-parameter-fv get-histogram-parameter-iv
 get-integerv get-light-fv get-light-iv get-map-dv get-map-fv get-map-iv
 get-material-fv get-material-iv get-minmax get-minmax-parameter-fv
 get-minmax-parameter-iv get-pixel-map-fv get-pixel-map-uiv get-pixel-map-usv
 get-pointerv get-polygon-stipple get-separable-filter get-string get-tex-env-fv
 get-tex-env-iv get-tex-gen-dv get-tex-gen-fv get-tex-gen-iv get-tex-image
 get-tex-level-parameter-fv get-tex-level-parameter-iv get-tex-parameter-fv
 get-tex-parameter-iv hint histogram index-mask index-pointer index-d index-dv
 index-f index-fv index-i index-iv index-s index-sv index-ub index-ubv init-names
 interleaved-arrays is-enabled is-list is-texture light-model-f light-model-fv
 light-model-i light-model-iv light-f light-fv light-i light-iv line-stipple
 line-width list-base load-identity load-matrixd load-matrixf load-name logic-op
 map-1d map-1f map-2d map-2f map-grid-1d map-grid-1f map-grid-2d map-grid-2f material-f
 material-fv material-i material-iv matrix-mode minmax mult-matrix-d mult-matrix-f
 new-list normal-3b normal-3bv normal-3d normal-3dv normal-3f normal-3fv normal-3i
 normal-3iv normal-3s normal-3sv normal-pointer ortho pass-through pixel-map-fv
 pixel-mapu-iv pixel-map-usv pixel-store-f pixel-store-i pixel-transfer-f
 pixel-transfer-i pixel-zoom point-size polygon-mode polygon-offset
 polygon-stipple pop-attrib pop-client-attrib pop-matrix pop-name
 prioritize-textures push-attrib push-client-attrib push-matrix push-name
 raster-pos-2d raster-pos-2dv raster-pos-2f raster-pos-2fv raster-pos-2i
 raster-pos-2iv raster-pos-2s raster-pos-2sv raster-pos-3d raster-pos-3dv
 raster-pos-3f raster-pos-3fv raster-pos-3i raster-pos-3iv raster-pos-3s
 raster-pos-3sv raster-pos-4d raster-pos-4dv raster-pos-4f raster-pos-4fv
 raster-pos-4i raster-pos-4iv raster-pos-4s raster-pos-4sv read-buffer read-pixels
 rect-d rect-dv rect-f rect-fv rect-i rect-iv rect-s rect-sv render-mode reset-histogram
 reset-minmax rotate-d rotate-f scale-d scale-f scissor select-buffer
 separable-filter-2d shade-model stencil-func stencil-mask stencil-op
 tex-coord-1d tex-coord-1dv tex-coord-1f tex-coord-1fv tex-coord-1i tex-coord-1iv
 tex-coord-1s tex-coord-1sv tex-coord-2d tex-coord-2dv tex-coord-2f tex-coord-2fv
 tex-coord-2i tex-coord-2iv tex-coord-2s tex-coord-2sv tex-coord-3d tex-coord-3dv
 tex-coord-3f tex-coord-3fv tex-coord-3i tex-coord-3iv tex-coord-3s tex-coord-3sv
 tex-coord-4d tex-coord-4dv tex-coord-4f tex-coord-4fv tex-coord-4i tex-coord-4iv
 tex-coord-4s tex-coord-4sv tex-coord-pointer tex-env-f tex-env-fv tex-env-i
 tex-env-iv tex-gen-d tex-gen-dv tex-gen-f tex-gen-fv tex-gen-i tex-gen-iv tex-image-1d
 tex-image-2d tex-image-3d tex-parameter-f tex-parameter-fv tex-parameter-i
 tex-parameter-iv tex-sub-image-1d tex-sub-image-2d tex-sub-image-3d translate-d
 translate-f vertex-2d vertex-2dv vertex-2f vertex-2fv vertex-2i vertex-2iv vertex-2s
 vertex-2sv vertex-3d vertex-3dv vertex-3f vertex-3fv vertex-3i vertex-3iv vertex-3s
 vertex-3sv vertex-4d vertex-4dv vertex-4f vertex-4fv vertex-4i vertex-4iv vertex-4s
 vertex-4sv vertex-pointer viewport sample-coverage sample-pass
 load-transpose-matrix-f load-transpose-matrixd mult-transpose-matrix-f
 mult-transpose-matrix-d compressed-tex-image-3d compressed-tex-image-2d
 compressed-tex-image-1d compressed-tex-sub-image-3d compressed-tex-sub-image-2d
 compressed-tex-sub-image-1d get-compressed-tex-image active-texture
 client-active-texture multi-tex-coord-1d multi-tex-coord-1dv multi-tex-coord-1f
 multi-tex-coord-1fv multi-tex-coord-1i multi-tex-coord-1iv multi-tex-coord-1s
 multi-tex-coord-1sv multi-tex-coord-2d multi-tex-coord-2dv multi-tex-coord-2f
 multi-tex-coord-2fv multi-tex-coord-2i multi-tex-coord-2iv multi-tex-coord-2s
 multi-tex-coord-2sv multi-tex-coord-3d multi-tex-coord-3dv multi-tex-coord-3f
 multi-tex-coord-3fv multi-tex-coord-3i multi-tex-coord-3iv multi-tex-coord-3s
 multi-tex-coord-3sv multi-tex-coord-4d multi-tex-coord-4dv multi-tex-coord-4f
 multi-tex-coord-4fv multi-tex-coord-4i multi-tex-coord-4iv multi-tex-coord-4s
 multi-tex-coord-4sv fog-coord-f fog-coord-fv fog-coord-d fog-coord-dv
 fog-coord-pointer secondary-color-3b secondary-color-3bv secondary-color-3d
 secondary-color-3dv secondary-color-3f secondary-color-3fv secondary-color-3i
 secondary-color-3iv secondary-color-3s secondary-color-3sv secondary-color-3ub
 secondary-color-3ubv secondary-color-3ui secondary-color-3uiv secondary-color-3us
 secondary-color-3usv secondary-color-pointer point-parameter-f point-parameter-fv
 point-parameter-i point-parameter-iv blend-func-separate multi-draw-arrays
 multi-draw-elements window-pos-2d window-pos-2dv window-pos-2f window-pos-2fv
 window-pos-2i window-pos-2iv window-pos-2s window-pos-2sv window-pos-3d
 window-pos-3dv window-pos-3f window-pos-3fv window-pos-3i window-pos-3iv
 window-pos-3s window-pos-3sv gen-queries delete-queries is-query begin-query
 end-query get-query-iv get-query-object-iv get-query-objectu-iv bind-buffer
 delete-buffers gen-buffers is-buffer buffer-data buffer-sub-data
 get-buffer-sub-data map-buffer unmap-buffer get-buffer-parameter-iv
 get-buffer-pointerv draw-buffers vertex-attrib-1d vertex-attrib-1dv
 vertex-attrib-1f vertex-attrib-1fv vertex-attrib-1s vertex-attrib-1sv
 vertex-attrib-2d vertex-attrib-2dv vertex-attrib-2f vertex-attrib-2fv
 vertex-attrib-2s vertex-attrib-2sv vertex-attrib-3d vertex-attrib-3dv
 vertex-attrib-3f vertex-attrib-3fv vertex-attrib-3s vertex-attrib-3sv
 vertex-attrib-4-nbv vertex-attrib-4-niv vertex-attrib-4-nsv vertex-attrib-4-nub
 vertex-attrib-4-nubv vertex-attrib-4-nuiv vertex-attrib-4-nusv vertex-attrib-4bv
 vertex-attrib-4d vertex-attrib-4dv vertex-attrib-4f vertex-attrib-4fv
 vertex-attrib-4iv vertex-attrib-4s vertex-attrib-4sv vertex-attrib-4ubv
 vertex-attrib-4uiv vertex-attrib-4usv vertex-attrib-pointer
 enable-vertex-attrib-array disable-vertex-attrib-array get-vertex-attrib-dv
 get-vertex-attrib-fv get-vertex-attrib-iv get-vertex-attrib-pointerv
 delete-shader detach-shader create-shader shader-source compile-shader
 create-program attach-shader link-program use-program delete-program
 validate-program uniform-1f uniform-2f uniform-3f uniform-4f uniform-1i uniform-2i
 uniform-3i uniform-4i uniform-1fv uniform-2fv uniform-3fv uniform-4fv uniform-1iv
 uniform-2iv uniform-3iv uniform-4iv uniform-matrix-2fv uniform-matrix-3fv
 uniform-matrix-4fv is-shader is-program get-shader-iv get-program-iv
 get-attached-shaders get-shader-info-log get-program-info-log
 get-uniform-location get-active-uniform get-uniformfv get-uniform-iv
 get-shader-source bind-attrib-location get-active-attrib get-attrib-location
 stencil-func-separate stencil-op-separate stencil-mask-separate

 v1f v2f v3f v4f
 v1d v2d v3d v4d
 ))


(in-package #:opengl)

(cffi:load-foreign-library '(:or (:framework "OpenGL")
                                 "opengl32.dll"
                                 (:default "libGL")
                                 (:default "opengl")
                                 (:default "opengl32")
                                 (:default "GL")
                                 (:default "gl")
                                 (:default "libOpenGL")
                                 (:default "OpenGL")))

;; TYPES

(defctype enum :uint32)
(defctype boolean :uint8)
(defctype bitfield :uint32)
(defctype byte :int8)
(defctype short :int16)
(defctype int :int32)
(defctype sizei :int32)
(defctype ubyte :uint8)
(defctype ushort :uint16)
(defctype uint :uint32)
(defctype float :float)
(defctype clampf :float)
(defctype double :double)
(defctype clampd :double)
(defctype void :void)

(defctype intptr :long)
(defctype sizeiptr :long)

(defctype char :char)

; (defcstruct v1f (x :float))
; (defcstruct v2f (x :float) (y :float))
; (defcstruct v3f (x :float) (y :float) (w :float))
; (defcstruct v4f (x :float) (y :float) (z :float) (w :float))
; (defcstruct v1d (x :double))
; (defcstruct v2d (x :double) (y :double))
; (defcstruct v3d (x :double) (y :double) (w :double))
; (defcstruct v4d (x :double) (y :double) (z :double) (w :double))

;; CONSTANTS

; Would  these ever be important? 1.0 only?
;(defconstant +logic-op+ +index-logic-op+)
;(defconstant +texture-components+ +texture-internal-format+)




(defconstant +version-1-1+ 1)
(defconstant +version-1-2+ 1)
(defconstant +version-1-3+ 1)
(defconstant +version-1-4+ 1)
(defconstant +version-1-5+ 1)
(defconstant +version-2-0+ 1)

(defconstant +accum+ #x0100)
(defconstant +load+ #x0101)
(defconstant +return+ #x0102)
(defconstant +mult+ #x0103)
(defconstant +add+ #x0104)

(defconstant +never+ #x0200)
(defconstant +less+ #x0201)
(defconstant +equal+ #x0202)
(defconstant +lequal+ #x0203)
(defconstant +greater+ #x0204)
(defconstant +notequal+ #x0205)
(defconstant +gequal+ #x0206)
(defconstant +always+ #x0207)

(defconstant +current-bit+ #x00000001)
(defconstant +point-bit+ #x00000002)
(defconstant +line-bit+ #x00000004)
(defconstant +polygon-bit+ #x00000008)
(defconstant +polygon-stipple-bit+ #x00000010)
(defconstant +pixel-mode-bit+ #x00000020)
(defconstant +lighting-bit+ #x00000040)
(defconstant +fog-bit+ #x00000080)
(defconstant +depth-buffer-bit+ #x00000100)
(defconstant +accum-buffer-bit+ #x00000200)
(defconstant +stencil-buffer-bit+ #x00000400)
(defconstant +viewport-bit+ #x00000800)
(defconstant +transform-bit+ #x00001000)
(defconstant +enable-bit+ #x00002000)
(defconstant +color-buffer-bit+ #x00004000)
(defconstant +hint-bit+ #x00008000)
(defconstant +eval-bit+ #x00010000)
(defconstant +list-bit+ #x00020000)
(defconstant +texture-bit+ #x00040000)
(defconstant +scissor-bit+ #x00080000)
(defconstant +all-attrib-bits+ #x000fffff)

(defconstant +points+ #x0000)
(defconstant +lines+ #x0001)
(defconstant +line-loop+ #x0002)
(defconstant +line-strip+ #x0003)
(defconstant +triangles+ #x0004)
(defconstant +triangle-strip+ #x0005)
(defconstant +triangle-fan+ #x0006)
(defconstant +quads+ #x0007)
(defconstant +quad-strip+ #x0008)
(defconstant +polygon+ #x0009)


(defconstant +zero+ 0)
(defconstant +one+ 1)
(defconstant +src-color+ #x0300)
(defconstant +one-minus-src-color+ #x0301)
(defconstant +src-alpha+ #x0302)
(defconstant +one-minus-src-alpha+ #x0303)
(defconstant +dst-alpha+ #x0304)
(defconstant +one-minus-dst-alpha+ #x0305)

(defconstant +dst-color+ #x0306)
(defconstant +one-minus-dst-color+ #x0307)
(defconstant +src-alpha-saturate+ #x0308)

(defconstant +true+ 1)
(defconstant +false+ 0)

(defconstant +clip-plane0+ #x3000)
(defconstant +clip-plane1+ #x3001)
(defconstant +clip-plane2+ #x3002)
(defconstant +clip-plane3+ #x3003)
(defconstant +clip-plane4+ #x3004)
(defconstant +clip-plane5+ #x3005)

(defconstant +byte+ #x1400)
(defconstant +unsigned-byte+ #x1401)
(defconstant +short+ #x1402)
(defconstant +unsigned-short+ #x1403)
(defconstant +int+ #x1404)
(defconstant +unsigned-int+ #x1405)
(defconstant +float+ #x1406)
(defconstant +2-bytes+ #x1407)
(defconstant +3-bytes+ #x1408)
(defconstant +4-bytes+ #x1409)
(defconstant +double+ #x140A)

(defconstant +none+ 0)
(defconstant +front-left+ #x0400)
(defconstant +front-right+ #x0401)
(defconstant +back-left+ #x0402)
(defconstant +back-right+ #x0403)
(defconstant +front+ #x0404)
(defconstant +back+ #x0405)
(defconstant +left+ #x0406)
(defconstant +right+ #x0407)
(defconstant +front-and-back+ #x0408)
(defconstant +aux0+ #x0409)
(defconstant +aux1+ #x040A)
(defconstant +aux2+ #x040B)
(defconstant +aux3+ #x040C)

(defconstant +no-error+ 0)
(defconstant +invalid-enum+ #x0500)
(defconstant +invalid-value+ #x0501)
(defconstant +invalid-operation+ #x0502)
(defconstant +stack-overflow+ #x0503)
(defconstant +stack-underflow+ #x0504)
(defconstant +out-of-memory+ #x0505)

(defconstant +2d+ #x0600)
(defconstant +3d+ #x0601)
(defconstant +3d-color+ #x0602)
(defconstant +3d-color-texture+ #x0603)
(defconstant +4d-color-texture+ #x0604)

(defconstant +pass-through-token+ #x0700)
(defconstant +point-token+ #x0701)
(defconstant +line-token+ #x0702)
(defconstant +polygon-token+ #x0703)
(defconstant +bitmap-token+ #x0704)
(defconstant +draw-pixel-token+ #x0705)
(defconstant +copy-pixel-token+ #x0706)
(defconstant +line-reset-token+ #x0707)

(defconstant +exp+ #x0800)
(defconstant +exp2+ #x0801)


(defconstant +cw+ #x0900)
(defconstant +ccw+ #x0901)




(defconstant +coeff+ #x0A00)
(defconstant +order+ #x0A01)
(defconstant +domain+ #x0A02)




(defconstant +current-color+ #x0B00)
(defconstant +current-index+ #x0B01)
(defconstant +current-normal+ #x0B02)
(defconstant +current-texture-coords+ #x0B03)
(defconstant +current-raster-color+ #x0B04)
(defconstant +current-raster-index+ #x0B05)
(defconstant +current-raster-texture-coords+ #x0B06)
(defconstant +current-raster-position+ #x0B07)
(defconstant +current-raster-position-valid+ #x0B08)
(defconstant +current-raster-distance+ #x0B09)
(defconstant +point-smooth+ #x0B10)
(defconstant +point-size+ #x0B11)
(defconstant +point-size-range+ #x0B12)
(defconstant +point-size-granularity+ #x0B13)
(defconstant +line-smooth+ #x0B20)
(defconstant +line-width+ #x0B21)
(defconstant +line-width-range+ #x0B22)
(defconstant +line-width-granularity+ #x0B23)
(defconstant +line-stipple+ #x0B24)
(defconstant +line-stipple-pattern+ #x0B25)
(defconstant +line-stipple-repeat+ #x0B26)
(defconstant +list-mode+ #x0B30)
(defconstant +max-list-nesting+ #x0B31)
(defconstant +list-base+ #x0B32)
(defconstant +list-index+ #x0B33)
(defconstant +polygon-mode+ #x0B40)
(defconstant +polygon-smooth+ #x0B41)
(defconstant +polygon-stipple+ #x0B42)
(defconstant +edge-flag+ #x0B43)
(defconstant +cull-face+ #x0B44)
(defconstant +cull-face-mode+ #x0B45)
(defconstant +front-face+ #x0B46)
(defconstant +lighting+ #x0B50)
(defconstant +light-model-local-viewer+ #x0B51)
(defconstant +light-model-two-side+ #x0B52)
(defconstant +light-model-ambient+ #x0B53)
(defconstant +shade-model+ #x0B54)
(defconstant +color-material-face+ #x0B55)
(defconstant +color-material-parameter+ #x0B56)
(defconstant +color-material+ #x0B57)
(defconstant +fog+ #x0B60)
(defconstant +fog-index+ #x0B61)
(defconstant +fog-density+ #x0B62)
(defconstant +fog-start+ #x0B63)
(defconstant +fog-end+ #x0B64)
(defconstant +fog-mode+ #x0B65)
(defconstant +fog-color+ #x0B66)
(defconstant +depth-range+ #x0B70)
(defconstant +depth-test+ #x0B71)
(defconstant +depth-writemask+ #x0B72)
(defconstant +depth-clear-value+ #x0B73)
(defconstant +depth-func+ #x0B74)
(defconstant +accum-clear-value+ #x0B80)
(defconstant +stencil-test+ #x0B90)
(defconstant +stencil-clear-value+ #x0B91)
(defconstant +stencil-func+ #x0B92)
(defconstant +stencil-value-mask+ #x0B93)
(defconstant +stencil-fail+ #x0B94)
(defconstant +stencil-pass-depth-fail+ #x0B95)
(defconstant +stencil-pass-depth-pass+ #x0B96)
(defconstant +stencil-ref+ #x0B97)
(defconstant +stencil-writemask+ #x0B98)
(defconstant +matrix-mode+ #x0BA0)
(defconstant +normalize+ #x0BA1)
(defconstant +viewport+ #x0BA2)
(defconstant +modelview-stack-depth+ #x0BA3)
(defconstant +projection-stack-depth+ #x0BA4)
(defconstant +texture-stack-depth+ #x0BA5)
(defconstant +modelview-matrix+ #x0BA6)
(defconstant +projection-matrix+ #x0BA7)
(defconstant +texture-matrix+ #x0BA8)
(defconstant +attrib-stack-depth+ #x0BB0)
(defconstant +client-attrib-stack-depth+ #x0BB1)
(defconstant +alpha-test+ #x0BC0)
(defconstant +alpha-test-func+ #x0BC1)
(defconstant +alpha-test-ref+ #x0BC2)
(defconstant +dither+ #x0BD0)
(defconstant +blend-dst+ #x0BE0)
(defconstant +blend-src+ #x0BE1)
(defconstant +blend+ #x0BE2)
(defconstant +logic-op-mode+ #x0BF0)
(defconstant +index-logic-op+ #x0BF1)
(defconstant +color-logic-op+ #x0BF2)
(defconstant +aux-buffers+ #x0C00)
(defconstant +draw-buffer+ #x0C01)
(defconstant +read-buffer+ #x0C02)
(defconstant +scissor-box+ #x0C10)
(defconstant +scissor-test+ #x0C11)
(defconstant +index-clear-value+ #x0C20)
(defconstant +index-writemask+ #x0C21)
(defconstant +color-clear-value+ #x0C22)
(defconstant +color-writemask+ #x0C23)
(defconstant +index-mode+ #x0C30)
(defconstant +rgba-mode+ #x0C31)
(defconstant +doublebuffer+ #x0C32)
(defconstant +stereo+ #x0C33)
(defconstant +render-mode+ #x0C40)
(defconstant +perspective-correction-hint+ #x0C50)
(defconstant +point-smooth-hint+ #x0C51)
(defconstant +line-smooth-hint+ #x0C52)
(defconstant +polygon-smooth-hint+ #x0C53)
(defconstant +fog-hint+ #x0C54)
(defconstant +texture-gen-s+ #x0C60)
(defconstant +texture-gen-t+ #x0C61)
(defconstant +texture-gen-r+ #x0C62)
(defconstant +texture-gen-q+ #x0C63)
(defconstant +pixel-map-i-to-i+ #x0C70)
(defconstant +pixel-map-s-to-s+ #x0C71)
(defconstant +pixel-map-i-to-r+ #x0C72)
(defconstant +pixel-map-i-to-g+ #x0C73)
(defconstant +pixel-map-i-to-b+ #x0C74)
(defconstant +pixel-map-i-to-a+ #x0C75)
(defconstant +pixel-map-r-to-r+ #x0C76)
(defconstant +pixel-map-g-to-g+ #x0C77)
(defconstant +pixel-map-b-to-b+ #x0C78)
(defconstant +pixel-map-a-to-a+ #x0C79)
(defconstant +pixel-map-i-to-i-size+ #x0CB0)
(defconstant +pixel-map-s-to-s-size+ #x0CB1)
(defconstant +pixel-map-i-to-r-size+ #x0CB2)
(defconstant +pixel-map-i-to-g-size+ #x0CB3)
(defconstant +pixel-map-i-to-b-size+ #x0CB4)
(defconstant +pixel-map-i-to-a-size+ #x0CB5)
(defconstant +pixel-map-r-to-r-size+ #x0CB6)
(defconstant +pixel-map-g-to-g-size+ #x0CB7)
(defconstant +pixel-map-b-to-b-size+ #x0CB8)
(defconstant +pixel-map-a-to-a-size+ #x0CB9)
(defconstant +unpack-swap-bytes+ #x0CF0)
(defconstant +unpack-lsb-first+ #x0CF1)
(defconstant +unpack-row-length+ #x0CF2)
(defconstant +unpack-skip-rows+ #x0CF3)
(defconstant +unpack-skip-pixels+ #x0CF4)
(defconstant +unpack-alignment+ #x0CF5)
(defconstant +pack-swap-bytes+ #x0D00)
(defconstant +pack-lsb-first+ #x0D01)
(defconstant +pack-row-length+ #x0D02)
(defconstant +pack-skip-rows+ #x0D03)
(defconstant +pack-skip-pixels+ #x0D04)
(defconstant +pack-alignment+ #x0D05)
(defconstant +map-color+ #x0D10)
(defconstant +map-stencil+ #x0D11)
(defconstant +index-shift+ #x0D12)
(defconstant +index-offset+ #x0D13)
(defconstant +red-scale+ #x0D14)
(defconstant +red-bias+ #x0D15)
(defconstant +zoom-x+ #x0D16)
(defconstant +zoom-y+ #x0D17)
(defconstant +green-scale+ #x0D18)
(defconstant +green-bias+ #x0D19)
(defconstant +blue-scale+ #x0D1A)
(defconstant +blue-bias+ #x0D1B)
(defconstant +alpha-scale+ #x0D1C)
(defconstant +alpha-bias+ #x0D1D)
(defconstant +depth-scale+ #x0D1E)
(defconstant +depth-bias+ #x0D1F)
(defconstant +max-eval-order+ #x0D30)
(defconstant +max-lights+ #x0D31)
(defconstant +max-clip-planes+ #x0D32)
(defconstant +max-texture-size+ #x0D33)
(defconstant +max-pixel-map-table+ #x0D34)
(defconstant +max-attrib-stack-depth+ #x0D35)
(defconstant +max-modelview-stack-depth+ #x0D36)
(defconstant +max-name-stack-depth+ #x0D37)
(defconstant +max-projection-stack-depth+ #x0D38)
(defconstant +max-texture-stack-depth+ #x0D39)
(defconstant +max-viewport-dims+ #x0D3A)
(defconstant +max-client-attrib-stack-depth+ #x0D3B)
(defconstant +subpixel-bits+ #x0D50)
(defconstant +index-bits+ #x0D51)
(defconstant +red-bits+ #x0D52)
(defconstant +green-bits+ #x0D53)
(defconstant +blue-bits+ #x0D54)
(defconstant +alpha-bits+ #x0D55)
(defconstant +depth-bits+ #x0D56)
(defconstant +stencil-bits+ #x0D57)
(defconstant +accum-red-bits+ #x0D58)
(defconstant +accum-green-bits+ #x0D59)
(defconstant +accum-blue-bits+ #x0D5A)
(defconstant +accum-alpha-bits+ #x0D5B)
(defconstant +name-stack-depth+ #x0D70)
(defconstant +auto-normal+ #x0D80)
(defconstant +map1-color-4+ #x0D90)
(defconstant +map1-index+ #x0D91)
(defconstant +map1-normal+ #x0D92)
(defconstant +map1-texture-coord-1+ #x0D93)
(defconstant +map1-texture-coord-2+ #x0D94)
(defconstant +map1-texture-coord-3+ #x0D95)
(defconstant +map1-texture-coord-4+ #x0D96)
(defconstant +map1-vertex-3+ #x0D97)
(defconstant +map1-vertex-4+ #x0D98)
(defconstant +map2-color-4+ #x0DB0)
(defconstant +map2-index+ #x0DB1)
(defconstant +map2-normal+ #x0DB2)
(defconstant +map2-texture-coord-1+ #x0DB3)
(defconstant +map2-texture-coord-2+ #x0DB4)
(defconstant +map2-texture-coord-3+ #x0DB5)
(defconstant +map2-texture-coord-4+ #x0DB6)
(defconstant +map2-vertex-3+ #x0DB7)
(defconstant +map2-vertex-4+ #x0DB8)
(defconstant +map1-grid-domain+ #x0DD0)
(defconstant +map1-grid-segments+ #x0DD1)
(defconstant +map2-grid-domain+ #x0DD2)
(defconstant +map2-grid-segments+ #x0DD3)
(defconstant +texture-1d+ #x0DE0)
(defconstant +texture-2d+ #x0DE1)
(defconstant +feedback-buffer-pointer+ #x0DF0)
(defconstant +feedback-buffer-size+ #x0DF1)
(defconstant +feedback-buffer-type+ #x0DF2)
(defconstant +selection-buffer-pointer+ #x0DF3)
(defconstant +selection-buffer-size+ #x0DF4)

(defconstant +texture-width+ #x1000)
(defconstant +texture-height+ #x1001)
(defconstant +texture-internal-format+ #x1003)
(defconstant +texture-border-color+ #x1004)
(defconstant +texture-border+ #x1005)

(defconstant +dont-care+ #x1100)
(defconstant +fastest+ #x1101)
(defconstant +nicest+ #x1102)






(defconstant +light0+ #x4000)
(defconstant +light1+ #x4001)
(defconstant +light2+ #x4002)
(defconstant +light3+ #x4003)
(defconstant +light4+ #x4004)
(defconstant +light5+ #x4005)
(defconstant +light6+ #x4006)
(defconstant +light7+ #x4007)

(defconstant +ambient+ #x1200)
(defconstant +diffuse+ #x1201)
(defconstant +specular+ #x1202)
(defconstant +position+ #x1203)
(defconstant +spot-direction+ #x1204)
(defconstant +spot-exponent+ #x1205)
(defconstant +spot-cutoff+ #x1206)
(defconstant +constant-attenuation+ #x1207)
(defconstant +linear-attenuation+ #x1208)
(defconstant +quadratic-attenuation+ #x1209)


(defconstant +compile+ #x1300)
(defconstant +compile-and-execute+ #x1301)


(defconstant +clear+ #x1500)
(defconstant +and+ #x1501)
(defconstant +and-reverse+ #x1502)
(defconstant +copy+ #x1503)
(defconstant +and-inverted+ #x1504)
(defconstant +noop+ #x1505)
(defconstant +xor+ #x1506)
(defconstant +or+ #x1507)
(defconstant +nor+ #x1508)
(defconstant +equiv+ #x1509)
(defconstant +invert+ #x150A)
(defconstant +or-reverse+ #x150B)
(defconstant +copy-inverted+ #x150C)
(defconstant +or-inverted+ #x150D)
(defconstant +nand+ #x150E)
(defconstant +set+ #x150F)



(defconstant +emission+ #x1600)
(defconstant +shininess+ #x1601)
(defconstant +ambient-and-diffuse+ #x1602)
(defconstant +color-indexes+ #x1603)

(defconstant +modelview+ #x1700)
(defconstant +projection+ #x1701)
(defconstant +texture+ #x1702)





(defconstant +color+ #x1800)
(defconstant +depth+ #x1801)
(defconstant +stencil+ #x1802)

(defconstant +color-index+ #x1900)
(defconstant +stencil-index+ #x1901)
(defconstant +depth-component+ #x1902)
(defconstant +red+ #x1903)
(defconstant +green+ #x1904)
(defconstant +blue+ #x1905)
(defconstant +alpha+ #x1906)
(defconstant +rgb+ #x1907)
(defconstant +rgba+ #x1908)
(defconstant +luminance+ #x1909)
(defconstant +luminance-alpha+ #x190A)





(defconstant +bitmap+ #x1A00)

(defconstant +point+ #x1B00)
(defconstant +line+ #x1B01)
(defconstant +fill+ #x1B02)


(defconstant +render+ #x1C00)
(defconstant +feedback+ #x1C01)
(defconstant +select+ #x1C02)


(defconstant +flat+ #x1D00)
(defconstant +smooth+ #x1D01)


(defconstant +keep+ #x1E00)
(defconstant +replace+ #x1E01)
(defconstant +incr+ #x1E02)
(defconstant +decr+ #x1E03)

(defconstant +vendor+ #x1F00)
(defconstant +renderer+ #x1F01)
(defconstant +version+ #x1F02)
(defconstant +extensions+ #x1F03)

(defconstant +s+ #x2000)
(defconstant +t+ #x2001)
(defconstant +r+ #x2002)
(defconstant +q+ #x2003)


(defconstant +modulate+ #x2100)
(defconstant +decal+ #x2101)

(defconstant +texture-env-mode+ #x2200)
(defconstant +texture-env-color+ #x2201)

(defconstant +texture-env+ #x2300)

(defconstant +eye-linear+ #x2400)
(defconstant +object-linear+ #x2401)
(defconstant +sphere-map+ #x2402)

(defconstant +texture-gen-mode+ #x2500)
(defconstant +object-plane+ #x2501)
(defconstant +eye-plane+ #x2502)

(defconstant +nearest+ #x2600)
(defconstant +linear+ #x2601)

(defconstant +nearest-mipmap-nearest+ #x2700)
(defconstant +linear-mipmap-nearest+ #x2701)
(defconstant +nearest-mipmap-linear+ #x2702)
(defconstant +linear-mipmap-linear+ #x2703)

(defconstant +texture-mag-filter+ #x2800)
(defconstant +texture-min-filter+ #x2801)
(defconstant +texture-wrap-s+ #x2802)
(defconstant +texture-wrap-t+ #x2803)


(defconstant +clamp+ #x2900)
(defconstant +repeat+ #x2901)


(defconstant +client-pixel-store-bit+ #x00000001)
(defconstant +client-vertex-array-bit+ #x00000002)
(defconstant +client-all-attrib-bits+ #xffffffff)

(defconstant +polygon-offset-factor+ #x8038)
(defconstant +polygon-offset-units+ #x2A00)
(defconstant +polygon-offset-point+ #x2A01)
(defconstant +polygon-offset-line+ #x2A02)
(defconstant +polygon-offset-fill+ #x8037)

(defconstant +alpha4+ #x803B)
(defconstant +alpha8+ #x803C)
(defconstant +alpha12+ #x803D)
(defconstant +alpha16+ #x803E)
(defconstant +luminance4+ #x803F)
(defconstant +luminance8+ #x8040)
(defconstant +luminance12+ #x8041)
(defconstant +luminance16+ #x8042)
(defconstant +luminance4-alpha4+ #x8043)
(defconstant +luminance6-alpha2+ #x8044)
(defconstant +luminance8-alpha8+ #x8045)
(defconstant +luminance12-alpha4+ #x8046)
(defconstant +luminance12-alpha12+ #x8047)
(defconstant +luminance16-alpha16+ #x8048)
(defconstant +intensity+ #x8049)
(defconstant +intensity4+ #x804A)
(defconstant +intensity8+ #x804B)
(defconstant +intensity12+ #x804C)
(defconstant +intensity16+ #x804D)
(defconstant +r3-g3-b2+ #x2A10)
(defconstant +rgb4+ #x804F)
(defconstant +rgb5+ #x8050)
(defconstant +rgb8+ #x8051)
(defconstant +rgb10+ #x8052)
(defconstant +rgb12+ #x8053)
(defconstant +rgb16+ #x8054)
(defconstant +rgba2+ #x8055)
(defconstant +rgba4+ #x8056)
(defconstant +rgb5-a1+ #x8057)
(defconstant +rgba8+ #x8058)
(defconstant +rgb10-a2+ #x8059)
(defconstant +rgba12+ #x805A)
(defconstant +rgba16+ #x805B)
(defconstant +texture-red-size+ #x805C)
(defconstant +texture-green-size+ #x805D)
(defconstant +texture-blue-size+ #x805E)
(defconstant +texture-alpha-size+ #x805F)
(defconstant +texture-luminance-size+ #x8060)
(defconstant +texture-intensity-size+ #x8061)
(defconstant +proxy-texture-1d+ #x8063)
(defconstant +proxy-texture-2d+ #x8064)

(defconstant +texture-priority+ #x8066)
(defconstant +texture-resident+ #x8067)
(defconstant +texture-binding-1d+ #x8068)
(defconstant +texture-binding-2d+ #x8069)
(defconstant +texture-binding-3d+ #x806A)

(defconstant +vertex-array+ #x8074)
(defconstant +normal-array+ #x8075)
(defconstant +color-array+ #x8076)
(defconstant +index-array+ #x8077)
(defconstant +texture-coord-array+ #x8078)
(defconstant +edge-flag-array+ #x8079)
(defconstant +vertex-array-size+ #x807A)
(defconstant +vertex-array-type+ #x807B)
(defconstant +vertex-array-stride+ #x807C)
(defconstant +normal-array-type+ #x807E)
(defconstant +normal-array-stride+ #x807F)
(defconstant +color-array-size+ #x8081)
(defconstant +color-array-type+ #x8082)
(defconstant +color-array-stride+ #x8083)
(defconstant +index-array-type+ #x8085)
(defconstant +index-array-stride+ #x8086)
(defconstant +texture-coord-array-size+ #x8088)
(defconstant +texture-coord-array-type+ #x8089)
(defconstant +texture-coord-array-stride+ #x808A)
(defconstant +edge-flag-array-stride+ #x808C)
(defconstant +vertex-array-pointer+ #x808E)
(defconstant +normal-array-pointer+ #x808F)
(defconstant +color-array-pointer+ #x8090)
(defconstant +index-array-pointer+ #x8091)
(defconstant +texture-coord-array-pointer+ #x8092)
(defconstant +edge-flag-array-pointer+ #x8093)
(defconstant +v2f+ #x2A20)
(defconstant +v3f+ #x2A21)
(defconstant +c4ub-v2f+ #x2A22)
(defconstant +c4ub-v3f+ #x2A23)
(defconstant +c3f-v3f+ #x2A24)
(defconstant +n3f-v3f+ #x2A25)
(defconstant +c4f-n3f-v3f+ #x2A26)
(defconstant +t2f-v3f+ #x2A27)
(defconstant +t4f-v4f+ #x2A28)
(defconstant +t2f-c4ub-v3f+ #x2A29)
(defconstant +t2f-c3f-v3f+ #x2A2A)
(defconstant +t2f-n3f-v3f+ #x2A2B)
(defconstant +t2f-c4f-n3f-v3f+ #x2A2C)
(defconstant +t4f-c4f-n3f-v4f+ #x2A2D)

(defconstant +bgr+ #x80E0)
(defconstant +bgra+ #x80E1)

(defconstant +constant-color+ #x8001)
(defconstant +one-minus-constant-color+ #x8002)
(defconstant +constant-alpha+ #x8003)
(defconstant +one-minus-constant-alpha+ #x8004)
(defconstant +blend-color+ #x8005)

(defconstant +func-add+ #x8006)
(defconstant +min+ #x8007)
(defconstant +max+ #x8008)
(defconstant +blend-equation+ #x8009)

(defconstant +blend-equation-rgb+ #x8009)
(defconstant +blend-equation-alpha+ #x883D)

(defconstant +func-subtract+ #x800A)
(defconstant +func-reverse-subtract+ #x800B)

(defconstant +color-matrix+ #x80B1)
(defconstant +color-matrix-stack-depth+ #x80B2)
(defconstant +max-color-matrix-stack-depth+ #x80B3)
(defconstant +post-color-matrix-red-scale+ #x80B4)
(defconstant +post-color-matrix-green-scale+ #x80B5)
(defconstant +post-color-matrix-blue-scale+ #x80B6)
(defconstant +post-color-matrix-alpha-scale+ #x80B7)
(defconstant +post-color-matrix-red-bias+ #x80B8)
(defconstant +post-color-matrix-green-bias+ #x80B9)
(defconstant +post-color-matrix-blue-bias+ #x80BA)
(defconstant +post-color-matrix-alpha-bias+ #x80BB)

(defconstant +color-table+ #x80D0)
(defconstant +post-convolution-color-table+ #x80D1)
(defconstant +post-color-matrix-color-table+ #x80D2)
(defconstant +proxy-color-table+ #x80D3)
(defconstant +proxy-post-convolution-color-table+ #x80D4)
(defconstant +proxy-post-color-matrix-color-table+ #x80D5)
(defconstant +color-table-scale+ #x80D6)
(defconstant +color-table-bias+ #x80D7)
(defconstant +color-table-format+ #x80D8)
(defconstant +color-table-width+ #x80D9)
(defconstant +color-table-red-size+ #x80DA)
(defconstant +color-table-green-size+ #x80DB)
(defconstant +color-table-blue-size+ #x80DC)
(defconstant +color-table-alpha-size+ #x80DD)
(defconstant +color-table-luminance-size+ #x80DE)
(defconstant +color-table-intensity-size+ #x80DF)

(defconstant +convolution-1d+ #x8010)
(defconstant +convolution-2d+ #x8011)
(defconstant +separable-2d+ #x8012)
(defconstant +convolution-border-mode+ #x8013)
(defconstant +convolution-filter-scale+ #x8014)
(defconstant +convolution-filter-bias+ #x8015)
(defconstant +reduce+ #x8016)
(defconstant +convolution-format+ #x8017)
(defconstant +convolution-width+ #x8018)
(defconstant +convolution-height+ #x8019)
(defconstant +max-convolution-width+ #x801A)
(defconstant +max-convolution-height+ #x801B)
(defconstant +post-convolution-red-scale+ #x801C)
(defconstant +post-convolution-green-scale+ #x801D)
(defconstant +post-convolution-blue-scale+ #x801E)
(defconstant +post-convolution-alpha-scale+ #x801F)
(defconstant +post-convolution-red-bias+ #x8020)
(defconstant +post-convolution-green-bias+ #x8021)
(defconstant +post-convolution-blue-bias+ #x8022)
(defconstant +post-convolution-alpha-bias+ #x8023)
(defconstant +constant-border+ #x8151)
(defconstant +replicate-border+ #x8153)
(defconstant +convolution-border-color+ #x8154)

(defconstant +max-elements-vertices+ #x80E8)
(defconstant +max-elements-indices+ #x80E9)

(defconstant +histogram+ #x8024)
(defconstant +proxy-histogram+ #x8025)
(defconstant +histogram-width+ #x8026)
(defconstant +histogram-format+ #x8027)
(defconstant +histogram-red-size+ #x8028)
(defconstant +histogram-green-size+ #x8029)
(defconstant +histogram-blue-size+ #x802A)
(defconstant +histogram-alpha-size+ #x802B)
(defconstant +histogram-luminance-size+ #x802C)
(defconstant +histogram-sink+ #x802D)
(defconstant +minmax+ #x802E)
(defconstant +minmax-format+ #x802F)
(defconstant +minmax-sink+ #x8030)
(defconstant +table-too-large+ #x8031)

(defconstant +unsigned-byte-3-3-2+ #x8032)
(defconstant +unsigned-short-4-4-4-4+ #x8033)
(defconstant +unsigned-short-5-5-5-1+ #x8034)
(defconstant +unsigned-int-8-8-8-8+ #x8035)
(defconstant +unsigned-int-10-10-10-2+ #x8036)
(defconstant +unsigned-byte-2-3-3-rev+ #x8362)
(defconstant +unsigned-short-5-6-5+ #x8363)
(defconstant +unsigned-short-5-6-5-rev+ #x8364)
(defconstant +unsigned-short-4-4-4-4-rev+ #x8365)
(defconstant +unsigned-short-1-5-5-5-rev+ #x8366)
(defconstant +unsigned-int-8-8-8-8-rev+ #x8367)
(defconstant +unsigned-int-2-10-10-10-rev+ #x8368)

(defconstant +rescale-normal+ #x803A)

(defconstant +light-model-color-control+ #x81F8)
(defconstant +single-color+ #x81F9)
(defconstant +separate-specular-color+ #x81FA)

(defconstant +pack-skip-images+ #x806B)
(defconstant +pack-image-height+ #x806C)
(defconstant +unpack-skip-images+ #x806D)
(defconstant +unpack-image-height+ #x806E)
(defconstant +texture-3d+ #x806F)
(defconstant +proxy-texture-3d+ #x8070)
(defconstant +texture-depth+ #x8071)
(defconstant +texture-wrap-r+ #x8072)
(defconstant +max-3d-texture-size+ #x8073)

(defconstant +clamp-to-edge+ #x812F)
(defconstant +clamp-to-border+ #x812D)

(defconstant +texture-min-lod+ #x813A)
(defconstant +texture-max-lod+ #x813B)
(defconstant +texture-base-level+ #x813C)
(defconstant +texture-max-level+ #x813D)

(defconstant +smooth-point-size-range+ #x0B12)
(defconstant +smooth-point-size-granularity+ #x0B13)
(defconstant +smooth-line-width-range+ #x0B22)
(defconstant +smooth-line-width-granularity+ #x0B23)
(defconstant +aliased-point-size-range+ #x846D)
(defconstant +aliased-line-width-range+ #x846E)

(defconstant +texture0+ #x84C0)
(defconstant +texture1+ #x84C1)
(defconstant +texture2+ #x84C2)
(defconstant +texture3+ #x84C3)
(defconstant +texture4+ #x84C4)
(defconstant +texture5+ #x84C5)
(defconstant +texture6+ #x84C6)
(defconstant +texture7+ #x84C7)
(defconstant +texture8+ #x84C8)
(defconstant +texture9+ #x84C9)
(defconstant +texture10+ #x84CA)
(defconstant +texture11+ #x84CB)
(defconstant +texture12+ #x84CC)
(defconstant +texture13+ #x84CD)
(defconstant +texture14+ #x84CE)
(defconstant +texture15+ #x84CF)
(defconstant +texture16+ #x84D0)
(defconstant +texture17+ #x84D1)
(defconstant +texture18+ #x84D2)
(defconstant +texture19+ #x84D3)
(defconstant +texture20+ #x84D4)
(defconstant +texture21+ #x84D5)
(defconstant +texture22+ #x84D6)
(defconstant +texture23+ #x84D7)
(defconstant +texture24+ #x84D8)
(defconstant +texture25+ #x84D9)
(defconstant +texture26+ #x84DA)
(defconstant +texture27+ #x84DB)
(defconstant +texture28+ #x84DC)
(defconstant +texture29+ #x84DD)
(defconstant +texture30+ #x84DE)
(defconstant +texture31+ #x84DF)
(defconstant +active-texture+ #x84E0)
(defconstant +client-active-texture+ #x84E1)
(defconstant +max-texture-units+ #x84E2)

(defconstant +combine+ #x8570)
(defconstant +combine-rgb+ #x8571)
(defconstant +combine-alpha+ #x8572)
(defconstant +rgb-scale+ #x8573)
(defconstant +add-signed+ #x8574)
(defconstant +interpolate+ #x8575)
(defconstant +constant+ #x8576)
(defconstant +primary-color+ #x8577)
(defconstant +previous+ #x8578)
(defconstant +subtract+ #x84E7)

(defconstant +src0-rgb+ #x8580)
(defconstant +src1-rgb+ #x8581)
(defconstant +src2-rgb+ #x8582)
(defconstant +src3-rgb+ #x8583)
(defconstant +src4-rgb+ #x8584)
(defconstant +src5-rgb+ #x8585)
(defconstant +src6-rgb+ #x8586)
(defconstant +src7-rgb+ #x8587)
(defconstant +src0-alpha+ #x8588)
(defconstant +src1-alpha+ #x8589)
(defconstant +src2-alpha+ #x858A)
(defconstant +src3-alpha+ #x858B)
(defconstant +src4-alpha+ #x858C)
(defconstant +src5-alpha+ #x858D)
(defconstant +src6-alpha+ #x858E)
(defconstant +src7-alpha+ #x858F)

(defconstant +source0-rgb+ #x8580)
(defconstant +source1-rgb+ #x8581)
(defconstant +source2-rgb+ #x8582)
(defconstant +source3-rgb+ #x8583)
(defconstant +source4-rgb+ #x8584)
(defconstant +source5-rgb+ #x8585)
(defconstant +source6-rgb+ #x8586)
(defconstant +source7-rgb+ #x8587)
(defconstant +source0-alpha+ #x8588)
(defconstant +source1-alpha+ #x8589)
(defconstant +source2-alpha+ #x858A)
(defconstant +source3-alpha+ #x858B)
(defconstant +source4-alpha+ #x858C)
(defconstant +source5-alpha+ #x858D)
(defconstant +source6-alpha+ #x858E)
(defconstant +source7-alpha+ #x858F)

(defconstant +operand0-rgb+ #x8590)
(defconstant +operand1-rgb+ #x8591)
(defconstant +operand2-rgb+ #x8592)
(defconstant +operand3-rgb+ #x8593)
(defconstant +operand4-rgb+ #x8594)
(defconstant +operand5-rgb+ #x8595)
(defconstant +operand6-rgb+ #x8596)
(defconstant +operand7-rgb+ #x8597)
(defconstant +operand0-alpha+ #x8598)
(defconstant +operand1-alpha+ #x8599)
(defconstant +operand2-alpha+ #x859A)
(defconstant +operand3-alpha+ #x859B)
(defconstant +operand4-alpha+ #x859C)
(defconstant +operand5-alpha+ #x859D)
(defconstant +operand6-alpha+ #x859E)
(defconstant +operand7-alpha+ #x859F)

(defconstant +dot3-rgb+ #x86AE)
(defconstant +dot3-rgba+ #x86AF)

(defconstant +transpose-modelview-matrix+ #x84E3)
(defconstant +transpose-projection-matrix+ #x84E4)
(defconstant +transpose-texture-matrix+ #x84E5)
(defconstant +transpose-color-matrix+ #x84E6)

(defconstant +normal-map+ #x8511)
(defconstant +reflection-map+ #x8512)
(defconstant +texture-cube-map+ #x8513)
(defconstant +texture-binding-cube-map+ #x8514)
(defconstant +texture-cube-map-positive-x+ #x8515)
(defconstant +texture-cube-map-negative-x+ #x8516)
(defconstant +texture-cube-map-positive-y+ #x8517)
(defconstant +texture-cube-map-negative-y+ #x8518)
(defconstant +texture-cube-map-positive-z+ #x8519)
(defconstant +texture-cube-map-negative-z+ #x851A)
(defconstant +proxy-texture-cube-map+ #x851B)
(defconstant +max-cube-map-texture-size+ #x851C)

(defconstant +compressed-alpha+ #x84E9)
(defconstant +compressed-luminance+ #x84EA)
(defconstant +compressed-luminance-alpha+ #x84EB)
(defconstant +compressed-intensity+ #x84EC)
(defconstant +compressed-rgb+ #x84ED)
(defconstant +compressed-rgba+ #x84EE)
(defconstant +texture-compression-hint+ #x84EF)
(defconstant +texture-compressed-image-size+ #x86A0)
(defconstant +texture-compressed+ #x86A1)
(defconstant +num-compressed-texture-formats+ #x86A2)
(defconstant +compressed-texture-formats+ #x86A3)

(defconstant +multisample+ #x809D)
(defconstant +sample-alpha-to-coverage+ #x809E)
(defconstant +sample-alpha-to-one+ #x809F)
(defconstant +sample-coverage+ #x80A0)
(defconstant +sample-buffers+ #x80A8)
(defconstant +samples+ #x80A9)
(defconstant +sample-coverage-value+ #x80AA)
(defconstant +sample-coverage-invert+ #x80AB)
(defconstant +multisample-bit+ #x20000000)

(defconstant +depth-component16+ #x81A5)
(defconstant +depth-component24+ #x81A6)
(defconstant +depth-component32+ #x81A7)
(defconstant +texture-depth-size+ #x884A)
(defconstant +depth-texture-mode+ #x884B)

(defconstant +texture-compare-mode+ #x884C)
(defconstant +texture-compare-func+ #x884D)
(defconstant +compare-r-to-texture+ #x884E)

(defconstant +query-counter-bits+ #x8864)
(defconstant +current-query+ #x8865)
(defconstant +query-result+ #x8866)
(defconstant +query-result-available+ #x8867)
(defconstant +samples-passed+ #x8914)

(defconstant +fog-coord-src+ #x8450)
(defconstant +fog-coord+ #x8451)
(defconstant +fragment-depth+ #x8452)
(defconstant +current-fog-coord+ #x8453)  
(defconstant +fog-coord-array-type+ #x8454)
(defconstant +fog-coord-array-stride+ #x8455)
(defconstant +fog-coord-array-pointer+ #x8456)
(defconstant +fog-coord-array+ #x8457)

(defconstant +fog-coordinate-source+ #x8450)
(defconstant +fog-coordinate+ #x8451)
(defconstant +current-fog-coordinate+ #x8453)  
(defconstant +fog-coordinate-array-type+ #x8454)
(defconstant +fog-coordinate-array-stride+ #x8455)
(defconstant +fog-coordinate-array-pointer+ #x8456)
(defconstant +fog-coordinate-array+ #x8457)

(defconstant +color-sum+ #x8458)
(defconstant +current-secondary-color+ #x8459)
(defconstant +secondary-color-array-size+ #x845A)
(defconstant +secondary-color-array-type+ #x845B)
(defconstant +secondary-color-array-stride+ #x845C)
(defconstant +secondary-color-array-pointer+ #x845D)
(defconstant +secondary-color-array+ #x845E)

(defconstant +point-size-min+ #x8126)
(defconstant +point-size-max+ #x8127)
(defconstant +point-fade-threshold-size+ #x8128)
(defconstant +point-distance-attenuation+ #x8129)

(defconstant +blend-dst-rgb+ #x80C8)
(defconstant +blend-src-rgb+ #x80C9)
(defconstant +blend-dst-alpha+ #x80CA)
(defconstant +blend-src-alpha+ #x80CB)

(defconstant +generate-mipmap+ #x8191)
(defconstant +generate-mipmap-hint+ #x8192)

(defconstant +incr-wrap+ #x8507)
(defconstant +decr-wrap+ #x8508)

(defconstant +mirrored-repeat+ #x8370)

(defconstant +max-texture-lod-bias+ #x84FD)
(defconstant +texture-filter-control+ #x8500)
(defconstant +texture-lod-bias+ #x8501)

(defconstant +array-buffer+ #x8892)
(defconstant +element-array-buffer+ #x8893)
(defconstant +array-buffer-binding+ #x8894)
(defconstant +element-array-buffer-binding+ #x8895)
(defconstant +vertex-array-buffer-binding+ #x8896)
(defconstant +normal-array-buffer-binding+ #x8897)
(defconstant +color-array-buffer-binding+ #x8898)
(defconstant +index-array-buffer-binding+ #x8899)
(defconstant +texture-coord-array-buffer-binding+ #x889A)
(defconstant +edge-flag-array-buffer-binding+ #x889B)
(defconstant +secondary-color-array-buffer-binding+ #x889C)
(defconstant +fog-coord-array-buffer-binding+ #x889D)
(defconstant +weight-array-buffer-binding+ #x889E)
(defconstant +vertex-attrib-array-buffer-binding+ #x889F)
(defconstant +stream-draw+ #x88E0)
(defconstant +stream-read+ #x88E1)
(defconstant +stream-copy+ #x88E2)
(defconstant +static-draw+ #x88E4)
(defconstant +static-read+ #x88E5)
(defconstant +static-copy+ #x88E6)
(defconstant +dynamic-draw+ #x88E8)
(defconstant +dynamic-read+ #x88E9)
(defconstant +dynamic-copy+ #x88EA)
(defconstant +read-only+ #x88B8)
(defconstant +write-only+ #x88B9)
(defconstant +read-write+ #x88BA)
(defconstant +buffer-size+ #x8764)
(defconstant +buffer-usage+ #x8765)
(defconstant +buffer-access+ #x88BB)
(defconstant +buffer-mapped+ #x88BC)
(defconstant +buffer-map-pointer+ #x88BD)
(defconstant +fog-coordinate-array-buffer-binding+ #x889D)

(defconstant +current-program+ #x8B8D)
(defconstant +shader-type+ #x8B4F)
(defconstant +delete-status+ #x8B80)
(defconstant +compile-status+ #x8B81)
(defconstant +link-status+ #x8B82)
(defconstant +validate-status+ #x8B83)
(defconstant +info-log-length+ #x8B84)
(defconstant +attached-shaders+ #x8B85)
(defconstant +active-uniforms+ #x8B86)
(defconstant +active-uniform-max-length+ #x8B87)
(defconstant +shader-source-length+ #x8B88)
(defconstant +float-vec2+ #x8B50)
(defconstant +float-vec3+ #x8B51)
(defconstant +float-vec4+ #x8B52)
(defconstant +int-vec2+ #x8B53)
(defconstant +int-vec3+ #x8B54)
(defconstant +int-vec4+ #x8B55)
(defconstant +bool+ #x8B56)
(defconstant +bool-vec2+ #x8B57)
(defconstant +bool-vec3+ #x8B58)
(defconstant +bool-vec4+ #x8B59)
(defconstant +float-mat2+ #x8B5A)
(defconstant +float-mat3+ #x8B5B)
(defconstant +float-mat4+ #x8B5C)
(defconstant +sampler-1d+ #x8B5D)
(defconstant +sampler-2d+ #x8B5E)
(defconstant +sampler-3d+ #x8B5F)
(defconstant +sampler-cube+ #x8B60)
(defconstant +sampler-1d-shadow+ #x8B61)
(defconstant +sampler-2d-shadow+ #x8B62)
(defconstant +shading-language-version+ #x8B8C)
(defconstant +vertex-shader+ #x8B31)
(defconstant +max-vertex-uniform-components+ #x8B4A)
(defconstant +max-varying-floats+ #x8B4B)
(defconstant +max-vertex-texture-image-units+ #x8B4C)
(defconstant +max-combined-texture-image-units+ #x8B4D)
(defconstant +active-attributes+ #x8B89)
(defconstant +active-attribute-max-length+ #x8B8A)
(defconstant +fragment-shader+ #x8B30)
(defconstant +max-fragment-uniform-components+ #x8B49)
(defconstant +fragment-shader-derivative-hint+ #x8B8B)
(defconstant +max-vertex-attribs+ #x8869)
(defconstant +vertex-attrib-array-enabled+ #x8622)
(defconstant +vertex-attrib-array-size+ #x8623)
(defconstant +vertex-attrib-array-stride+ #x8624)
(defconstant +vertex-attrib-array-type+ #x8625)
(defconstant +vertex-attrib-array-normalized+ #x886A)
(defconstant +current-vertex-attrib+ #x8626)
(defconstant +vertex-attrib-array-pointer+ #x8645)
(defconstant +vertex-program-point-size+ #x8642)
(defconstant +vertex-program-two-side+ #x8643)
(defconstant +max-texture-coords+ #x8871)
(defconstant +max-texture-image-units+ #x8872)
(defconstant +max-draw-buffers+ #x8824)
(defconstant +draw-buffer0+ #x8825)
(defconstant +draw-buffer1+ #x8826)
(defconstant +draw-buffer2+ #x8827)
(defconstant +draw-buffer3+ #x8828)
(defconstant +draw-buffer4+ #x8829)
(defconstant +draw-buffer5+ #x882A)
(defconstant +draw-buffer6+ #x882B)
(defconstant +draw-buffer7+ #x882C)
(defconstant +draw-buffer8+ #x882D)
(defconstant +draw-buffer9+ #x882E)
(defconstant +draw-buffer10+ #x882F)
(defconstant +draw-buffer11+ #x8830)
(defconstant +draw-buffer12+ #x8831)
(defconstant +draw-buffer13+ #x8832)
(defconstant +draw-buffer14+ #x8833)
(defconstant +draw-buffer15+ #x8834)
(defconstant +point-sprite+ #x8861)
(defconstant +coord-replace+ #x8862)
(defconstant +point-sprite-coord-origin+ #x8CA0)
(defconstant +lower-left+ #x8CA1)
(defconstant +upper-left+ #x8CA2)
(defconstant +stencil-back-func+ #x8800)
(defconstant +stencil-back-value-mask+ #x8CA4)
(defconstant +stencil-back-ref+ #x8CA3)
(defconstant +stencil-back-fail+ #x8801)
(defconstant +stencil-back-pass-depth-fail+ #x8802)
(defconstant +stencil-back-pass-depth-pass+ #x8803)
(defconstant +stencil-back-writemask+ #x8CA5)

;; FUNCTIONS

;(defcfun ("glLockArrays" lock-arrays) :void (first gl:int) (count gl:sizei))
;(defcfun ("glUnlockArrays" unlock-arrays) :void)

(defcfun ("glAccum"                    accum) :void  (op enum) (value float ))
(defcfun ("glAlphaFunc"                alpha-func) :void  (func enum) (ref clampf ))
(defcfun ("glAreTexturesResident"      are-textures-resident) boolean  (n sizei) (textures :pointer) (residences :pointer))
(defcfun ("glArrayElement"             array-element) :void  (i int ))
(defcfun ("glBegin"                    begin) :void  (mode enum ))
(defcfun ("glBindTexture"              bind-texture) :void  (target enum) (texture uint ))
(defcfun ("glBitmap"                   bitmap) :void  (width sizei) (height sizei) (xorig float) (yorig float) (xmove float) (ymove float) (bitmap :pointer))
(defcfun ("glBlendColor"               blend-color) :void  (red clampf) (green clampf) (blue clampf) (alpha clampf ))
(defcfun ("glBlendEquation"            blend-equation) :void  (mode enum ))
(defcfun ("glBlendEquationSeparate"    blend-equation-separate) :void (modeRGB enum) (modeAlpha enum ))
(defcfun ("glBlendFunc"                blend-func) :void  (sfactor enum) (dfactor enum ))
(defcfun ("glCallList"                 call-list) :void  (list uint ))
(defcfun ("glCallLists"                call-lists) :void  (n sizei) (type enum) (lists :pointer ))
(defcfun ("glClear"                    clear) :void  (mask bitfield ))
(defcfun ("glClearAccum"               clear-accum) :void  (red float) (green float) (blue float) (alpha float ))
(defcfun ("glClearColor"               clear-color) :void  (red clampf) (green clampf) (blue clampf) (alpha clampf ))
(defcfun ("glClearDepth"               clear-depth) :void  (depth clampd ))
(defcfun ("glClearIndex"               clear-index) :void  (c float ))
(defcfun ("glClearStencil"             clear-stencil) :void  (s int ))
(defcfun ("glClipPlane"                clip-plane) :void  (plane enum) (equation :pointer))
(defcfun ("glColor3b"                  color-3b) :void  (red byte) (green byte) (blue byte ))
(defcfun ("glColor3bv"                 color-3bv) :void  (v :pointer))
(defcfun ("glColor3d"                  color-3d) :void  (red double) (green double) (blue double ))
(defcfun ("glColor3dv"                 color-3dv) :void  (v :pointer))
(defcfun ("glColor3f"                  color-3f) :void  (red float) (green float) (blue float ))
(defcfun ("glColor3fv"                 color-3fv) :void  (v :pointer))
(defcfun ("glColor3i"                  color-3i) :void  (red int) (green int) (blue int ))
(defcfun ("glColor3iv"                 color-3iv) :void  (v :pointer))
(defcfun ("glColor3s"                  color-3s) :void  (red short) (green short) (blue short ))
(defcfun ("glColor3sv"                 color-3sv) :void  (v :pointer))
(defcfun ("glColor3ub"                 color-3ub) :void  (red ubyte) (green ubyte) (blue ubyte ))
(defcfun ("glColor3ubv"                color-3ubv) :void  (v :pointer))
(defcfun ("glColor3ui"                 color-3ui) :void  (red uint) (green uint) (blue uint ))
(defcfun ("glColor3uiv"                color-3uiv) :void  (v :pointer))
(defcfun ("glColor3us"                 color-3us) :void  (red ushort) (green ushort) (blue ushort ))
(defcfun ("glColor3usv"                color-3usv) :void  (v :pointer))
(defcfun ("glColor4b"                  color-4b) :void  (red byte) (green byte) (blue byte) (alpha byte ))
(defcfun ("glColor4bv"                 color-4bv) :void  (v :pointer))
(defcfun ("glColor4d"                  color-4d) :void  (red double) (green double) (blue double) (alpha double ))
(defcfun ("glColor4dv"                 color-4dv) :void  (v :pointer))
(defcfun ("glColor4f"                  color-4f) :void  (red float) (green float) (blue float) (alpha float ))
(defcfun ("glColor4fv"                 color-4fv) :void  (v :pointer))
(defcfun ("glColor4i"                  color-4i) :void  (red int) (green int) (blue int) (alpha int ))
(defcfun ("glColor4iv"                 color-4iv) :void  (v :pointer))
(defcfun ("glColor4s"                  color-4s) :void  (red short) (green short) (blue short) (alpha short ))
(defcfun ("glColor4sv"                 color-4sv) :void  (v :pointer))
(defcfun ("glColor4ub"                 color-4ub) :void  (red ubyte) (green ubyte) (blue ubyte) (alpha ubyte ))
(defcfun ("glColor4ubv"                color-4ubv) :void  (v :pointer))
(defcfun ("glColor4ui"                 color-4ui) :void  (red uint) (green uint) (blue uint) (alpha uint ))
(defcfun ("glColor4uiv"                color-4uiv) :void  (v :pointer))
(defcfun ("glColor4us"                 color-4us) :void  (red ushort) (green ushort) (blue ushort) (alpha ushort ))
(defcfun ("glColor4usv"                color-4usv) :void  (v :pointer))
(defcfun ("glColorMask"                color-mask) :void  (red boolean) (green boolean) (blue boolean) (alpha boolean ))
(defcfun ("glColorMaterial"            color-material) :void  (face enum) (mode enum ))
(defcfun ("glColorPointer"             color-pointer) :void  (size int) (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glColorSubTable"            color-sub-table) :void  (target enum) (start sizei) (count sizei) (format enum) (type enum) (data :pointer))
(defcfun ("glColorTable"               color-table) :void  (target enum) (internalformat enum) (width sizei) (format enum) (type enum) (table :pointer))
(defcfun ("glColorTableParameterfv"    color-table-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glColorTableParameteriv"    color-table-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glConvolutionFilter1D"      convolution-filter1d) :void  (target enum) (internalformat enum) (width sizei) (format enum) (type enum) (image :pointer))
(defcfun ("glConvolutionFilter2D"      convolution-filter2d) :void  (target enum) (internalformat enum) (width sizei) (height sizei) (format enum) (type enum) (image :pointer))
(defcfun ("glConvolutionParameterf"    convolution-parameter-f) :void  (target enum) (pname enum) (params float ))
(defcfun ("glConvolutionParameterfv"   convolution-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glConvolutionParameteri"    convolution-parameter-i) :void  (target enum) (pname enum) (params int ))
(defcfun ("glConvolutionParameteriv"   convolution-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glCopyColorSubTable"        copy-color-sub-table) :void  (target enum) (start sizei) (x int) (y int) (width sizei ))
(defcfun ("glCopyColorTable"           copy-color-table) :void  (target enum) (internalformat enum) (x int) (y int) (width sizei ))
(defcfun ("glCopyConvolutionFilter1D"  copy-convolution-filter-1d) :void  (target enum) (internalformat enum) (x int) (y int) (width sizei ))
(defcfun ("glCopyConvolutionFilter2D"  copy-convolution-filter-2d) :void  (target enum) (internalformat enum) (x int) (y int) (width sizei) (height sizei ))
(defcfun ("glCopyPixels"               copy-pixels) :void  (x int) (y int) (width sizei) (height sizei) (type enum ))
(defcfun ("glCopyTexImage1D"           copy-tex-image-1d) :void  (target enum) (level int) (internalformat enum) (x int) (y int) (width sizei) (border int ))
(defcfun ("glCopyTexImage2D"           copy-tex-image-2d) :void  (target enum) (level int) (internalformat enum) (x int) (y int) (width sizei) (height sizei) (border int ))
(defcfun ("glCopyTexSubImage1D"        copy-tex-sub-image-1d) :void  (target enum) (level int) (xoffset int) (x int) (y int) (width sizei ))
(defcfun ("glCopyTexSubImage2D"        copy-tex-sub-image-2d) :void  (target enum) (level int) (xoffset int) (yoffset int) (x int) (y int) (width sizei) (height sizei ))
(defcfun ("glCopyTexSubImage3D"        copy-tex-sub-image-3d) :void  (target enum) (level int) (xoffset int) (yoffset int) (zoffset int) (x int) (y int) (width sizei) (height sizei ))
(defcfun ("glCullFace"                 cull-face) :void  (mode enum ))
(defcfun ("glDeleteLists"              delete-lists) :void  (list uint) (range sizei ))
(defcfun ("glDeleteTextures"           delete-textures) :void  (n sizei) (textures :pointer))
(defcfun ("glDepthFunc"                depth-func) :void  (func enum ))
(defcfun ("glDepthMask"                depth-mask) :void  (flag boolean ))
(defcfun ("glDepthRange"               depth-range) :void  (zNear clampd) (zFar clampd ))
(defcfun ("glDisable"                  disable) :void  (cap enum ))
(defcfun ("glDisableClientState"       disable-client-state) :void  (array enum ))
(defcfun ("glDrawArrays"               draw-arrays) :void  (mode enum) (first int) (count sizei ))
(defcfun ("glDrawBuffer"               draw-buffer) :void  (mode enum ))
(defcfun ("glDrawElements"             draw-elements) :void  (mode enum) (count sizei) (type enum) (indices :pointer))
(defcfun ("glDrawPixels"               draw-pixels) :void  (width sizei) (height sizei) (format enum) (type enum) (pixels :pointer))
(defcfun ("glDrawRangeElements"        draw-range-elements) :void  (mode enum) (start uint) (end uint) (count sizei) (type enum) (indices :pointer))
(defcfun ("glEdgeFlag"                 edge-flag) :void  (flag boolean ))
(defcfun ("glEdgeFlagPointer"          edge-flag-pointer) :void  (stride sizei) (pointer :pointer))
(defcfun ("glEdgeFlagv"                edge-flagv) :void  (flag :pointer))
(defcfun ("glEnable"                   enable) :void  (cap enum ))
(defcfun ("glEnableClientState"        enable-client-state) :void  (array enum ))
(defcfun ("glEnd"                      end) :void)
(defcfun ("glEndList"                  end-list) :void)
(defcfun ("glEvalCoord1d"              eval-coord-1d) :void  (u double ))
(defcfun ("glEvalCoord1dv"             eval-coord-1dv) :void  (u :pointer))
(defcfun ("glEvalCoord1f"              eval-coord-1f) :void  (u float ))
(defcfun ("glEvalCoord1fv"             eval-coord-1fv) :void  (u :pointer))
(defcfun ("glEvalCoord2d"              eval-coord-2d) :void  (u double) (v double ))
(defcfun ("glEvalCoord2dv"             eval-coord-2dv) :void  (u :pointer))
(defcfun ("glEvalCoord2f"              eval-coord-2f) :void  (u float) (v float ))
(defcfun ("glEvalCoord2fv"             eval-coord-2fv) :void  (u :pointer))
(defcfun ("glEvalMesh1"                eval-mesh-1) :void  (mode enum) (i1 int) (i2 int ))
(defcfun ("glEvalMesh2"                eval-mesh-2) :void  (mode enum) (i1 int) (i2 int) (j1 int) (j2 int ))
(defcfun ("glEvalPoint1"               eval-point-1) :void  (i int ))
(defcfun ("glEvalPoint2"               eval-point-2) :void  (i int) (j int ))
(defcfun ("glFeedbackBuffer"           feedback-buffer) :void  (size sizei) (type enum) (buffer :pointer))
(defcfun ("glFinish"                   finish) :void)
(defcfun ("glFlush"                    flush) :void)
(defcfun ("glFogf"                     fog-f) :void  (pname enum) (param float ))
(defcfun ("glFogfv"                    fog-fv) :void  (pname enum) (params :pointer))
(defcfun ("glFogi"                     fog-i) :void  (pname enum) (param int ))
(defcfun ("glFogiv"                    fog-iv) :void  (pname enum) (params :pointer))
(defcfun ("glFrontFace"                front-face) :void  (mode enum ))
(defcfun ("glFrustum"                  frustum) :void  (left double) (right double) (bottom double) (top double) (zNear double) (zFar double ))
(defcfun ("glGenLists"                 gen-lists) uint  (range sizei ))
(defcfun ("glGenTextures"              gen-textures) :void  (n sizei) (textures :pointer))
(defcfun ("glGetBooleanv"              get-booleanv) :void  (pname enum) (params :pointer))
(defcfun ("glGetClipPlane"             get-clip-plane) :void  (plane enum) (equation :pointer))
(defcfun ("glGetColorTable"            get-color-table) :void  (target enum) (format enum) (type enum) (table :pointer))
(defcfun ("glGetColorTableParameterfv" get-color-table-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetColorTableParameteriv" get-color-table-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetConvolutionFilter"     get-convolution-filter) :void  (target enum) (format enum) (type enum) (image :pointer))
(defcfun ("glGetConvolutionParameterfv"get-convolution-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetConvolutionParameteriv"get-convolution-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetDoublev"               get-doublev) :void  (pname enum) (params :pointer))
(defcfun ("glGetError"                 get-error) enum)
(defcfun ("glGetFloatv"                get-floatv) :void  (pname enum) (params :pointer))
(defcfun ("glGetHistogram"             get-histogram) :void  (target enum) (reset boolean) (format enum) (type enum) (values :pointer))
(defcfun ("glGetHistogramParameterfv"  get-histogram-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetHistogramParameteriv"  get-histogram-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetIntegerv"              get-integerv) :void  (pname enum) (params :pointer))
(defcfun ("glGetLightfv"               get-light-fv) :void  (light enum) (pname enum) (params :pointer))
(defcfun ("glGetLightiv"               get-light-iv) :void  (light enum) (pname enum) (params :pointer))
(defcfun ("glGetMapdv"                 get-map-dv) :void  (target enum) (query enum) (v :pointer))
(defcfun ("glGetMapfv"                 get-map-fv) :void  (target enum) (query enum) (v :pointer))
(defcfun ("glGetMapiv"                 get-map-iv) :void  (target enum) (query enum) (v :pointer))
(defcfun ("glGetMaterialfv"            get-material-fv) :void  (face enum) (pname enum) (params :pointer))
(defcfun ("glGetMaterialiv"            get-material-iv) :void  (face enum) (pname enum) (params :pointer))
(defcfun ("glGetMinmax"                get-minmax) :void  (target enum) (reset boolean) (format enum) (type enum) (values :pointer))
(defcfun ("glGetMinmaxParameterfv"     get-minmax-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetMinmaxParameteriv"     get-minmax-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetPixelMapfv"            get-pixel-map-fv) :void  (map enum) (values :pointer))
(defcfun ("glGetPixelMapuiv"           get-pixel-map-uiv) :void  (map enum) (values :pointer))
(defcfun ("glGetPixelMapusv"           get-pixel-map-usv) :void  (map enum) (values :pointer))
(defcfun ("glGetPointerv"              get-pointerv) :void  (pname enum) (params :pointer))
(defcfun ("glGetPolygonStipple"        get-polygon-stipple) :void  (mask :pointer))
(defcfun ("glGetSeparableFilter"       get-separable-filter) :void  (target enum) (format enum) (type enum) (row :pointer) (column :pointer) (span :pointer))
(defcfun ("glGetString"                get-string) :pointer (name enum))
(defcfun ("glGetTexEnvfv"              get-tex-env-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetTexEnviv"              get-tex-env-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetTexGendv"              get-tex-gen-dv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glGetTexGenfv"              get-tex-gen-fv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glGetTexGeniv"              get-tex-gen-iv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glGetTexImage"              get-tex-image) :void  (target enum) (level int) (format enum) (type enum) (pixels :pointer))
(defcfun ("glGetTexLevelParameterfv"   get-tex-level-parameter-fv) :void  (target enum) (level int) (pname enum) (params :pointer))
(defcfun ("glGetTexLevelParameteriv"   get-tex-level-parameter-iv) :void  (target enum) (level int) (pname enum) (params :pointer))
(defcfun ("glGetTexParameterfv"        get-tex-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetTexParameteriv"        get-tex-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glHint"                     hint) :void  (target enum) (mode enum ))
(defcfun ("glHistogram"                histogram) :void  (target enum) (width sizei) (internalformat enum) (sink boolean ))
(defcfun ("glIndexMask"                index-mask) :void  (mask uint ))
(defcfun ("glIndexPointer"             index-pointer) :void  (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glIndexd"                   index-d) :void  (c double ))
(defcfun ("glIndexdv"                  index-dv) :void  (c :pointer))
(defcfun ("glIndexf"                   index-f) :void  (c float ))
(defcfun ("glIndexfv"                  index-fv) :void  (c :pointer))
(defcfun ("glIndexi"                   index-i) :void  (c int ))
(defcfun ("glIndexiv"                  index-iv) :void  (c :pointer))
(defcfun ("glIndexs"                   index-s) :void  (c short ))
(defcfun ("glIndexsv"                  index-sv) :void  (c :pointer))
(defcfun ("glIndexub"                  index-ub) :void  (c ubyte ))
(defcfun ("glIndexubv"                 index-ubv) :void  (c :pointer))
(defcfun ("glInitNames"                init-names) :void)
(defcfun ("glInterleavedArrays"        interleaved-arrays) :void  (format enum) (stride sizei) (pointer :pointer))
(defcfun ("glIsEnabled"                is-enabled) boolean  (cap enum ))
(defcfun ("glIsList"                   is-list) boolean  (list uint ))
(defcfun ("glIsTexture"                is-texture) boolean  (texture uint ))
(defcfun ("glLightModelf"              light-model-f) :void  (pname enum) (param float ))
(defcfun ("glLightModelfv"             light-model-fv) :void  (pname enum) (params :pointer))
(defcfun ("glLightModeli"              light-model-i) :void  (pname enum) (param int ))
(defcfun ("glLightModeliv"             light-model-iv) :void  (pname enum) (params :pointer))
(defcfun ("glLightf"                   light-f) :void  (light enum) (pname enum) (param float ))
(defcfun ("glLightfv"                  light-fv) :void  (light enum) (pname enum) (params :pointer))
(defcfun ("glLighti"                   light-i) :void  (light enum) (pname enum) (param int ))
(defcfun ("glLightiv"                  light-iv) :void  (light enum) (pname enum) (params :pointer))
(defcfun ("glLineStipple"              line-stipple) :void  (factor int) (pattern ushort ))
(defcfun ("glLineWidth"                line-width) :void  (width float ))
(defcfun ("glListBase"                 list-base) :void  (base uint ))
(defcfun ("glLoadIdentity"             load-identity) :void)
(defcfun ("glLoadMatrixd"              load-matrix-d) :void  (m :pointer))
(defcfun ("glLoadMatrixf"              load-matrix-f) :void  (m :pointer))
(defcfun ("glLoadName"                 load-name) :void  (name uint ))
(defcfun ("glLogicOp"                  logic-op) :void  (opcode enum ))
(defcfun ("glMap1d"                    map-1d) :void  (target enum) (u1 double) (u2 double) (stride int) (order int) (points :pointer))
(defcfun ("glMap1f"                    map-1f) :void  (target enum) (u1 float) (u2 float) (stride int) (order int) (points :pointer))
(defcfun ("glMap2d"                    map-2d) :void  (target enum) (u1 double) (u2 double) (ustride int) (uorder int) (v1 double) (v2 double) (vstride int) (vorder int) (points :pointer))
(defcfun ("glMap2f"                    map-2f) :void  (target enum) (u1 float) (u2 float) (ustride int) (uorder int) (v1 float) (v2 float) (vstride int) (vorder int) (points :pointer))
(defcfun ("glMapGrid1d"                map-grid-1d) :void  (un int) (u1 double) (u2 double ))
(defcfun ("glMapGrid1f"                map-grid-1f) :void  (un int) (u1 float) (u2 float ))
(defcfun ("glMapGrid2d"                map-grid-2d) :void  (un int) (u1 double) (u2 double) (vn int) (v1 double) (v2 double ))
(defcfun ("glMapGrid2f"                map-grid-2f) :void  (un int) (u1 float) (u2 float) (vn int) (v1 float) (v2 float ))
(defcfun ("glMaterialf"                material-f) :void  (face enum) (pname enum) (param float ))
(defcfun ("glMaterialfv"               material-fv) :void  (face enum) (pname enum) (params :pointer))
(defcfun ("glMateriali"                material-i) :void  (face enum) (pname enum) (param int ))
(defcfun ("glMaterialiv"               material-iv) :void  (face enum) (pname enum) (params :pointer))
(defcfun ("glMatrixMode"               matrix-mode) :void  (mode enum ))
(defcfun ("glMinmax"                   minmax) :void  (target enum) (internalformat enum) (sink boolean ))
(defcfun ("glMultMatrixd"              mult-matrix-d) :void  (m :pointer))
(defcfun ("glMultMatrixf"              mult-matrix-f) :void  (m :pointer))
(defcfun ("glNewList"                  new-list) :void  (list uint) (mode enum ))
(defcfun ("glNormal3b"                 normal-3b) :void  (nx byte) (ny byte) (nz byte ))
(defcfun ("glNormal3bv"                normal-3bv) :void  (v :pointer))
(defcfun ("glNormal3d"                 normal-3d) :void  (nx double) (ny double) (nz double ))
(defcfun ("glNormal3dv"                normal-3dv) :void  (v :pointer))
(defcfun ("glNormal3f"                 normal-3f) :void  (nx float) (ny float) (nz float ))
(defcfun ("glNormal3fv"                normal-3fv) :void  (v :pointer))
(defcfun ("glNormal3i"                 normal-3i) :void  (nx int) (ny int) (nz int ))
(defcfun ("glNormal3iv"                normal-3iv) :void  (v :pointer))
(defcfun ("glNormal3s"                 normal-3s) :void  (nx short) (ny short) (nz short ))
(defcfun ("glNormal3sv"                normal-3sv) :void  (v :pointer))
(defcfun ("glNormalPointer"            normal-pointer) :void  (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glOrtho"                    ortho) :void  (left double) (right double) (bottom double) (top double) (zNear double) (zFar double ))
(defcfun ("glPassThrough"              pass-through) :void  (token float ))
(defcfun ("glPixelMapfv"               pixel-map-fv) :void  (map enum) (mapsize int) (values :pointer))
(defcfun ("glPixelMapuiv"              pixel-map-uiv) :void  (map enum) (mapsize int) (values :pointer))
(defcfun ("glPixelMapusv"              pixel-map-usv) :void  (map enum) (mapsize int) (values :pointer))
(defcfun ("glPixelStoref"              pixel-store-f) :void  (pname enum) (param float ))
(defcfun ("glPixelStorei"              pixel-store-i) :void  (pname enum) (param int ))
(defcfun ("glPixelTransferf"           pixel-transfer-f) :void  (pname enum) (param float ))
(defcfun ("glPixelTransferi"           pixel-transfer-i) :void  (pname enum) (param int ))
(defcfun ("glPixelZoom"                pixel-zoom) :void  (xfactor float) (yfactor float ))
(defcfun ("glPointSize"                point-size) :void  (size float ))
(defcfun ("glPolygonMode"              polygon-mode) :void  (face enum) (mode enum ))
(defcfun ("glPolygonOffset"            polygon-offset) :void  (factor float) (units float ))
(defcfun ("glPolygonStipple"           polygon-stipple) :void  (mask :pointer))
(defcfun ("glPopAttrib"                pop-attrib) :void)
(defcfun ("glPopClientAttrib"          pop-client-attrib) :void)
(defcfun ("glPopMatrix"                pop-matrix) :void)
(defcfun ("glPopName"                  pop-name) :void)
(defcfun ("glPrioritizeTextures"       prioritize-textures) :void  (n sizei) (textures :pointer) (priorities :pointer))
(defcfun ("glPushAttrib"               push-attrib) :void  (mask bitfield ))
(defcfun ("glPushClientAttrib"         push-client-attrib) :void  (mask bitfield ))
(defcfun ("glPushMatrix"               push-matrix) :void)
(defcfun ("glPushName"                 push-name) :void  (name uint ))
(defcfun ("glRasterPos2d"              raster-pos-2d) :void  (x double) (y double ))
(defcfun ("glRasterPos2dv"             raster-pos-2dv) :void  (v :pointer))
(defcfun ("glRasterPos2f"              raster-pos-2f) :void  (x float) (y float ))
(defcfun ("glRasterPos2fv"             raster-pos-2fv) :void  (v :pointer))
(defcfun ("glRasterPos2i"              raster-pos-2i) :void  (x int) (y int ))
(defcfun ("glRasterPos2iv"             raster-pos-2iv) :void  (v :pointer))
(defcfun ("glRasterPos2s"              raster-pos-2s) :void  (x short) (y short ))
(defcfun ("glRasterPos2sv"             raster-pos-2sv) :void  (v :pointer))
(defcfun ("glRasterPos3d"              raster-pos-3d) :void  (x double) (y double) (z double ))
(defcfun ("glRasterPos3dv"             raster-pos-3dv) :void  (v :pointer))
(defcfun ("glRasterPos3f"              raster-pos-3f) :void  (x float) (y float) (z float ))
(defcfun ("glRasterPos3fv"             raster-pos-3fv) :void  (v :pointer))
(defcfun ("glRasterPos3i"              raster-pos-3i) :void  (x int) (y int) (z int ))
(defcfun ("glRasterPos3iv"             raster-pos-3iv) :void  (v :pointer))
(defcfun ("glRasterPos3s"              raster-pos-3s) :void  (x short) (y short) (z short ))
(defcfun ("glRasterPos3sv"             raster-pos-3sv) :void  (v :pointer))
(defcfun ("glRasterPos4d"              raster-pos-4d) :void  (x double) (y double) (z double) (w double ))
(defcfun ("glRasterPos4dv"             raster-pos-4dv) :void  (v :pointer))
(defcfun ("glRasterPos4f"              raster-pos-4f) :void  (x float) (y float) (z float) (w float ))
(defcfun ("glRasterPos4fv"             raster-pos-4fv) :void  (v :pointer))
(defcfun ("glRasterPos4i"              raster-pos-4i) :void  (x int) (y int) (z int) (w int ))
(defcfun ("glRasterPos4iv"             raster-pos-4iv) :void  (v :pointer))
(defcfun ("glRasterPos4s"              raster-pos-4s) :void  (x short) (y short) (z short) (w short ))
(defcfun ("glRasterPos4sv"             raster-pos-4sv) :void  (v :pointer))
(defcfun ("glReadBuffer"               read-buffer) :void  (mode enum ))
(defcfun ("glReadPixels"               read-pixels) :void  (x int) (y int) (width sizei) (height sizei) (format enum) (type enum) (pixels :pointer))
(defcfun ("glRectd"                    rect-d) :void  (x1 double) (y1 double) (x2 double) (y2 double ))
(defcfun ("glRectdv"                   rect-dv) :void  (v1 :pointer) (v2 :pointer))
(defcfun ("glRectf"                    rect-f) :void  (x1 float) (y1 float) (x2 float) (y2 float ))
(defcfun ("glRectfv"                   rect-fv) :void  (v1 :pointer) (v2 :pointer))
(defcfun ("glRecti"                    rect-i) :void  (x1 int) (y1 int) (x2 int) (y2 int ))
(defcfun ("glRectiv"                   rect-iv) :void  (v1 :pointer) (v2 :pointer))
(defcfun ("glRects"                    rect-s) :void  (x1 short) (y1 short) (x2 short) (y2 short ))
(defcfun ("glRectsv"                   rect-sv) :void  (v1 :pointer) (v2 :pointer))
(defcfun ("glRenderMode"               render-mode) int  (mode enum ))
(defcfun ("glResetHistogram"           reset-histogram) :void  (target enum ))
(defcfun ("glResetMinmax"              reset-minmax) :void  (target enum ))
(defcfun ("glRotated"                  rotate-d) :void  (angle double) (x double) (y double) (z double ))
(defcfun ("glRotatef"                  rotate-f) :void  (angle float) (x float) (y float) (z float ))
(defcfun ("glScaled"                   scale-d) :void  (x double) (y double) (z double ))
(defcfun ("glScalef"                   scale-f) :void  (x float) (y float) (z float ))
(defcfun ("glScissor"                  scissor) :void  (x int) (y int) (width sizei) (height sizei ))
(defcfun ("glSelectBuffer"             select-buffer) :void  (size sizei) (buffer :pointer))
(defcfun ("glSeparableFilter2D"        separable-filter-2d) :void  (target enum) (internalformat enum) (width sizei) (height sizei) (format enum) (type enum) (row :pointer) (column :pointer))
(defcfun ("glShadeModel"               shade-model) :void  (mode enum ))
(defcfun ("glStencilFunc"              stencil-func) :void  (func enum) (ref int) (mask uint ))
(defcfun ("glStencilMask"              stencil-mask) :void  (mask uint ))
(defcfun ("glStencilOp"                stencil-op) :void  (fail enum) (zfail enum) (zpass enum ))
(defcfun ("glTexCoord1d"               tex-coord-1d) :void  (s double ))
(defcfun ("glTexCoord1dv"              tex-coord-1dv) :void  (v :pointer))
(defcfun ("glTexCoord1f"               tex-coord-1f) :void  (s float ))
(defcfun ("glTexCoord1fv"              tex-coord-1fv) :void  (v :pointer))
(defcfun ("glTexCoord1i"               tex-coord-1i) :void  (s int ))
(defcfun ("glTexCoord1iv"              tex-coord-1iv) :void  (v :pointer))
(defcfun ("glTexCoord1s"               tex-coord-1s) :void  (s short ))
(defcfun ("glTexCoord1sv"              tex-coord-1sv) :void  (v :pointer))
(defcfun ("glTexCoord2d"               tex-coord-2d) :void  (s double) (_t double ))
(defcfun ("glTexCoord2dv"              tex-coord-2dv) :void  (v :pointer))
(defcfun ("glTexCoord2f"               tex-coord-2f) :void  (s float) (_t float ))
(defcfun ("glTexCoord2fv"              tex-coord-2fv) :void  (v :pointer))
(defcfun ("glTexCoord2i"               tex-coord-2i) :void  (s int) (_t int ))
(defcfun ("glTexCoord2iv"              tex-coord-2iv) :void  (v :pointer))
(defcfun ("glTexCoord2s"               tex-coord-2s) :void  (s short) (_t short ))
(defcfun ("glTexCoord2sv"              tex-coord-2sv) :void  (v :pointer))
(defcfun ("glTexCoord3d"               tex-coord-3d) :void  (s double) (_t double) (r double ))
(defcfun ("glTexCoord3dv"              tex-coord-3dv) :void  (v :pointer))
(defcfun ("glTexCoord3f"               tex-coord-3f) :void  (s float) (_t float) (r float ))
(defcfun ("glTexCoord3fv"              tex-coord-3fv) :void  (v :pointer))
(defcfun ("glTexCoord3i"               tex-coord-3i) :void  (s int) (_t int) (r int ))
(defcfun ("glTexCoord3iv"              tex-coord-3iv) :void  (v :pointer))
(defcfun ("glTexCoord3s"               tex-coord-3s) :void  (s short) (_t short) (r short ))
(defcfun ("glTexCoord3sv"              tex-coord-3sv) :void  (v :pointer))
(defcfun ("glTexCoord4d"               tex-coord-4d) :void  (s double) (_t double) (r double) (q double ))
(defcfun ("glTexCoord4dv"              tex-coord-4dv) :void  (v :pointer))
(defcfun ("glTexCoord4f"               tex-coord-4f) :void  (s float) (_t float) (r float) (q float ))
(defcfun ("glTexCoord4fv"              tex-coord-4fv) :void  (v :pointer))
(defcfun ("glTexCoord4i"               tex-coord-4i) :void  (s int) (_t int) (r int) (q int ))
(defcfun ("glTexCoord4iv"              tex-coord-4iv) :void  (v :pointer))
(defcfun ("glTexCoord4s"               tex-coord-4s) :void  (s short) (_t short) (r short) (q short ))
(defcfun ("glTexCoord4sv"              tex-coord-4sv) :void  (v :pointer))
(defcfun ("glTexCoordPointer"          tex-coord-pointer) :void  (size int) (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glTexEnvf"                  tex-env-f) :void  (target enum) (pname enum) (param float ))
(defcfun ("glTexEnvfv"                 tex-env-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glTexEnvi"                  tex-env-i) :void  (target enum) (pname enum) (param int ))
(defcfun ("glTexEnviv"                 tex-env-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glTexGend"                  tex-gen-d) :void  (coord enum) (pname enum) (param double ))
(defcfun ("glTexGendv"                 tex-gen-dv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glTexGenf"                  tex-gen-f) :void  (coord enum) (pname enum) (param float ))
(defcfun ("glTexGenfv"                 tex-gen-fv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glTexGeni"                  tex-gen-i) :void  (coord enum) (pname enum) (param int ))
(defcfun ("glTexGeniv"                 tex-gen-iv) :void  (coord enum) (pname enum) (params :pointer))
(defcfun ("glTexImage1D"               tex-image-1d) :void  (target enum) (level int) (internalformat enum) (width sizei) (border int) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTexImage2D"               tex-image-2d) :void  (target enum) (level int) (internalformat enum) (width sizei) (height sizei) (border int) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTexImage3D"               tex-image-3d) :void  (target enum) (level int) (internalformat enum) (width sizei) (height sizei) (depth sizei) (border int) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTexParameterf"            tex-parameter-f) :void  (target enum) (pname enum) (param float ))
(defcfun ("glTexParameterfv"           tex-parameter-fv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glTexParameteri"            tex-parameter-i) :void  (target enum) (pname enum) (param int ))
(defcfun ("glTexParameteriv"           tex-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glTexSubImage1D"            tex-sub-image-1d) :void  (target enum) (level int) (xoffset int) (width sizei) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTexSubImage2D"            tex-sub-image-2d) :void  (target enum) (level int) (xoffset int) (yoffset int) (width sizei) (height sizei) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTexSubImage3D"            tex-sub-image-3d) :void  (target enum) (level int) (xoffset int) (yoffset int) (zoffset int) (width sizei) (height sizei) (depth sizei) (format enum) (type enum) (pixels :pointer))
(defcfun ("glTranslated"               translate-d) :void  (x double) (y double) (z double ))
(defcfun ("glTranslatef"               translate-f) :void  (x float) (y float) (z float ))
(defcfun ("glVertex2d"                 vertex-2d) :void  (x double) (y double ))
(defcfun ("glVertex2dv"                vertex-2dv) :void  (v :pointer))
(defcfun ("glVertex2f"                 vertex-2f) :void  (x float) (y float ))
(defcfun ("glVertex2fv"                vertex-2fv) :void  (v :pointer))
(defcfun ("glVertex2i"                 vertex-2i) :void  (x int) (y int ))
(defcfun ("glVertex2iv"                vertex-2iv) :void  (v :pointer))
(defcfun ("glVertex2s"                 vertex-2s) :void  (x short) (y short ))
(defcfun ("glVertex2sv"                vertex-2sv) :void  (v :pointer))
(defcfun ("glVertex3d"                 vertex-3d) :void  (x double) (y double) (z double ))
(defcfun ("glVertex3dv"                vertex-3dv) :void  (v :pointer))
(defcfun ("glVertex3f"                 vertex-3f) :void  (x float) (y float) (z float ))
(defcfun ("glVertex3fv"                vertex-3fv) :void  (v :pointer))
(defcfun ("glVertex3i"                 vertex-3i) :void  (x int) (y int) (z int ))
(defcfun ("glVertex3iv"                vertex-3iv) :void  (v :pointer))
(defcfun ("glVertex3s"                 vertex-3s) :void  (x short) (y short) (z short ))
(defcfun ("glVertex3sv"                vertex-3sv) :void  (v :pointer))
(defcfun ("glVertex4d"                 vertex-4d) :void  (x double) (y double) (z double) (w double ))
(defcfun ("glVertex4dv"                vertex-4dv) :void  (v :pointer))
(defcfun ("glVertex4f"                 vertex-4f) :void  (x float) (y float) (z float) (w float ))
(defcfun ("glVertex4fv"                vertex-4fv) :void  (v :pointer))
(defcfun ("glVertex4i"                 vertex-4i) :void  (x int) (y int) (z int) (w int ))
(defcfun ("glVertex4iv"                vertex-4iv) :void  (v :pointer))
(defcfun ("glVertex4s"                 vertex-4s) :void  (x short) (y short) (z short) (w short ))
(defcfun ("glVertex4sv"                vertex-4sv) :void  (v :pointer))
(defcfun ("glVertexPointer"            vertex-pointer) :void  (size int) (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glViewport"                 viewport) :void  (x int) (y int) (width sizei) (height sizei ))
(defcfun ("glSampleCoverage"           sample-coverage) :void  (value clampf) (invert boolean ))
(defcfun ("glSamplePass"               sample-pass) :void  (pass enum ))
(defcfun ("glLoadTransposeMatrixf"     load-transpose-matrix-f) :void  (m :pointer))
(defcfun ("glLoadTransposeMatrixd"     load-transpose-matrix-d) :void  (m :pointer))
(defcfun ("glMultTransposeMatrixf"     mult-transpose-matrix-f) :void  (m :pointer))
(defcfun ("glMultTransposeMatrixd"     mult-transpose-matrix-d) :void  (m :pointer))
(defcfun ("glCompressedTexImage3D"     compressed-tex-image-3d) :void  (target enum) (level int) (internalformat enum) (width sizei) (height sizei) (depth sizei) (border int) (imageSize sizei) (data :pointer))
(defcfun ("glCompressedTexImage2D"     compressed-tex-image-2d) :void  (target enum) (level int) (internalformat enum) (width sizei) (height sizei) (border int) (imageSize sizei) (data :pointer))
(defcfun ("glCompressedTexImage1D"     compressed-tex-image-1d) :void  (target enum) (level int) (internalformat enum) (width sizei) (border int) (imageSize sizei) (data :pointer))
(defcfun ("glCompressedTexSubImage3D"  compressed-tex-sub-image-3d) :void  (target enum) (level int) (xoffset int) (yoffset int) (zoffset int) (width sizei) (height sizei) (depth sizei) (format enum) (imageSize sizei) (data :pointer))
(defcfun ("glCompressedTexSubImage2D"  compressed-tex-sub-image-2d) :void  (target enum) (level int) (xoffset int) (yoffset int) (width sizei) (height sizei) (format enum) (imageSize sizei) (data :pointer))
(defcfun ("glCompressedTexSubImage1D"  compressed-tex-sub-image-1d) :void  (target enum) (level int) (xoffset int) (width sizei) (format enum) (imageSize sizei) (data :pointer))
(defcfun ("glGetCompressedTexImage"    get-compressed-tex-image) :void  (target enum) (lod int) (img :pointer))
(defcfun ("glActiveTexture"            active-texture) :void  (texture enum ))
(defcfun ("glClientActiveTexture"      client-active-texture) :void  (texture enum ))
(defcfun ("glMultiTexCoord1d"          multi-tex-coord-1d) :void  (target enum) (s double ))
(defcfun ("glMultiTexCoord1dv"         multi-tex-coord-1dv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord1f"          multi-tex-coord-1f) :void  (target enum) (s float ))
(defcfun ("glMultiTexCoord1fv"         multi-tex-coord-1fv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord1i"          multi-tex-coord-1i) :void  (target enum) (s int ))
(defcfun ("glMultiTexCoord1iv"         multi-tex-coord-1iv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord1s"          multi-tex-coord-1s) :void  (target enum) (s short ))
(defcfun ("glMultiTexCoord1sv"         multi-tex-coord-1sv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord2d"          multi-tex-coord-2d) :void  (target enum) (s double) (_t double ))
(defcfun ("glMultiTexCoord2dv"         multi-tex-coord-2dv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord2f"          multi-tex-coord-2f) :void  (target enum) (s float) (_t float ))
(defcfun ("glMultiTexCoord2fv"         multi-tex-coord-2fv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord2i"          multi-tex-coord-2i) :void  (target enum) (s int) (_t int ))
(defcfun ("glMultiTexCoord2iv"         multi-tex-coord-2iv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord2s"          multi-tex-coord-2s) :void  (target enum) (s short) (_t short ))
(defcfun ("glMultiTexCoord2sv"         multi-tex-coord-2sv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord3d"          multi-tex-coord-3d) :void  (target enum) (s double) (_t double) (r double ))
(defcfun ("glMultiTexCoord3dv"         multi-tex-coord-3dv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord3f"          multi-tex-coord-3f) :void  (target enum) (s float) (_t float) (r float ))
(defcfun ("glMultiTexCoord3fv"         multi-tex-coord-3fv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord3i"          multi-tex-coord-3i) :void  (target enum) (s int) (_t int) (r int ))
(defcfun ("glMultiTexCoord3iv"         multi-tex-coord-3iv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord3s"          multi-tex-coord-3s) :void  (target enum) (s short) (_t short) (r short ))
(defcfun ("glMultiTexCoord3sv"         multi-tex-coord-3sv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord4d"          multi-tex-coord-4d) :void  (target enum) (s double) (_t double) (r double) (q double ))
(defcfun ("glMultiTexCoord4dv"         multi-tex-coord-4dv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord4f"          multi-tex-coord-4f) :void  (target enum) (s float) (_t float) (r float) (q float ))
(defcfun ("glMultiTexCoord4fv"         multi-tex-coord-4fv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord4i"          multi-tex-coord-4i) :void  (target enum) (q int) (s int) (_t int) (r int ))
(defcfun ("glMultiTexCoord4iv"         multi-tex-coord-4iv) :void  (target enum) (v :pointer))
(defcfun ("glMultiTexCoord4s"          multi-tex-coord-4s) :void  (target enum) (s short) (_t short) (r short) (q short ))
(defcfun ("glMultiTexCoord4sv"         multi-tex-coord-4sv) :void  (target enum) (v :pointer))
(defcfun ("glFogCoordf"                fog-coord-f) :void  (coord float ))
(defcfun ("glFogCoordfv"               fog-coord-fv) :void  (coord :pointer))  
(defcfun ("glFogCoordd"                fog-coord-d) :void  (coord double ))
(defcfun ("glFogCoorddv"               fog-coord-dv) :void  (coord :pointer))   
(defcfun ("glFogCoordPointer"          fog-coord-pointer) :void  (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glSecondaryColor3b"         secondary-color-3b) :void  (red byte) (green byte) (blue byte ))
(defcfun ("glSecondaryColor3bv"        secondary-color-3bv) :void  (v :pointer))
(defcfun ("glSecondaryColor3d"         secondary-color-3d) :void  (red double) (green double) (blue double ))
(defcfun ("glSecondaryColor3dv"        secondary-color-3dv) :void  (v :pointer))
(defcfun ("glSecondaryColor3f"         secondary-color-3f) :void  (red float) (green float) (blue float ))
(defcfun ("glSecondaryColor3fv"        secondary-color-3fv) :void  (v :pointer))
(defcfun ("glSecondaryColor3i"         secondary-color-3i) :void  (red int) (green int) (blue int ))
(defcfun ("glSecondaryColor3iv"        secondary-color-3iv) :void  (v :pointer))
(defcfun ("glSecondaryColor3s"         secondary-color-3s) :void  (red short) (green short) (blue short ))
(defcfun ("glSecondaryColor3sv"        secondary-color-3sv) :void  (v :pointer))
(defcfun ("glSecondaryColor3ub"        secondary-color-3ub) :void  (red ubyte) (green ubyte) (blue ubyte ))
(defcfun ("glSecondaryColor3ubv"       secondary-color-3ubv) :void  (v :pointer))
(defcfun ("glSecondaryColor3ui"        secondary-color-3ui) :void  (red uint) (green uint) (blue uint ))
(defcfun ("glSecondaryColor3uiv"       secondary-color-3uiv) :void  (v :pointer))
(defcfun ("glSecondaryColor3us"        secondary-color-3us) :void  (red ushort) (green ushort) (blue ushort ))
(defcfun ("glSecondaryColor3usv"       secondary-color-3usv) :void  (v :pointer))
(defcfun ("glSecondaryColorPointer"    secondary-color--pointer) :void  (size int) (type enum) (stride sizei) (pointer :pointer))
(defcfun ("glPointParameterf"          point-parameter-f) :void  (pname enum) (param float )) 
(defcfun ("glPointParameterfv"         point-parameter-fv) :void  (pname enum) (params :pointer))
(defcfun ("glPointParameteri"          point-parameter-i) :void  (pname enum) (param int )) 
(defcfun ("glPointParameteriv"         point-parameter-iv) :void  (pname enum) (params :pointer))
(defcfun ("glBlendFuncSeparate"        blend-func-separate) :void  (srcRGB enum) (dstRGB enum) (srcAlpha enum) (dstAlpha enum ))
(defcfun ("glMultiDrawArrays"          multi-draw-arrays) :void  (mode enum) (first :pointer) (count :pointer) (primcount sizei ))
(defcfun ("glMultiDrawElements"        multi-draw-elements) :void  (mode enum) (count :pointer) (type enum) (indices :pointer) (primcount sizei ))
(defcfun ("glWindowPos2d"              window-pos-2d) :void  (x double) (y double ))
(defcfun ("glWindowPos2dv"             window-pos-2dv) :void  (v :pointer))
(defcfun ("glWindowPos2f"              window-pos-2f) :void  (x float) (y float ))
(defcfun ("glWindowPos2fv"             window-pos-2fv) :void  (v :pointer))
(defcfun ("glWindowPos2i"              window-pos-2i) :void  (x int) (y int )) 
(defcfun ("glWindowPos2iv"             window-pos-2iv) :void  (v :pointer))
(defcfun ("glWindowPos2s"              window-pos-2s) :void  (x short) (y short ))
(defcfun ("glWindowPos2sv"             window-pos-2sv) :void  (v :pointer))
(defcfun ("glWindowPos3d"              window-pos-3d) :void  (x double) (y double) (z double ))
(defcfun ("glWindowPos3dv"             window-pos-3dv) :void  (v :pointer))
(defcfun ("glWindowPos3f"              window-pos-3f) :void  (x float) (y float) (z float ))
(defcfun ("glWindowPos3fv"             window-pos-3fv) :void  (v :pointer))
(defcfun ("glWindowPos3i"              window-pos-3i) :void  (x int) (y int) (z int ))
(defcfun ("glWindowPos3iv"             window-pos-3iv) :void  (v :pointer))
(defcfun ("glWindowPos3s"              window-pos-3s) :void  (x short) (y short) (z short ))
(defcfun ("glWindowPos3sv"             window-pos-3sv) :void  (v :pointer))
(defcfun ("glGenQueries"               gen-queries) :void (n sizei) (ids :pointer))
(defcfun ("glDeleteQueries"            delete-queries) :void (n sizei) (ids :pointer))
(defcfun ("glIsQuery"                  is-query) boolean (id uint ))
(defcfun ("glBeginQuery"               begin-query) :void (target enum) (id uint ))
(defcfun ("glEndQuery"                 end-query) :void (target enum ))
(defcfun ("glGetQueryiv"               get-queryiv) :void (target enum) (pname enum) (params :pointer))
(defcfun ("glGetQueryObjectiv"         get-query-object-iv) :void (id uint) (pname enum) (params :pointer))
(defcfun ("glGetQueryObjectuiv"        get-query-object-uiv) :void (id uint) (pname enum) (params :pointer))
(defcfun ("glBindBuffer"               bind-buffer) :void  (target enum) (buffer uint ))
(defcfun ("glDeleteBuffers"            delete-buffers) :void  (n sizei) (buffers :pointer))
(defcfun ("glGenBuffers"               gen-buffers) :void  (n sizei) (buffers :pointer))
(defcfun ("glIsBuffer"                 is-buffer) boolean  (buffer uint ))
(defcfun ("glBufferData"               buffer-data) :void  (target enum) (size sizeiptr) (data :pointer) (usage enum ))
(defcfun ("glBufferSubData"            buffer-sub-data) :void  (target enum) (offset intptr) (size sizeiptr) (data :pointer))
(defcfun ("glGetBufferSubData"         get-buffer-sub-data) :void  (target enum) (offset intptr) (size sizeiptr) (data :pointer))
(defcfun ("glMapBuffer"                map-buffer) :pointer  (target enum) (access enum ))
(defcfun ("glUnmapBuffer"              unmap-buffer) boolean  (target enum ))
(defcfun ("glGetBufferParameteriv"     get-buffer-parameter-iv) :void  (target enum) (pname enum) (params :pointer))
(defcfun ("glGetBufferPointerv"        get-buffer-pointer-v) :void  (target enum) (pname enum) (*params :pointer))
(defcfun ("glDrawBuffers"              draw-buffers) :void  (n sizei) (bufs :pointer))
(defcfun ("glVertexAttrib1d"           vertex-attrib-1d) :void  (index uint) (x double ))
(defcfun ("glVertexAttrib1dv"          vertex-attrib-1dv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib1f"           vertex-attrib-1f) :void  (index uint) (x float ))
(defcfun ("glVertexAttrib1fv"          vertex-attrib-1fv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib1s"           vertex-attrib-1s) :void  (index uint) (x short ))
(defcfun ("glVertexAttrib1sv"          vertex-attrib-1sv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib2d"           vertex-attrib-2d) :void  (index uint) (x double) (y double ))
(defcfun ("glVertexAttrib2dv"          vertex-attrib-2dv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib2f"           vertex-attrib-2f) :void  (index uint) (x float) (y float ))
(defcfun ("glVertexAttrib2fv"          vertex-attrib-2fv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib2s"           vertex-attrib-2s) :void  (index uint) (x short) (y short ))
(defcfun ("glVertexAttrib2sv"          vertex-attrib-2sv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib3d"           vertex-attrib-3d) :void  (index uint) (x double) (y double) (z double ))
(defcfun ("glVertexAttrib3dv"          vertex-attrib-3dv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib3f"           vertex-attrib-3f) :void  (index uint) (x float) (y float) (z float ))
(defcfun ("glVertexAttrib3fv"          vertex-attrib-3fv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib3s"           vertex-attrib-3s) :void  (index uint) (x short) (y short) (z short ))
(defcfun ("glVertexAttrib3sv"          vertex-attrib-3sv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Nbv"         vertex-attrib-4-nbv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Niv"         vertex-attrib-4-niv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Nsv"         vertex-attrib-4-nsv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Nub"         vertex-attrib-4-nub) :void  (index uint) (x ubyte) (y ubyte) (z ubyte) (w ubyte ))
(defcfun ("glVertexAttrib4Nubv"        vertex-attrib-4-nubv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Nuiv"        vertex-attrib-4-nuiv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4Nusv"        vertex-attrib-4-nusv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4bv"          vertex-attrib-4bv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4d"           vertex-attrib-4d) :void  (index uint) (x double) (y double) (z double) (w double ))
(defcfun ("glVertexAttrib4dv"          vertex-attrib-4dv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4f"           vertex-attrib-4f) :void  (index uint) (x float) (y float) (z float) (w float ))
(defcfun ("glVertexAttrib4fv"          vertex-attrib-4fv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4iv"          vertex-attrib-4iv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4s"           vertex-attrib-4s) :void  (index uint) (x short) (y short) (z short) (w short ))
(defcfun ("glVertexAttrib4sv"          vertex-attrib-4sv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4ubv"         vertex-attrib-4ubv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4uiv"         vertex-attrib-4uiv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttrib4usv"         vertex-attrib-4usv) :void  (index uint) (v :pointer))
(defcfun ("glVertexAttribPointer"      vertex-attrib-pointer) :void  (index uint) (size int) (type enum) (normalized boolean) (stride sizei) (pointer :pointer))
(defcfun ("glEnableVertexAttribArray"  enable-vertex-attrib-array) :void  (index uint ))
(defcfun ("glDisableVertexAttribArray" disable-vertex-attrib-array) :void  (index uint ))
(defcfun ("glGetVertexAttribdv"        get-vertex-attrib-dv) :void  (index uint) (pname enum) (params :pointer))
(defcfun ("glGetVertexAttribfv"        get-vertex-attrib-fv) :void  (index uint) (pname enum) (params :pointer))
(defcfun ("glGetVertexAttribiv"        get-vertex-attrib-iv) :void  (index uint) (pname enum) (params :pointer))
(defcfun ("glGetVertexAttribPointerv"  get-vertex-attrib-pointer-v) :void  (index uint) (pname enum) (pointer :pointer))
(defcfun ("glDeleteShader"             delete-shader) :void  (shader uint ))
(defcfun ("glDetachShader"             detach-shader) :void  (program uint) (shader uint ))
(defcfun ("glCreateShader"             create-shader) uint  (type enum ))
(defcfun ("glShaderSource"             shader-source) :void  (shader uint) (count sizei) (string :pointer) (length :pointer))
(defcfun ("glCompileShader"            compile-shader) :void  (shader uint ))
(defcfun ("glCreateProgram"            create-program) uint)
(defcfun ("glAttachShader"             attach-shader) :void  (program uint) (shader uint ))
(defcfun ("glLinkProgram"              link-program) :void  (program uint ))
(defcfun ("glUseProgram"               use-program) :void  (program uint ))
(defcfun ("glDeleteProgram"            delete-program) :void  (program uint ))
(defcfun ("glValidateProgram"          validate-program) :void  (program uint ))
(defcfun ("glUniform1f"                uniform-1f) :void  (location int) (v0 float ))
(defcfun ("glUniform2f"                uniform-2f) :void  (location int) (v0 float) (v1 float ))
(defcfun ("glUniform3f"                uniform-3f) :void  (location int) (v0 float) (v1 float) (v2 float ))
(defcfun ("glUniform4f"                uniform-4f) :void  (location int) (v0 float) (v1 float) (v2 float) (v3 float ))
(defcfun ("glUniform1i"                uniform-1i) :void  (location int) (v0 int ))
(defcfun ("glUniform2i"                uniform-2i) :void  (location int) (v0 int) (v1 int ))
(defcfun ("glUniform3i"                uniform-3i) :void  (location int) (v0 int) (v1 int) (v2 int ))
(defcfun ("glUniform4i"                uniform-4i) :void  (location int) (v0 int) (v1 int) (v2 int) (v3 int ))
(defcfun ("glUniform1fv"               uniform-1fv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform2fv"               uniform-2fv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform3fv"               uniform-3fv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform4fv"               uniform-4fv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform1iv"               uniform-1iv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform2iv"               uniform-2iv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform3iv"               uniform-3iv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniform4iv"               uniform-4iv) :void  (location int) (count sizei) (value :pointer))
(defcfun ("glUniformMatrix2fv"         uniform-matrix-2fv) :void  (location int) (count sizei) (transpose boolean) (value :pointer))
(defcfun ("glUniformMatrix3fv"         uniform-matrix-3fv) :void  (location int) (count sizei) (transpose boolean) (value :pointer))
(defcfun ("glUniformMatrix4fv"         uniform-matrix-4fv) :void  (location int) (count sizei) (transpose boolean) (value :pointer))
(defcfun ("glIsShader"                 is-shader) boolean  (shader uint ))
(defcfun ("glIsProgram"                is-program) boolean  (program uint ))
(defcfun ("glGetShaderiv"              get-shader-iv) :void  (shader uint) (pname enum) (params :pointer))
(defcfun ("glGetProgramiv"             get-program-iv) :void  (program uint) (pname enum) (params :pointer))
(defcfun ("glGetAttachedShaders"       get-attached-shaders) :void  (program uint) (maxCount sizei) (count :pointer) (shaders :pointer))
(defcfun ("glGetShaderInfoLog"         get-shader-info-log) :void  (shader uint) (bufSize sizei) (length :pointer) (infoLog :pointer))
(defcfun ("glGetProgramInfoLog"        get-program-info-log) :void  (program uint) (bufSize sizei) (length :pointer) (infoLog :pointer))
(defcfun ("glGetUniformLocation"       get-uniform-location) int  (program uint) (name :pointer))
(defcfun ("glGetActiveUniform"         get-active-uniform) :void  (program uint) (index uint) (bufSize sizei) (length :pointer) (size :pointer) (type :pointer) (name :pointer))
(defcfun ("glGetUniformfv"             get-uniform-fv) :void  (program uint) (location int) (params :pointer))
(defcfun ("glGetUniformiv"             get-uniform-iv) :void  (program uint) (location int) (params :pointer))
(defcfun ("glGetShaderSource"          get-shader-source) :void  (shader uint) (bufSize sizei) (length :pointer) (source :pointer))
(defcfun ("glBindAttribLocation"       bind-attrib-location) :void  (program uint) (index uint) (name :pointer))
(defcfun ("glGetActiveAttrib"          get-active-attrib) :void  (program uint) (index uint) (bufSize sizei) (length :pointer) (size :pointer) (type :pointer) (name :pointer))
(defcfun ("glGetAttribLocation"        get-attrib-location) int  (program uint) (name :pointer))
(defcfun ("glStencilFuncSeparate"      stencil-func-separate) :void  (face enum) (func enum) (ref int) (mask uint ))
(defcfun ("glStencilOpSeparate"        stencil-op-separate) :void  (face enum) (fail enum) (zfail enum) (zpass enum ))
(defcfun ("glStencilMaskSeparate"      stencil-mask-separate) :void (face enum) (mask uint))


;; CUSTOM

(defmacro with-enabled-client-attribs (pointer-constants &rest body)
  `(prog1
     (progn
       (push-client-attrib +client-vertex-array-bit+)
       ,@(loop for a in pointer-constants collecting (list 'enable-client-state a))
       (progn ,@body))
     (pop-client-attrib)))

(defun create-float-array-as-copy (input)
  (foreign-alloc 'float :initial-contents input))


(defun print-errors (&optional prefix)
  (let ((err (get-error))
        (prefix (or prefix "GL Error")))
    (when (/= err +no-error+)
      (format t "~a: ~a~%" prefix (error-string err)))))
