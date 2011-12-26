;;; examples of using the GL bindings

(use-modules (ice-9 format))
(provide 'snd-snd-gl.scm)

;;; ---------------- gl-info ----------------
(define+ (gl-info)
  ;; taken loosely from glxvisuals.c
  "(gl-info) prints out GL-related info"
  (define (class-of n)
    (and (number? n)
	 (cond ((= n StaticGray) "static-gray")
	       ((= n GrayScale) "gray-scale")
	       ((= n StaticColor) "static-color")
	       ((= n PseudoColor) "pseudo-color")
	       ((= n TrueColor) "true-color")
	       ((= n DirectColor) "direct-color")
	       (#t "??"))))
  (let* ((cx (snd-glx-context))
	 (dpy (XtDisplay (cadr (main-widgets))))
	 (version (glXQueryVersion dpy 0 0)))
    (if (car version)
	(let* ((scr (DefaultScreen dpy))
	       (visuals (XGetVisualInfo dpy 0 (list 'XVisualInfo 0))))
	  (glXMakeCurrent dpy (XtWindow (cadr (main-widgets))) cx)
	  (snd-print (format #f "GL version: ~A.~A, (~A ~A ~A)~%"
			     (cadr version) (caddr version)
			     (glGetString GL_VENDOR) (glGetString GL_RENDERER) (glGetString GL_VERSION)))
	  (snd-print (format #f "  with: ~A~A~%"
			     (glXQueryExtensionsString dpy (XScreenNumberOfScreen (DefaultScreenOfDisplay dpy)))
			     (if (glXIsDirect dpy cx) ", direct rendering support" "")))
	  (for-each 
	   (lambda (visual)
	     (if (= (cadr (glXGetConfig dpy visual GLX_USE_GL)) 1)
		 ;; found a visual that can support GL
		 (let ((buffersize (cadr (glXGetConfig dpy visual GLX_BUFFER_SIZE)))
		       (level (cadr (glXGetConfig dpy visual GLX_LEVEL)))
		       (rgba (cadr (glXGetConfig dpy visual GLX_RGBA)))
		       (doublebuffer (cadr (glXGetConfig dpy visual GLX_DOUBLEBUFFER)))
		       (stereo (cadr (glXGetConfig dpy visual GLX_STEREO)))
		       (auxbuffers (cadr (glXGetConfig dpy visual GLX_AUX_BUFFERS)))
		       (redsize (cadr (glXGetConfig dpy visual GLX_RED_SIZE)))
		       (bluesize (cadr (glXGetConfig dpy visual GLX_BLUE_SIZE)))
		       (greensize (cadr (glXGetConfig dpy visual GLX_GREEN_SIZE)))
		       (alphasize (cadr (glXGetConfig dpy visual GLX_ALPHA_SIZE)))
		       (depthsize (cadr (glXGetConfig dpy visual GLX_DEPTH_SIZE)))
		       (stencilsize (cadr (glXGetConfig dpy visual GLX_STENCIL_SIZE)))
		       (acredsize (cadr (glXGetConfig dpy visual GLX_ACCUM_RED_SIZE)))
		       (acgreensize (cadr (glXGetConfig dpy visual GLX_ACCUM_GREEN_SIZE)))
		       (acbluesize (cadr (glXGetConfig dpy visual GLX_ACCUM_BLUE_SIZE)))
		       (acalphasize (cadr (glXGetConfig dpy visual GLX_ACCUM_ALPHA_SIZE))))
		   (snd-print (format #f "  id: #x~X depth: ~D class: ~S~%" (.visualid visual) (.depth visual) (class-of (.class visual))))
		   (snd-print (format #f "      buffersize: ~D, level: ~D, rgba: ~A, doublebuffer: ~A, stereo: ~A~%"
				      buffersize level
				      (if (= rgba 1) "#t" "#f")
				      (if (= doublebuffer 1) "#t" "#f")
				      (if (= stereo 1) "#t" "#f")))
		   (snd-print (format #f "      r: ~A, g: ~D, b: ~D, alpha: ~D, accum-r: ~D, accum-g: ~D, accum-b: ~D, accum-alpha: ~D~%"
				      redsize greensize bluesize alphasize 
				      acredsize acgreensize acbluesize acalphasize))
		   (snd-print (format #f "      auxbuffs: ~D, depth: ~D, acalpha: ~D~%"
				      auxbuffers depthsize stencilsize))

		   
		   )))
	   visuals))
	(snd-print "no GL found!"))))


;;; ---------------- waterfall spectrum ----------------
(define waterfall
  (let* ((drawer #f)
	 (input-port #f)
	 (input-proc 0)
	 (gl-list #f)
	 (slices 256) ; number of traces displayed
	 (slice 0)
	 (data (make-vector slices))
	 (bins 512) ; fft size
	 (input-data #f)
	 (scaler 1.0)  ; data scaler before GL turns it into colors
	 (cutoff 0.2)) ; 0.5 is full spectrum
    
    (define (redraw-graph)
      (let* ((win (XtWindow drawer))
	     (dpy (XtDisplay drawer))
	     (cx (snd-glx-context)))
	(glXMakeCurrent dpy win cx)
	(if gl-list (glDeleteLists gl-list 1))
	(set! gl-list (glGenLists 1))
	(glEnable GL_DEPTH_TEST)
	(glShadeModel GL_SMOOTH)
	(glClearDepth 1.0)
	(glClearColor 1.0 1.0 1.0 1.0) ; todo: bg color here
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	;;gl_spectrogram(XEN data, XEN gl_list, XEN cutoff, XEN use_dB, XEN min_dB, XEN scale, XEN br, XEN bg, XEN bb)
	(glSpectrogram data gl-list cutoff #f -60.0 scaler 65535 65535 65535)
	(let ((vals (XtVaGetValues drawer (list XmNwidth 0 XmNheight 0))))
	  (glViewport 0 0 (list-ref vals 1) (list-ref vals 3)))
	(glMatrixMode GL_PROJECTION)
	(glLoadIdentity)
	(glRotatef (spectro-x-angle) 1.0 0.0 0.0)
	(glRotatef (spectro-y-angle) 0.0 1.0 0.0)
	(glRotatef (spectro-z-angle) 0.0 0.0 1.0)
	(glScalef (spectro-x-scale) (spectro-y-scale) (spectro-z-scale))
	(glCallList gl-list)
	;; todo: make axis
	(glXSwapBuffers dpy win)
	(glDrawBuffer GL_BACK)))
    
    (define (tick-audio id)
      ;; background process reads incoming audio data, creates spectrum, displays next trace
      (mus-audio-read input-port input-data (* bins 2))
      (let ((rl-data (sound-data->vct input-data 0 (vector-ref data slice))))
	(snd-spectrum rl-data blackman2-window bins #t 0.0 #t #f)
	(redraw-graph))
      (set! slice (1+ slice))
      (if (>= slice slices)
	  (set! slice 0))
      #f)
    
    (define (stop-it)
      ;; turn off the waterfall display
      (if input-port
	  (begin
	    (mus-audio-close input-port)
	    (set! input-port #f)))
      (if (XtWorkProcId? input-proc)
	  (begin
	    (XtRemoveWorkProc input-proc)
	    (set! input-proc 0)))
      (do ((i 0 (1+ i)))
	  ((= i slices))
	(vct-scale! (vector-ref data i) 0.0)))
    
    (define (start-it)
      (define (add-main-pane name type args)
	(XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))
      (if (not drawer)
	  (let* ((outer (add-main-pane "Waterfall" xmFormWidgetClass
				       (list XmNbackground (basic-color)
					     XmNpaneMinimum 320))))
	    (set! drawer (XtCreateManagedWidget "draw" xmDrawingAreaWidgetClass outer
						(list XmNbackground       (graph-color)
						      XmNforeground       (data-color)
						      XmNleftAttachment   XmATTACH_FORM
						      XmNtopAttachment    XmATTACH_FORM
						      XmNbottomAttachment XmATTACH_FORM
						      XmNrightAttachment  XmATTACH_FORM)))
	    (XtAddCallback drawer XmNresizeCallback (lambda (w context info) (redraw-graph)))
	    (XtAddCallback drawer XmNexposeCallback (lambda (w context info) (redraw-graph)))
	    (add-hook! orientation-hook (lambda () (redraw-graph)))
	    (add-hook! color-hook (lambda () (redraw-graph)))))
      ;; start the waterfall display
      (if (not (or input-port (XtWorkProcId? input-proc)))
	  (begin
	    (set! input-port (mus-audio-open-input mus-audio-default 22050 1 mus-lshort 512))
	    (set! input-proc (XtAppAddWorkProc (car (main-widgets)) tick-audio)))))
    
    ;; turn display with orientation dialog
    ;;  for example: x-angle 290, y angle: 60
    
    (lambda* (start :optional scl pc-spectrum fft-size)
	     (if start
		 (begin
		   (set! cutoff pc-spectrum)
		   (set! scaler scl)
		   (set! bins fft-size)
		   (set! input-data (make-sound-data 1 (* bins 2)))
		   (do ((i 0 (1+ i)))
		       ((= i slices))
		     (vector-set! data i (make-vct bins)))
		   (start-it))
		 (stop-it)))))

  
(define* (start-waterfall :optional (scl 1.0) (pc-spectrum 0.2) (fft-size 512))
  "(start-waterfall (scl 1.0) (pc-spectrum 0.2) (fft-size 512)) starts a 'waterfall' spectrum display of the incoming audio data"
  (waterfall #t scl pc-spectrum fft-size))

(define (stop-waterfall)
  "(stop-waterfall) stops a waterfall display"
  (waterfall #f))


;;; -------- dump GL state

(define (gl-dump-state)
  ;; based on Mesa/util/dumpstate.c by Stephane Rehel

  (display (format #f "GL_CURRENT_COLOR: ~A~%" (glGetFloatv GL_CURRENT_COLOR)))
  (display (format #f "GL_CURRENT_INDEX: ~A~%" (glGetIntegerv GL_CURRENT_INDEX)))
  (display (format #f "GL_CURRENT_TEXTURE_COORDS: ~A~%" (glGetFloatv GL_CURRENT_TEXTURE_COORDS)))
  (display (format #f "GL_CURRENT_NORMAL: ~A~%" (glGetFloatv GL_CURRENT_NORMAL)))
  (display (format #f "GL_CURRENT_RASTER_POSITION: ~A~%" (glGetFloatv GL_CURRENT_RASTER_POSITION)))
  (display (format #f "GL_CURRENT_RASTER_DISTANCE: ~A~%" (glGetFloatv GL_CURRENT_RASTER_DISTANCE)))
  (display (format #f "GL_CURRENT_RASTER_COLOR: ~A~%" (glGetFloatv GL_CURRENT_RASTER_COLOR)))
  (display (format #f "GL_CURRENT_RASTER_INDEX: ~A~%" (glGetIntegerv GL_CURRENT_RASTER_INDEX)))
  (display (format #f "GL_CURRENT_RASTER_TEXTURE_COORDS: ~A~%" (glGetFloatv GL_CURRENT_RASTER_TEXTURE_COORDS)))
  (display (format #f "GL_CURRENT_RASTER_POSITION_VALID: ~A~%" (glGetBooleanv GL_CURRENT_RASTER_POSITION_VALID)))
  (display (format #f "GL_EDGE_FLAG: ~A~%" (glGetBooleanv GL_EDGE_FLAG)))
  (display (format #f "GL_VERTEX_ARRAY: ~A~%" (glGetBooleanv GL_VERTEX_ARRAY)))
  (display (format #f "GL_VERTEX_ARRAY_SIZE: ~A~%" (glGetIntegerv GL_VERTEX_ARRAY_SIZE)))
  (display (format #f "GL_VERTEX_ARRAY_TYPE: ~A~%" (glGetIntegerv GL_VERTEX_ARRAY_TYPE)))
  (display (format #f "GL_VERTEX_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_VERTEX_ARRAY_STRIDE)))
  (display (format #f "GL_VERTEX_ARRAY_POINTER: ~A~%" (glGetPointerv GL_VERTEX_ARRAY_POINTER)))
  (display (format #f "GL_NORMAL_ARRAY: ~A~%" (glGetBooleanv GL_NORMAL_ARRAY)))
  (display (format #f "GL_NORMAL_ARRAY_TYPE: ~A~%" (glGetIntegerv GL_NORMAL_ARRAY_TYPE)))
  (display (format #f "GL_NORMAL_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_NORMAL_ARRAY_STRIDE)))
  (display (format #f "GL_NORMAL_ARRAY_POINTER: ~A~%" (glGetPointerv GL_NORMAL_ARRAY_POINTER)))
  (display (format #f "GL_COLOR_ARRAY: ~A~%" (glGetBooleanv GL_COLOR_ARRAY)))
  (display (format #f "GL_COLOR_ARRAY_SIZE: ~A~%" (glGetIntegerv GL_COLOR_ARRAY_SIZE)))
  (display (format #f "GL_COLOR_ARRAY_TYPE: ~A~%" (glGetIntegerv GL_COLOR_ARRAY_TYPE)))
  (display (format #f "GL_COLOR_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_COLOR_ARRAY_STRIDE)))
  (display (format #f "GL_COLOR_ARRAY_POINTER: ~A~%" (glGetPointerv GL_COLOR_ARRAY_POINTER)))
  (display (format #f "GL_INDEX_ARRAY: ~A~%" (glGetBooleanv GL_INDEX_ARRAY)))
  (display (format #f "GL_INDEX_ARRAY_TYPE: ~A~%" (glGetIntegerv GL_INDEX_ARRAY_TYPE)))
  (display (format #f "GL_INDEX_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_INDEX_ARRAY_STRIDE)))
  (display (format #f "GL_INDEX_ARRAY_POINTER: ~A~%" (glGetPointerv GL_INDEX_ARRAY_POINTER)))
  (display (format #f "GL_TEXTURE_COORD_ARRAY: ~A~%" (glGetBooleanv GL_TEXTURE_COORD_ARRAY)))
  (display (format #f "GL_TEXTURE_COORD_ARRAY_SIZE: ~A~%" (glGetIntegerv GL_TEXTURE_COORD_ARRAY_SIZE)))
  (display (format #f "GL_TEXTURE_COORD_ARRAY_TYPE: ~A~%" (glGetIntegerv GL_TEXTURE_COORD_ARRAY_TYPE)))
  (display (format #f "GL_TEXTURE_COORD_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_TEXTURE_COORD_ARRAY_STRIDE)))
  (display (format #f "GL_TEXTURE_COORD_ARRAY_POINTER: ~A~%" (glGetPointerv GL_TEXTURE_COORD_ARRAY_POINTER)))
  (display (format #f "GL_EDGE_FLAG_ARRAY: ~A~%" (glGetBooleanv GL_EDGE_FLAG_ARRAY)))
  (display (format #f "GL_EDGE_FLAG_ARRAY_STRIDE: ~A~%" (glGetIntegerv GL_EDGE_FLAG_ARRAY_STRIDE)))
  (display (format #f "GL_EDGE_FLAG_ARRAY_POINTER: ~A~%" (glGetPointerv GL_EDGE_FLAG_ARRAY_POINTER)))
  (display (format #f "GL_MODELVIEW_MATRIX: ~A~%" (glGetFloatv GL_MODELVIEW_MATRIX)))
  (display (format #f "GL_PROJECTION_MATRIX: ~A~%" (glGetFloatv GL_PROJECTION_MATRIX)))
  (display (format #f "GL_TEXTURE_MATRIX: ~A~%" (glGetFloatv GL_TEXTURE_MATRIX)))
  (display (format #f "GL_VIEWPORT: ~A~%" (glGetIntegerv GL_VIEWPORT)))
  (display (format #f "GL_DEPTH_RANGE: ~A~%" (glGetFloatv GL_DEPTH_RANGE)))
  (display (format #f "GL_MODELVIEW_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MODELVIEW_STACK_DEPTH)))
  (display (format #f "GL_PROJECTION_STACK_DEPTH: ~A~%" (glGetIntegerv GL_PROJECTION_STACK_DEPTH)))
  (display (format #f "GL_TEXTURE_STACK_DEPTH: ~A~%" (glGetIntegerv GL_TEXTURE_STACK_DEPTH)))
  (display (format #f "GL_MATRIX_MODE: ~A~%" (glGetIntegerv GL_MATRIX_MODE)))
  (display (format #f "GL_NORMALIZE: ~A~%" (glGetBooleanv GL_NORMALIZE)))
  (display (format #f "GL_CLIP_PLANE0: ~A~%" (glGetBooleanv GL_CLIP_PLANE0)))
  (display (format #f "GL_CLIP_PLANE1: ~A~%" (glGetBooleanv GL_CLIP_PLANE1)))
  (display (format #f "GL_CLIP_PLANE2: ~A~%" (glGetBooleanv GL_CLIP_PLANE2)))
  (display (format #f "GL_CLIP_PLANE3: ~A~%" (glGetBooleanv GL_CLIP_PLANE3)))
  (display (format #f "GL_CLIP_PLANE4: ~A~%" (glGetBooleanv GL_CLIP_PLANE4)))
  (display (format #f "GL_CLIP_PLANE5: ~A~%" (glGetBooleanv GL_CLIP_PLANE5)))
  (display (format #f "GL_FOG_COLOR: ~A~%" (glGetFloatv GL_FOG_COLOR)))
  (display (format #f "GL_FOG_INDEX: ~A~%" (glGetIntegerv GL_FOG_INDEX)))
  (display (format #f "GL_FOG_DENSITY: ~A~%" (glGetFloatv GL_FOG_DENSITY)))
  (display (format #f "GL_FOG_START: ~A~%" (glGetFloatv GL_FOG_START)))
  (display (format #f "GL_FOG_END: ~A~%" (glGetFloatv GL_FOG_END)))
  (display (format #f "GL_FOG_MODE: ~A~%" (glGetIntegerv GL_FOG_MODE)))
  (display (format #f "GL_FOG: ~A~%" (glGetBooleanv GL_FOG)))
  (display (format #f "GL_SHADE_MODEL: ~A~%" (glGetIntegerv GL_SHADE_MODEL)))
  (display (format #f "GL_LIGHTING: ~A~%" (glGetBooleanv GL_LIGHTING)))
  (display (format #f "GL_COLOR_MATERIAL: ~A~%" (glGetBooleanv GL_COLOR_MATERIAL)))
  (display (format #f "GL_COLOR_MATERIAL_PARAMETER: ~A~%" (glGetIntegerv GL_COLOR_MATERIAL_PARAMETER)))
  (display (format #f "GL_COLOR_MATERIAL_FACE: ~A~%" (glGetIntegerv GL_COLOR_MATERIAL_FACE)))
  (display (format #f "GL_BACK GL_AMBIENT: ~A~%" (glGetMaterialfv GL_BACK GL_AMBIENT)))
  (display (format #f "GL_FRONT GL_AMBIENT: ~A~%" (glGetMaterialfv GL_FRONT GL_AMBIENT)))
  (display (format #f "GL_BACK GL_DIFFUSE: ~A~%" (glGetMaterialfv GL_BACK GL_DIFFUSE)))
  (display (format #f "GL_FRONT GL_DIFFUSE: ~A~%" (glGetMaterialfv GL_FRONT GL_DIFFUSE)))
  (display (format #f "GL_BACK GL_SPECULAR: ~A~%" (glGetMaterialfv GL_BACK GL_SPECULAR)))
  (display (format #f "GL_FRONT GL_SPECULAR: ~A~%" (glGetMaterialfv GL_FRONT GL_SPECULAR)))
  (display (format #f "GL_BACK GL_EMISSION: ~A~%" (glGetMaterialfv GL_BACK GL_EMISSION)))
  (display (format #f "GL_FRONT GL_EMISSION: ~A~%" (glGetMaterialfv GL_FRONT GL_EMISSION)))
  (display (format #f "GL_BACK GL_SHININESS: ~A~%" (glGetMaterialfv GL_BACK GL_SHININESS)))
  (display (format #f "GL_FRONT GL_SHININESS: ~A~%" (glGetMaterialfv GL_FRONT GL_SHININESS)))
  (display (format #f "GL_LIGHT_MODEL_AMBIENT: ~A~%" (glGetFloatv GL_LIGHT_MODEL_AMBIENT)))
  (display (format #f "GL_LIGHT_MODEL_LOCAL_VIEWER: ~A~%" (glGetBooleanv GL_LIGHT_MODEL_LOCAL_VIEWER)))
  (display (format #f "GL_LIGHT_MODEL_TWO_SIDE: ~A~%" (glGetBooleanv GL_LIGHT_MODEL_TWO_SIDE)))

  (let ((nlights (car (glGetIntegerv GL_MAX_LIGHTS))))
    (do ((i 0 (1+ i)))
	((= i nlights))
      (if (car (glGetBooleanv (+ GL_LIGHT0  i)))
	  (begin

	    (glGetFloatv i AMBIENT)
	    (glGetFloatv i DIFFUSE)
	    (glGetFloatv i SPECULAR)
	    (glGetLightfv i POSITION)
	    (glGetLightfv i CONSTANT_ATTENUATION)
	    (glGetLightfv i LINEAR_ATTENUATION)
	    (glGetLightfv i QUADRATIC_ATTENUATION)
	    (glGetLightfv i SPOT_DIRECTION)
	    (glGetLightfv i SPOT_EXPONENT)
	    (glGetLightfv i SPOT_CUTOFF)
	    ))))

  (display (format #f "GL_POINT_SIZE: ~A~%" (glGetFloatv GL_POINT_SIZE)))
  (display (format #f "GL_POINT_SMOOTH: ~A~%" (glGetBooleanv GL_POINT_SMOOTH)))
  (display (format #f "GL_LINE_WIDTH: ~A~%" (glGetFloatv GL_LINE_WIDTH)))
  (display (format #f "GL_LINE_SMOOTH: ~A~%" (glGetBooleanv GL_LINE_SMOOTH)))
  (display (format #f "GL_LINE_STIPPLE_PATTERN: ~A~%" (glGetIntegerv GL_LINE_STIPPLE_PATTERN)))
  (display (format #f "GL_LINE_STIPPLE_REPEAT: ~A~%" (glGetIntegerv GL_LINE_STIPPLE_REPEAT)))
  (display (format #f "GL_LINE_STIPPLE: ~A~%" (glGetBooleanv GL_LINE_STIPPLE)))
  (display (format #f "GL_CULL_FACE: ~A~%" (glGetBooleanv GL_CULL_FACE)))
  (display (format #f "GL_CULL_FACE_MODE: ~A~%" (glGetIntegerv GL_CULL_FACE_MODE)))
  (display (format #f "GL_FRONT_FACE: ~A~%" (glGetIntegerv GL_FRONT_FACE)))
  (display (format #f "GL_POLYGON_SMOOTH: ~A~%" (glGetBooleanv GL_POLYGON_SMOOTH)))
  (display (format #f "GL_POLYGON_MODE: ~A~%" (glGetIntegerv GL_POLYGON_MODE)))
  (display (format #f "GL_POLYGON_OFFSET_FACTOR: ~A~%" (glGetFloatv GL_POLYGON_OFFSET_FACTOR)))
  (display (format #f "GL_POLYGON_OFFSET_UNITS: ~A~%" (glGetFloatv GL_POLYGON_OFFSET_UNITS)))
  (display (format #f "GL_POLYGON_OFFSET_POINT: ~A~%" (glGetBooleanv GL_POLYGON_OFFSET_POINT)))
  (display (format #f "GL_POLYGON_OFFSET_LINE: ~A~%" (glGetBooleanv GL_POLYGON_OFFSET_LINE)))
  (display (format #f "GL_POLYGON_OFFSET_FILL: ~A~%" (glGetBooleanv GL_POLYGON_OFFSET_FILL)))
  (display (format #f "GL_POLYGON_STIPPLE: ~A~%" (glGetBooleanv GL_POLYGON_STIPPLE)))
  (display (format #f "GL_TEXTURE_1D: ~A~%" (glGetBooleanv GL_TEXTURE_1D)))
  (display (format #f "GL_TEXTURE_2D: ~A~%" (glGetBooleanv GL_TEXTURE_2D)))
  (display (format #f "GL_TEXTURE_BINDING_1D: ~A~%" (glGetIntegerv GL_TEXTURE_BINDING_1D)))
  (display (format #f "GL_TEXTURE_BINDING_2D: ~A~%" (glGetIntegerv GL_TEXTURE_BINDING_2D)))
  (display (format #f "GL_TEXTURE_GEN_S: ~A~%" (glGetBooleanv GL_TEXTURE_GEN_S)))
  (display (format #f "GL_TEXTURE_GEN_T: ~A~%" (glGetBooleanv GL_TEXTURE_GEN_T)))
  (display (format #f "GL_TEXTURE_GEN_R: ~A~%" (glGetBooleanv GL_TEXTURE_GEN_R)))
  (display (format #f "GL_TEXTURE_GEN_Q: ~A~%" (glGetBooleanv GL_TEXTURE_GEN_Q)))
  (display (format #f "GL_SCISSOR_TEST: ~A~%" (glGetBooleanv GL_SCISSOR_TEST)))
  (display (format #f "GL_SCISSOR_BOX: ~A~%" (glGetIntegerv GL_SCISSOR_BOX)))
  (display (format #f "GL_ALPHA_TEST: ~A~%" (glGetBooleanv GL_ALPHA_TEST)))
  (display (format #f "GL_ALPHA_TEST_FUNC: ~A~%" (glGetIntegerv GL_ALPHA_TEST_FUNC)))
  (display (format #f "GL_ALPHA_TEST_REF: ~A~%" (glGetFloatv GL_ALPHA_TEST_REF)))
  (display (format #f "GL_STENCIL_TEST: ~A~%" (glGetBooleanv GL_STENCIL_TEST)))
  (display (format #f "GL_STENCIL_FUNC: ~A~%" (glGetIntegerv GL_STENCIL_FUNC)))
  (display (format #f "GL_STENCIL_VALUE_MASK: ~A~%" (glGetIntegerv GL_STENCIL_VALUE_MASK)))
  (display (format #f "GL_STENCIL_REF: ~A~%" (glGetIntegerv GL_STENCIL_REF)))
  (display (format #f "GL_STENCIL_FAIL: ~A~%" (glGetIntegerv GL_STENCIL_FAIL)))
  (display (format #f "GL_STENCIL_PASS_DEPTH_FAIL: ~A~%" (glGetIntegerv GL_STENCIL_PASS_DEPTH_FAIL)))
  (display (format #f "GL_STENCIL_PASS_DEPTH_PASS: ~A~%" (glGetIntegerv GL_STENCIL_PASS_DEPTH_PASS)))
  (display (format #f "GL_DEPTH_TEST: ~A~%" (glGetBooleanv GL_DEPTH_TEST)))
  (display (format #f "GL_DEPTH_FUNC: ~A~%" (glGetIntegerv GL_DEPTH_FUNC)))
  (display (format #f "GL_BLEND: ~A~%" (glGetBooleanv GL_BLEND)))
  (display (format #f "GL_BLEND_SRC: ~A~%" (glGetIntegerv GL_BLEND_SRC)))
  (display (format #f "GL_BLEND_DST: ~A~%" (glGetIntegerv GL_BLEND_DST)))
  (display (format #f "GL_DITHER: ~A~%" (glGetBooleanv GL_DITHER)))
  (display (format #f "GL_LOGIC_OP: ~A~%" (glGetBooleanv GL_LOGIC_OP)))
  (display (format #f "GL_COLOR_LOGIC_OP: ~A~%" (glGetBooleanv GL_COLOR_LOGIC_OP)))
  (display (format #f "GL_DRAW_BUFFER: ~A~%" (glGetIntegerv GL_DRAW_BUFFER)))
  (display (format #f "GL_INDEX_WRITEMASK: ~A~%" (glGetIntegerv GL_INDEX_WRITEMASK)))
  (display (format #f "GL_COLOR_WRITEMASK: ~A~%" (glGetBooleanv GL_COLOR_WRITEMASK)))
  (display (format #f "GL_DEPTH_WRITEMASK: ~A~%" (glGetBooleanv GL_DEPTH_WRITEMASK)))
  (display (format #f "GL_STENCIL_WRITEMASK: ~A~%" (glGetIntegerv GL_STENCIL_WRITEMASK)))
  (display (format #f "GL_COLOR_CLEAR_VALUE: ~A~%" (glGetFloatv GL_COLOR_CLEAR_VALUE)))
  (display (format #f "GL_INDEX_CLEAR_VALUE: ~A~%" (glGetIntegerv GL_INDEX_CLEAR_VALUE)))
  (display (format #f "GL_DEPTH_CLEAR_VALUE: ~A~%" (glGetFloatv GL_DEPTH_CLEAR_VALUE)))
  (display (format #f "GL_STENCIL_CLEAR_VALUE: ~A~%" (glGetIntegerv GL_STENCIL_CLEAR_VALUE)))
  (display (format #f "GL_ACCUM_CLEAR_VALUE: ~A~%" (glGetFloatv GL_ACCUM_CLEAR_VALUE)))
  (display (format #f "GL_UNPACK_SWAP_BYTES: ~A~%" (glGetBooleanv GL_UNPACK_SWAP_BYTES)))
  (display (format #f "GL_UNPACK_LSB_FIRST: ~A~%" (glGetBooleanv GL_UNPACK_LSB_FIRST)))
  (display (format #f "GL_UNPACK_ROW_LENGTH: ~A~%" (glGetIntegerv GL_UNPACK_ROW_LENGTH)))
  (display (format #f "GL_UNPACK_SKIP_ROWS: ~A~%" (glGetIntegerv GL_UNPACK_SKIP_ROWS)))
  (display (format #f "GL_UNPACK_SKIP_PIXELS: ~A~%" (glGetIntegerv GL_UNPACK_SKIP_PIXELS)))
  (display (format #f "GL_UNPACK_ALIGNMENT: ~A~%" (glGetIntegerv GL_UNPACK_ALIGNMENT)))
  (display (format #f "GL_PACK_SWAP_BYTES: ~A~%" (glGetBooleanv GL_PACK_SWAP_BYTES)))
  (display (format #f "GL_PACK_LSB_FIRST: ~A~%" (glGetBooleanv GL_PACK_LSB_FIRST)))
  (display (format #f "GL_PACK_ROW_LENGTH: ~A~%" (glGetIntegerv GL_PACK_ROW_LENGTH)))
  (display (format #f "GL_PACK_SKIP_ROWS: ~A~%" (glGetIntegerv GL_PACK_SKIP_ROWS)))
  (display (format #f "GL_PACK_SKIP_PIXELS: ~A~%" (glGetIntegerv GL_PACK_SKIP_PIXELS)))
  (display (format #f "GL_PACK_ALIGNMENT: ~A~%" (glGetIntegerv GL_PACK_ALIGNMENT)))
  (display (format #f "GL_MAP_COLOR: ~A~%" (glGetBooleanv GL_MAP_COLOR)))
  (display (format #f "GL_MAP_STENCIL: ~A~%" (glGetBooleanv GL_MAP_STENCIL)))
  (display (format #f "GL_INDEX_SHIFT: ~A~%" (glGetIntegerv GL_INDEX_SHIFT)))
  (display (format #f "GL_INDEX_OFFSET: ~A~%" (glGetIntegerv GL_INDEX_OFFSET)))
  (display (format #f "GL_RED_SCALE: ~A~%" (glGetFloatv GL_RED_SCALE)))
  (display (format #f "GL_GREEN_SCALE: ~A~%" (glGetFloatv GL_GREEN_SCALE)))
  (display (format #f "GL_BLUE_SCALE: ~A~%" (glGetFloatv GL_BLUE_SCALE)))
  (display (format #f "GL_ALPHA_SCALE: ~A~%" (glGetFloatv GL_ALPHA_SCALE)))
  (display (format #f "GL_DEPTH_SCALE: ~A~%" (glGetFloatv GL_DEPTH_SCALE)))
  (display (format #f "GL_RED_BIAS: ~A~%" (glGetFloatv GL_RED_BIAS)))
  (display (format #f "GL_GREEN_BIAS: ~A~%" (glGetFloatv GL_GREEN_BIAS)))
  (display (format #f "GL_BLUE_BIAS: ~A~%" (glGetFloatv GL_BLUE_BIAS)))
  (display (format #f "GL_ALPHA_BIAS: ~A~%" (glGetFloatv GL_ALPHA_BIAS)))
  (display (format #f "GL_DEPTH_BIAS: ~A~%" (glGetFloatv GL_DEPTH_BIAS)))
  (display (format #f "GL_ZOOM_X: ~A~%" (glGetFloatv GL_ZOOM_X)))
  (display (format #f "GL_ZOOM_Y: ~A~%" (glGetFloatv GL_ZOOM_Y)))
  (display (format #f "GL_READ_BUFFER: ~A~%" (glGetIntegerv GL_READ_BUFFER)))
  (display (format #f "GL_AUTO_NORMAL: ~A~%" (glGetBooleanv GL_AUTO_NORMAL)))
  (display (format #f "GL_PERSPECTIVE_CORRECTION_HINT: ~A~%" (glGetIntegerv GL_PERSPECTIVE_CORRECTION_HINT)))
  (display (format #f "GL_POINT_SMOOTH_HINT: ~A~%" (glGetIntegerv GL_POINT_SMOOTH_HINT)))
  (display (format #f "GL_LINE_SMOOTH_HINT: ~A~%" (glGetIntegerv GL_LINE_SMOOTH_HINT)))
  (display (format #f "GL_POLYGON_SMOOTH_HINT: ~A~%" (glGetIntegerv GL_POLYGON_SMOOTH_HINT)))
  (display (format #f "GL_FOG_HINT: ~A~%" (glGetIntegerv GL_FOG_HINT)))
  (display (format #f "GL_MAX_LIGHTS: ~A~%" (glGetIntegerv GL_MAX_LIGHTS)))
  (display (format #f "GL_MAX_CLIP_PLANES: ~A~%" (glGetIntegerv GL_MAX_CLIP_PLANES)))
  (display (format #f "GL_MAX_MODELVIEW_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_MODELVIEW_STACK_DEPTH)))
  (display (format #f "GL_MAX_PROJECTION_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_PROJECTION_STACK_DEPTH)))
  (display (format #f "GL_MAX_TEXTURE_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_TEXTURE_STACK_DEPTH)))
  (display (format #f "GL_SUBPIXEL_BITS: ~A~%" (glGetIntegerv GL_SUBPIXEL_BITS)))
  (display (format #f "GL_MAX_TEXTURE_SIZE: ~A~%" (glGetIntegerv GL_MAX_TEXTURE_SIZE)))
  (display (format #f "GL_MAX_PIXEL_MAP_TABLE: ~A~%" (glGetIntegerv GL_MAX_PIXEL_MAP_TABLE)))
  (display (format #f "GL_MAX_NAME_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_NAME_STACK_DEPTH)))
  (display (format #f "GL_MAX_LIST_NESTING: ~A~%" (glGetIntegerv GL_MAX_LIST_NESTING)))
  (display (format #f "GL_MAX_EVAL_ORDER: ~A~%" (glGetIntegerv GL_MAX_EVAL_ORDER)))
  (display (format #f "GL_MAX_VIEWPORT_DIMS: ~A~%" (glGetIntegerv GL_MAX_VIEWPORT_DIMS)))
  (display (format #f "GL_MAX_ATTRIB_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_ATTRIB_STACK_DEPTH)))
  (display (format #f "GL_MAX_CLIENT_ATTRIB_STACK_DEPTH: ~A~%" (glGetIntegerv GL_MAX_CLIENT_ATTRIB_STACK_DEPTH)))
  (display (format #f "GL_AUX_BUFFERS: ~A~%" (glGetIntegerv GL_AUX_BUFFERS)))
  (display (format #f "GL_RGBA_MODE: ~A~%" (glGetBooleanv GL_RGBA_MODE)))
  (display (format #f "GL_INDEX_MODE: ~A~%" (glGetBooleanv GL_INDEX_MODE)))
  (display (format #f "GL_DOUBLEBUFFER: ~A~%" (glGetBooleanv GL_DOUBLEBUFFER)))
  (display (format #f "GL_STEREO: ~A~%" (glGetBooleanv GL_STEREO)))
  (display (format #f "GL_LINE_WIDTH_RANGE: ~A~%" (glGetFloatv GL_LINE_WIDTH_RANGE)))
  (display (format #f "GL_LINE_WIDTH_GRANULARITY: ~A~%" (glGetFloatv GL_LINE_WIDTH_GRANULARITY)))
  (display (format #f "GL_RED_BITS: ~A~%" (glGetIntegerv GL_RED_BITS)))
  (display (format #f "GL_GREEN_BITS: ~A~%" (glGetIntegerv GL_GREEN_BITS)))
  (display (format #f "GL_BLUE_BITS: ~A~%" (glGetIntegerv GL_BLUE_BITS)))
  (display (format #f "GL_ALPHA_BITS: ~A~%" (glGetIntegerv GL_ALPHA_BITS)))
  (display (format #f "GL_INDEX_BITS: ~A~%" (glGetIntegerv GL_INDEX_BITS)))
  (display (format #f "GL_DEPTH_BITS: ~A~%" (glGetIntegerv GL_DEPTH_BITS)))
  (display (format #f "GL_STENCIL_BITS: ~A~%" (glGetIntegerv GL_STENCIL_BITS)))
  (display (format #f "GL_ACCUM_RED_BITS: ~A~%" (glGetIntegerv GL_ACCUM_RED_BITS)))
  (display (format #f "GL_ACCUM_GREEN_BITS: ~A~%" (glGetIntegerv GL_ACCUM_GREEN_BITS)))
  (display (format #f "GL_ACCUM_BLUE_BITS: ~A~%" (glGetIntegerv GL_ACCUM_BLUE_BITS)))
  (display (format #f "GL_ACCUM_ALPHA_BITS: ~A~%" (glGetIntegerv GL_ACCUM_ALPHA_BITS)))
  (display (format #f "GL_LIST_BASE: ~A~%" (glGetIntegerv GL_LIST_BASE)))
  (display (format #f "GL_LIST_INDEX: ~A~%" (glGetIntegerv GL_LIST_INDEX)))
  (display (format #f "GL_LIST_MODE: ~A~%" (glGetIntegerv GL_LIST_MODE)))
  (display (format #f "GL_ATTRIB_STACK_DEPTH: ~A~%" (glGetIntegerv GL_ATTRIB_STACK_DEPTH)))
  (display (format #f "GL_CLIENT_ATTRIB_STACK_DEPTH: ~A~%" (glGetIntegerv GL_CLIENT_ATTRIB_STACK_DEPTH)))
  (display (format #f "GL_NAME_STACK_DEPTH: ~A~%" (glGetIntegerv GL_NAME_STACK_DEPTH)))
  (display (format #f "GL_RENDER_MODE: ~A~%" (glGetIntegerv GL_RENDER_MODE)))
  (display (format #f "GL_SELECTION_BUFFER_POINTER: ~A~%" (glGetPointerv GL_SELECTION_BUFFER_POINTER)))
  (display (format #f "GL_SELECTION_BUFFER_SIZE: ~A~%" (glGetIntegerv GL_SELECTION_BUFFER_SIZE)))
  (display (format #f "GL_FEEDBACK_BUFFER_POINTER: ~A~%" (glGetPointerv GL_FEEDBACK_BUFFER_POINTER)))
  (display (format #f "GL_FEEDBACK_BUFFER_SIZE: ~A~%" (glGetIntegerv GL_FEEDBACK_BUFFER_SIZE)))
  (display (format #f "GL_FEEDBACK_BUFFER_TYPE: ~A~%" (glGetIntegerv GL_FEEDBACK_BUFFER_TYPE)))
)


;;; -------- complexify --------

(define complexify
  (let* ((gl-list #f)
	 (drawer #f))
    
    (define (redraw-graph)
      (let* ((win (XtWindow drawer))
	     (dpy (XtDisplay drawer))
	     (cx (snd-glx-context)))
	(glXMakeCurrent dpy win cx)
	(if gl-list (glDeleteLists gl-list 1))
	(set! gl-list (glGenLists 1))
	(glEnable GL_DEPTH_TEST)
	(glShadeModel GL_SMOOTH)
	(glClearDepth 1.0)
	(glClearColor 1.0 1.0 1.0 1.0)
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(let* ((rl (channel->vct (left-sample) 512))
	       (im (make-vct 512 0.0)))
	  (mus-fft rl im)
	  (let ((peak (* 2 (max (vct-peak rl) (vct-peak im)))))
	    (vct-scale! rl (/ 1.0 peak))
	    (vct-scale! im (/ 1.0 peak)))
	  ;; display each element in the complex plane rotated to stack along the x axis
	  (glNewList gl-list GL_COMPILE)
	    (glBegin GL_LINES)
	    (apply glColor3f (color->list (data-color)))
	    (do ((i 0 (1+ i)))
		((= i 256))
	      (glVertex3f (/ i 256.0) 0.0 0.0)
	      (glVertex3f (/ i 256.0) (vct-ref rl i) (vct-ref im i)))
	    (glEnd)
	  (glEndList))
	(let ((vals (XtVaGetValues drawer (list XmNwidth 0 XmNheight 0))))
	  (glViewport 0 0 (list-ref vals 1) (list-ref vals 3)))
	(glMatrixMode GL_PROJECTION)
	(glLoadIdentity)
	(glOrtho -0.2 1.0 -1.5 1.0 -1.0 1.0)
	(glRotatef (spectro-x-angle) 1.0 0.0 0.0)
	(glRotatef (spectro-y-angle) 0.0 1.0 0.0)
	(glRotatef (spectro-z-angle) 0.0 0.0 1.0)
	(glScalef (spectro-x-scale) (spectro-y-scale) (spectro-z-scale))
	(glCallList gl-list)
	(glXSwapBuffers dpy win)
	(glDrawBuffer GL_BACK)))
    
      (define (add-main-pane name type args)
	(XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))

      (lambda ()
	(if (not drawer)
	    (let* ((outer (add-main-pane "Waterfall" xmFormWidgetClass
					 (list XmNbackground (basic-color)
					       XmNpaneMinimum 320))))
	      (set! drawer (XtCreateManagedWidget "draw" xmDrawingAreaWidgetClass outer
						  (list XmNbackground       (graph-color)
							XmNforeground       (data-color)
							XmNleftAttachment   XmATTACH_FORM
							XmNtopAttachment    XmATTACH_FORM
							XmNbottomAttachment XmATTACH_FORM
							XmNrightAttachment  XmATTACH_FORM)))
	      (set! (spectro-x-angle) 210.0)
	      (set! (spectro-y-angle) 60.0)
	      (set! (spectro-z-angle) 30.0)
	      (set! (spectro-x-scale) 3.0)
	      (XtAddCallback drawer XmNresizeCallback (lambda (w context info) (redraw-graph)))
	      (XtAddCallback drawer XmNexposeCallback (lambda (w context info) (redraw-graph)))
	      (add-hook! after-graph-hook (lambda (s c) (redraw-graph)))
	      (add-hook! orientation-hook (lambda () (redraw-graph)))
	      (add-hook! color-hook (lambda () (redraw-graph))))))))
