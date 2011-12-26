(in-package :cl-user)

(defvar *libportaudio*
  (let ((type #+(or darwin macos macosx) "dylib"
              #+(or linux linux-target (and unix pc386) freebsd) "so"
              #+(or win32 microsoft-32 cygwin) "dll")
        (paths (list (truename *load-pathname*) "/usr/local/lib/"
		     "/usr/lib/")))
    (loop for d in paths
       for p = (make-pathname :name "libportaudio" :type type
                              :defaults d)
       when (probe-file p) do (return p)
       finally  
         (error "Library \"libportaudio.~A\" not found. Fix cl-user::*libportaudio*."
                type))))

(cffi:load-foreign-library *libportaudio*)

(defpackage :portaudio
  (:use :common-lisp :cffi))

(in-package #:portaudio)

(defctype :float* :pointer)
(defctype :off_t :long) ;or long long?

(defcfun ("Pa_GetVersion" Pa_GetVersion) :int)

(defcfun ("Pa_GetVersionText" Pa_GetVersionText) :string)
#|
(defcenum PaErrorCode
	(:paNoError 0)
	(:paNotInitialized -10000)
	:paUnanticipatedHostError
	:paInvalidChannelCount
	:paInvalidSampleRate
	:paInvalidDevice
	:paInvalidFlag
	:paSampleFormatNotSupported
	:paBadIODeviceCombination
	:paInsufficientMemory
	:paBufferTooBig
	:paBufferTooSmall
	:paNullCallback
	:paBadStreamPtr
	:paTimedOut
	:paInternalError
	:paDeviceUnavailable
	:paIncompatibleHostApiSpecificStreamInfo
	:paStreamIsStopped
	:paStreamIsNotStopped
	:paInputOverflowed
	:paOutputUnderflowed
	:paHostApiNotFound
	:paInvalidHostApi
	:paCanNotReadFromACallbackStream
	:paCanNotWriteToACallbackStream
	:paCanNotReadFromAnOutputOnlyStream
	:paCanNotWriteToAnInputOnlyStream
	:paIncompatibleStreamHostApi
	:paBadBufferPtr)
|#
(defcfun ("Pa_GetErrorText" Pa_GetErrorText) :string
  (errorCode :int))

(defcfun ("Pa_Initialize" Pa_Initialize) :int)

(defcfun ("Pa_Terminate" Pa_Terminate) :int)

(defcfun ("Pa_GetHostApiCount" Pa_GetHostApiCount) :int)

(defcfun ("Pa_GetDefaultHostApi" Pa_GetDefaultHostApi) :int)
#|
(defcenum PaHostApiTypeId
	(:paInDevelopment 0)
	(:paDirectSound 1)
	(:paMME 2)
	(:paASIO 3)
	(:paSoundManager 4)
	(:paCoreAudio 5)
	(:paOSS 7)
	(:paALSA 8)
	(:paAL 9)
	(:paBeOS 10)
	(:paWDMKS 11)
	(:paJACK 12)
	(:paWASAPI 13)
	(:paAudioScienceHPI 14))

(defcstruct PaHostApiInfo
	(structVersion :int)
	(type :pointer)
	(name :string)
	(deviceCount :int)
	(defaultInputDevice :int)
	(defaultOutputDevice :int))
|#
(defcfun ("Pa_GetHostApiInfo" Pa_GetHostApiInfo) :pointer
  (hostApi :int))

(defcfun ("Pa_HostApiTypeIdToHostApiIndex" Pa_HostApiTypeIdToHostApiIndex) :int
  (type :pointer))

(defcfun ("Pa_HostApiDeviceIndexToDeviceIndex" Pa_HostApiDeviceIndexToDeviceIndex) :int
  (hostApi :int)
  (hostApiDeviceIndex :int))

(defcstruct PaHostErrorInfo
	(hostApiType :pointer)
	(errorCode :long)
	(errorText :string))

(defcfun ("Pa_GetLastHostErrorInfo" Pa_GetLastHostErrorInfo) :pointer)

(defcfun ("Pa_GetDeviceCount" Pa_GetDeviceCount) :int)

(defcfun ("Pa_GetDefaultInputDevice" Pa_GetDefaultInputDevice) :int)

(defcfun ("Pa_GetDefaultOutputDevice" Pa_GetDefaultOutputDevice) :int)

(defcstruct PaDeviceInfo
	(structVersion :int)
	(name :string)
	(hostApi :int)
	(maxInputChannels :int)
	(maxOutputChannels :int)
	(defaultLowInputLatency :double)
	(defaultLowOutputLatency :double)
	(defaultHighInputLatency :double)
	(defaultHighOutputLatency :double)
	(defaultSampleRate :double))

(defcfun ("Pa_GetDeviceInfo" Pa_GetDeviceInfo) :pointer
  (device :int))

(defcstruct PaStreamParameters
	(device :int)
	(channelCount :int)
	(sampleFormat :unsigned-long)
	(suggestedLatency :double)
	(hostApiSpecificStreamInfo :pointer))

(defconstant paFormatIsSupported 0)

(defcfun ("Pa_IsFormatSupported" Pa_IsFormatSupported) :int
  (inputParameters :pointer)
  (outputParameters :pointer)
  (sampleRate :double))

(defconstant paFramesPerBufferUnspecified 0)

(defcstruct PaStreamCallbackTimeInfo
	(inputBufferAdcTime :double)
	(currentTime :double)
	(outputBufferDacTime :double))

#|
(defcenum PaStreamCallbackResult
	(:paContinue 0)
	(:paComplete 1)
	(:paAbort 2))
|#

(defcfun ("Pa_OpenStream" Pa_OpenStream) :int
  (stream :pointer)
  (inputParameters :pointer)
  (outputParameters :pointer)
  (sampleRate :double)
  (framesPerBuffer :unsigned-long)
  (streamFlags :unsigned-long)
  (streamCallback :pointer)
  (userData :pointer))

(defcfun ("Pa_OpenDefaultStream" Pa_OpenDefaultStream) :int
  (stream :pointer)
  (numInputChannels :int)
  (numOutputChannels :int)
  (sampleFormat :unsigned-long)
  (sampleRate :double)
  (framesPerBuffer :unsigned-long)
  (streamCallback :pointer)
  (userData :pointer))

(defcfun ("Pa_CloseStream" Pa_CloseStream) :int
  (stream :pointer))

(defcfun ("Pa_SetStreamFinishedCallback" Pa_SetStreamFinishedCallback) :int
  (stream :pointer)
  (streamFinishedCallback :pointer))

(defcfun ("Pa_StartStream" Pa_StartStream) :int
  (stream :pointer))

(defcfun ("Pa_StopStream" Pa_StopStream) :int
  (stream :pointer))

(defcfun ("Pa_AbortStream" Pa_AbortStream) :int
  (stream :pointer))

(defcfun ("Pa_IsStreamStopped" Pa_IsStreamStopped) :int
  (stream :pointer))

(defcfun ("Pa_IsStreamActive" Pa_IsStreamActive) :int
  (stream :pointer))

(defcstruct PaStreamInfo
	(structVersion :int)
	(inputLatency :double)
	(outputLatency :double)
	(sampleRate :double))

(defcfun ("Pa_GetStreamInfo" Pa_GetStreamInfo) :pointer
  (stream :pointer))

(defcfun ("Pa_GetStreamTime" Pa_GetStreamTime) :double
  (stream :pointer))

(defcfun ("Pa_GetStreamCpuLoad" Pa_GetStreamCpuLoad) :double
  (stream :pointer))

(defcfun ("Pa_ReadStream" Pa_ReadStream) :int
  (stream :pointer)
  (buffer :pointer)
  (frames :unsigned-long))

(defcfun ("Pa_WriteStream" Pa_WriteStream) :int
  (stream :pointer)
  (buffer :pointer)
  (frames :unsigned-long))

(defcfun ("Pa_GetStreamReadAvailable" Pa_GetStreamReadAvailable) :long
  (stream :pointer))

(defcfun ("Pa_GetStreamWriteAvailable" Pa_GetStreamWriteAvailable) :long
  (stream :pointer))

(defcfun ("Pa_GetSampleSize" Pa_GetSampleSize) :int
  (format :unsigned-long))

(defcfun ("Pa_Sleep" Pa_Sleep) :void
  (msec :long))


