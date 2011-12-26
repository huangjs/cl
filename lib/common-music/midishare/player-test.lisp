(in-package :ms)

;; You must check if MidiShare is installed before using the Player
;; library

(Midishare) 

;; You can check the Player library version

(Version)

;; Open a new Player

(defparameter *player* (OpeNplayer "PlayerLisp"))

*player*

(MidiGetName *player*)

;; Each player is associated with a MidiShare application Open-player
;; gives the refence number of the MidiShare application. It means
;; that some of the MidiShare functions can be used for the Player
;; like MidiConnect, MidiSetname....

;; Connect the Player to MidiShare (input and ouput)

(MidiConnect 0 *player* -1) 
(MidiConnect *player* 0  -1) 

(MidiIsConnected *player* 0)
(MidiIsConnected 0 *player*)

;; Load a MidiFile
;; Allocate an empty MidiShare sequence

(defparameter *seq* (MidiNewSeq)) 

;; Allocate a MidiFile record

(defparameter *info* (MidiNewMidiFileInfos))

;; Load a  Midifile (set YOUR pathname)

(MidiFileLoad "/Users/hkt/test.mid" *seq* *info*)

;; Set the loaded MidiShare sequence in the Player (You can set
;; directly a MidiShare sequence "built from scratch" in a player Look
;; at the Track management example)

(SetAllTrackPlayer *player* *seq*  (mf-clicks *info*))

;; Free *info* record

(MidiFreeMidiFileInfos *info*)

;; Transport control

(StartPlayer *player*)
(PausePlayer *player*)
(ContPlayer *player*)
(StopPlayer *player*)

;; State management
;; GETSTATEPLAYER  can be used during playback

(defparameter *state* (MidiNewPlayerState))

;; Print the state record

(defun midi-string-date (d)
  (let (hh mn ss ms)
    (multiple-value-setq (ss ms) (floor d 1000))
    (multiple-value-setq (mn ss) (floor ss 60))
    (multiple-value-setq (hh mn) (floor mn 60))
    (format nil "~2,'0d:~2,'0d:~2,'0d.~3,'0d" hh mn ss ms)))

(defun print-state ()
  (format t "Player current state ~%~%")
  (GetStatePlayer *player* *state*)
  (format t  "Position BBU :   ~2d ~2d ~3d ~%" 
              (s-bar *state*) (s-beat *state*) (s-unit *state*) )
  (format t  "Position Ms  :    ~2d  ~%" 
              (midi-string-date (s-date *state*)))
  (format t  "Tempo :     ~2d  ~%" 
              (round (/ 60000000 (s-tempo *state*))))
  (format t  "TimeSign :  ~2d ~2d ~2d ~2d  ~%" 
              (s-num *state*)
              (s-denom *state*)
              (s-click *state*)
              (s-quarter *state*))
  (format t  "State :   ~2d  ~%" 
              (s-state *state*))
  (format t  "SyncIn :  ~2d  ~%" 
              (s-syncin *state*))
  (format t  "SyncOut : ~2d  ~%" 
              (s-syncout *state*)))

(print-state)

;; Free *state* record

(MidiFreePlayerState *state*)

;; Position management
;; SETPOSBBUPLAYER and SETPOSMSPLAYER can be used during playback

(defparameter *pos* (MidiNewPos))

;; set the postion in bar,beat,unit

(defun set-pos-bbu-player (bar beat unit)
  (p-bar *pos* bar)
  (p-beat *pos* beat)
  (p-unit *pos* unit)
  (setposbbuplayer *player* *pos*))

(set-pos-bbu-player 5 1 1)

;; set the postion in min,sec,ms

(defun set-pos-ms-player (min sec ms)
  (SetPosMsPlayer *player* (+ (* 60000 min) (* 1000 sec) ms)))

(set-pos-ms-player 1 2 1)

;; Step playing management. if used during playback FORWARDSTEPPLAYER
;; and BACKWARDSTEPPLAYER first stop the player you will have a
;; Sequencer error when using forwardstepplayer at the end of the
;; sequence or backwardstepplayer at the beginning of the sequence.

(ForwardStepPlayer *player* kStepPlay)  
(BackwardStepPlayer *player* kStepPlay)  

(defun forward-step (n)
  (dotimes (i n)
    (ForwardStepPlayer  *player*  kStepPlay) 
    (sleep 0.05)))

(forward-step 10)

;; Loop management
;; Loop management functions can be used during playback you will have
;; a Sequencer error when setting loopstart marker after loopend
;; marker or the contrary

(defun set-loopend-bbu-player (bar beat unit)
  (p-bar *pos* bar)
  (p-beat *pos* beat)
  (p-unit *pos* unit)
  (SetLoopEndBBUPlayer *player* *pos*))

(set-loopend-bbu-player 15 1 1)

(defun set-loopstart-bbu-player (bar beat unit)
  (p-bar *pos* bar)
  (p-beat *pos* beat)
  (p-unit *pos* unit)
  (SetLoopStartBBUPlayer *player* *pos*))

(set-loopstart-bbu-player 5 1 1)

;; enable the loop

(SetLoopPlayer *player* kloopon)
(StartPlayer *player*)
(StopPlayer *player*)

;; disable the loop

(SetLoopPlayer *player* kloopoff)

;; Free *pos* record

(MidiFreePos *pos*)

;; You can also use SETLOOPSTARTMSPLAYER and SETLOOPENDMSPLAYER

;; Record management
;; Each player has 256 tracks, track number 0 is reserved for the
;; Tempo Map
;; Set a track in record mode

(RecordPlayer *player* 1)
(StartPlayer *player*)
(StopPlayer *player*)

;; By default, the record mode is kmixmode, new Midi events are mixed
;; on the recording track Change the record mode so that the track is
;; erased between punch-in/punch-out points when recording

(SetRecordModePlayer *player* kerasemode)

;; Tracks management un applicateur de fonction sur une sequence
;; MidiShare

(defun apply-seq (seq fun)
  (unless (cffi:null-pointer-p seq)
    (do ((ev (firstev seq) (link ev)))
        ((cffi:null-pointer-p ev))
      (funcall fun ev))))

;; Functions to print a Midi event in textual format

(defun midi-string-date (d)
  (let (hh mn ss ms)
    (multiple-value-setq (ss ms) (floor d 1000))
    (multiple-value-setq (mn ss) (floor ss 60))
    (multiple-value-setq (hh mn) (floor mn 60))
    (format nil "~2,'0d:~2,'0d:~2,'0d.~3,'0d" hh mn ss ms)))

(defun midi-list-fields (e &optional (n 4))
  (if (or (and (>= (evType e) typeTextual) (<= (evType e) typeCuePoint))
          (= (evType e) typeSpecific))
    (text e)
    (let (l)
      (dotimes (i (min n (MidiCountFields e)))
        (push (MidiGetField e i) l))
      (nreverse l))))

(defun midi-string-type (n)
  "convert a MidiShare event type to its name, a string."
  (let* ((lName '((0 . "Note") (1 . "KeyOn") (2 . "KeyOff")
                  (3 . "KeyPress") (4 . "CtrlChange") (5 . "ProgChange")
                  (6 . "ChanPress") (7 . "PitchBend") (8 . "SongPos")
                  (9 . "SongSel") (10 . "Clock") (11 . "Start")
                  (12 . "Cont") (13 . "Stop") (14 . "Tune")
                  (15 . "ActiveSens") (16 . "Reset") (17 . "SysEx")
                  (18 . "Stream") (128 . "Process") (129 . "DProcess")
                  (130 . "QFrame") (131 . "Ctrl14b")
                  (132 . "NonRegParam") (133 . "RegParam") 
                  (134 . "SeqNum") (135 . "Text:") (136 . "Copyright:")
                  (137 . "SeqName:") (138 . "InstrName:") (139 . "Lyric:")
                  (140 . "Marker:") (141 . "CuePoint:")
                  (142 . "ChanPrefix") (143 . "EndTrack")
                  (144 . "Tempo") (145 . "SMPTEOffset") 
                  (146 . "TimeSign") (147 . "KeySign") (148 . "Specific:")
                  (255 . "Dead")))
         (l (assoc n lName)) )
    (cond (l 
           (cdr l))
          ((< n typeProcess)
           "Private")
          (t
           "Undefined") )))

(defun midi-string-ev (e &optional (f 4))
  (unless (cffi:null-pointer-p e)
    (list 
    (format nil "~A ~3D/~4A ~A ~:a" 
                (midi-string-date (date e)) 
                (port e) (chan e) 
                (midi-string-type (evType e)) 
                (midi-list-fields e f)))))

;; Fonctions to print a MidiShare sequence in textual format

(defun print-seq (seq ) 
  (apply-seq seq #'(lambda (ev) (print (midi-string-ev ev)))))

;; Each track can be individually accessed, changed, muted...
;; Get a COPY of the track number 1

(setq *seq* (GetTrackPlayer *player* 1))
(defparameter *seq1* (GetAllTrackPlayer *player*))
(print-seq *seq1*)

;; Warning : you must free the MidiShare sequence when they are no
;; longer used

(MidiFreeSeq *seq*)
(MidiFreeSeq *seq1*)

;; The third parameter to the setalltrackplayer function is the
;; tick-per-quarter info.  This value is usually read in the MidiFile
;; header.  The internal time unit of a player is a "tick" which is
;; related to absolute time (in millisecond) using tempo informations
;; and tick-per-quarter value.
;; When opening a new empty player, the default tempo is set to 120
;; beat-per-minute. If you set the tick-per-quater value to 500, 1
;; tick will correspond exactly to 1 millisecond.
;; Replace all the tracks with a new MidiShare sequence Tracks are
;; mixed in a unique MidiShare sequence but are distinguish by the
;; refnum of their Midi event

(defun add-track (seq tracknum pitch startdate)
  (dotimes (i 500)
    (let ((ev (MidiNewEv typeNote)))
      (unless (cffi:null-pointer-p ev)
        (MidiSetField ev 0 pitch)
        (MidiSetField ev 1 80)
        (MidiSetField ev 2 125)
        (date ev (+ startdate  (* i 500))) ; date in "ticks"  
        (ref ev tracknum)  ; set track number in event refnum field 
        (MidiAddSeq seq ev)))))

(defun buid-multitrack-score ( tracklist)
  (let ((seq (MidiNewSeq)))
    (mapc #'(lambda (tracknum)
              (add-track seq tracknum (+ tracknum 60) (* tracknum 100)))
          tracklist)
    seq))

(SetAllTrackPlayer *player* (buid-multitrack-score '(1 2 3)) 500)
(StartPlayer *player*)
(StopPlayer *player*)

;; Set a new track in the player

(defun build-track ()
  (let ((seq (MidiNewSeq)))
    (dotimes (i 50)
      (let ((ev (MidiNewEv typeNote)))
        (unless (cffi:null-pointer-p ev)
          (MidiSetField ev 0 72)
          (MidiSetField ev 1 80)
          (MidiSetField ev 2 125)
          (date ev (* i 1000))
          (MidiAddSeq seq ev))))
    seq))

(SetTrackPlayer *player* 1 (build-track))

;; You can clear tracks by putting a new empty MidiShare sequence in
;; the player

;; Clear the track 1

(SetTrackPlayer *player* 1 (MidiNewSeq))

;; Mute the track 1

(SetParamPlayer *player* 1 kMute kMuteOn)

(StartPlayer *player*)
(StopPlayer *player*)

;; Unmute a the track 1

(SetParamPlayer *player* 1 kMute kMuteOff)

;; The track 1 plays solo

(SetParamPlayer *player* 1 kSolo kSoloOn)
(StartPlayer *player*)
(StopPlayer *player*)
(ContPlayer *player*)
(SetParamPlayer *player* 1 kSolo kSoloOff)

;; Synchronisation  management
;; By default, the player is internally synchronized (it uses it's
;; internal tempo map)

;; External synchronisation
;; Set the Player in External synchro mode

(SetSynchroInPlayer *player* kExternalSync)
(StartPlayer *player*)
(SetTempoPlayer *player* 1000000) ;; Tempo in micro-sec/per/quarter
(SetTempoPlayer *player* 500000)
(SetTempoPlayer *player* 250000)
(SetTempoPlayer *player* 100000)
(SetTempoPlayer *player* 50000)
(StopPlayer *player*)
(SetTempoPlayer *player* 250000)

;; Midi Clock synchronisation
;; Set the Player in Clock synchro mode

(SetSynchroInPlayer *player* kClockSync)

;; Use a new MidiShare application to send synchronisation Midi messages

(defparameter *synchro* (MidiOpen "synchro"))
(midiconnect *synchro* *player* -1)

;; Send a Midi start message

(defun start-clock-player ()
  (MidiSendIm *synchro* (MidiNewEv typeStart)))

;; Send a Midi stop message

(defun stop-clock-player ()
  (MidiSendIm *synchro* (MidiNewEv typeStop)))

;; "Play" a sequence of Midi clock messages
 
(defun send-synchro (n tempo)
  (let ((time (MidiGetTime)))
    (dotimes (i n)
      (MidiSendAt *synchro* (MidiNewEv typeclock)
                  (round (+ time (* i (/ 60000 (* tempo 24)))))))))

(start-clock-player )
(send-synchro 500 120)
(stop-clock-player)
(start-clock-player )
(send-synchro 500 240)
(stop-clock-player)
(MidiClose *synchro*)

;; A player can send Midi synchro messages (start, stop, clock,
;; continue...) to be used as a master

(SetSynchroOutPlayer *player* kClockSyncOut)

;; SMPTE synchronisation 
;; defined in the MidiShare.lisp file

(defparameter *syncinfo* (cffi:foreign-alloc 'tsmpte-location))

(defun set-SMPTE-offset (hrs min sec frames frac)
  (setf (cffi:foreign-slot-value *syncinfo* 'tsmpte-location-1 'hours)
        hrs)
  (setf (cffi:foreign-slot-value *syncinfo* 'tsmpte-location-1 'minutes)
        min)
  (setf (cffi:foreign-slot-value *syncinfo* 'tsmpte-location-1 'seconds)
        sec)
  (setf (cffi:foreign-slot-value *syncinfo* 'tsmpte-location-1 'frames)
        frames)
  (setf (cffi:foreign-slot-value *syncinfo* 'tsmpte-location-1 'fracs)
        frac)
  (SetSMPTEOffsetPlayer *player* *syncinfo*))

;; Set MidiShare in SMPTE synchro mode

(MidiSetSyncMode (logior MidiExternalSync MidiSyncAnyPort))

;; Set the Player in SMPTE synchro mode

(SetSynchroInPlayer *player* kSMPTESync)

;; Set a new SMPTE Offset

(set-SMPTE-offset 0 1 30 0 0) 

;; Now the Player will start when the SMPTE date coming from 
;; an external source is reached.

;; MidiFile management 

(setq *info* (MidiNewMidiFileInfos)) 

;; Load a  Midifile

(setq *seq* (MidiNewSeq))

;; Fill a MidiShare sequence with the MidiFile

(midiFileLoad "/Users/hkt/test.midi" *seq* *info*)

;; Save a MidiFile 

(setq *seq* (GetAllTrackPlayer *player*))
(mf-format *info* midifile1)
(mf-timedef *info* TicksPerQuarterNote)
(mf-clicks *info* 500)

;; Warning : the tick-per-quater (here 500) is either read in the
;; MidiFile header or set to 500 if you start your session with a
;; empty Player.

(MidiFileSave "/tmp/foo.mid" *seq* *info*)

;; The MidiShare sequence got with getalltrackplayer must now be
;; desallocated

(MidiFreeSeq *seq*) 

;; Free *info* record

(MidiFreeMidiFileInfos *info*)

;; Close the Player: the internal score is garbage collected

(ClosePlayer *player*)


