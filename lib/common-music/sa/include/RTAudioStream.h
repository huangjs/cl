/**********************************************************************
* Copyright (C) 2006 Todd Ingalls (testcase@asu.edu) 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
**********************************************************************

$Name:  $
$Revision: 1.1.1.1 $
$Date: 2007/01/05 04:27:33 $
**********************************************************************/


#ifndef _RTAUDIOSTREAM_H 
#define _RTAUDIOSTREAM_H

#include "SndObj.h"
#include "Mix.h"
#include "portaudio.h"


typedef void (*synthFreeFunc) (SndObj *synth);
extern "C" void *RTRunAudio(void *as) ;
extern "C" int paCallback(const void *input, void *output, unsigned long frameCount, const PaStreamCallbackTimeInfo* timeInfo,
			   PaStreamCallbackFlags statusFlags, void *userData ) ;

enum {
	DSP_STOPPED,
	DSP_RUNNING,
	DSP_PAUSED
};


class Synth : public SndObj {
	
protected:	
    SndObjList*  m_InObj;  // pointer to a linked list of SndObj
    int          m_ObjNo;  // number of input objects
	long		 m_CurrentDur;
	long		m_Duration;
	Synth*		m_next;
	Synth*		m_previous;
	synthFreeFunc m_FreeFunc;
	
	
public:
		
	Synth();
	Synth(int ObjNo, SndObj** InObjs, long duration, void (*synthFreeFunc)(SndObj *synth), int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	~Synth();
	
	void SetDuration(long duration);
	long GetDuration();
	int  GetObjNo() { return m_ObjNo; } 
	short DoProcess();
    char* ErrorMessage();
	int Add();
	int Free();
	Synth* Next() { return m_next; }
	Synth* Previous() { return m_previous; }
};



class RTAudioStream {
	
public:
	static RTAudioStream* Instance(float sampleRate=44100.0, int inputChannels=2, int outputChannels=2, int busses=2, int bufferSize=256);
	int Start(int priority=80);
	int Stop();
	float GetSampleRate() { return  m_sampleRate;}
	int GetVectorSize() { return m_bufferSize;}
	PaStream *GetPAStream() {return stream; }
	float m_sampleRate;
	int m_inputChannels;
	int m_outputChannels;
	int m_busses;
	int m_bufferSize;
	Synth *m_synthHead;
	SndObj *m_outputs[32];
	SndObj *m_inputs[32];
	SndObj *m_inbusses[128];
	SndObj *m_outbusses[128];
	int m_status;
	
	
protected:
	RTAudioStream(float sampleRate, int inputChannels, int outputChannels, int busses, int bufferSize);
	RTAudioStream(const RTAudioStream&);
	RTAudioStream& operator= (const RTAudioStream&);
	
	pthread_t rtaudioThread;
	PaStream *stream;

	
private:
	static RTAudioStream* m_instance;
	
};

#endif
