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
$Date: 2007/01/05 04:27:34 $
**********************************************************************/

#include "RTAudioStream.h"

Synth::Synth(){
	m_ObjNo = 0;
	m_InObj = 0;
	m_FreeFunc = 0;
	m_next = 0;
	m_previous = 0;
}

Synth::Synth(int ObjNo, SndObj** InObjs, long duration,  void (*synthFreeFunc)(SndObj *synth), int vecsize, float sr):
SndObj(0, vecsize, sr){
	
	int i;
	SndObjList* temp;
	m_ObjNo = 0;
	m_InObj = 0;
	
	for(i = 0; i < ObjNo; i++) {
		temp = m_InObj;
		m_InObj = new SndObjList;
		m_InObj->obj = InObjs[i];
		m_InObj->next = temp;
		m_ObjNo++;
	}
	m_Duration = duration;
	m_CurrentDur = 0;
	m_FreeFunc = synthFreeFunc;
	m_next = 0;
	m_previous = 0;
}

Synth::~Synth(){
	
	SndObjList* temp;
	
	while(m_InObj){
		temp = m_InObj;
		m_InObj = temp->next;
		delete temp;
	}
	
}

void Synth::SetDuration(long duration) {
	m_Duration = duration;
}

long Synth::GetDuration() {
	return m_Duration;
}

int Synth::Add() {
	m_previous =  RTAudioStream::Instance()->m_synthHead;
	m_next = RTAudioStream::Instance()->m_synthHead->Next();
	
	
	if(m_next)
		m_next->m_previous = this;
	
	RTAudioStream::Instance()->m_synthHead->m_next = this;
	
	return(1);
	
}

int Synth::Free() {
//	if(m_FreeFunc)
//		(*m_FreeFunc)(this);
	int i;
	
	if(m_next) 
	{
		m_previous->m_next = m_next;
		m_next->m_previous = m_previous;
	}
	else
		m_previous->m_next = 0;

//	delete InObjs;	
	return(1);
}

short Synth::DoProcess(){
	long diffdur = 0;
	if(!m_error){
		if(m_ObjNo){
			SndObjList* temp; 
			if(m_enable){ 
				temp = m_InObj;
				if( GetDuration() < 0) {
					while(temp){ 
						(temp->obj)->DoProcess();
						temp = temp->next;					
					}
				} else {
					diffdur = GetDuration() - m_CurrentDur;
					if(diffdur > m_vecsize) {
						while(temp){ 
							(temp->obj)->DoProcess();
							temp = temp->next;
							
						}
						m_CurrentDur += m_vecsize;
					} else {
						SetVectorSize(diffdur);
						while(temp){ 
							(temp->obj)->DoProcess();
							temp = temp->next;
						}	
						Free();
					}
		
				}
				
			}
			return 1;
		}
		else {
			m_error = 11;
			return 0;
		}
	}
	else return 0;
}




char* Synth::ErrorMessage(){
	
	char *message;
	
	switch(m_error){
		
		case 11:
			message = "DoProcess() failed, no input objects\n";
			break;
			
			//case 12 :
			//			message = "Cannot add object: incompatible sampling rate\n";
			//			break;
			
		case 13:
			message = "Cannot delete object: obj not present in the input list\n";
			
		default :
			message =  SndObj::ErrorMessage();
			break;
	}
	
	return message;
	
}


RTAudioStream* RTAudioStream::m_instance = 0;// initialize pointer

RTAudioStream* RTAudioStream::Instance(float sampleRate, int inputChannels, int outputChannels, int busses,int bufferSize) {
    if (m_instance == 0)  // is it the first call? 
	{  
		m_instance = new RTAudioStream(sampleRate, inputChannels, outputChannels, busses, bufferSize); // create sole instance
    }
    return m_instance; // address of sole instance
}


RTAudioStream::RTAudioStream(float sampleRate, int inputChannels, int outputChannels, int busses,int bufferSize ) { 
	
	int err=0,i=0;
	PaDeviceIndex defaultOutput;
	PaStreamParameters outputParams;
	
	err = Pa_Initialize();
	printf("pa init %i\n", err);
	m_sampleRate = sampleRate;
	m_inputChannels = inputChannels % 32;
	m_outputChannels = outputChannels % 32;
	m_busses = busses % 128;
	m_bufferSize = bufferSize;
	m_status = DSP_STOPPED;
	
	defaultOutput = Pa_GetDefaultOutputDevice();
	outputParams.device = defaultOutput;
	outputParams.channelCount = outputChannels;
	outputParams.sampleFormat = paFloat32;
	outputParams.suggestedLatency = Pa_GetDeviceInfo( outputParams.device )->defaultHighOutputLatency;
	outputParams.hostApiSpecificStreamInfo = NULL;
	
	//	global_dsp_info->synth_head = (synth*)malloc(sizeof(synth));
	m_synthHead = new Synth();
		
	for(i=0;i<m_outputChannels;i++)
		m_outputs[i] = new SndObj(NULL, bufferSize, sampleRate );
	
	for(i=0;i<m_busses ;i++)
		m_outbusses[i] = new SndObj(NULL, bufferSize, sampleRate );
	
	for(i=0;i<m_busses ;i++)
		m_inbusses[i] = new SndObj(m_outbusses[i], bufferSize, sampleRate );
	
	for(i=0;i<m_inputChannels;i++)
		m_inputs[i] = new SndObj(NULL, bufferSize, sampleRate );
	
	err = Pa_OpenStream(&stream, NULL, &outputParams, (double)sampleRate, bufferSize, (paClipOff | paDitherOff), paCallback, (void *)this);	
	
	if( err == paNoError ) {
		m_bufferSize = bufferSize;
		m_sampleRate = sampleRate;

	}
}


int RTAudioStream::Start(int priority) {
	int res;
	pthread_attr_t attr;
	struct sched_param parm;
	m_status = DSP_RUNNING;
	
	pthread_attr_init(&attr);	
	parm.sched_priority=priority;
	pthread_attr_setschedparam(&attr, &parm);
	pthread_attr_setschedpolicy(&attr, SCHED_RR);
	res = pthread_create(&rtaudioThread, &attr, RTRunAudio, (void *)this);
	
	return res;
}

int RTAudioStream::Stop() {
	
	int err;
	err = Pa_StopStream( GetPAStream() );
	m_status = DSP_STOPPED;
	return err;
	
}


void *RTRunAudio(void *as) {
	RTAudioStream *audioStream = (RTAudioStream*)as;
	int err;
	err = Pa_StartStream( audioStream->GetPAStream() );
	return 0;
}

int paCallback(const void *input, void *output, unsigned long frameCount, const PaStreamCallbackTimeInfo* timeInfo,
						  PaStreamCallbackFlags statusFlags, void *userData ) {
	
	unsigned long i;
	int j;
	RTAudioStream *info = (RTAudioStream*)userData;
	
	Synth *head = info->m_synthHead;
	
	float *out = (float*)output;
	Synth *current_synth;
	current_synth = head;
	
	while(current_synth->Next()) {
		current_synth = current_synth->Next();
		current_synth->DoProcess();
	}
	
	for(i=0;i<frameCount;i++) {
		for(j=0;j<info->m_outputChannels;j++) {
			*out++ = info->m_outputs[j]->Output(i);
		}
	}
	
	for(j=0;j<info->m_outputChannels;j++) {
		*info->m_outputs[j] *= 0.0f;
	}	
	
	
	for(j=0;j<info->m_busses;j++) {
		info->m_inbusses[j]->DoProcess();
		*info->m_outbusses[j] *= 0.0f;
	}
	
	return 0;
}

