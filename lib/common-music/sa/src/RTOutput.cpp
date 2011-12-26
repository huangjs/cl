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

#include "RTOutput.h"



RTOutput::RTOutput() {
	m_chan = 0;
	m_input = 0;
}

RTOutput::RTOutput(int chan, SndObj* input, int vecsize, float sr): 
SndObj(input, vecsize, sr) {
	m_chan = chan % RTAudioStream::Instance()->m_outputChannels;
	m_sr = sr;
	m_input = input;
}

RTOutput::~RTOutput(){}

short RTOutput::DoProcess(){	
	if(!m_error){
		if(m_enable){ 
			*RTAudioStream::Instance()->m_outputs[m_chan] += *m_input;
			
		}
		return 1;
	}
	else return 0;
}
