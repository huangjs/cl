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

#include "BusWrite.h"
#include "RTAudioStream.h"


BusWrite::BusWrite() {
	m_bus = 0;
	m_input = 0;
}


BusWrite::BusWrite(int bus, SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_bus = bus % RTAudioStream::Instance()->m_busses;
	m_sr = sr;
	m_input = input;
}


BusWrite::~BusWrite(){}

short BusWrite::DoProcess(){	
	if(!m_error){
		if(m_enable){ 
			*RTAudioStream::Instance()->m_outbusses[m_bus] += *m_input;			
		}
		return 1;
	}
	else return 0;
}
