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


#ifndef _RTOUTPUT_H 
#define _RTOUTPUT_H

#include "SndObj.h"
#include "RTAudioStream.h"

class RTOutput : public SndObj {

protected:
	int m_chan;
	
public:
	RTOutput();
	RTOutput(int chan, SndObj* input = 0, int vecsize=DEF_VECSIZE, float sr = DEF_SR);
	~RTOutput();
	
	void SetChan(int chan) {
		m_chan = chan;
	}

	short DoProcess();
};

#endif
