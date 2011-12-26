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

#include "FloatSig.h"


FloatSig::FloatSig() {
	m_val = 0.0;
}


FloatSig::FloatSig(float val, int vecsize, float sr):
SndObj(0, vecsize, sr) {
	m_val = val;
}


FloatSig::~FloatSig(){}


short FloatSig::DoProcess(){	
	if(!m_error){
		for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
			if(m_enable) {
				m_output[m_vecpos] = m_val;
			}
			else m_output[m_vecpos] = 0.f;
		}
		return 1;
	} else {
		m_error=3;
		return 0;
	}
}


