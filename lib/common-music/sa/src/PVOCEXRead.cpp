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

#include "PVOCEXRead.h"
#include "SndPVOCEX.h"

PVOCEXRead::PVOCEXRead(){
	
	m_ioinput = 0;
	m_outobj = 0;
	m_channels = 0;
	m_count = 0;
}


PVOCEXRead::PVOCEXRead(char* name, int vecsize, float sr) 
: SndObj(0, vecsize, sr) {
	
	m_count = 0;
	m_vecsize = vecsize;
	int i;
	
	m_ioinput = new SndPVOCEX(name, READ);

	if(m_sr != m_ioinput->GetSr()) m_error = 22;
	
	m_channels = 1;
	
	m_outobj = new SndObj*[m_channels];
	for(i=0;i< m_channels; i++)
		m_outobj[i] = new SndObj(0,vecsize,sr);
	
	AddMsg("pitch", 21);
	AddMsg("scale", 22);
	
}

PVOCEXRead::~PVOCEXRead(){
	
	delete m_ioinput;
	delete[] m_outobj;
	
}




void PVOCEXRead::SetInput(char* name){
	
	if(m_ioinput) {
		delete m_ioinput;
		delete[] m_outobj;
	}
	
	int i;
	
	m_ioinput = new SndPVOCEX(name, READ);

	
	if(m_sr != m_ioinput->GetSr()) m_error = 22;
	m_channels = 1;
	
	m_outobj = new SndObj*[m_channels];
	for(i=0;i< m_channels; i++)
		m_outobj[i] = new SndObj(0,m_vecsize, m_sr);
	
}

short PVOCEXRead::DoProcess(){
	if(!m_error){
		m_count = m_ioinput->Read();
		for(m_vecpos=0;m_vecpos < m_vecsize; m_vecpos++){
			if(m_enable){
				if(m_ioinput){
					m_output[m_vecpos] = m_ioinput->Output(m_vecpos);   
				}
				else{	
					m_error = 10;
					return 0;
				}
			}
			else m_output[m_vecpos] = 0.f;
			
		}
		
		return 1;
	}
	else return 0;
}