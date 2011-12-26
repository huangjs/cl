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


#ifndef _BUSREAD_H 
#define _BUSREAD_H

#include "SndObj.h"

class BusRead : public SndObj {
	
protected:
	int m_bus;
	
public:
	BusRead();
	BusRead(int bus, int vecsize=DEF_VECSIZE, float sr = DEF_SR);
	~BusRead();
	
	void SetBus(int bus) {
		m_bus = bus;
	}
	
	short DoProcess();
};

#endif
