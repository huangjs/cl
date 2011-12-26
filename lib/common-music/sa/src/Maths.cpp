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


#include "SndObj.h"
#include "Maths.h"
#include <math.h>


Acos::Acos(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Acos::~Acos() {
}

short Acos::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = acosf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Asin::Asin(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Asin::~Asin() {
}

short Asin::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = asinf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}



Atan::Atan(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Atan::~Atan() {
}

short Atan::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = atanf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}



Atan2::Atan2(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
}

Atan2::~Atan2() {
}

short Atan2::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = atan2f(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Cos::Cos(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Cos::~Cos() {
}

short Cos::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = cosf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Sin::Sin(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Sin::~Sin() {
}

short Sin::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = sinf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Tan::Tan(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Tan::~Tan() {
}

short Tan::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = tanf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}




Acosh::Acosh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Acosh::~Acosh() {
}

short Acosh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = acoshf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}




Asinh::Asinh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Asinh::~Asinh() {
}

short Asinh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = asinhf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Atanh::Atanh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Atanh::~Atanh() {
}

short Atanh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = atanhf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Cosh::Cosh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Cosh::~Cosh() {
}

short Cosh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = coshf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Sinh::Sinh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Sinh::~Sinh() {
}

short Sinh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = sinhf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Tanh::Tanh(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Tanh::~Tanh() {
}

short Tanh::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = tanhf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Exp::Exp(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Exp::~Exp() {
}

short Exp::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = expf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Exp2::Exp2(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Exp2::~Exp2() {
}

short Exp2::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = exp2f(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Expm1::Expm1(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Expm1::~Expm1() {
}

short Expm1::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = expm1f(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Log::Log(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Log::~Log() {
}

short Log::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = logf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Log10::Log10(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Log10::~Log10() {
}

short Log10::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = log10f(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Log2::Log2(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Log2::~Log2() {
}

short Log2::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = log2f(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Log1p::Log1p(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Log1p::~Log1p() {
}

short Log1p::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = log1pf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Logb::Logb(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Logb::~Logb() {
}

short Logb::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = logbf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Fabs::Fabs(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Fabs::~Fabs() {
}

short Fabs::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fabsf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Cbrt::Cbrt(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Cbrt::~Cbrt() {
}

short Cbrt::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = cbrtf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Hypot::Hypot(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
}

Hypot::Hypot(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}



Hypot::~Hypot() {
}

short Hypot::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = hypotf(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else if (m_input ){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = hypotf(m_input->Output(m_vecpos), f_input2);
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Pow::Pow(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
}

Pow::Pow(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}


Pow::~Pow() {
}

short Pow::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = powf(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = powf(m_input->Output(m_vecpos), f_input2);
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else 
			
		{
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Sqrt::Sqrt(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Sqrt::~Sqrt() {
}

short Sqrt::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = sqrtf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Ceil::Ceil(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Ceil::~Ceil() {
}

short Ceil::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = ceilf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Floor::Floor(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Floor::~Floor() {
}

short Floor::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = floorf(m_input->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Fdim::Fdim(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Fdim::~Fdim() {
}

short Fdim::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fdimf(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Fmax::Fmax(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
}

Fmax::Fmax(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

Fmax::~Fmax() {
}

short Fmax::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fmaxf(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fmaxf(m_input->Output(m_vecpos), f_input2);
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else
		{	
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Fmin::Fmin(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
}

Fmin::Fmin(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

Fmin::~Fmin() {
}

short Fmin::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fminf(m_input->Output(m_vecpos), m_input2->Output(m_vecpos));
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = fminf(m_input->Output(m_vecpos), f_input2);
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else
		{	
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Multiply::Multiply(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input = input;
	m_input2 = input2;
	f_vecposmult = 0.0;
}

Multiply::Multiply(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

Multiply::~Multiply() {
}

//short Multiply::DoProcess(){
//	if(!m_error){
//		if(m_input && m_input2){
//			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
//				if(m_enable) 
//					m_output[m_vecpos] = m_input->Output(m_vecpos) * m_input2->Output(m_vecpos);
//				else m_output[m_vecpos] = 0.f;
//			}
//			return 1;
//		} else if(m_input){
//			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
//				if(m_enable) 
//					m_output[m_vecpos] = m_input->Output(m_vecpos) * f_input2;
//				else m_output[m_vecpos] = 0.f;
//			}
//			return 1;
//		} else
//		{
//			m_error=3;
//			return 0;
//		}
//	}
//	else return 0;
//}

short Multiply::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) * m_input2->Output(m_vecpos*f_vecposmult);
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) * m_input2->Output(m_vecpos);
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input && f_input2){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) * f_input2;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else
		{
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Divide::Divide(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

Divide::Divide(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}


Divide::~Divide() {
}

short Divide::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) / m_input2->Output(m_vecpos*f_vecposmult);
					else m_output[m_vecpos] = 0.f;
				}	
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) / m_input2->Output(m_vecpos);
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) / f_input2;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

Add::Add(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

Add::Add(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}


Add::~Add() {
}

short Add::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) + m_input2->Output(m_vecpos*f_vecposmult);
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) + m_input2->Output(m_vecpos);
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) + f_input2;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		}	else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Subtract::Subtract(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

Subtract::Subtract(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

Subtract::~Subtract() {
}

short Subtract::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) - m_input2->Output(m_vecpos*f_vecposmult);
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) - m_input2->Output(m_vecpos);
					else m_output[m_vecpos] = 0.f;
				}
			}			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) - f_input2;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		}	else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


GreaterThan::GreaterThan(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

GreaterThan::GreaterThan(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

GreaterThan::~GreaterThan() {
}

short GreaterThan::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) > m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) > m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) > f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


GreaterThanEqual::GreaterThanEqual(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

GreaterThanEqual::GreaterThanEqual(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

GreaterThanEqual::~GreaterThanEqual() {
}

short GreaterThanEqual::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) >= m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) >= m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) >= f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


LessThan::LessThan(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

LessThan::LessThan(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

LessThan::~LessThan() {
}

short LessThan::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) < m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) < m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) < f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}

LessThanEqual::LessThanEqual(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

LessThanEqual::LessThanEqual(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

LessThanEqual::~LessThanEqual() {
}

short LessThanEqual::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) < m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) < m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) <= f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}




And::And(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}


And::And(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}


And::~And() {
}

short And::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) && m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) && m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}
			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) && f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Or::Or(SndObj* input, SndObj* input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_vecposmult = 0.0;
}

Or::Or(SndObj* input, float input2, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
}

Or::~Or() {
}

short Or::DoProcess(){
	if(!m_error){
		if(m_input && m_input2){
			if( m_input->GetVectorSize() != m_input2->GetVectorSize()) {
				f_vecposmult = (float)m_input2->GetVectorSize() / m_input->GetVectorSize();
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) || m_input2->Output(m_vecpos*f_vecposmult) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				} 
			}else {
				for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
					if(m_enable) 
						m_output[m_vecpos] = m_input->Output(m_vecpos) || m_input2->Output(m_vecpos) ? 1.0 : 0.0;
					else m_output[m_vecpos] = 0.f;
				}
			}			return 1;
		} else if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) || f_input2 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


Not::Not(SndObj* input, int vecsize, float sr):
SndObj(input, vecsize, sr) {
}

Not::~Not() {
}

short Not::DoProcess(){
	if(!m_error){
		if(m_input){
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) 
					m_output[m_vecpos] = m_input->Output(m_vecpos) == 0.0 ? 1.0 : 0.0;
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}




If::If(SndObj* input, SndObj* input2, SndObj* input3, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	m_input3 = input3;
}

If::If(SndObj* input, float input2, SndObj* input3, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
	m_input3 = input3;
}

If::If(SndObj* input, SndObj* input2, float input3, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	m_input2 = input2;
	f_input3 = input3;
}

If::If(SndObj* input, float input2, float input3, int vecsize, float sr):
SndObj(input, vecsize, sr) {
	f_input2 = input2;
	f_input3 = input3;
}



If::~If() {
}

short If::DoProcess(){
	if(!m_error){
		if(m_input && m_input2 && m_input3){
			float val;
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) {
					if( m_input->Output(m_vecpos) != 0.0 )
						val = m_input2->Output(m_vecpos);
					else	
						val = m_input3->Output(m_vecpos);
					
					m_output[m_vecpos] = val;
				}
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
		} else if (m_input && m_input3) {
			float val;
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) {
					if( m_input->Output(m_vecpos) != 0.0 )
						val = f_input2;
					else	
						val = m_input3->Output(m_vecpos);
					
					m_output[m_vecpos] = val;
				}
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
			
		} else if (m_input && m_input2) {
			float val;
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) {
					if( m_input->Output(m_vecpos) != 0.0 )
						val = m_input2->Output(m_vecpos);
					else	
						val = m_input3->Output(m_vecpos);
					
					m_output[m_vecpos] = val;
				}
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
			
		} else if (m_input) {
			float val;
			for(m_vecpos = 0; m_vecpos < m_vecsize; m_vecpos++){
				if(m_enable) {
					if( m_input->Output(m_vecpos) != 0.0 )
						val = f_input2;
					else	
						val = f_input3;
					
					m_output[m_vecpos] = val;
				}
				else m_output[m_vecpos] = 0.f;
			}
			return 1;
			
		} else {
			m_error=3;
			return 0;
		}
	}
	else return 0;
}


