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
#ifndef _MATHS_H 
#define _MATHS_H

#include "SndObj.h"


class Acos : public SndObj{
public:
	
	Acos(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Acos();
	
    short DoProcess();
	
};


class Asin : public SndObj{
public:
	
    Asin(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Asin();
	
    short DoProcess();
	
};


class Atan : public SndObj{
	
public:
	
    Atan(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Atan();
	
    short DoProcess();
	
};

class Atan2 : public SndObj{

protected:
	SndObj* m_input2;
public:
	
    Atan2(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Atan2();
	
    short DoProcess();
	
};


class Cos : public SndObj{
	
public:
	
    Cos(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Cos();
	
    short DoProcess();
	
};


class Sin : public SndObj{
	
public:
	
    Sin(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Sin();
	
    short DoProcess();
	
};

class Tan : public SndObj{
	
public:
	
    Tan(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Tan();
	
    short DoProcess();
	
};


class Acosh : public SndObj{
	
public:
	
    Acosh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Acosh();
	
    short DoProcess();
	
};

class Asinh : public SndObj{
	
public:
	
    Asinh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Asinh();
	
    short DoProcess();
	
};

class Atanh : public SndObj{
	
public:
	
    Atanh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Atanh();
	
    short DoProcess();
	
};


class Cosh : public SndObj{
	
public:
	
    Cosh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Cosh();
	
    short DoProcess();
	
};


class Sinh : public SndObj{
	
public:
	
    Sinh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Sinh();
	
    short DoProcess();
	
};


class Tanh : public SndObj{
	
public:
	
    Tanh(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Tanh();
	
    short DoProcess();
	
};

class Exp : public SndObj{
	
public:
	
    Exp(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Exp();
	
    short DoProcess();
	
};

class Exp2 : public SndObj{
	
public:
	
    Exp2(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Exp2();
	
    short DoProcess();
	
};

class Expm1 : public SndObj{
	
public:
	
    Expm1(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Expm1();
	
    short DoProcess();
	
};


class Log : public SndObj{
	
public:
	
    Log(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Log();
	
    short DoProcess();
	
};


class Log10 : public SndObj{
	
public:
	
    Log10(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Log10();
	
    short DoProcess();
	
};


class Log2 : public SndObj{
	
public:
	
    Log2(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Log2();
	
    short DoProcess();
	
};


class Log1p : public SndObj{
	
public:
	
    Log1p(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Log1p();
	
    short DoProcess();
	
};

class Logb : public SndObj{
	
public:
	
    Logb(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Logb();
	
    short DoProcess();
	
};


class Fabs : public SndObj{
	
public:
	
    Fabs(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Fabs();
	
    short DoProcess();
	
};


class Cbrt : public SndObj{
	
public:
	
    Cbrt(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Cbrt();
	
    short DoProcess();
	
};


class Hypot : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
public:
	
    Hypot(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Hypot(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Hypot();
	
    short DoProcess();
	
};


class Pow : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
public:
	
    Pow(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Pow(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Pow();
	
    short DoProcess();
	
};


class Sqrt : public SndObj{
	
public:
	
    Sqrt(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Sqrt();
	
    short DoProcess();
	
};

class Ceil : public SndObj{
	
public:
	
    Ceil(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Ceil();
	
    short DoProcess();
	
};

class Floor : public SndObj{
	
public:
	
    Floor(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Floor();
	
    short DoProcess();
	
};

class Fdim : public SndObj{
	
protected:
	SndObj* m_input2;
public:
	
    Fdim(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Fdim();
	
    short DoProcess();
	
};


class Fmax : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
public:
	
    Fmax(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Fmax(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Fmax();
	
    short DoProcess();
	
};


class Fmin : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
public:
	
    Fmin(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Fmin(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Fmin();
	
    short DoProcess();
	
};


class Multiply : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    Multiply(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Multiply(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Multiply();
	
    short DoProcess();
};


class Divide : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    Divide(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Divide(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Divide();
	
    short DoProcess();
};

class Add : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    Add(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Add(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Add();
	
    short DoProcess();
};

class Subtract : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    Subtract(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Subtract(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Subtract();
	
    short DoProcess();
};


class GreaterThan : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    GreaterThan(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	 GreaterThan(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~GreaterThan();
	
    short DoProcess();
};



class GreaterThanEqual : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    GreaterThanEqual(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	GreaterThanEqual(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~GreaterThanEqual();
	
    short DoProcess();
};

class LessThan : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    LessThan(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	LessThan(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~LessThan();
	
    short DoProcess();
};


class LessThanEqual : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    LessThanEqual(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	LessThanEqual(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~LessThanEqual();
	
    short DoProcess();
};


class And : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    And(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	 And(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~And();
	
    short DoProcess();
};

class Or : public SndObj{
	
protected:
	SndObj* m_input2;
	float f_input2;
	float f_vecposmult;
public:
	
    Or(SndObj* input, SndObj* input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	Or(SndObj* input, float input2, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Or();
	
    short DoProcess();
};


class Not : public SndObj{
	
public:
	
    Not(SndObj* input, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~Not();
	
    short DoProcess();
	
};


class If : public SndObj{
	
protected:
	float val;
	float f_input2;
	float f_input3;
	SndObj* m_input2;
	SndObj* m_input3;
	float f_vecposmult;
public:
	
    If(SndObj* input, SndObj* input2, SndObj* input3, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	If(SndObj* input, float input2, SndObj* input3, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	If(SndObj* input, SndObj* input2, float input3, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
	If(SndObj* input, float input2, float input3, int vecsize=DEF_VECSIZE, float sr=DEF_SR);
    ~If();
	
    short DoProcess();
};




#endif




