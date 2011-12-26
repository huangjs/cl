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
$Date: 2007/01/05 04:27:35 $
**********************************************************************/

#include "sa.h"


EXPORT Acos *_wrap_new_Acos (SndObj *larg1, int larg2, float larg3) {
  Acos * lresult = (Acos *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Acos *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Acos *)new Acos(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Acos *)0;
  }
}


EXPORT void _wrap_delete_Acos (Acos *larg1) {
  Acos *arg1 = (Acos *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Acos_DoProcess (Acos *larg1) {
  short lresult = (short)0 ;
  Acos *arg1 = (Acos *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Asin *_wrap_new_Asin (SndObj *larg1, int larg2, float larg3) {
  Asin * lresult = (Asin *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Asin *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Asin *)new Asin(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Asin *)0;
  }
}


EXPORT void _wrap_delete_Asin (Asin *larg1) {
  Asin *arg1 = (Asin *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Asin_DoProcess (Asin *larg1) {
  short lresult = (short)0 ;
  Asin *arg1 = (Asin *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Atan *_wrap_new_Atan (SndObj *larg1, int larg2, float larg3) {
  Atan * lresult = (Atan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Atan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Atan *)new Atan(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Atan *)0;
  }
}


EXPORT void _wrap_delete_Atan (Atan *larg1) {
  Atan *arg1 = (Atan *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Atan_DoProcess (Atan *larg1) {
  short lresult = (short)0 ;
  Atan *arg1 = (Atan *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Atan2 *_wrap_new_Atan2 (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Atan2 * lresult = (Atan2 *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Atan2 *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Atan2 *)new Atan2(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Atan2 *)0;
  }
}


EXPORT void _wrap_delete_Atan2 (Atan2 *larg1) {
  Atan2 *arg1 = (Atan2 *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Atan2_DoProcess (Atan2 *larg1) {
  short lresult = (short)0 ;
  Atan2 *arg1 = (Atan2 *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Cos *_wrap_new_Cos (SndObj *larg1, int larg2, float larg3) {
  Cos * lresult = (Cos *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Cos *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Cos *)new Cos(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Cos *)0;
  }
}


EXPORT void _wrap_delete_Cos (Cos *larg1) {
  Cos *arg1 = (Cos *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Cos_DoProcess (Cos *larg1) {
  short lresult = (short)0 ;
  Cos *arg1 = (Cos *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Sin *_wrap_new_Sin (SndObj *larg1, int larg2, float larg3) {
  Sin * lresult = (Sin *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Sin *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Sin *)new Sin(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Sin *)0;
  }
}


EXPORT void _wrap_delete_Sin (Sin *larg1) {
  Sin *arg1 = (Sin *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Sin_DoProcess (Sin *larg1) {
  short lresult = (short)0 ;
  Sin *arg1 = (Sin *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Tan *_wrap_new_Tan (SndObj *larg1, int larg2, float larg3) {
  Tan * lresult = (Tan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Tan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Tan *)new Tan(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tan *)0;
  }
}


EXPORT void _wrap_delete_Tan (Tan *larg1) {
  Tan *arg1 = (Tan *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Tan_DoProcess (Tan *larg1) {
  short lresult = (short)0 ;
  Tan *arg1 = (Tan *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Acosh *_wrap_new_Acosh (SndObj *larg1, int larg2, float larg3) {
  Acosh * lresult = (Acosh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Acosh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Acosh *)new Acosh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Acosh *)0;
  }
}


EXPORT void _wrap_delete_Acosh (Acosh *larg1) {
  Acosh *arg1 = (Acosh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Acosh_DoProcess (Acosh *larg1) {
  short lresult = (short)0 ;
  Acosh *arg1 = (Acosh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Asinh *_wrap_new_Asinh (SndObj *larg1, int larg2, float larg3) {
  Asinh * lresult = (Asinh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Asinh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Asinh *)new Asinh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Asinh *)0;
  }
}


EXPORT void _wrap_delete_Asinh (Asinh *larg1) {
  Asinh *arg1 = (Asinh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Asinh_DoProcess (Asinh *larg1) {
  short lresult = (short)0 ;
  Asinh *arg1 = (Asinh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Atanh *_wrap_new_Atanh (SndObj *larg1, int larg2, float larg3) {
  Atanh * lresult = (Atanh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Atanh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Atanh *)new Atanh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Atanh *)0;
  }
}


EXPORT void _wrap_delete_Atanh (Atanh *larg1) {
  Atanh *arg1 = (Atanh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Atanh_DoProcess (Atanh *larg1) {
  short lresult = (short)0 ;
  Atanh *arg1 = (Atanh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Cosh *_wrap_new_Cosh (SndObj *larg1, int larg2, float larg3) {
  Cosh * lresult = (Cosh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Cosh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Cosh *)new Cosh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Cosh *)0;
  }
}


EXPORT void _wrap_delete_Cosh (Cosh *larg1) {
  Cosh *arg1 = (Cosh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Cosh_DoProcess (Cosh *larg1) {
  short lresult = (short)0 ;
  Cosh *arg1 = (Cosh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Sinh *_wrap_new_Sinh (SndObj *larg1, int larg2, float larg3) {
  Sinh * lresult = (Sinh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Sinh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Sinh *)new Sinh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Sinh *)0;
  }
}


EXPORT void _wrap_delete_Sinh (Sinh *larg1) {
  Sinh *arg1 = (Sinh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Sinh_DoProcess (Sinh *larg1) {
  short lresult = (short)0 ;
  Sinh *arg1 = (Sinh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Tanh *_wrap_new_Tanh (SndObj *larg1, int larg2, float larg3) {
  Tanh * lresult = (Tanh *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Tanh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Tanh *)new Tanh(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tanh *)0;
  }
}


EXPORT void _wrap_delete_Tanh (Tanh *larg1) {
  Tanh *arg1 = (Tanh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Tanh_DoProcess (Tanh *larg1) {
  short lresult = (short)0 ;
  Tanh *arg1 = (Tanh *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Exp *_wrap_new_Exp (SndObj *larg1, int larg2, float larg3) {
  Exp * lresult = (Exp *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Exp *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Exp *)new Exp(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Exp *)0;
  }
}


EXPORT void _wrap_delete_Exp (Exp *larg1) {
  Exp *arg1 = (Exp *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Exp_DoProcess (Exp *larg1) {
  short lresult = (short)0 ;
  Exp *arg1 = (Exp *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Exp2 *_wrap_new_Exp2 (SndObj *larg1, int larg2, float larg3) {
  Exp2 * lresult = (Exp2 *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Exp2 *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Exp2 *)new Exp2(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Exp2 *)0;
  }
}


EXPORT void _wrap_delete_Exp2 (Exp2 *larg1) {
  Exp2 *arg1 = (Exp2 *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Exp2_DoProcess (Exp2 *larg1) {
  short lresult = (short)0 ;
  Exp2 *arg1 = (Exp2 *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Expm1 *_wrap_new_Expm1 (SndObj *larg1, int larg2, float larg3) {
  Expm1 * lresult = (Expm1 *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Expm1 *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Expm1 *)new Expm1(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Expm1 *)0;
  }
}


EXPORT void _wrap_delete_Expm1 (Expm1 *larg1) {
  Expm1 *arg1 = (Expm1 *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Expm1_DoProcess (Expm1 *larg1) {
  short lresult = (short)0 ;
  Expm1 *arg1 = (Expm1 *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Log *_wrap_new_Log (SndObj *larg1, int larg2, float larg3) {
  Log * lresult = (Log *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Log *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Log *)new Log(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Log *)0;
  }
}


EXPORT void _wrap_delete_Log (Log *larg1) {
  Log *arg1 = (Log *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Log_DoProcess (Log *larg1) {
  short lresult = (short)0 ;
  Log *arg1 = (Log *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Log10 *_wrap_new_Log10 (SndObj *larg1, int larg2, float larg3) {
  Log10 * lresult = (Log10 *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Log10 *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Log10 *)new Log10(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Log10 *)0;
  }
}


EXPORT void _wrap_delete_Log10 (Log10 *larg1) {
  Log10 *arg1 = (Log10 *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Log10_DoProcess (Log10 *larg1) {
  short lresult = (short)0 ;
  Log10 *arg1 = (Log10 *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Log2 *_wrap_new_Log2 (SndObj *larg1, int larg2, float larg3) {
  Log2 * lresult = (Log2 *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Log2 *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Log2 *)new Log2(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Log2 *)0;
  }
}


EXPORT void _wrap_delete_Log2 (Log2 *larg1) {
  Log2 *arg1 = (Log2 *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Log2_DoProcess (Log2 *larg1) {
  short lresult = (short)0 ;
  Log2 *arg1 = (Log2 *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Log1p *_wrap_new_Log1p (SndObj *larg1, int larg2, float larg3) {
  Log1p * lresult = (Log1p *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Log1p *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Log1p *)new Log1p(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Log1p *)0;
  }
}


EXPORT void _wrap_delete_Log1p (Log1p *larg1) {
  Log1p *arg1 = (Log1p *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Log1p_DoProcess (Log1p *larg1) {
  short lresult = (short)0 ;
  Log1p *arg1 = (Log1p *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Logb *_wrap_new_Logb (SndObj *larg1, int larg2, float larg3) {
  Logb * lresult = (Logb *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Logb *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Logb *)new Logb(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Logb *)0;
  }
}


EXPORT void _wrap_delete_Logb (Logb *larg1) {
  Logb *arg1 = (Logb *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Logb_DoProcess (Logb *larg1) {
  short lresult = (short)0 ;
  Logb *arg1 = (Logb *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Fabs *_wrap_new_Fabs (SndObj *larg1, int larg2, float larg3) {
  Fabs * lresult = (Fabs *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Fabs *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Fabs *)new Fabs(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fabs *)0;
  }
}


EXPORT void _wrap_delete_Fabs (Fabs *larg1) {
  Fabs *arg1 = (Fabs *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Fabs_DoProcess (Fabs *larg1) {
  short lresult = (short)0 ;
  Fabs *arg1 = (Fabs *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Cbrt *_wrap_new_Cbrt (SndObj *larg1, int larg2, float larg3) {
  Cbrt * lresult = (Cbrt *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Cbrt *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Cbrt *)new Cbrt(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Cbrt *)0;
  }
}


EXPORT void _wrap_delete_Cbrt (Cbrt *larg1) {
  Cbrt *arg1 = (Cbrt *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Cbrt_DoProcess (Cbrt *larg1) {
  short lresult = (short)0 ;
  Cbrt *arg1 = (Cbrt *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Hypot *_wrap_new_Hypot (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Hypot * lresult = (Hypot *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Hypot *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Hypot *)new Hypot(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Hypot *)0;
  }
}


EXPORT Hypot *_wrap_new_Hypotf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Hypot * lresult = (Hypot *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Hypot *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Hypot *)new Hypot(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Hypot *)0;
  }
}


EXPORT void _wrap_delete_Hypot (Hypot *larg1) {
  Hypot *arg1 = (Hypot *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Hypot_DoProcess (Hypot *larg1) {
  short lresult = (short)0 ;
  Hypot *arg1 = (Hypot *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Pow *_wrap_new_Pow (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Pow * lresult = (Pow *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Pow *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Pow *)new Pow(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pow *)0;
  }
}


EXPORT Pow *_wrap_new_Powf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Pow * lresult = (Pow *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Pow *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Pow *)new Pow(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pow *)0;
  }
}


EXPORT void _wrap_delete_Pow (Pow *larg1) {
  Pow *arg1 = (Pow *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Pow_DoProcess (Pow *larg1) {
  short lresult = (short)0 ;
  Pow *arg1 = (Pow *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Sqrt *_wrap_new_Sqrt (SndObj *larg1, int larg2, float larg3) {
  Sqrt * lresult = (Sqrt *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Sqrt *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Sqrt *)new Sqrt(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Sqrt *)0;
  }
}


EXPORT void _wrap_delete_Sqrt (Sqrt *larg1) {
  Sqrt *arg1 = (Sqrt *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Sqrt_DoProcess (Sqrt *larg1) {
  short lresult = (short)0 ;
  Sqrt *arg1 = (Sqrt *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Ceil *_wrap_new_Ceil (SndObj *larg1, int larg2, float larg3) {
  Ceil * lresult = (Ceil *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Ceil *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Ceil *)new Ceil(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Ceil *)0;
  }
}


EXPORT void _wrap_delete_Ceil (Ceil *larg1) {
  Ceil *arg1 = (Ceil *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Ceil_DoProcess (Ceil *larg1) {
  short lresult = (short)0 ;
  Ceil *arg1 = (Ceil *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Floor *_wrap_new_Floor (SndObj *larg1, int larg2, float larg3) {
  Floor * lresult = (Floor *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Floor *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Floor *)new Floor(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Floor *)0;
  }
}


EXPORT void _wrap_delete_Floor (Floor *larg1) {
  Floor *arg1 = (Floor *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Floor_DoProcess (Floor *larg1) {
  short lresult = (short)0 ;
  Floor *arg1 = (Floor *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Fdim *_wrap_new_Fdim (SndObj *larg1, int larg2, float larg3) {
  Fdim * lresult = (Fdim *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Fdim *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Fdim *)new Fdim(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fdim *)0;
  }
}


EXPORT void _wrap_delete_Fdim (Fdim *larg1) {
  Fdim *arg1 = (Fdim *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Fdim_DoProcess (Fdim *larg1) {
  short lresult = (short)0 ;
  Fdim *arg1 = (Fdim *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Fmax *_wrap_new_Fmax (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Fmax * lresult = (Fmax *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Fmax *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Fmax *)new Fmax(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fmax *)0;
  }
}


EXPORT Fmax *_wrap_new_Fmaxf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Fmax * lresult = (Fmax *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Fmax *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Fmax *)new Fmax(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fmax *)0;
  }
}


EXPORT void _wrap_delete_Fmax (Fmax *larg1) {
  Fmax *arg1 = (Fmax *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Fmax_DoProcess (Fmax *larg1) {
  short lresult = (short)0 ;
  Fmax *arg1 = (Fmax *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Fmin *_wrap_new_Fmin (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Fmin * lresult = (Fmin *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Fmin *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Fmin *)new Fmin(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fmin *)0;
  }
}


EXPORT Fmin *_wrap_new_Fminf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Fmin * lresult = (Fmin *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Fmin *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Fmin *)new Fmin(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Fmin *)0;
  }
}


EXPORT void _wrap_delete_Fmin (Fmin *larg1) {
  Fmin *arg1 = (Fmin *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Fmin_DoProcess (Fmin *larg1) {
  short lresult = (short)0 ;
  Fmin *arg1 = (Fmin *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Multiply *_wrap_new_Multiply (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Multiply * lresult = (Multiply *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Multiply *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Multiply *)new Multiply(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Multiply *)0;
  }
}


EXPORT Multiply *_wrap_new_Multiplyf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Multiply * lresult = (Multiply *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Multiply *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Multiply *)new Multiply(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Multiply *)0;
  }
}


EXPORT void _wrap_delete_Multiply (Multiply *larg1) {
  Multiply *arg1 = (Multiply *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Multiply_DoProcess (Multiply *larg1) {
  short lresult = (short)0 ;
  Multiply *arg1 = (Multiply *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Divide *_wrap_new_Divide (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Divide * lresult = (Divide *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Divide *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Divide *)new Divide(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Divide *)0;
  }
}


EXPORT Divide *_wrap_new_Dividef (SndObj *larg1, float larg2, int larg3, float larg4) {
  Divide * lresult = (Divide *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Divide *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Divide *)new Divide(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Divide *)0;
  }
}


EXPORT void _wrap_delete_Divide (Divide *larg1) {
  Divide *arg1 = (Divide *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Divide_DoProcess (Divide *larg1) {
  short lresult = (short)0 ;
  Divide *arg1 = (Divide *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Add *_wrap_new_Add (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Add * lresult = (Add *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Add *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Add *)new Add(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Add *)0;
  }
}


EXPORT Add *_wrap_new_Addf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Add * lresult = (Add *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Add *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Add *)new Add(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Add *)0;
  }
}


EXPORT void _wrap_delete_Add (Add *larg1) {
  Add *arg1 = (Add *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Add_DoProcess (Add *larg1) {
  short lresult = (short)0 ;
  Add *arg1 = (Add *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Subtract *_wrap_new_Subtract (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Subtract * lresult = (Subtract *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Subtract *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Subtract *)new Subtract(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Subtract *)0;
  }
}


EXPORT Subtract *_wrap_new_Subtractf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Subtract * lresult = (Subtract *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Subtract *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Subtract *)new Subtract(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Subtract *)0;
  }
}


EXPORT void _wrap_delete_Subtract (Subtract *larg1) {
  Subtract *arg1 = (Subtract *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Subtract_DoProcess (Subtract *larg1) {
  short lresult = (short)0 ;
  Subtract *arg1 = (Subtract *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT GreaterThan *_wrap_new_GreaterThan (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  GreaterThan * lresult = (GreaterThan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  GreaterThan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (GreaterThan *)new GreaterThan(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (GreaterThan *)0;
  }
}


EXPORT GreaterThan *_wrap_new_GreaterThanf (SndObj *larg1, float larg2, int larg3, float larg4) {
  GreaterThan * lresult = (GreaterThan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  GreaterThan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (GreaterThan *)new GreaterThan(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (GreaterThan *)0;
  }
}


EXPORT void _wrap_delete_GreaterThan (GreaterThan *larg1) {
  GreaterThan *arg1 = (GreaterThan *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_GreaterThan_DoProcess (GreaterThan *larg1) {
  short lresult = (short)0 ;
  GreaterThan *arg1 = (GreaterThan *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT GreaterThanEqual *_wrap_new_GreaterThanEqual (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  GreaterThanEqual * lresult = (GreaterThanEqual *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  GreaterThanEqual *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (GreaterThanEqual *)new GreaterThanEqual(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (GreaterThanEqual *)0;
  }
}


EXPORT GreaterThanEqual *_wrap_new_GreaterThanEqualf (SndObj *larg1, float larg2, int larg3, float larg4) {
  GreaterThanEqual * lresult = (GreaterThanEqual *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  GreaterThanEqual *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (GreaterThanEqual *)new GreaterThanEqual(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (GreaterThanEqual *)0;
  }
}


EXPORT void _wrap_delete_GreaterThanEqual (GreaterThanEqual *larg1) {
  GreaterThanEqual *arg1 = (GreaterThanEqual *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_GreaterThanEqual_DoProcess (GreaterThanEqual *larg1) {
  short lresult = (short)0 ;
  GreaterThanEqual *arg1 = (GreaterThanEqual *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT LessThan *_wrap_new_LessThan (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  LessThan * lresult = (LessThan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  LessThan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (LessThan *)new LessThan(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LessThan *)0;
  }
}


EXPORT LessThan *_wrap_new_LessThanf (SndObj *larg1, float larg2, int larg3, float larg4) {
  LessThan * lresult = (LessThan *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  LessThan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (LessThan *)new LessThan(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LessThan *)0;
  }
}


EXPORT void _wrap_delete_LessThan (LessThan *larg1) {
  LessThan *arg1 = (LessThan *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_LessThan_DoProcess (LessThan *larg1) {
  short lresult = (short)0 ;
  LessThan *arg1 = (LessThan *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT LessThanEqual *_wrap_new_LessThanEqual (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  LessThanEqual * lresult = (LessThanEqual *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  LessThanEqual *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (LessThanEqual *)new LessThanEqual(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LessThanEqual *)0;
  }
}


EXPORT LessThanEqual *_wrap_new_LessThanEqualf (SndObj *larg1, float larg2, int larg3, float larg4) {
  LessThanEqual * lresult = (LessThanEqual *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  LessThanEqual *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (LessThanEqual *)new LessThanEqual(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LessThanEqual *)0;
  }
}


EXPORT void _wrap_delete_LessThanEqual (LessThanEqual *larg1) {
  LessThanEqual *arg1 = (LessThanEqual *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_LessThanEqual_DoProcess (LessThanEqual *larg1) {
  short lresult = (short)0 ;
  LessThanEqual *arg1 = (LessThanEqual *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT And *_wrap_new_And (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  And * lresult = (And *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  And *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (And *)new And(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (And *)0;
  }
}


EXPORT And *_wrap_new_Andf (SndObj *larg1, float larg2, int larg3, float larg4) {
  And * lresult = (And *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  And *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (And *)new And(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (And *)0;
  }
}


EXPORT void _wrap_delete_And (And *larg1) {
  And *arg1 = (And *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_And_DoProcess (And *larg1) {
  short lresult = (short)0 ;
  And *arg1 = (And *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Or *_wrap_new_Or (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Or * lresult = (Or *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Or *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Or *)new Or(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Or *)0;
  }
}


EXPORT Or *_wrap_new_Orf (SndObj *larg1, float larg2, int larg3, float larg4) {
  Or * lresult = (Or *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Or *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Or *)new Or(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Or *)0;
  }
}


EXPORT void _wrap_delete_Or (Or *larg1) {
  Or *arg1 = (Or *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Or_DoProcess (Or *larg1) {
  short lresult = (short)0 ;
  Or *arg1 = (Or *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT Not *_wrap_new_Not (SndObj *larg1, int larg2, float larg3) {
  Not * lresult = (Not *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Not *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Not *)new Not(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Not *)0;
  }
}


EXPORT void _wrap_delete_Not (Not *larg1) {
  Not *arg1 = (Not *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Not_DoProcess (Not *larg1) {
  short lresult = (short)0 ;
  Not *arg1 = (Not *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT If *_wrap_new_If (SndObj *larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5) {
  If * lresult = (If *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  If *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (If *)new If(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (If *)0;
  }
}


EXPORT If *_wrap_new_Iffs (SndObj *larg1, float larg2, SndObj *larg3, int larg4, float larg5) {
  If * lresult = (If *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  If *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (If *)new If(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (If *)0;
  }
}


EXPORT If *_wrap_new_Ifsf (SndObj *larg1, SndObj *larg2, float larg3, int larg4, float larg5) {
  If * lresult = (If *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  If *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (If *)new If(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (If *)0;
  }
}


EXPORT If *_wrap_new_Ifff (SndObj *larg1, float larg2, float larg3, int larg4, float larg5) {
  If * lresult = (If *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  float arg3 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  If *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (If *)new If(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (If *)0;
  }
}


EXPORT void _wrap_delete_If (If *larg1) {
  If *arg1 = (If *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_If_DoProcess (If *larg1) {
  short lresult = (short)0 ;
  If *arg1 = (If *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT RTOutput *_wrap_new_RTOutput_empty () {
  RTOutput * lresult = (RTOutput *)0 ;
  RTOutput *result = 0 ;
  
  try {
    result = (RTOutput *)new RTOutput();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (RTOutput *)0;
  }
}


EXPORT RTOutput *_wrap_new_RTOutput (int larg1, SndObj *larg2, int larg3, float larg4) {
  RTOutput * lresult = (RTOutput *)0 ;
  int arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  RTOutput *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (RTOutput *)new RTOutput(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (RTOutput *)0;
  }
}


EXPORT void _wrap_delete_RTOutput (RTOutput *larg1) {
  RTOutput *arg1 = (RTOutput *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_RTOutput_SetChan (RTOutput *larg1, int larg2) {
  RTOutput *arg1 = (RTOutput *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetChan(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_RTOutput_DoProcess (RTOutput *larg1) {
  short lresult = (short)0 ;
  RTOutput *arg1 = (RTOutput *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT BusWrite *_wrap_new_BusWrite_empty () {
  BusWrite * lresult = (BusWrite *)0 ;
  BusWrite *result = 0 ;
  
  try {
    result = (BusWrite *)new BusWrite();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (BusWrite *)0;
  }
}


EXPORT BusWrite *_wrap_new_BusWrite (int larg1, SndObj *larg2, int larg3, float larg4) {
  BusWrite * lresult = (BusWrite *)0 ;
  int arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  BusWrite *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (BusWrite *)new BusWrite(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (BusWrite *)0;
  }
}


EXPORT void _wrap_delete_BusWrite (BusWrite *larg1) {
  BusWrite *arg1 = (BusWrite *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_BusWrite_SetBus (BusWrite *larg1, int larg2) {
  BusWrite *arg1 = (BusWrite *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetBus(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_BusWrite_DoProcess (BusWrite *larg1) {
  short lresult = (short)0 ;
  BusWrite *arg1 = (BusWrite *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT BusRead *_wrap_new_BusRead_empty () {
  BusRead * lresult = (BusRead *)0 ;
  BusRead *result = 0 ;
  
  try {
    result = (BusRead *)new BusRead();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (BusRead *)0;
  }
}


EXPORT BusRead *_wrap_new_BusRead (int larg1, int larg2, float larg3) {
  BusRead * lresult = (BusRead *)0 ;
  int arg1 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  BusRead *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (BusRead *)new BusRead(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (BusRead *)0;
  }
}


EXPORT void _wrap_delete_BusRead (BusRead *larg1) {
  BusRead *arg1 = (BusRead *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_BusRead_SetBus (BusRead *larg1, int larg2) {
  BusRead *arg1 = (BusRead *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetBus(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_BusRead_DoProcess (BusRead *larg1) {
  short lresult = (short)0 ;
  BusRead *arg1 = (BusRead *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT SndObj *_wrap_PVOCEXRead_Outchannel (PVOCEXRead *larg1, int larg2) {
  SndObj * lresult = (SndObj *)0 ;
  PVOCEXRead *arg1 = (PVOCEXRead *) 0 ;
  int arg2 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (SndObj *)(arg1)->Outchannel(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT PVOCEXRead *_wrap_new_PVOCEXRead_empty () {
  PVOCEXRead * lresult = (PVOCEXRead *)0 ;
  PVOCEXRead *result = 0 ;
  
  try {
    result = (PVOCEXRead *)new PVOCEXRead();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVOCEXRead *)0;
  }
}


EXPORT PVOCEXRead *_wrap_new_PVOCEXRead (char *larg1, int larg2, float larg3) {
  PVOCEXRead * lresult = (PVOCEXRead *)0 ;
  char *arg1 = (char *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  PVOCEXRead *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (PVOCEXRead *)new PVOCEXRead(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVOCEXRead *)0;
  }
}


EXPORT void _wrap_delete_PVOCEXRead (PVOCEXRead *larg1) {
  PVOCEXRead *arg1 = (PVOCEXRead *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVOCEXRead_SetInput (PVOCEXRead *larg1, char *larg2) {
  PVOCEXRead *arg1 = (PVOCEXRead *) 0 ;
  char *arg2 = (char *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVOCEXRead_DoProcess (PVOCEXRead *larg1) {
  short lresult = (short)0 ;
  PVOCEXRead *arg1 = (PVOCEXRead *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


//EXPORT FloatSig *_wrap_new_FloatSig_empty () {
//  FloatSig * lresult = (FloatSig *)0 ;
//  FloatSig *result = 0 ;
//  
//  try {
//    result = (FloatSig *)new FloatSig();
//    
//    lresult = result;
//    return lresult;
//  } catch (...) {
//    return (FloatSig *)0;
//  }
//}


EXPORT FloatSig *_wrap_new_FloatSig (float larg1, int larg2, float larg3) {
  FloatSig * lresult = (FloatSig *)0 ;
  float arg1 = (float) 0.0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  FloatSig *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (FloatSig *)new FloatSig(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FloatSig *)0;
  }
}


EXPORT void _wrap_delete_FloatSig (FloatSig *larg1) {
  FloatSig *arg1 = (FloatSig *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FloatSig_SetVal (FloatSig *larg1, float larg2) {
  FloatSig *arg1 = (FloatSig *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetVal(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_FloatSig_DoProcess (FloatSig *larg1) {
  short lresult = (short)0 ;
  FloatSig *arg1 = (FloatSig *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT void *_wrap_RTRunAudio (void *larg1) {
  void * lresult = (void *)0 ;
  void *arg1 = (void *) 0 ;
  void *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (void *)RTRunAudio(arg1);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (void *)0;
  }
}


EXPORT int _wrap_paCallback (void *larg1, void *larg2, unsigned long larg3, PaStreamCallbackTimeInfo *larg4, PaStreamCallbackFlags *larg5, void *larg6) {
  int lresult = (int)0 ;
  void *arg1 = (void *) 0 ;
  void *arg2 = (void *) 0 ;
  unsigned long arg3 ;
  PaStreamCallbackTimeInfo *arg4 = (PaStreamCallbackTimeInfo *) 0 ;
  PaStreamCallbackFlags arg5 ;
  void *arg6 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = *larg5;
  arg6 = larg6;
  try {
    result = (int)paCallback((void const *)arg1,arg2,arg3,(PaStreamCallbackTimeInfo const *)arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT Synth *_wrap_new_Synth_empty () {
  Synth * lresult = (Synth *)0 ;
  Synth *result = 0 ;
  
  try {
    result = (Synth *)new Synth();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Synth *)0;
  }
}


EXPORT Synth *_wrap_new_Synth (int larg1, SndObj **larg2, long larg3, void (*larg4)(SndObj *), int larg5, float larg6) {
  Synth * lresult = (Synth *)0 ;
  int arg1 ;
  SndObj **arg2 = (SndObj **) 0 ;
  long arg3 ;
  void (*arg4)(SndObj *) = (void (*)(SndObj *)) 0 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  Synth *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (Synth *)new Synth(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Synth *)0;
  }
}


EXPORT void _wrap_delete_Synth (Synth *larg1) {
  Synth *arg1 = (Synth *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Synth_SetDuration (Synth *larg1, long larg2) {
  Synth *arg1 = (Synth *) 0 ;
  long arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDuration(arg2);
    
  } catch (...) {
    
  }
}


EXPORT long _wrap_Synth_GetDuration (Synth *larg1) {
  long lresult = (long)0 ;
  Synth *arg1 = (Synth *) 0 ;
  long result;
  
  arg1 = larg1;
  try {
    result = (long)(arg1)->GetDuration();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (long)0;
  }
}


EXPORT int _wrap_Synth_GetObjNo (Synth *larg1) {
  int lresult = (int)0 ;
  Synth *arg1 = (Synth *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetObjNo();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Synth_DoProcess (Synth *larg1) {
  short lresult = (short)0 ;
  Synth *arg1 = (Synth *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->DoProcess();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_Synth_ErrorMessage (Synth *larg1) {
  char * lresult = (char *)0 ;
  Synth *arg1 = (Synth *) 0 ;
  char *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (char *)(arg1)->ErrorMessage();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (char *)0;
  }
}


EXPORT int _wrap_Synth_Add (Synth *larg1) {
  int lresult = (int)0 ;
  Synth *arg1 = (Synth *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->Add();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Synth_Free (Synth *larg1) {
  int lresult = (int)0 ;
  Synth *arg1 = (Synth *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->Free();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT Synth *_wrap_Synth_Next (Synth *larg1) {
  Synth * lresult = (Synth *)0 ;
  Synth *arg1 = (Synth *) 0 ;
  Synth *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (Synth *)(arg1)->Next();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Synth *)0;
  }
}


EXPORT Synth *_wrap_Synth_Previous (Synth *larg1) {
  Synth * lresult = (Synth *)0 ;
  Synth *arg1 = (Synth *) 0 ;
  Synth *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (Synth *)(arg1)->Previous();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Synth *)0;
  }
}


EXPORT RTAudioStream *_wrap_RTAudioStream_Instance (float larg1, int larg2, int larg3, int larg4, int larg5) {
  RTAudioStream * lresult = (RTAudioStream *)0 ;
  float arg1 = (float) 44100.0 ;
  int arg2 = (int) 2 ;
  int arg3 = (int) 2 ;
  int arg4 = (int) 2 ;
  int arg5 = (int) 256 ;
  RTAudioStream *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (RTAudioStream *)RTAudioStream::Instance(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (RTAudioStream *)0;
  }
}


EXPORT int _wrap_RTAudioStream_Start (RTAudioStream *larg1, int larg2) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 = (int) 80 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (int)(arg1)->Start(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_RTAudioStream_Stop (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->Stop();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT float _wrap_RTAudioStream_GetSampleRate (RTAudioStream *larg1) {
  float lresult = (float)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float)(arg1)->GetSampleRate();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT int _wrap_RTAudioStream_GetVectorSize (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetVectorSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT PaStream *_wrap_RTAudioStream_GetPAStream (RTAudioStream *larg1) {
  PaStream * lresult = (PaStream *)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  PaStream *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (PaStream *)(arg1)->GetPAStream();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PaStream *)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_sampleRate_set (RTAudioStream *larg1, float larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_sampleRate = arg2;
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_RTAudioStream_m_sampleRate_get (RTAudioStream *larg1) {
  float lresult = (float)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float) ((arg1)->m_sampleRate);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_inputChannels_set (RTAudioStream *larg1, int larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_inputChannels = arg2;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_RTAudioStream_m_inputChannels_get (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int) ((arg1)->m_inputChannels);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_outputChannels_set (RTAudioStream *larg1, int larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_outputChannels = arg2;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_RTAudioStream_m_outputChannels_get (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int) ((arg1)->m_outputChannels);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_busses_set (RTAudioStream *larg1, int larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_busses = arg2;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_RTAudioStream_m_busses_get (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int) ((arg1)->m_busses);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_bufferSize_set (RTAudioStream *larg1, int larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_bufferSize = arg2;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_RTAudioStream_m_bufferSize_get (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int) ((arg1)->m_bufferSize);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_synthHead_set (RTAudioStream *larg1, Synth *larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  Synth *arg2 = (Synth *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_synthHead = arg2;
    
  } catch (...) {
    
  }
}


EXPORT Synth *_wrap_RTAudioStream_m_synthHead_get (RTAudioStream *larg1) {
  Synth * lresult = (Synth *)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  Synth *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (Synth *) ((arg1)->m_synthHead);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Synth *)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_outputs_set (RTAudioStream *larg1, SndObj **larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      size_t ii;
      SndObj * *b = (SndObj * *) arg1->m_outputs;
      for (ii = 0; ii < (size_t)32; ii++) b[ii] = *((SndObj * *) arg2 + ii);
    }
  } catch (...) {
    
  }
}


EXPORT SndObj **_wrap_RTAudioStream_m_outputs_get (RTAudioStream *larg1) {
  SndObj ** lresult = (SndObj **)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj **)(SndObj **) ((arg1)->m_outputs);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj **)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_inputs_set (RTAudioStream *larg1, SndObj **larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      size_t ii;
      SndObj * *b = (SndObj * *) arg1->m_inputs;
      for (ii = 0; ii < (size_t)32; ii++) b[ii] = *((SndObj * *) arg2 + ii);
    }
  } catch (...) {
    
  }
}


EXPORT SndObj **_wrap_RTAudioStream_m_inputs_get (RTAudioStream *larg1) {
  SndObj ** lresult = (SndObj **)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj **)(SndObj **) ((arg1)->m_inputs);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj **)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_inbusses_set (RTAudioStream *larg1, SndObj **larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      size_t ii;
      SndObj * *b = (SndObj * *) arg1->m_inbusses;
      for (ii = 0; ii < (size_t)128; ii++) b[ii] = *((SndObj * *) arg2 + ii);
    }
  } catch (...) {
    
  }
}


EXPORT SndObj **_wrap_RTAudioStream_m_inbusses_get (RTAudioStream *larg1) {
  SndObj ** lresult = (SndObj **)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj **)(SndObj **) ((arg1)->m_inbusses);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj **)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_outbusses_set (RTAudioStream *larg1, SndObj **larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      size_t ii;
      SndObj * *b = (SndObj * *) arg1->m_outbusses;
      for (ii = 0; ii < (size_t)128; ii++) b[ii] = *((SndObj * *) arg2 + ii);
    }
  } catch (...) {
    
  }
}


EXPORT SndObj **_wrap_RTAudioStream_m_outbusses_get (RTAudioStream *larg1) {
  SndObj ** lresult = (SndObj **)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  SndObj **result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj **)(SndObj **) ((arg1)->m_outbusses);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj **)0;
  }
}


EXPORT void _wrap_RTAudioStream_m_status_set (RTAudioStream *larg1, int larg2) {
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_status = arg2;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_RTAudioStream_m_status_get (RTAudioStream *larg1) {
  int lresult = (int)0 ;
  RTAudioStream *arg1 = (RTAudioStream *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int) ((arg1)->m_status);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


