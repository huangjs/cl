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
$Date: 2007/01/05 04:27:37 $
**********************************************************************/


#include "sa.h"


EXPORT bool _wrap_SndObj_IsProcessing (SndObj *larg1) {
  bool lresult = (bool)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  bool result;
  
  arg1 = larg1;
  try {
    result = (bool)(arg1)->IsProcessing();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (bool)0;
  }
}


EXPORT int _wrap_SndObj_GetError (SndObj *larg1) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetError();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjEqualSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj arg2 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = *larg2;
  try {
    result = (arg1)->operator =(arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSumAssignSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator +=(*arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSubtractAssignSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator -=(*arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjMultiplyAssignSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator *=(*arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSumAssignFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator +=(arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSubtractAssignFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator -=(arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjMultiplyAssignFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    {
      SndObj &_result_ref = (arg1)->operator *=(arg2);
      result = (SndObj *) &_result_ref;
    }
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSumSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator +(*arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSubtractSndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator -(*arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjMultiplySndObj (SndObj *larg1, SndObj *larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = 0 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator *(*arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSumFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator +(arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjSubtractFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator -(arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_SndObj_SndObjMultiplyFloat (SndObj *larg1, float larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  SndObj result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (arg1)->operator *(arg2);
    
    lresult = new SndObj(result);
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT void _wrap_SndObj_SndObjShiftLeftFloat (SndObj *larg1, float larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->operator <<(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndObj_SndObjShiftLeftFloatVector (SndObj *larg1, float *larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  float *arg2 = (float *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->operator <<(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndObj_SndObjShiftRightSndIO (SndObj *larg1, SndIO *larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  SndIO *arg2 = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->operator >>(*arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndObj_SndObjShiftLeftSndIO (SndObj *larg1, SndIO *larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  SndIO *arg2 = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->operator <<(*arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndObj_PushIn (SndObj *larg1, float *larg2, int larg3) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float *arg2 = (float *) 0 ;
  int arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->PushIn(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndObj_PopOut (SndObj *larg1, float *larg2, int larg3) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float *arg2 = (float *) 0 ;
  int arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->PopOut(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndObj_AddOut (SndObj *larg1, float *larg2, int larg3) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float *arg2 = (float *) 0 ;
  int arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->AddOut(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SndObj_GetMsgList (SndObj *larg1, string *larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  string *arg2 = (string *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->GetMsgList(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndObj_Enable (SndObj *larg1) {
  SndObj *arg1 = (SndObj *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Enable();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndObj_Disable (SndObj *larg1) {
  SndObj *arg1 = (SndObj *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Disable();
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_SndObj_Output (SndObj *larg1, int larg2) {
  float lresult = (float)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 ;
  float result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (float)(arg1)->Output(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT int _wrap_SndObj_GetVectorSize (SndObj *larg1) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
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


EXPORT void _wrap_SndObj_SetVectorSize (SndObj *larg1, int larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetVectorSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_SndObj_GetSr (SndObj *larg1) {
  float lresult = (float)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float)(arg1)->GetSr();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT void _wrap_SndObj_SetSr (SndObj *larg1, float larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndObj_Set (SndObj *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndObj_Connect (SndObj *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SndObj_SetInput (SndObj *larg1, SndObj *larg2) {
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_SndObj_GetInput (SndObj *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *)(arg1)->GetInput();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_new_SndObj (SndObj *larg1, int larg2, float larg3) {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (SndObj *)new SndObj(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SndObj *_wrap_new_SndObj_empty () {
  SndObj * lresult = (SndObj *)0 ;
  SndObj *result = 0 ;
  
  try {
    result = (SndObj *)new SndObj();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT void _wrap_delete_SndObj (SndObj *larg1) {
  SndObj *arg1 = (SndObj *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_SndObj_ErrorMessage (SndObj *larg1) {
  char * lresult = (char *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
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


EXPORT short _wrap_SndObj_DoProcess (SndObj *larg1) {
  short lresult = (short)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
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


EXPORT void _wrap_SndIO_m_sampsize_set (SndIO *larg1, short larg2) {
  SndIO *arg1 = (SndIO *) 0 ;
  short arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_sampsize = arg2;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndIO_m_sampsize_get (SndIO *larg1) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short) ((arg1)->m_sampsize);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT float _wrap_SndIO_GetSr (SndIO *larg1) {
  float lresult = (float)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float)(arg1)->GetSr();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT int _wrap_SndIO_GetVectorSize (SndIO *larg1) {
  int lresult = (int)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
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


EXPORT short _wrap_SndIO_GetChannels (SndIO *larg1) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->GetChannels();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndIO_GetSize (SndIO *larg1) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->GetSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT float _wrap_SndIO_Output (SndIO *larg1, int larg2, int larg3) {
  float lresult = (float)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  int arg2 ;
  int arg3 ;
  float result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (float)(arg1)->Output(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT short _wrap_SndIO_SetOutput (SndIO *larg1, short larg2, SndObj *larg3) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  short result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (short)(arg1)->SetOutput(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT SndIO *_wrap_new_SndIO (short larg1, short larg2, SndObj **larg3, int larg4, float larg5) {
  SndIO * lresult = (SndIO *)0 ;
  short arg1 = (short) 1 ;
  short arg2 = (short) 16 ;
  SndObj **arg3 = (SndObj **) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  SndIO *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (SndIO *)new SndIO(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndIO *)0;
  }
}


EXPORT void _wrap_delete_SndIO (SndIO *larg1) {
  SndIO *arg1 = (SndIO *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndIO_Read (SndIO *larg1) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndIO_Write (SndIO *larg1) {
  short lresult = (short)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_SndIO_ErrorMessage (SndIO *larg1) {
  char * lresult = (char *)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
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


EXPORT int _wrap_SndIO_Error (SndIO *larg1) {
  int lresult = (int)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->Error();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT long _wrap_Table_GetLen (Table *larg1) {
  long lresult = (long)0 ;
  Table *arg1 = (Table *) 0 ;
  long result;
  
  arg1 = larg1;
  try {
    result = (long)(arg1)->GetLen();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (long)0;
  }
}


EXPORT float *_wrap_Table_GetTable (Table *larg1) {
  float * lresult = (float *)0 ;
  Table *arg1 = (Table *) 0 ;
  float *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (float *)(arg1)->GetTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float *)0;
  }
}


EXPORT float _wrap_Table_Lookup (Table *larg1, int larg2) {
  float lresult = (float)0 ;
  Table *arg1 = (Table *) 0 ;
  int arg2 ;
  float result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (float)(arg1)->Lookup(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT void _wrap_delete_Table (Table *larg1) {
  Table *arg1 = (Table *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_Table_ErrorMessage (Table *larg1) {
  char * lresult = (char *)0 ;
  Table *arg1 = (Table *) 0 ;
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


EXPORT short _wrap_Table_MakeTable (Table *larg1) {
  short lresult = (short)0 ;
  Table *arg1 = (Table *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT void _wrap_Oscil_SetSr (Oscil *larg1, float larg2) {
  Oscil *arg1 = (Oscil *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Oscil_m_factor_set (Oscil *larg1, float larg2) {
  Oscil *arg1 = (Oscil *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->m_factor = arg2;
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_Oscil_m_factor_get (Oscil *larg1) {
  float lresult = (float)0 ;
  Oscil *arg1 = (Oscil *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float) ((arg1)->m_factor);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT Oscil *_wrap_new_Oscil_empty () {
  Oscil * lresult = (Oscil *)0 ;
  Oscil *result = 0 ;
  
  try {
    result = (Oscil *)new Oscil();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscil *)0;
  }
}


EXPORT Oscil *_wrap_new_Oscil (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Oscil * lresult = (Oscil *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 = (float) 440.f ;
  float arg3 = (float) 1.f ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Oscil *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Oscil *)new Oscil(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscil *)0;
  }
}


EXPORT void _wrap_delete_Oscil (Oscil *larg1) {
  Oscil *arg1 = (Oscil *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Oscil_SetPhase (Oscil *larg1, float larg2) {
  short lresult = (short)0 ;
  Oscil *arg1 = (Oscil *) 0 ;
  float arg2 ;
  short result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (short)(arg1)->SetPhase(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT void _wrap_Oscil_SetTable (Oscil *larg1, Table *larg2) {
  Oscil *arg1 = (Oscil *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Oscil_SetFreq (Oscil *larg1, float larg2, SndObj *larg3) {
  Oscil *arg1 = (Oscil *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Oscil_SetAmp (Oscil *larg1, float larg2, SndObj *larg3) {
  Oscil *arg1 = (Oscil *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Oscil_SetFreqSndObj (Oscil *larg1, SndObj *larg2) {
  Oscil *arg1 = (Oscil *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Oscil_SetAmpSndObj (Oscil *larg1, SndObj *larg2) {
  Oscil *arg1 = (Oscil *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetAmp(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Oscil_Connect (Oscil *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Oscil *arg1 = (Oscil *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Oscil_Set (Oscil *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Oscil *arg1 = (Oscil *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Oscil_DoProcess (Oscil *larg1) {
  short lresult = (short)0 ;
  Oscil *arg1 = (Oscil *) 0 ;
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


EXPORT Oscilt *_wrap_new_Oscilt_empty () {
  Oscilt * lresult = (Oscilt *)0 ;
  Oscilt *result = 0 ;
  
  try {
    result = (Oscilt *)new Oscilt();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscilt *)0;
  }
}


EXPORT Oscilt *_wrap_new_Oscilt (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Oscilt * lresult = (Oscilt *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 = (float) 440.f ;
  float arg3 = (float) 1.f ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Oscilt *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Oscilt *)new Oscilt(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscilt *)0;
  }
}


EXPORT void _wrap_delete_Oscilt (Oscilt *larg1) {
  Oscilt *arg1 = (Oscilt *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Oscilt_DoProcess (Oscilt *larg1) {
  short lresult = (short)0 ;
  Oscilt *arg1 = (Oscilt *) 0 ;
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


EXPORT Oscili *_wrap_new_Oscili_empty () {
  Oscili * lresult = (Oscili *)0 ;
  Oscili *result = 0 ;
  
  try {
    result = (Oscili *)new Oscili();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscili *)0;
  }
}


EXPORT Oscili *_wrap_new_Oscili (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Oscili * lresult = (Oscili *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 = (float) 440.f ;
  float arg3 = (float) 1.f ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Oscili *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Oscili *)new Oscili(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Oscili *)0;
  }
}


EXPORT void _wrap_delete_Oscili (Oscili *larg1) {
  Oscili *arg1 = (Oscili *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Oscili_DoProcess (Oscili *larg1) {
  short lresult = (short)0 ;
  Oscili *arg1 = (Oscili *) 0 ;
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


EXPORT FastOsc *_wrap_new_FastOsc_empty () {
  FastOsc * lresult = (FastOsc *)0 ;
  FastOsc *result = 0 ;
  
  try {
    result = (FastOsc *)new FastOsc();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FastOsc *)0;
  }
}


EXPORT FastOsc *_wrap_new_FastOsc (Table *larg1, float larg2, float larg3, int larg4, float larg5) {
  FastOsc * lresult = (FastOsc *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 ;
  float arg3 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  FastOsc *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (FastOsc *)new FastOsc(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FastOsc *)0;
  }
}


EXPORT void _wrap_delete_FastOsc (FastOsc *larg1) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FastOsc_SetFreq (FastOsc *larg1, float larg2) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FastOsc_SetAmp (FastOsc *larg1, float larg2) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetAmp(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FastOsc_SetPhase (FastOsc *larg1, float larg2) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPhase(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FastOsc_SetTable (FastOsc *larg1, Table *larg2) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_FastOsc_Set (FastOsc *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  FastOsc *arg1 = (FastOsc *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_FastOsc_Connect (FastOsc *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  FastOsc *arg1 = (FastOsc *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_FastOsc_SetSr (FastOsc *larg1, float larg2) {
  FastOsc *arg1 = (FastOsc *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_FastOsc_DoProcess (FastOsc *larg1) {
  short lresult = (short)0 ;
  FastOsc *arg1 = (FastOsc *) 0 ;
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


EXPORT Osc *_wrap_new_Osc_empty () {
  Osc * lresult = (Osc *)0 ;
  Osc *result = 0 ;
  
  try {
    result = (Osc *)new Osc();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Osc *)0;
  }
}


EXPORT Osc *_wrap_new_Osc (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Osc * lresult = (Osc *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 ;
  float arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Osc *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Osc *)new Osc(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Osc *)0;
  }
}


EXPORT void _wrap_delete_Osc (Osc *larg1) {
  Osc *arg1 = (Osc *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Osc_SetFreq (Osc *larg1, SndObj *larg2) {
  Osc *arg1 = (Osc *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Osc_SetAmp (Osc *larg1, SndObj *larg2) {
  Osc *arg1 = (Osc *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetAmp(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Osc_Connect (Osc *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Osc *arg1 = (Osc *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Osc_DoProcess (Osc *larg1) {
  short lresult = (short)0 ;
  Osc *arg1 = (Osc *) 0 ;
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


EXPORT Osci *_wrap_new_Osci_empty () {
  Osci * lresult = (Osci *)0 ;
  Osci *result = 0 ;
  
  try {
    result = (Osci *)new Osci();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Osci *)0;
  }
}


EXPORT Osci *_wrap_new_Osci (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Osci * lresult = (Osci *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 ;
  float arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Osci *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Osci *)new Osci(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Osci *)0;
  }
}


EXPORT void _wrap_delete_Osci (Osci *larg1) {
  Osci *arg1 = (Osci *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Osci_SetTable (Osci *larg1, Table *larg2) {
  Osci *arg1 = (Osci *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Osci_Connect (Osci *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Osci *arg1 = (Osci *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Osci_DoProcess (Osci *larg1) {
  short lresult = (short)0 ;
  Osci *arg1 = (Osci *) 0 ;
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


EXPORT SndIn *_wrap_new_SndIn_empty () {
  SndIn * lresult = (SndIn *)0 ;
  SndIn *result = 0 ;
  
  try {
    result = (SndIn *)new SndIn();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndIn *)0;
  }
}


EXPORT SndIn *_wrap_new_SndIn (SndIO *larg1, short larg2, int larg3, float larg4) {
  SndIn * lresult = (SndIn *)0 ;
  SndIO *arg1 = (SndIO *) 0 ;
  short arg2 = (short) 1 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  SndIn *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SndIn *)new SndIn(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndIn *)0;
  }
}


EXPORT void _wrap_delete_SndIn (SndIn *larg1) {
  SndIn *arg1 = (SndIn *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndIn_SetInput (SndIn *larg1, SndIO *larg2, short larg3) {
  SndIn *arg1 = (SndIn *) 0 ;
  SndIO *arg2 = (SndIO *) 0 ;
  short arg3 = (short) 1 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetInput(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndIn_Connect (SndIn *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SndIn *arg1 = (SndIn *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndIn_Set (SndIn *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SndIn *arg1 = (SndIn *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SndIn_DoProcess (SndIn *larg1) {
  short lresult = (short)0 ;
  SndIn *arg1 = (SndIn *) 0 ;
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


EXPORT char *_wrap_SndIn_ErrorMessage (SndIn *larg1) {
  char * lresult = (char *)0 ;
  SndIn *arg1 = (SndIn *) 0 ;
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


EXPORT SndObj *_wrap_SndRead_Outchannel (SndRead *larg1, int larg2) {
  SndObj * lresult = (SndObj *)0 ;
  SndRead *arg1 = (SndRead *) 0 ;
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


EXPORT SndRead *_wrap_new_SndRead_empty () {
  SndRead * lresult = (SndRead *)0 ;
  SndRead *result = 0 ;
  
  try {
    result = (SndRead *)new SndRead();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndRead *)0;
  }
}


EXPORT SndRead *_wrap_new_SndRead (char *larg1, float larg2, float larg3, int larg4, float larg5) {
  SndRead * lresult = (SndRead *)0 ;
  char *arg1 = (char *) 0 ;
  float arg2 = (float) 1.f ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  SndRead *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (SndRead *)new SndRead(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndRead *)0;
  }
}


EXPORT void _wrap_delete_SndRead (SndRead *larg1) {
  SndRead *arg1 = (SndRead *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndRead_SetInput (SndRead *larg1, char *larg2) {
  SndRead *arg1 = (SndRead *) 0 ;
  char *arg2 = (char *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndRead_SetScale (SndRead *larg1, float larg2) {
  SndRead *arg1 = (SndRead *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetScale(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndRead_SetPitch (SndRead *larg1, float larg2) {
  SndRead *arg1 = (SndRead *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPitch(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndRead_Set (SndRead *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SndRead *arg1 = (SndRead *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SndRead_DoProcess (SndRead *larg1) {
  short lresult = (short)0 ;
  SndRead *arg1 = (SndRead *) 0 ;
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


EXPORT ADSR *_wrap_new_ADSR_empty () {
  ADSR * lresult = (ADSR *)0 ;
  ADSR *result = 0 ;
  
  try {
    result = (ADSR *)new ADSR();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ADSR *)0;
  }
}


EXPORT ADSR *_wrap_new_ADSR (float larg1, float larg2, float larg3, float larg4, float larg5, float larg6, SndObj *larg7, int larg8, float larg9) {
  ADSR * lresult = (ADSR *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  float arg6 ;
  SndObj *arg7 = (SndObj *) 0 ;
  int arg8 = (int) DEF_VECSIZE ;
  float arg9 = (float) DEF_SR ;
  ADSR *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  try {
    result = (ADSR *)new ADSR(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ADSR *)0;
  }
}


EXPORT void _wrap_delete_ADSR (ADSR *larg1) {
  ADSR *arg1 = (ADSR *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_SetSr (ADSR *larg1, float larg2) {
  ADSR *arg1 = (ADSR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_SetMaxAmp (ADSR *larg1, float larg2) {
  ADSR *arg1 = (ADSR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaxAmp(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_Sustain (ADSR *larg1) {
  ADSR *arg1 = (ADSR *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Sustain();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_Release (ADSR *larg1) {
  ADSR *arg1 = (ADSR *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Release();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_Restart (ADSR *larg1) {
  ADSR *arg1 = (ADSR *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Restart();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_SetADSR (ADSR *larg1, float larg2, float larg3, float larg4, float larg5) {
  ADSR *arg1 = (ADSR *) 0 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    (arg1)->SetADSR(arg2,arg3,arg4,arg5);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ADSR_SetDur (ADSR *larg1, float larg2) {
  ADSR *arg1 = (ADSR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDur(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_ADSR_Set (ADSR *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  ADSR *arg1 = (ADSR *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_ADSR_DoProcess (ADSR *larg1) {
  short lresult = (short)0 ;
  ADSR *arg1 = (ADSR *) 0 ;
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


EXPORT IADSR *_wrap_new_IADSR_empty () {
  IADSR * lresult = (IADSR *)0 ;
  IADSR *result = 0 ;
  
  try {
    result = (IADSR *)new IADSR();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IADSR *)0;
  }
}


EXPORT IADSR *_wrap_new_IADSR (float larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, SndObj *larg9, int larg10, float larg11) {
  IADSR * lresult = (IADSR *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  float arg6 ;
  float arg7 ;
  float arg8 ;
  SndObj *arg9 = (SndObj *) 0 ;
  int arg10 = (int) DEF_VECSIZE ;
  float arg11 = (float) DEF_SR ;
  IADSR *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  try {
    result = (IADSR *)new IADSR(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IADSR *)0;
  }
}


EXPORT void _wrap_delete_IADSR (IADSR *larg1) {
  IADSR *arg1 = (IADSR *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_IADSR_SetInit (IADSR *larg1, float larg2) {
  IADSR *arg1 = (IADSR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInit(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_IADSR_SetEnd (IADSR *larg1, float larg2) {
  IADSR *arg1 = (IADSR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetEnd(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_IADSR_Set (IADSR *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  IADSR *arg1 = (IADSR *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_IADSR_DoProcess (IADSR *larg1) {
  short lresult = (short)0 ;
  IADSR *arg1 = (IADSR *) 0 ;
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


EXPORT Buzz *_wrap_new_Buzz_empty () {
  Buzz * lresult = (Buzz *)0 ;
  Buzz *result = 0 ;
  
  try {
    result = (Buzz *)new Buzz();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Buzz *)0;
  }
}


EXPORT Buzz *_wrap_new_Buzz (float larg1, float larg2, short larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Buzz * lresult = (Buzz *)0 ;
  float arg1 ;
  float arg2 ;
  short arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Buzz *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Buzz *)new Buzz(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Buzz *)0;
  }
}


EXPORT void _wrap_delete_Buzz (Buzz *larg1) {
  Buzz *arg1 = (Buzz *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Buzz_SetFreq (Buzz *larg1, float larg2, SndObj *larg3) {
  Buzz *arg1 = (Buzz *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Buzz_SetAmp (Buzz *larg1, float larg2, SndObj *larg3) {
  Buzz *arg1 = (Buzz *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Buzz_SetSr (Buzz *larg1, float larg2) {
  Buzz *arg1 = (Buzz *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Buzz_SetHarm (Buzz *larg1, int larg2) {
  Buzz *arg1 = (Buzz *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHarm(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Buzz_Set (Buzz *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Buzz *arg1 = (Buzz *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Buzz_Connect (Buzz *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Buzz *arg1 = (Buzz *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT char *_wrap_Buzz_ErrorMessage (Buzz *larg1) {
  char * lresult = (char *)0 ;
  Buzz *arg1 = (Buzz *) 0 ;
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


EXPORT short _wrap_Buzz_DoProcess (Buzz *larg1) {
  short lresult = (short)0 ;
  Buzz *arg1 = (Buzz *) 0 ;
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


EXPORT void _wrap_Balance_SetInput (Balance *larg1, SndObj *larg2, SndObj *larg3) {
  Balance *arg1 = (Balance *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetInput(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Balance_SetLPFreq (Balance *larg1, float larg2) {
  Balance *arg1 = (Balance *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetLPFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Balance_SetSr (Balance *larg1, float larg2) {
  Balance *arg1 = (Balance *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Balance_Set (Balance *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Balance *arg1 = (Balance *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT Balance *_wrap_new_Balance_empty () {
  Balance * lresult = (Balance *)0 ;
  Balance *result = 0 ;
  
  try {
    result = (Balance *)new Balance();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Balance *)0;
  }
}


EXPORT Balance *_wrap_new_Balance (SndObj *larg1, SndObj *larg2, float larg3, int larg4, float larg5) {
  Balance * lresult = (Balance *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 10.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Balance *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Balance *)new Balance(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Balance *)0;
  }
}


EXPORT void _wrap_delete_Balance (Balance *larg1) {
  Balance *arg1 = (Balance *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_Balance_ErrorMessage (Balance *larg1) {
  char * lresult = (char *)0 ;
  Balance *arg1 = (Balance *) 0 ;
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


EXPORT short _wrap_Balance_DoProcess (Balance *larg1) {
  short lresult = (short)0 ;
  Balance *arg1 = (Balance *) 0 ;
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


EXPORT int _wrap_Balance_Connect (Balance *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Balance *arg1 = (Balance *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT DelayLine *_wrap_new_DelayLine_empty () {
  DelayLine * lresult = (DelayLine *)0 ;
  DelayLine *result = 0 ;
  
  try {
    result = (DelayLine *)new DelayLine();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (DelayLine *)0;
  }
}


EXPORT DelayLine *_wrap_new_DelayLine (float larg1, SndObj *larg2, int larg3, float larg4) {
  DelayLine * lresult = (DelayLine *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  DelayLine *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (DelayLine *)new DelayLine(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (DelayLine *)0;
  }
}


EXPORT void _wrap_delete_DelayLine (DelayLine *larg1) {
  DelayLine *arg1 = (DelayLine *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT float *_wrap_DelayLine_Buffer (DelayLine *larg1) {
  float * lresult = (float *)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
  float *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (float *)(arg1)->Buffer();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float *)0;
  }
}


EXPORT long _wrap_DelayLine_GetWritePointerPos (DelayLine *larg1) {
  long lresult = (long)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
  long result;
  
  arg1 = larg1;
  try {
    result = (long)(arg1)->GetWritePointerPos();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (long)0;
  }
}


EXPORT float _wrap_DelayLine_GetDelayTime (DelayLine *larg1) {
  float lresult = (float)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float)(arg1)->GetDelayTime();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT void _wrap_DelayLine_SetSr (DelayLine *larg1, float larg2) {
  DelayLine *arg1 = (DelayLine *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_DelayLine_Reset (DelayLine *larg1) {
  DelayLine *arg1 = (DelayLine *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Reset();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_DelayLine_SetDelayTime (DelayLine *larg1, float larg2) {
  DelayLine *arg1 = (DelayLine *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_DelayLine_Set (DelayLine *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_DelayLine_DoProcess (DelayLine *larg1) {
  short lresult = (short)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
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


EXPORT char *_wrap_DelayLine_ErrorMessage (DelayLine *larg1) {
  char * lresult = (char *)0 ;
  DelayLine *arg1 = (DelayLine *) 0 ;
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


EXPORT Tap *_wrap_new_Tap_empty () {
  Tap * lresult = (Tap *)0 ;
  Tap *result = 0 ;
  
  try {
    result = (Tap *)new Tap();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tap *)0;
  }
}


EXPORT Tap *_wrap_new_Tap (float larg1, DelayLine *larg2, int larg3, float larg4) {
  Tap * lresult = (Tap *)0 ;
  float arg1 ;
  DelayLine *arg2 = (DelayLine *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Tap *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Tap *)new Tap(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tap *)0;
  }
}


EXPORT void _wrap_delete_Tap (Tap *larg1) {
  Tap *arg1 = (Tap *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Tap_SetDelayTime (Tap *larg1, float larg2) {
  Tap *arg1 = (Tap *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Tap_SetDelayTap (Tap *larg1, DelayLine *larg2) {
  Tap *arg1 = (Tap *) 0 ;
  DelayLine *arg2 = (DelayLine *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayTap(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Tap_Set (Tap *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Tap *arg1 = (Tap *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Tap_Connect (Tap *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Tap *arg1 = (Tap *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Tap_DoProcess (Tap *larg1) {
  short lresult = (short)0 ;
  Tap *arg1 = (Tap *) 0 ;
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


EXPORT char *_wrap_Tap_ErrorMessage (Tap *larg1) {
  char * lresult = (char *)0 ;
  Tap *arg1 = (Tap *) 0 ;
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


EXPORT Tapi *_wrap_new_Tapi_empty () {
  Tapi * lresult = (Tapi *)0 ;
  Tapi *result = 0 ;
  
  try {
    result = (Tapi *)new Tapi();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tapi *)0;
  }
}


EXPORT Tapi *_wrap_new_Tapi (SndObj *larg1, DelayLine *larg2, int larg3, float larg4) {
  Tapi * lresult = (Tapi *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  DelayLine *arg2 = (DelayLine *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Tapi *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Tapi *)new Tapi(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Tapi *)0;
  }
}


EXPORT void _wrap_delete_Tapi (Tapi *larg1) {
  Tapi *arg1 = (Tapi *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Tapi_SetDelayInput (Tapi *larg1, SndObj *larg2) {
  Tapi *arg1 = (Tapi *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Tapi_Connect (Tapi *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Tapi *arg1 = (Tapi *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Tapi_DoProcess (Tapi *larg1) {
  short lresult = (short)0 ;
  Tapi *arg1 = (Tapi *) 0 ;
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


EXPORT Comb *_wrap_new_Comb_empty () {
  Comb * lresult = (Comb *)0 ;
  Comb *result = 0 ;
  
  try {
    result = (Comb *)new Comb();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Comb *)0;
  }
}


EXPORT Comb *_wrap_new_Comb (float larg1, float larg2, SndObj *larg3, int larg4, float larg5) {
  Comb * lresult = (Comb *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Comb *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Comb *)new Comb(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Comb *)0;
  }
}


EXPORT void _wrap_delete_Comb (Comb *larg1) {
  Comb *arg1 = (Comb *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Comb_SetGain (Comb *larg1, float larg2) {
  Comb *arg1 = (Comb *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetGain(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Comb_Set (Comb *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Comb *arg1 = (Comb *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Comb_DoProcess (Comb *larg1) {
  short lresult = (short)0 ;
  Comb *arg1 = (Comb *) 0 ;
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


EXPORT Allpass *_wrap_new_Allpass_empty () {
  Allpass * lresult = (Allpass *)0 ;
  Allpass *result = 0 ;
  
  try {
    result = (Allpass *)new Allpass();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Allpass *)0;
  }
}


EXPORT Allpass *_wrap_new_Allpass (float larg1, float larg2, SndObj *larg3, int larg4, float larg5) {
  Allpass * lresult = (Allpass *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Allpass *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Allpass *)new Allpass(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Allpass *)0;
  }
}


EXPORT void _wrap_delete_Allpass (Allpass *larg1) {
  Allpass *arg1 = (Allpass *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Allpass_DoProcess (Allpass *larg1) {
  short lresult = (short)0 ;
  Allpass *arg1 = (Allpass *) 0 ;
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


EXPORT StringFlt *_wrap_new_StringFlt_empty () {
  StringFlt * lresult = (StringFlt *)0 ;
  StringFlt *result = 0 ;
  
  try {
    result = (StringFlt *)new StringFlt();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (StringFlt *)0;
  }
}


EXPORT StringFlt *_wrap_new_StringFlt (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  StringFlt * lresult = (StringFlt *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  StringFlt *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (StringFlt *)new StringFlt(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (StringFlt *)0;
  }
}


EXPORT StringFlt *_wrap_new_StringFlt_decay (float larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6) {
  StringFlt * lresult = (StringFlt *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  StringFlt *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (StringFlt *)new StringFlt(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (StringFlt *)0;
  }
}


EXPORT void _wrap_delete_StringFlt (StringFlt *larg1) {
  StringFlt *arg1 = (StringFlt *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_StringFlt_SetSr (StringFlt *larg1, float larg2) {
  StringFlt *arg1 = (StringFlt *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_StringFlt_SetDecay (StringFlt *larg1, float larg2) {
  StringFlt *arg1 = (StringFlt *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDecay(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_StringFlt_SetFreq (StringFlt *larg1, float larg2, SndObj *larg3) {
  StringFlt *arg1 = (StringFlt *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_StringFlt_SetFdbgain (StringFlt *larg1, float larg2) {
  StringFlt *arg1 = (StringFlt *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFdbgain(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_StringFlt_Set (StringFlt *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  StringFlt *arg1 = (StringFlt *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_StringFlt_Connect (StringFlt *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  StringFlt *arg1 = (StringFlt *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_StringFlt_DoProcess (StringFlt *larg1) {
  short lresult = (short)0 ;
  StringFlt *arg1 = (StringFlt *) 0 ;
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


EXPORT Pluck *_wrap_new_Pluck_empty () {
  Pluck * lresult = (Pluck *)0 ;
  Pluck *result = 0 ;
  
  try {
    result = (Pluck *)new Pluck();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pluck *)0;
  }
}


EXPORT Pluck *_wrap_new_Pluck (float larg1, float larg2, float larg3, SndObj *larg4, float larg5, int larg6, float larg7) {
  Pluck * lresult = (Pluck *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  float arg5 = (float) 32767.f ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Pluck *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Pluck *)new Pluck(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pluck *)0;
  }
}


EXPORT Pluck *_wrap_new_Pluck_decay (float larg1, float larg2, SndObj *larg3, float larg4, float larg5, int larg6, float larg7) {
  Pluck * lresult = (Pluck *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  float arg4 = (float) 20.f ;
  float arg5 = (float) 32767.f ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Pluck *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Pluck *)new Pluck(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pluck *)0;
  }
}


EXPORT void _wrap_delete_Pluck (Pluck *larg1) {
  Pluck *arg1 = (Pluck *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Pluck_RePluck (Pluck *larg1) {
  Pluck *arg1 = (Pluck *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->RePluck();
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Pluck_Set (Pluck *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Pluck *arg1 = (Pluck *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Pluck_SetAmp (Pluck *larg1, float larg2, float larg3) {
  Pluck *arg1 = (Pluck *) 0 ;
  float arg2 ;
  float arg3 = (float) 32767.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Pluck_DoProcess (Pluck *larg1) {
  short lresult = (short)0 ;
  Pluck *arg1 = (Pluck *) 0 ;
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


EXPORT VDelay *_wrap_new_VDelay_empty () {
  VDelay * lresult = (VDelay *)0 ;
  VDelay *result = 0 ;
  
  try {
    result = (VDelay *)new VDelay();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (VDelay *)0;
  }
}


EXPORT VDelay *_wrap_new_VDelay (float larg1, float larg2, float larg3, float larg4, SndObj *larg5, SndObj *larg6, SndObj *larg7, SndObj *larg8, SndObj *larg9, int larg10, float larg11) {
  VDelay * lresult = (VDelay *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  SndObj *arg5 = (SndObj *) 0 ;
  SndObj *arg6 = (SndObj *) 0 ;
  SndObj *arg7 = (SndObj *) 0 ;
  SndObj *arg8 = (SndObj *) 0 ;
  SndObj *arg9 = (SndObj *) 0 ;
  int arg10 = (int) DEF_VECSIZE ;
  float arg11 = (float) DEF_SR ;
  VDelay *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  try {
    result = (VDelay *)new VDelay(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (VDelay *)0;
  }
}


EXPORT VDelay *_wrap_new_VDelay_delaytime (float larg1, float larg2, float larg3, float larg4, float larg5, SndObj *larg6, SndObj *larg7, SndObj *larg8, SndObj *larg9, SndObj *larg10, int larg11, float larg12) {
  VDelay * lresult = (VDelay *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  SndObj *arg6 = (SndObj *) 0 ;
  SndObj *arg7 = (SndObj *) 0 ;
  SndObj *arg8 = (SndObj *) 0 ;
  SndObj *arg9 = (SndObj *) 0 ;
  SndObj *arg10 = (SndObj *) 0 ;
  int arg11 = (int) DEF_VECSIZE ;
  float arg12 = (float) DEF_SR ;
  VDelay *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  arg12 = larg12;
  try {
    result = (VDelay *)new VDelay(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (VDelay *)0;
  }
}


EXPORT void _wrap_delete_VDelay (VDelay *larg1) {
  VDelay *arg1 = (VDelay *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_VDelay_Set (VDelay *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  VDelay *arg1 = (VDelay *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_VDelay_Connect (VDelay *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  VDelay *arg1 = (VDelay *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_VDelay_SetMaxDelayTime (VDelay *larg1, float larg2) {
  VDelay *arg1 = (VDelay *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaxDelayTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_VDelay_SetDelayTime (VDelay *larg1, float larg2) {
  VDelay *arg1 = (VDelay *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_VDelay_SetVdtInput (VDelay *larg1, SndObj *larg2) {
  VDelay *arg1 = (VDelay *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetVdtInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_VDelay_SetFdbgain (VDelay *larg1, float larg2, SndObj *larg3) {
  VDelay *arg1 = (VDelay *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFdbgain(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_VDelay_SetFwdgain (VDelay *larg1, float larg2, SndObj *larg3) {
  VDelay *arg1 = (VDelay *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFwdgain(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_VDelay_SetDirgain (VDelay *larg1, float larg2, SndObj *larg3) {
  VDelay *arg1 = (VDelay *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetDirgain(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_VDelay_DoProcess (VDelay *larg1) {
  short lresult = (short)0 ;
  VDelay *arg1 = (VDelay *) 0 ;
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


EXPORT Pitch *_wrap_new_Pitch_empty () {
  Pitch * lresult = (Pitch *)0 ;
  Pitch *result = 0 ;
  
  try {
    result = (Pitch *)new Pitch();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pitch *)0;
  }
}


EXPORT Pitch *_wrap_new_Pitch (float larg1, SndObj *larg2, float larg3, int larg4, float larg5) {
  Pitch * lresult = (Pitch *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Pitch *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Pitch *)new Pitch(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pitch *)0;
  }
}


EXPORT Pitch *_wrap_new_Pitch_semitones (float larg1, SndObj *larg2, int larg3, int larg4, float larg5) {
  Pitch * lresult = (Pitch *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Pitch *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Pitch *)new Pitch(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pitch *)0;
  }
}


EXPORT void _wrap_delete_Pitch (Pitch *larg1) {
  Pitch *arg1 = (Pitch *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Pitch_SetPitch (Pitch *larg1, float larg2) {
  Pitch *arg1 = (Pitch *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPitch(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Pitch_SetPitch_semitones (Pitch *larg1, int larg2) {
  Pitch *arg1 = (Pitch *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPitch(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Pitch_Set (Pitch *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Pitch *arg1 = (Pitch *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Pitch_DoProcess (Pitch *larg1) {
  short lresult = (short)0 ;
  Pitch *arg1 = (Pitch *) 0 ;
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


EXPORT SndLoop *_wrap_new_SndLoop_empty () {
  SndLoop * lresult = (SndLoop *)0 ;
  SndLoop *result = 0 ;
  
  try {
    result = (SndLoop *)new SndLoop();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndLoop *)0;
  }
}


EXPORT SndLoop *_wrap_new_SndLoop (float larg1, float larg2, SndObj *larg3, float larg4, int larg5, float larg6) {
  SndLoop * lresult = (SndLoop *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  float arg4 = (float) 1.f ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  SndLoop *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (SndLoop *)new SndLoop(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndLoop *)0;
  }
}


EXPORT void _wrap_delete_SndLoop (SndLoop *larg1) {
  SndLoop *arg1 = (SndLoop *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndLoop_SetXFade (SndLoop *larg1, float larg2) {
  SndLoop *arg1 = (SndLoop *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetXFade(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndLoop_SetPitch (SndLoop *larg1, float larg2) {
  SndLoop *arg1 = (SndLoop *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPitch(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndLoop_ReSample (SndLoop *larg1) {
  SndLoop *arg1 = (SndLoop *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->ReSample();
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndLoop_Set (SndLoop *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SndLoop *arg1 = (SndLoop *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SndLoop_DoProcess (SndLoop *larg1) {
  short lresult = (short)0 ;
  SndLoop *arg1 = (SndLoop *) 0 ;
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


EXPORT int _wrap_FIR_Connect (FIR *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  FIR *arg1 = (FIR *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_FIR_Set (FIR *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  FIR *arg1 = (FIR *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT FIR *_wrap_new_Fir_empty () {
  FIR * lresult = (FIR *)0 ;
  FIR *result = 0 ;
  
  try {
    result = (FIR *)new FIR();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FIR *)0;
  }
}


EXPORT FIR *_wrap_new_FIR (Table *larg1, SndObj *larg2, int larg3, float larg4) {
  FIR * lresult = (FIR *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  FIR *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (FIR *)new FIR(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FIR *)0;
  }
}


EXPORT FIR *_wrap_new_FIR_impulse (float *larg1, int larg2, SndObj *larg3, int larg4, float larg5) {
  FIR * lresult = (FIR *)0 ;
  float *arg1 = (float *) 0 ;
  int arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  FIR *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (FIR *)new FIR(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FIR *)0;
  }
}


EXPORT void _wrap_delete_FIR (FIR *larg1) {
  FIR *arg1 = (FIR *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FIR_SetTable (FIR *larg1, Table *larg2) {
  FIR *arg1 = (FIR *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FIR_SetImpulse (FIR *larg1, float *larg2, int larg3) {
  FIR *arg1 = (FIR *) 0 ;
  float *arg2 = (float *) 0 ;
  int arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetImpulse(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FIR_SetDelayTime (FIR *larg1, float larg2) {
  FIR *arg1 = (FIR *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDelayTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_FIR_DoProcess (FIR *larg1) {
  short lresult = (short)0 ;
  FIR *arg1 = (FIR *) 0 ;
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


EXPORT Filter *_wrap_new_Filter_empty () {
  Filter * lresult = (Filter *)0 ;
  Filter *result = 0 ;
  
  try {
    result = (Filter *)new Filter();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Filter *)0;
  }
}


EXPORT Filter *_wrap_new_Filter (float larg1, float larg2, SndObj *larg3, int larg4, float larg5) {
  Filter * lresult = (Filter *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Filter *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Filter *)new Filter(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Filter *)0;
  }
}


EXPORT void _wrap_delete_Filter (Filter *larg1) {
  Filter *arg1 = (Filter *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Filter_SetFreq (Filter *larg1, float larg2) {
  Filter *arg1 = (Filter *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Filter_SetBW (Filter *larg1, float larg2) {
  Filter *arg1 = (Filter *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetBW(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Filter_Set (Filter *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Filter *arg1 = (Filter *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Filter_SetSr (Filter *larg1, float larg2) {
  Filter *arg1 = (Filter *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_Filter_ErrorMessage (Filter *larg1) {
  char * lresult = (char *)0 ;
  Filter *arg1 = (Filter *) 0 ;
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


EXPORT short _wrap_Filter_DoProcess (Filter *larg1) {
  short lresult = (short)0 ;
  Filter *arg1 = (Filter *) 0 ;
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


EXPORT TpTz *_wrap_new_TpTz_empty () {
  TpTz * lresult = (TpTz *)0 ;
  TpTz *result = 0 ;
  
  try {
    result = (TpTz *)new TpTz();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (TpTz *)0;
  }
}


EXPORT TpTz *_wrap_new_TpTz (double larg1, double larg2, double larg3, double larg4, double larg5, SndObj *larg6, int larg7, float larg8) {
  TpTz * lresult = (TpTz *)0 ;
  double arg1 ;
  double arg2 ;
  double arg3 ;
  double arg4 ;
  double arg5 ;
  SndObj *arg6 = (SndObj *) 0 ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  TpTz *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (TpTz *)new TpTz(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (TpTz *)0;
  }
}


EXPORT void _wrap_delete_TpTz (TpTz *larg1) {
  TpTz *arg1 = (TpTz *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_TpTz_SetParam (TpTz *larg1, double larg2, double larg3, double larg4, double larg5, double larg6) {
  TpTz *arg1 = (TpTz *) 0 ;
  double arg2 ;
  double arg3 ;
  double arg4 ;
  double arg5 ;
  double arg6 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    (arg1)->SetParam(arg2,arg3,arg4,arg5,arg6);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_TpTz_Set (TpTz *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  TpTz *arg1 = (TpTz *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_TpTz_DoProcess (TpTz *larg1) {
  short lresult = (short)0 ;
  TpTz *arg1 = (TpTz *) 0 ;
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


EXPORT Reson *_wrap_new_Reson_empty () {
  Reson * lresult = (Reson *)0 ;
  Reson *result = 0 ;
  
  try {
    result = (Reson *)new Reson();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Reson *)0;
  }
}


EXPORT Reson *_wrap_new_Reson (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Reson * lresult = (Reson *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Reson *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Reson *)new Reson(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Reson *)0;
  }
}


EXPORT void _wrap_Reson_SetFreq (Reson *larg1, float larg2, SndObj *larg3) {
  Reson *arg1 = (Reson *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Reson_SetBW (Reson *larg1, float larg2, SndObj *larg3) {
  Reson *arg1 = (Reson *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetBW(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_delete_Reson (Reson *larg1) {
  Reson *arg1 = (Reson *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Reson_Connect (Reson *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Reson *arg1 = (Reson *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Reson_DoProcess (Reson *larg1) {
  short lresult = (short)0 ;
  Reson *arg1 = (Reson *) 0 ;
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


EXPORT Lp *_wrap_new_Lp_empty () {
  Lp * lresult = (Lp *)0 ;
  Lp *result = 0 ;
  
  try {
    result = (Lp *)new Lp();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lp *)0;
  }
}


EXPORT Lp *_wrap_new_Lp (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Lp * lresult = (Lp *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Lp *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Lp *)new Lp(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lp *)0;
  }
}


EXPORT void _wrap_delete_Lp (Lp *larg1) {
  Lp *arg1 = (Lp *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Lp_SetSr (Lp *larg1, float larg2) {
  Lp *arg1 = (Lp *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Lp_Set (Lp *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Lp *arg1 = (Lp *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Lp_DoProcess (Lp *larg1) {
  short lresult = (short)0 ;
  Lp *arg1 = (Lp *) 0 ;
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


EXPORT ButtBP *_wrap_new_ButtBP_empty () {
  ButtBP * lresult = (ButtBP *)0 ;
  ButtBP *result = 0 ;
  
  try {
    result = (ButtBP *)new ButtBP();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtBP *)0;
  }
}


EXPORT ButtBP *_wrap_new_ButtBP (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  ButtBP * lresult = (ButtBP *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  ButtBP *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (ButtBP *)new ButtBP(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtBP *)0;
  }
}


EXPORT void _wrap_delete_ButtBP (ButtBP *larg1) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_ButtBP_Set (ButtBP *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  ButtBP *arg1 = (ButtBP *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_ButtBP_SetFreq (ButtBP *larg1, float larg2) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ButtBP_SetBW (ButtBP *larg1, float larg2) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetBW(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ButtBP_SetFreq_mod (ButtBP *larg1, float larg2, SndObj *larg3) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ButtBP_SetBW_mod (ButtBP *larg1, float larg2, SndObj *larg3) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetBW(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ButtBP_SetSr (ButtBP *larg1, float larg2) {
  ButtBP *arg1 = (ButtBP *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_ButtBP_Connect (ButtBP *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  ButtBP *arg1 = (ButtBP *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_ButtBP_DoProcess (ButtBP *larg1) {
  short lresult = (short)0 ;
  ButtBP *arg1 = (ButtBP *) 0 ;
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


EXPORT ButtBR *_wrap_new_ButtBR_empty () {
  ButtBR * lresult = (ButtBR *)0 ;
  ButtBR *result = 0 ;
  
  try {
    result = (ButtBR *)new ButtBR();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtBR *)0;
  }
}


EXPORT ButtBR *_wrap_new_ButtBR (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  ButtBR * lresult = (ButtBR *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  ButtBR *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (ButtBR *)new ButtBR(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtBR *)0;
  }
}


EXPORT void _wrap_delete_ButtBR (ButtBR *larg1) {
  ButtBR *arg1 = (ButtBR *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT ButtHP *_wrap_new_ButtHP_empty () {
  ButtHP * lresult = (ButtHP *)0 ;
  ButtHP *result = 0 ;
  
  try {
    result = (ButtHP *)new ButtHP();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtHP *)0;
  }
}


EXPORT ButtHP *_wrap_new_ButtHP (float larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5) {
  ButtHP * lresult = (ButtHP *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  ButtHP *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (ButtHP *)new ButtHP(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtHP *)0;
  }
}


EXPORT void _wrap_delete_ButtHP (ButtHP *larg1) {
  ButtHP *arg1 = (ButtHP *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT ButtLP *_wrap_new_ButtLP_empty () {
  ButtLP * lresult = (ButtLP *)0 ;
  ButtLP *result = 0 ;
  
  try {
    result = (ButtLP *)new ButtLP();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtLP *)0;
  }
}


EXPORT ButtLP *_wrap_new_ButtLP (float larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5) {
  ButtLP * lresult = (ButtLP *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  ButtLP *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (ButtLP *)new ButtLP(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ButtLP *)0;
  }
}


EXPORT void _wrap_delete_ButtLP (ButtLP *larg1) {
  ButtLP *arg1 = (ButtLP *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT Ap *_wrap_new_Ap_empty () {
  Ap * lresult = (Ap *)0 ;
  Ap *result = 0 ;
  
  try {
    result = (Ap *)new Ap();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Ap *)0;
  }
}


EXPORT Ap *_wrap_new_Ap (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7) {
  Ap * lresult = (Ap *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Ap *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Ap *)new Ap(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Ap *)0;
  }
}


EXPORT void _wrap_delete_Ap (Ap *larg1) {
  Ap *arg1 = (Ap *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Ap_SetFreq (Ap *larg1, float larg2, SndObj *larg3) {
  Ap *arg1 = (Ap *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Ap_SetR (Ap *larg1, float larg2, SndObj *larg3) {
  Ap *arg1 = (Ap *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetR(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Ap_SetSr (Ap *larg1, float larg2) {
  Ap *arg1 = (Ap *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Ap_Set (Ap *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Ap *arg1 = (Ap *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Ap_Connect (Ap *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Ap *arg1 = (Ap *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Ap_DoProcess (Ap *larg1) {
  short lresult = (short)0 ;
  Ap *arg1 = (Ap *) 0 ;
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


EXPORT char *_wrap_Ap_ErrorMessage (Ap *larg1) {
  char * lresult = (char *)0 ;
  Ap *arg1 = (Ap *) 0 ;
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


EXPORT LoPass *_wrap_new_LoPass_empty () {
  LoPass * lresult = (LoPass *)0 ;
  LoPass *result = 0 ;
  
  try {
    result = (LoPass *)new LoPass();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LoPass *)0;
  }
}


EXPORT LoPass *_wrap_new_LoPass (float larg1, SndObj *larg2, int larg3, float larg4) {
  LoPass * lresult = (LoPass *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  LoPass *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (LoPass *)new LoPass(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LoPass *)0;
  }
}


EXPORT void _wrap_delete_LoPass (LoPass *larg1) {
  LoPass *arg1 = (LoPass *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_LoPass_SetFreq (LoPass *larg1, float larg2) {
  LoPass *arg1 = (LoPass *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_LoPass_SetSr (LoPass *larg1, float larg2) {
  LoPass *arg1 = (LoPass *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_LoPass_Set (LoPass *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  LoPass *arg1 = (LoPass *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_LoPass_DoProcess (LoPass *larg1) {
  short lresult = (short)0 ;
  LoPass *arg1 = (LoPass *) 0 ;
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


EXPORT HiPass *_wrap_new_HiPass_empty () {
  HiPass * lresult = (HiPass *)0 ;
  HiPass *result = 0 ;
  
  try {
    result = (HiPass *)new HiPass();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HiPass *)0;
  }
}


EXPORT HiPass *_wrap_new_HiPass (float larg1, SndObj *larg2, int larg3, float larg4) {
  HiPass * lresult = (HiPass *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  HiPass *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (HiPass *)new HiPass(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HiPass *)0;
  }
}


EXPORT void _wrap_delete_HiPass (HiPass *larg1) {
  HiPass *arg1 = (HiPass *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_HiPass_SetFreq (HiPass *larg1, float larg2) {
  HiPass *arg1 = (HiPass *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_HiPass_SetSr (HiPass *larg1, float larg2) {
  HiPass *arg1 = (HiPass *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_HiPass_Set (HiPass *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  HiPass *arg1 = (HiPass *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Hilb_real_set (Hilb *larg1, SndObj *larg2) {
  Hilb *arg1 = (Hilb *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->real = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_Hilb_real_get (Hilb *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  Hilb *arg1 = (Hilb *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->real);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT void _wrap_Hilb_imag_set (Hilb *larg1, SndObj *larg2) {
  Hilb *arg1 = (Hilb *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->imag = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_Hilb_imag_get (Hilb *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  Hilb *arg1 = (Hilb *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->imag);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT Hilb *_wrap_new_Hilb_empty () {
  Hilb * lresult = (Hilb *)0 ;
  Hilb *result = 0 ;
  
  try {
    result = (Hilb *)new Hilb();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Hilb *)0;
  }
}


EXPORT Hilb *_wrap_new_Hilb (SndObj *larg1, int larg2, float larg3) {
  Hilb * lresult = (Hilb *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_VECSIZE ;
  float arg3 = (float) DEF_SR ;
  Hilb *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (Hilb *)new Hilb(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Hilb *)0;
  }
}


EXPORT void _wrap_delete_Hilb (Hilb *larg1) {
  Hilb *arg1 = (Hilb *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Hilb_DoProcess (Hilb *larg1) {
  short lresult = (short)0 ;
  Hilb *arg1 = (Hilb *) 0 ;
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


EXPORT char *_wrap_Hilb_ErrorMessage (Hilb *larg1) {
  char * lresult = (char *)0 ;
  Hilb *arg1 = (Hilb *) 0 ;
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


EXPORT SyncGrain *_wrap_new_SyncGrain_empty () {
  SyncGrain * lresult = (SyncGrain *)0 ;
  SyncGrain *result = 0 ;
  
  try {
    result = (SyncGrain *)new SyncGrain();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SyncGrain *)0;
  }
}


EXPORT SyncGrain *_wrap_new_SyncGrain (Table *larg1, Table *larg2, float larg3, float larg4, float larg5, float larg6, float larg7, SndObj *larg8, SndObj *larg9, SndObj *larg10, SndObj *larg11, int larg12, int larg13, float larg14) {
  SyncGrain * lresult = (SyncGrain *)0 ;
  Table *arg1 = (Table *) 0 ;
  Table *arg2 = (Table *) 0 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  float arg6 ;
  float arg7 = (float) 1.f ;
  SndObj *arg8 = (SndObj *) 0 ;
  SndObj *arg9 = (SndObj *) 0 ;
  SndObj *arg10 = (SndObj *) 0 ;
  SndObj *arg11 = (SndObj *) 0 ;
  int arg12 = (int) 100 ;
  int arg13 = (int) DEF_VECSIZE ;
  float arg14 = (float) DEF_SR ;
  SyncGrain *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  arg12 = larg12;
  arg13 = larg13;
  arg14 = larg14;
  try {
    result = (SyncGrain *)new SyncGrain(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SyncGrain *)0;
  }
}


EXPORT void _wrap_delete_SyncGrain (SyncGrain *larg1) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_Offset (SyncGrain *larg1, int larg2) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->Offset(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_Offset_seconds (SyncGrain *larg1, float larg2) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->Offset(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetWaveTable (SyncGrain *larg1, Table *larg2) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetWaveTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetEnvelopeTable (SyncGrain *larg1, Table *larg2) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetEnvelopeTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetFreq (SyncGrain *larg1, float larg2, SndObj *larg3) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetAmp (SyncGrain *larg1, float larg2, SndObj *larg3) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetPitch (SyncGrain *larg1, float larg2, SndObj *larg3) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetPitch(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetGrainSize (SyncGrain *larg1, float larg2, SndObj *larg3) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetGrainSize(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SyncGrain_SetPointerRate (SyncGrain *larg1, float larg2) {
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPointerRate(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SyncGrain_Set (SyncGrain *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SyncGrain_Connect (SyncGrain *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SyncGrain *arg1 = (SyncGrain *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SyncGrain_DoProcess (SyncGrain *larg1) {
  short lresult = (short)0 ;
  SyncGrain *arg1 = (SyncGrain *) 0 ;
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


EXPORT char *_wrap_SyncGrain_ErrorMessage (SyncGrain *larg1) {
  char * lresult = (char *)0 ;
  SyncGrain *arg1 = (SyncGrain *) 0 ;
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


EXPORT Mixer *_wrap_new_Mixer_empty () {
  Mixer * lresult = (Mixer *)0 ;
  Mixer *result = 0 ;
  
  try {
    result = (Mixer *)new Mixer();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Mixer *)0;
  }
}


EXPORT Mixer *_wrap_new_Mixer (int larg1, SndObj **larg2, int larg3, float larg4) {
  Mixer * lresult = (Mixer *)0 ;
  int arg1 ;
  SndObj **arg2 = (SndObj **) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Mixer *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Mixer *)new Mixer(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Mixer *)0;
  }
}


EXPORT void _wrap_delete_Mixer (Mixer *larg1) {
  Mixer *arg1 = (Mixer *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Mixer_GetObjNo (Mixer *larg1) {
  int lresult = (int)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
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


EXPORT short _wrap_Mixer_AddObj (Mixer *larg1, SndObj *larg2) {
  short lresult = (short)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  short result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (short)(arg1)->AddObj(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_Mixer_DeleteObj (Mixer *larg1, SndObj *larg2) {
  short lresult = (short)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  short result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (short)(arg1)->DeleteObj(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_Mixer_DoProcess (Mixer *larg1) {
  short lresult = (short)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
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


EXPORT int _wrap_Mixer_Connect (Mixer *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT char *_wrap_Mixer_ErrorMessage (Mixer *larg1) {
  char * lresult = (char *)0 ;
  Mixer *arg1 = (Mixer *) 0 ;
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


EXPORT void _wrap_Pan_left_set (Pan *larg1, SndObj *larg2) {
  Pan *arg1 = (Pan *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->left = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_Pan_left_get (Pan *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  Pan *arg1 = (Pan *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->left);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT void _wrap_Pan_right_set (Pan *larg1, SndObj *larg2) {
  Pan *arg1 = (Pan *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->right = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_Pan_right_get (Pan *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  Pan *arg1 = (Pan *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->right);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT Pan *_wrap_new_Pan_empty () {
  Pan * lresult = (Pan *)0 ;
  Pan *result = 0 ;
  
  try {
    result = (Pan *)new Pan();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pan *)0;
  }
}


EXPORT Pan *_wrap_new_Pan (float larg1, SndObj *larg2, SndObj *larg3, int larg4, int larg5, float larg6) {
  Pan * lresult = (Pan *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) 1024 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  Pan *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (Pan *)new Pan(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Pan *)0;
  }
}


EXPORT void _wrap_delete_Pan (Pan *larg1) {
  Pan *arg1 = (Pan *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Pan_SetPan (Pan *larg1, float larg2, SndObj *larg3) {
  Pan *arg1 = (Pan *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetPan(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Pan_Set (Pan *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Pan *arg1 = (Pan *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Pan_Connect (Pan *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Pan *arg1 = (Pan *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Pan_DoProcess (Pan *larg1) {
  short lresult = (short)0 ;
  Pan *arg1 = (Pan *) 0 ;
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


EXPORT char *_wrap_Pan_ErrorMessage (Pan *larg1) {
  char * lresult = (char *)0 ;
  Pan *arg1 = (Pan *) 0 ;
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


EXPORT Gain *_wrap_new_Gain_empty () {
  Gain * lresult = (Gain *)0 ;
  Gain *result = 0 ;
  
  try {
    result = (Gain *)new Gain();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Gain *)0;
  }
}


EXPORT Gain *_wrap_new_Gain (float larg1, SndObj *larg2, int larg3, float larg4) {
  Gain * lresult = (Gain *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Gain *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Gain *)new Gain(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Gain *)0;
  }
}


EXPORT void _wrap_delete_Gain (Gain *larg1) {
  Gain *arg1 = (Gain *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Gain_Set (Gain *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Gain *arg1 = (Gain *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Gain_SetGain (Gain *larg1, float larg2) {
  Gain *arg1 = (Gain *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetGain(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Gain_SetGainM (Gain *larg1, float larg2) {
  Gain *arg1 = (Gain *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetGainM(arg2);
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_Gain_dBToAmp (Gain *larg1, float larg2) {
  float lresult = (float)0 ;
  Gain *arg1 = (Gain *) 0 ;
  float arg2 ;
  float result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (float)(arg1)->dBToAmp(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT short _wrap_Gain_DoProcess (Gain *larg1) {
  short lresult = (short)0 ;
  Gain *arg1 = (Gain *) 0 ;
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


EXPORT Interp *_wrap_new_Interp_empty () {
  Interp * lresult = (Interp *)0 ;
  Interp *result = 0 ;
  
  try {
    result = (Interp *)new Interp();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Interp *)0;
  }
}


EXPORT Interp *_wrap_new_Interp (float larg1, float larg2, float larg3, float larg4, int larg5, float larg6) {
  Interp * lresult = (Interp *)0 ;
  float arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 = (float) 0.f ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  Interp *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (Interp *)new Interp(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Interp *)0;
  }
}


EXPORT void _wrap_delete_Interp (Interp *larg1) {
  Interp *arg1 = (Interp *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Interp_Set (Interp *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Interp *arg1 = (Interp *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Interp_SetSr (Interp *larg1, float larg2) {
  Interp *arg1 = (Interp *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Interp_Restart (Interp *larg1) {
  Interp *arg1 = (Interp *) 0 ;
  
  arg1 = larg1;
  try {
    (arg1)->Restart();
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Interp_SetCurve (Interp *larg1, float larg2, float larg3, float larg4) {
  Interp *arg1 = (Interp *) 0 ;
  float arg2 ;
  float arg3 ;
  float arg4 = (float) 0.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    (arg1)->SetCurve(arg2,arg3,arg4);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Interp_SetDur (Interp *larg1, float larg2) {
  Interp *arg1 = (Interp *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetDur(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Interp_DoProcess (Interp *larg1) {
  short lresult = (short)0 ;
  Interp *arg1 = (Interp *) 0 ;
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


EXPORT Phase *_wrap_new_Phase_empty () {
  Phase * lresult = (Phase *)0 ;
  Phase *result = 0 ;
  
  try {
    result = (Phase *)new Phase();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Phase *)0;
  }
}


EXPORT Phase *_wrap_new_Phase (float larg1, SndObj *larg2, float larg3, int larg4, float larg5) {
  Phase * lresult = (Phase *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 0.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Phase *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Phase *)new Phase(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Phase *)0;
  }
}


EXPORT void _wrap_delete_Phase (Phase *larg1) {
  Phase *arg1 = (Phase *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Phase_SetFreq (Phase *larg1, float larg2, SndObj *larg3) {
  Phase *arg1 = (Phase *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Phase_SetPhase (Phase *larg1, float larg2) {
  Phase *arg1 = (Phase *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPhase(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Phase_Set (Phase *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Phase *arg1 = (Phase *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Phase_Connect (Phase *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Phase *arg1 = (Phase *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Phase_DoProcess (Phase *larg1) {
  short lresult = (short)0 ;
  Phase *arg1 = (Phase *) 0 ;
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


EXPORT Ring *_wrap_new_Ring_empty () {
  Ring * lresult = (Ring *)0 ;
  Ring *result = 0 ;
  
  try {
    result = (Ring *)new Ring();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Ring *)0;
  }
}


EXPORT Ring *_wrap_new_Ring (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  Ring * lresult = (Ring *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Ring *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Ring *)new Ring(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Ring *)0;
  }
}


EXPORT void _wrap_delete_Ring (Ring *larg1) {
  Ring *arg1 = (Ring *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Ring_SetInput1 (Ring *larg1, SndObj *larg2) {
  Ring *arg1 = (Ring *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput1(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Ring_SetInput2 (Ring *larg1, SndObj *larg2) {
  Ring *arg1 = (Ring *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput2(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Ring_DoProcess (Ring *larg1) {
  short lresult = (short)0 ;
  Ring *arg1 = (Ring *) 0 ;
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


EXPORT int _wrap_Ring_Connect (Ring *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Ring *arg1 = (Ring *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT Unit *_wrap_new_Unit_empty () {
  Unit * lresult = (Unit *)0 ;
  Unit *result = 0 ;
  
  try {
    result = (Unit *)new Unit();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Unit *)0;
  }
}


EXPORT Unit *_wrap_new_Unit (float larg1, short larg2, float larg3, int larg4, float larg5) {
  Unit * lresult = (Unit *)0 ;
  float arg1 ;
  short arg2 = (short) UNIT_SAMPLE ;
  float arg3 = (float) 0.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Unit *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Unit *)new Unit(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Unit *)0;
  }
}


EXPORT void _wrap_delete_Unit (Unit *larg1) {
  Unit *arg1 = (Unit *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Unit_SetAmp (Unit *larg1, float larg2) {
  Unit *arg1 = (Unit *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetAmp(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Unit_SetStep (Unit *larg1, float larg2) {
  Unit *arg1 = (Unit *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetStep(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Unit_SetMode (Unit *larg1, short larg2) {
  Unit *arg1 = (Unit *) 0 ;
  short arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMode(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Unit_Set (Unit *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Unit *arg1 = (Unit *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Unit_DoProcess (Unit *larg1) {
  short lresult = (short)0 ;
  Unit *arg1 = (Unit *) 0 ;
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


EXPORT Lookup *_wrap_new_Lookup_empty () {
  Lookup * lresult = (Lookup *)0 ;
  Lookup *result = 0 ;
  
  try {
    result = (Lookup *)new Lookup();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lookup *)0;
  }
}


EXPORT Lookup *_wrap_new_Lookup (Table *larg1, long larg2, SndObj *larg3, int larg4, int larg5, int larg6, float larg7) {
  Lookup * lresult = (Lookup *)0 ;
  Table *arg1 = (Table *) 0 ;
  long arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) WRAP_AROUND ;
  int arg5 = (int) RAW_VALUE ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Lookup *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Lookup *)new Lookup(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lookup *)0;
  }
}


EXPORT void _wrap_Lookup_SetMode (Lookup *larg1, int larg2, int larg3) {
  Lookup *arg1 = (Lookup *) 0 ;
  int arg2 ;
  int arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetMode(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_delete_Lookup (Lookup *larg1) {
  Lookup *arg1 = (Lookup *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Lookup_Offset (Lookup *larg1, long larg2) {
  Lookup *arg1 = (Lookup *) 0 ;
  long arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->Offset(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Lookup_SetTable (Lookup *larg1, Table *larg2) {
  Lookup *arg1 = (Lookup *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Lookup_Set (Lookup *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Lookup *arg1 = (Lookup *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Lookup_Connect (Lookup *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Lookup *arg1 = (Lookup *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Lookup_DoProcess (Lookup *larg1) {
  short lresult = (short)0 ;
  Lookup *arg1 = (Lookup *) 0 ;
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


EXPORT Lookupi *_wrap_new_Lookupi_empty () {
  Lookupi * lresult = (Lookupi *)0 ;
  Lookupi *result = 0 ;
  
  try {
    result = (Lookupi *)new Lookupi();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lookupi *)0;
  }
}


EXPORT Lookupi *_wrap_new_Lookupi (Table *larg1, long larg2, SndObj *larg3, int larg4, int larg5, int larg6, float larg7) {
  Lookupi * lresult = (Lookupi *)0 ;
  Table *arg1 = (Table *) 0 ;
  long arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  int arg4 = (int) WRAP_AROUND ;
  int arg5 = (int) RAW_VALUE ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  Lookupi *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (Lookupi *)new Lookupi(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Lookupi *)0;
  }
}


EXPORT void _wrap_delete_Lookupi (Lookupi *larg1) {
  Lookupi *arg1 = (Lookupi *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Lookupi_DoProcess (Lookupi *larg1) {
  short lresult = (short)0 ;
  Lookupi *arg1 = (Lookupi *) 0 ;
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


EXPORT Rand *_wrap_new_Rand_empty () {
  Rand * lresult = (Rand *)0 ;
  Rand *result = 0 ;
  
  try {
    result = (Rand *)new Rand();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Rand *)0;
  }
}


EXPORT Rand *_wrap_new_Rand (float larg1, SndObj *larg2, int larg3, float larg4) {
  Rand * lresult = (Rand *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  Rand *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (Rand *)new Rand(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Rand *)0;
  }
}


EXPORT void _wrap_delete_Rand (Rand *larg1) {
  Rand *arg1 = (Rand *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Rand_SetAmp (Rand *larg1, float larg2, SndObj *larg3) {
  Rand *arg1 = (Rand *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Rand_Set (Rand *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Rand *arg1 = (Rand *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Rand_Connect (Rand *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Rand *arg1 = (Rand *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Rand_DoProcess (Rand *larg1) {
  short lresult = (short)0 ;
  Rand *arg1 = (Rand *) 0 ;
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


EXPORT PhOscili *_wrap_new_PhOscili_empty () {
  PhOscili * lresult = (PhOscili *)0 ;
  PhOscili *result = 0 ;
  
  try {
    result = (PhOscili *)new PhOscili();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PhOscili *)0;
  }
}


EXPORT PhOscili *_wrap_new_PhOscili (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, SndObj *larg6, int larg7, float larg8) {
  PhOscili * lresult = (PhOscili *)0 ;
  Table *arg1 = (Table *) 0 ;
  float arg2 ;
  float arg3 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  SndObj *arg6 = (SndObj *) 0 ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  PhOscili *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (PhOscili *)new PhOscili(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PhOscili *)0;
  }
}


EXPORT void _wrap_delete_PhOscili (PhOscili *larg1) {
  PhOscili *arg1 = (PhOscili *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_PhOscili_Connect (PhOscili *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  PhOscili *arg1 = (PhOscili *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_PhOscili_DoProcess (PhOscili *larg1) {
  short lresult = (short)0 ;
  PhOscili *arg1 = (PhOscili *) 0 ;
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


EXPORT Randh *_wrap_new_Randh_empty () {
  Randh * lresult = (Randh *)0 ;
  Randh *result = 0 ;
  
  try {
    result = (Randh *)new Randh();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Randh *)0;
  }
}


EXPORT Randh *_wrap_new_Randh (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  Randh * lresult = (Randh *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  Randh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (Randh *)new Randh(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Randh *)0;
  }
}


EXPORT void _wrap_delete_Randh (Randh *larg1) {
  Randh *arg1 = (Randh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Randh_Connect (Randh *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Randh *arg1 = (Randh *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Randh_SetSr (Randh *larg1, float larg2) {
  Randh *arg1 = (Randh *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_Randh_SetFreq (Randh *larg1, float larg2, SndObj *larg3) {
  Randh *arg1 = (Randh *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreq(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Randh_Set (Randh *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Randh *arg1 = (Randh *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_Randh_DoProcess (Randh *larg1) {
  short lresult = (short)0 ;
  Randh *arg1 = (Randh *) 0 ;
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


EXPORT Randi *_wrap_new_Randi_empty () {
  Randi * lresult = (Randi *)0 ;
  Randi *result = 0 ;
  
  try {
    result = (Randi *)new Randi();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Randi *)0;
  }
}


EXPORT Randi *_wrap_new_Randi (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  Randi * lresult = (Randi *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  Randi *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (Randi *)new Randi(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Randi *)0;
  }
}


EXPORT void _wrap_delete_Randi (Randi *larg1) {
  Randi *arg1 = (Randi *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Randi_DoProcess (Randi *larg1) {
  short lresult = (short)0 ;
  Randi *arg1 = (Randi *) 0 ;
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


EXPORT FFT *_wrap_new_FFT_empty () {
  FFT * lresult = (FFT *)0 ;
  FFT *result = 0 ;
  
  try {
    result = (FFT *)new FFT();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FFT *)0;
  }
}


EXPORT FFT *_wrap_new_FFT (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6) {
  FFT * lresult = (FFT *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_FFTSIZE ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  FFT *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (FFT *)new FFT(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FFT *)0;
  }
}


EXPORT void _wrap_delete_FFT (FFT *larg1) {
  FFT *arg1 = (FFT *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_FFT_GetFFTSize (FFT *larg1) {
  int lresult = (int)0 ;
  FFT *arg1 = (FFT *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetFFTSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_FFT_GetHopSize (FFT *larg1) {
  int lresult = (int)0 ;
  FFT *arg1 = (FFT *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetHopSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_FFT_SetWindow (FFT *larg1, Table *larg2) {
  FFT *arg1 = (FFT *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetWindow(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_FFT_Connect (FFT *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  FFT *arg1 = (FFT *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_FFT_Set (FFT *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  FFT *arg1 = (FFT *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_FFT_SetScale (FFT *larg1, float larg2) {
  FFT *arg1 = (FFT *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetScale(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FFT_SetFFTSize (FFT *larg1, int larg2) {
  FFT *arg1 = (FFT *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFFTSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_FFT_SetHopSize (FFT *larg1, int larg2) {
  FFT *arg1 = (FFT *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHopSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_FFT_DoProcess (FFT *larg1) {
  short lresult = (short)0 ;
  FFT *arg1 = (FFT *) 0 ;
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


EXPORT IFFT *_wrap_new_IFFT_empty () {
  IFFT * lresult = (IFFT *)0 ;
  IFFT *result = 0 ;
  
  try {
    result = (IFFT *)new IFFT();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFFT *)0;
  }
}


EXPORT IFFT *_wrap_new_IFFT (Table *larg1, SndObj *larg2, int larg3, int larg4, float larg5) {
  IFFT * lresult = (IFFT *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  IFFT *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (IFFT *)new IFFT(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFFT *)0;
  }
}


EXPORT void _wrap_delete_IFFT (IFFT *larg1) {
  IFFT *arg1 = (IFFT *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_IFFT_GetFFTSize (IFFT *larg1) {
  int lresult = (int)0 ;
  IFFT *arg1 = (IFFT *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetFFTSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_IFFT_GetHopSize (IFFT *larg1) {
  int lresult = (int)0 ;
  IFFT *arg1 = (IFFT *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetHopSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_IFFT_SetWindow (IFFT *larg1, Table *larg2) {
  IFFT *arg1 = (IFFT *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetWindow(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_IFFT_Connect (IFFT *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  IFFT *arg1 = (IFFT *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_IFFT_Set (IFFT *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  IFFT *arg1 = (IFFT *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_IFFT_SetFFTSize (IFFT *larg1, int larg2) {
  IFFT *arg1 = (IFFT *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFFTSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_IFFT_SetHopSize (IFFT *larg1, int larg2) {
  IFFT *arg1 = (IFFT *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHopSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_IFFT_DoProcess (IFFT *larg1) {
  short lresult = (short)0 ;
  IFFT *arg1 = (IFFT *) 0 ;
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


EXPORT PVA *_wrap_new_PVA_empty () {
  PVA * lresult = (PVA *)0 ;
  PVA *result = 0 ;
  
  try {
    result = (PVA *)new PVA();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVA *)0;
  }
}


EXPORT PVA *_wrap_new_PVA (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6) {
  PVA * lresult = (PVA *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_FFTSIZE ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  PVA *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVA *)new PVA(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVA *)0;
  }
}


EXPORT void _wrap_delete_PVA (PVA *larg1) {
  PVA *arg1 = (PVA *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT float _wrap_PVA_Outphases (PVA *larg1, int larg2) {
  float lresult = (float)0 ;
  PVA *arg1 = (PVA *) 0 ;
  int arg2 ;
  float result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (float)(arg1)->Outphases(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT int _wrap_PVA_Set (PVA *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVA *arg1 = (PVA *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVA_SetFFTSize (PVA *larg1, int larg2) {
  PVA *arg1 = (PVA *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFFTSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVA_SetHopSize (PVA *larg1, int larg2) {
  PVA *arg1 = (PVA *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHopSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVA_DoProcess (PVA *larg1) {
  short lresult = (short)0 ;
  PVA *arg1 = (PVA *) 0 ;
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


EXPORT PVS *_wrap_new_PVS_empty () {
  PVS * lresult = (PVS *)0 ;
  PVS *result = 0 ;
  
  try {
    result = (PVS *)new PVS();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVS *)0;
  }
}


EXPORT PVS *_wrap_new_PVS (Table *larg1, SndObj *larg2, int larg3, int larg4, float larg5) {
  PVS * lresult = (PVS *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  PVS *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (PVS *)new PVS(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVS *)0;
  }
}


EXPORT void _wrap_delete_PVS (PVS *larg1) {
  PVS *arg1 = (PVS *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_PVS_Set (PVS *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVS *arg1 = (PVS *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVS_SetFFTSize (PVS *larg1, int larg2) {
  PVS *arg1 = (PVS *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFFTSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVS_SetHopSize (PVS *larg1, int larg2) {
  PVS *arg1 = (PVS *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHopSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVS_DoProcess (PVS *larg1) {
  short lresult = (short)0 ;
  PVS *arg1 = (PVS *) 0 ;
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


EXPORT SndObj *_wrap_PVRead_Outchannel (PVRead *larg1, int larg2) {
  SndObj * lresult = (SndObj *)0 ;
  PVRead *arg1 = (PVRead *) 0 ;
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


EXPORT int _wrap_PVRead_Set (PVRead *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVRead *arg1 = (PVRead *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVRead_SetInput (PVRead *larg1, char *larg2) {
  PVRead *arg1 = (PVRead *) 0 ;
  char *arg2 = (char *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVRead_SetTimescale (PVRead *larg1, float larg2) {
  PVRead *arg1 = (PVRead *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTimescale(arg2);
    
  } catch (...) {
    
  }
}


EXPORT PVRead *_wrap_new_PVRead_empty () {
  PVRead * lresult = (PVRead *)0 ;
  PVRead *result = 0 ;
  
  try {
    result = (PVRead *)new PVRead();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVRead *)0;
  }
}


EXPORT PVRead *_wrap_new_PVRead (char *larg1, float larg2, int larg3, float larg4) {
  PVRead * lresult = (PVRead *)0 ;
  char *arg1 = (char *) 0 ;
  float arg2 = (float) 1.0 ;
  int arg3 = (int) DEF_VECSIZE ;
  float arg4 = (float) DEF_SR ;
  PVRead *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (PVRead *)new PVRead(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVRead *)0;
  }
}


EXPORT void _wrap_delete_PVRead (PVRead *larg1) {
  PVRead *arg1 = (PVRead *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVRead_DoProcess (PVRead *larg1) {
  short lresult = (short)0 ;
  PVRead *arg1 = (PVRead *) 0 ;
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


EXPORT IFGram *_wrap_new_IFGram_empty () {
  IFGram * lresult = (IFGram *)0 ;
  IFGram *result = 0 ;
  
  try {
    result = (IFGram *)new IFGram();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFGram *)0;
  }
}


EXPORT IFGram *_wrap_new_IFGram (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6) {
  IFGram * lresult = (IFGram *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_FFTSIZE ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  IFGram *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (IFGram *)new IFGram(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFGram *)0;
  }
}


EXPORT void _wrap_delete_IFGram (IFGram *larg1) {
  IFGram *arg1 = (IFGram *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_IFGram_Set (IFGram *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  IFGram *arg1 = (IFGram *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_IFGram_Connect (IFGram *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  IFGram *arg1 = (IFGram *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_IFGram_SetFFTSize (IFGram *larg1, int larg2) {
  IFGram *arg1 = (IFGram *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFFTSize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_IFGram_DoProcess (IFGram *larg1) {
  short lresult = (short)0 ;
  IFGram *arg1 = (IFGram *) 0 ;
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


EXPORT SinAnal *_wrap_new_SinAnal_empty () {
  SinAnal * lresult = (SinAnal *)0 ;
  SinAnal *result = 0 ;
  
  try {
    result = (SinAnal *)new SinAnal();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SinAnal *)0;
  }
}


EXPORT SinAnal *_wrap_new_SinAnal (SndObj *larg1, float larg2, int larg3, int larg4, int larg5, float larg6) {
  SinAnal * lresult = (SinAnal *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 ;
  int arg4 = (int) 1 ;
  int arg5 = (int) 3 ;
  float arg6 = (float) DEF_SR ;
  SinAnal *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (SinAnal *)new SinAnal(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SinAnal *)0;
  }
}


EXPORT void _wrap_delete_SinAnal (SinAnal *larg1) {
  SinAnal *arg1 = (SinAnal *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SinAnal_GetTrackID (SinAnal *larg1, int larg2) {
  int lresult = (int)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  int arg2 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (int)(arg1)->GetTrackID(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SinAnal_GetTracks (SinAnal *larg1) {
  int lresult = (int)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetTracks();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SinAnal_Set (SinAnal *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SinAnal_Connect (SinAnal *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SinAnal_SetThreshold (SinAnal *larg1, float larg2) {
  SinAnal *arg1 = (SinAnal *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetThreshold(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SinAnal_SetIFGram (SinAnal *larg1, SndObj *larg2) {
  SinAnal *arg1 = (SinAnal *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetIFGram(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SinAnal_SetMaxTracks (SinAnal *larg1, int larg2) {
  SinAnal *arg1 = (SinAnal *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaxTracks(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SinAnal_DoProcess (SinAnal *larg1) {
  short lresult = (short)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
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


EXPORT SinSyn *_wrap_new_SinSyn_empty () {
  SinSyn * lresult = (SinSyn *)0 ;
  SinSyn *result = 0 ;
  
  try {
    result = (SinSyn *)new SinSyn();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SinSyn *)0;
  }
}


EXPORT SinSyn *_wrap_new_SinSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, int larg5, float larg6) {
  SinSyn * lresult = (SinSyn *)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  int arg2 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 = (float) 1.f ;
  int arg5 = (int) DEF_VECSIZE ;
  float arg6 = (float) DEF_SR ;
  SinSyn *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (SinSyn *)new SinSyn(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SinSyn *)0;
  }
}


EXPORT void _wrap_delete_SinSyn (SinSyn *larg1) {
  SinSyn *arg1 = (SinSyn *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SinSyn_SetTable (SinSyn *larg1, Table *larg2) {
  SinSyn *arg1 = (SinSyn *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SinSyn_SetMaxTracks (SinSyn *larg1, int larg2) {
  SinSyn *arg1 = (SinSyn *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaxTracks(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SinSyn_SetScale (SinSyn *larg1, float larg2) {
  SinSyn *arg1 = (SinSyn *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetScale(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SinSyn_Set (SinSyn *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SinSyn *arg1 = (SinSyn *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SinSyn_Connect (SinSyn *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SinSyn *arg1 = (SinSyn *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SinSyn_DoProcess (SinSyn *larg1) {
  short lresult = (short)0 ;
  SinSyn *arg1 = (SinSyn *) 0 ;
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


EXPORT ReSyn *_wrap_new_ReSyn_empty () {
  ReSyn * lresult = (ReSyn *)0 ;
  ReSyn *result = 0 ;
  
  try {
    result = (ReSyn *)new ReSyn();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ReSyn *)0;
  }
}


EXPORT ReSyn *_wrap_new_ReSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, float larg5, float larg6, int larg7, float larg8) {
  ReSyn * lresult = (ReSyn *)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  int arg2 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 = (float) 1.f ;
  float arg5 = (float) 1.f ;
  float arg6 = (float) 1.f ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  ReSyn *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (ReSyn *)new ReSyn(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ReSyn *)0;
  }
}


EXPORT void _wrap_ReSyn_SetPitch (ReSyn *larg1, float larg2) {
  ReSyn *arg1 = (ReSyn *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPitch(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_ReSyn_SetTimeScale (ReSyn *larg1, float larg2) {
  ReSyn *arg1 = (ReSyn *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTimeScale(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_ReSyn_Set (ReSyn *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  ReSyn *arg1 = (ReSyn *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_delete_ReSyn (ReSyn *larg1) {
  ReSyn *arg1 = (ReSyn *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_ReSyn_DoProcess (ReSyn *larg1) {
  short lresult = (short)0 ;
  ReSyn *arg1 = (ReSyn *) 0 ;
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


EXPORT AdSyn *_wrap_new_AdSyn_empty () {
  AdSyn * lresult = (AdSyn *)0 ;
  AdSyn *result = 0 ;
  
  try {
    result = (AdSyn *)new AdSyn();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (AdSyn *)0;
  }
}


EXPORT AdSyn *_wrap_new_AdSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, float larg5, int larg6, float larg7) {
  AdSyn * lresult = (AdSyn *)0 ;
  SinAnal *arg1 = (SinAnal *) 0 ;
  int arg2 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 = (float) 1.f ;
  float arg5 = (float) 1.f ;
  int arg6 = (int) DEF_VECSIZE ;
  float arg7 = (float) DEF_SR ;
  AdSyn *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (AdSyn *)new AdSyn(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (AdSyn *)0;
  }
}


EXPORT void _wrap_delete_AdSyn (AdSyn *larg1) {
  AdSyn *arg1 = (AdSyn *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_AdSyn_DoProcess (AdSyn *larg1) {
  short lresult = (short)0 ;
  AdSyn *arg1 = (AdSyn *) 0 ;
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


EXPORT IFAdd *_wrap_new_IFAdd_empty () {
  IFAdd * lresult = (IFAdd *)0 ;
  IFAdd *result = 0 ;
  
  try {
    result = (IFAdd *)new IFAdd();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFAdd *)0;
  }
}


EXPORT IFAdd *_wrap_new_IFAdd (IFGram *larg1, int larg2, Table *larg3, float larg4, float larg5, float larg6, int larg7, float larg8) {
  IFAdd * lresult = (IFAdd *)0 ;
  IFGram *arg1 = (IFGram *) 0 ;
  int arg2 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 = (float) 1.f ;
  float arg5 = (float) 1.f ;
  float arg6 = (float) 1.f ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  IFAdd *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (IFAdd *)new IFAdd(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (IFAdd *)0;
  }
}


EXPORT void _wrap_delete_IFAdd (IFAdd *larg1) {
  IFAdd *arg1 = (IFAdd *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_IFAdd_DoProcess (IFAdd *larg1) {
  short lresult = (short)0 ;
  IFAdd *arg1 = (IFAdd *) 0 ;
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


EXPORT SpecMult *_wrap_new_SpecMult_empty () {
  SpecMult * lresult = (SpecMult *)0 ;
  SpecMult *result = 0 ;
  
  try {
    result = (SpecMult *)new SpecMult();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecMult *)0;
  }
}


EXPORT SpecMult *_wrap_new_SpecMult (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  SpecMult * lresult = (SpecMult *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecMult *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecMult *)new SpecMult(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecMult *)0;
  }
}


EXPORT SpecMult *_wrap_new_SpecMult_table (Table *larg1, SndObj *larg2, int larg3, float larg4) {
  SpecMult * lresult = (SpecMult *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecMult *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecMult *)new SpecMult(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecMult *)0;
  }
}


EXPORT void _wrap_delete_SpecMult (SpecMult *larg1) {
  SpecMult *arg1 = (SpecMult *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SpecMult_Connect (SpecMult *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SpecMult *arg1 = (SpecMult *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SpecMult_SetInput2 (SpecMult *larg1, SndObj *larg2) {
  SpecMult *arg1 = (SpecMult *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetInput2(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SpecMult_SetTable (SpecMult *larg1, Table *larg2) {
  SpecMult *arg1 = (SpecMult *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecMult_DoProcess (SpecMult *larg1) {
  short lresult = (short)0 ;
  SpecMult *arg1 = (SpecMult *) 0 ;
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


EXPORT SpecInterp *_wrap_new_SpecInterp_empty () {
  SpecInterp * lresult = (SpecInterp *)0 ;
  SpecInterp *result = 0 ;
  
  try {
    result = (SpecInterp *)new SpecInterp();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecInterp *)0;
  }
}


EXPORT SpecInterp *_wrap_new_SpecInterp (float larg1, SndObj *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  SpecInterp * lresult = (SpecInterp *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  SpecInterp *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (SpecInterp *)new SpecInterp(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecInterp *)0;
  }
}


EXPORT void _wrap_delete_SpecInterp (SpecInterp *larg1) {
  SpecInterp *arg1 = (SpecInterp *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SpecInterp_Connect (SpecInterp *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SpecInterp *arg1 = (SpecInterp *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SpecInterp_Set (SpecInterp *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SpecInterp *arg1 = (SpecInterp *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SpecInterp_SetInterp (SpecInterp *larg1, float larg2, SndObj *larg3) {
  SpecInterp *arg1 = (SpecInterp *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetInterp(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecInterp_DoProcess (SpecInterp *larg1) {
  short lresult = (short)0 ;
  SpecInterp *arg1 = (SpecInterp *) 0 ;
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


EXPORT PVMask *_wrap_new_PVMask_empty () {
  PVMask * lresult = (PVMask *)0 ;
  PVMask *result = 0 ;
  
  try {
    result = (PVMask *)new PVMask();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMask *)0;
  }
}


EXPORT PVMask *_wrap_new_PVMask (float larg1, SndObj *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  PVMask * lresult = (PVMask *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  PVMask *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVMask *)new PVMask(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMask *)0;
  }
}


EXPORT PVMask *_wrap_new_PVMask_table (float larg1, Table *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6) {
  PVMask * lresult = (PVMask *)0 ;
  float arg1 ;
  Table *arg2 = (Table *) 0 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  PVMask *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVMask *)new PVMask(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMask *)0;
  }
}


EXPORT void _wrap_delete_PVMask (PVMask *larg1) {
  PVMask *arg1 = (PVMask *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_PVMask_Connect (PVMask *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  PVMask *arg1 = (PVMask *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_PVMask_Set (PVMask *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVMask *arg1 = (PVMask *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVMask_SetMaskInput (PVMask *larg1, SndObj *larg2) {
  PVMask *arg1 = (PVMask *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaskInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVMask_SetMaskTable (PVMask *larg1, Table *larg2) {
  PVMask *arg1 = (PVMask *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMaskTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVMask_SetMaskGain (PVMask *larg1, float larg2, SndObj *larg3) {
  PVMask *arg1 = (PVMask *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetMaskGain(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVMask_DoProcess (PVMask *larg1) {
  short lresult = (short)0 ;
  PVMask *arg1 = (PVMask *) 0 ;
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


EXPORT int _wrap_PVTransp_Set (PVTransp *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVTransp *arg1 = (PVTransp *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_PVTransp_Connect (PVTransp *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  PVTransp *arg1 = (PVTransp *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVTransp_SetPitch (PVTransp *larg1, float larg2, SndObj *larg3) {
  PVTransp *arg1 = (PVTransp *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetPitch(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVTransp_SetMode (PVTransp *larg1, int larg2) {
  PVTransp *arg1 = (PVTransp *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMode(arg2);
    
  } catch (...) {
    
  }
}


EXPORT PVTransp *_wrap_new_PVTransp_empty () {
  PVTransp * lresult = (PVTransp *)0 ;
  PVTransp *result = 0 ;
  
  try {
    result = (PVTransp *)new PVTransp();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVTransp *)0;
  }
}


EXPORT PVTransp *_wrap_new_PVTransp (SndObj *larg1, float larg2, int larg3, SndObj *larg4, int larg5, float larg6) {
  PVTransp * lresult = (PVTransp *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) NORMAL_TRANSP ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  PVTransp *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVTransp *)new PVTransp(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVTransp *)0;
  }
}


EXPORT void _wrap_delete_PVTransp (PVTransp *larg1) {
  PVTransp *arg1 = (PVTransp *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVTransp_DoProcess (PVTransp *larg1) {
  short lresult = (short)0 ;
  PVTransp *arg1 = (PVTransp *) 0 ;
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


EXPORT PVMix *_wrap_new_PVMix_empty () {
  PVMix * lresult = (PVMix *)0 ;
  PVMix *result = 0 ;
  
  try {
    result = (PVMix *)new PVMix();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMix *)0;
  }
}


EXPORT PVMix *_wrap_new_PVMix (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  PVMix * lresult = (PVMix *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  PVMix *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (PVMix *)new PVMix(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMix *)0;
  }
}


EXPORT void _wrap_delete_PVMix (PVMix *larg1) {
  PVMix *arg1 = (PVMix *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVMix_DoProcess (PVMix *larg1) {
  short lresult = (short)0 ;
  PVMix *arg1 = (PVMix *) 0 ;
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


EXPORT int _wrap_PVBlur_Set (PVBlur *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVBlur *arg1 = (PVBlur *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVBlur_SetBlurTime (PVBlur *larg1, float larg2) {
  PVBlur *arg1 = (PVBlur *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetBlurTime(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVBlur_SetHopsize (PVBlur *larg1, int larg2) {
  PVBlur *arg1 = (PVBlur *) 0 ;
  int arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetHopsize(arg2);
    
  } catch (...) {
    
  }
}


EXPORT PVBlur *_wrap_new_PVBlur_empty () {
  PVBlur * lresult = (PVBlur *)0 ;
  PVBlur *result = 0 ;
  
  try {
    result = (PVBlur *)new PVBlur();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVBlur *)0;
  }
}


EXPORT PVBlur *_wrap_new_PVBlur (SndObj *larg1, float larg2, int larg3, int larg4, float larg5) {
  PVBlur * lresult = (PVBlur *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  float arg2 ;
  int arg3 = (int) DEF_VECSIZE ;
  int arg4 = (int) DEF_FFTSIZE ;
  float arg5 = (float) DEF_SR ;
  PVBlur *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (PVBlur *)new PVBlur(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVBlur *)0;
  }
}


EXPORT void _wrap_delete_PVBlur (PVBlur *larg1) {
  PVBlur *arg1 = (PVBlur *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVBlur_DoProcess (PVBlur *larg1) {
  short lresult = (short)0 ;
  PVBlur *arg1 = (PVBlur *) 0 ;
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


EXPORT PVFilter *_wrap_new_PVFilter_empty () {
  PVFilter * lresult = (PVFilter *)0 ;
  PVFilter *result = 0 ;
  
  try {
    result = (PVFilter *)new PVFilter();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVFilter *)0;
  }
}


EXPORT PVFilter *_wrap_new_PVFilter (SndObj *larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6) {
  PVFilter * lresult = (PVFilter *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  PVFilter *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVFilter *)new PVFilter(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVFilter *)0;
  }
}


EXPORT PVFilter *_wrap_new_PVFilter_table (Table *larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6) {
  PVFilter * lresult = (PVFilter *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  SndObj *arg4 = (SndObj *) 0 ;
  int arg5 = (int) DEF_FFTSIZE ;
  float arg6 = (float) DEF_SR ;
  PVFilter *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (PVFilter *)new PVFilter(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVFilter *)0;
  }
}


EXPORT void _wrap_delete_PVFilter (PVFilter *larg1) {
  PVFilter *arg1 = (PVFilter *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_PVFilter_Connect (PVFilter *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  PVFilter *arg1 = (PVFilter *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_PVFilter_Set (PVFilter *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVFilter *arg1 = (PVFilter *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVFilter_SetFilterInput (PVFilter *larg1, SndObj *larg2) {
  PVFilter *arg1 = (PVFilter *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFilterInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVFilter_SetFilterTable (PVFilter *larg1, Table *larg2) {
  PVFilter *arg1 = (PVFilter *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFilterTable(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVFilter_SetAmount (PVFilter *larg1, float larg2, SndObj *larg3) {
  PVFilter *arg1 = (PVFilter *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmount(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVFilter_DoProcess (PVFilter *larg1) {
  short lresult = (short)0 ;
  PVFilter *arg1 = (PVFilter *) 0 ;
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


EXPORT PVMorph *_wrap_new_PVMorph_empty () {
  PVMorph * lresult = (PVMorph *)0 ;
  PVMorph *result = 0 ;
  
  try {
    result = (PVMorph *)new PVMorph();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMorph *)0;
  }
}


EXPORT PVMorph *_wrap_new_PVMorph (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, SndObj *larg6, int larg7, float larg8) {
  PVMorph * lresult = (PVMorph *)0 ;
  float arg1 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  SndObj *arg4 = (SndObj *) 0 ;
  SndObj *arg5 = (SndObj *) 0 ;
  SndObj *arg6 = (SndObj *) 0 ;
  int arg7 = (int) DEF_FFTSIZE ;
  float arg8 = (float) DEF_SR ;
  PVMorph *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (PVMorph *)new PVMorph(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVMorph *)0;
  }
}


EXPORT void _wrap_delete_PVMorph (PVMorph *larg1) {
  PVMorph *arg1 = (PVMorph *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_PVMorph_Connect (PVMorph *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  PVMorph *arg1 = (PVMorph *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_PVMorph_Set (PVMorph *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  PVMorph *arg1 = (PVMorph *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_PVMorph_SetFreqMorph (PVMorph *larg1, float larg2, SndObj *larg3) {
  PVMorph *arg1 = (PVMorph *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreqMorph(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVMorph_SetAmpMorph (PVMorph *larg1, float larg2, SndObj *larg3) {
  PVMorph *arg1 = (PVMorph *) 0 ;
  float arg2 ;
  SndObj *arg3 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetAmpMorph(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVMorph_DoProcess (PVMorph *larg1) {
  short lresult = (short)0 ;
  PVMorph *arg1 = (PVMorph *) 0 ;
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


EXPORT SpecPolar *_wrap_new_SpecPolar_empty () {
  SpecPolar * lresult = (SpecPolar *)0 ;
  SpecPolar *result = 0 ;
  
  try {
    result = (SpecPolar *)new SpecPolar();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecPolar *)0;
  }
}


EXPORT SpecPolar *_wrap_new_SpecPolar (SndObj *larg1, int larg2, float larg3) {
  SpecPolar * lresult = (SpecPolar *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_FFTSIZE ;
  float arg3 = (float) DEF_SR ;
  SpecPolar *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (SpecPolar *)new SpecPolar(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecPolar *)0;
  }
}


EXPORT void _wrap_delete_SpecPolar (SpecPolar *larg1) {
  SpecPolar *arg1 = (SpecPolar *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecPolar_DoProcess (SpecPolar *larg1) {
  short lresult = (short)0 ;
  SpecPolar *arg1 = (SpecPolar *) 0 ;
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


EXPORT void _wrap_SpecSplit_magnitude_set (SpecSplit *larg1, SndObj *larg2) {
  SpecSplit *arg1 = (SpecSplit *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->magnitude = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_SpecSplit_magnitude_get (SpecSplit *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  SpecSplit *arg1 = (SpecSplit *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->magnitude);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT void _wrap_SpecSplit_phase_set (SpecSplit *larg1, SndObj *larg2) {
  SpecSplit *arg1 = (SpecSplit *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    if (arg1) (arg1)->phase = arg2;
    
  } catch (...) {
    
  }
}


EXPORT SndObj *_wrap_SpecSplit_phase_get (SpecSplit *larg1) {
  SndObj * lresult = (SndObj *)0 ;
  SpecSplit *arg1 = (SpecSplit *) 0 ;
  SndObj *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (SndObj *) ((arg1)->phase);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndObj *)0;
  }
}


EXPORT SpecSplit *_wrap_new_SpecSplit_empty () {
  SpecSplit * lresult = (SpecSplit *)0 ;
  SpecSplit *result = 0 ;
  
  try {
    result = (SpecSplit *)new SpecSplit();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecSplit *)0;
  }
}


EXPORT SpecSplit *_wrap_new_SpecSplit (SndObj *larg1, int larg2, float larg3) {
  SpecSplit * lresult = (SpecSplit *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_FFTSIZE+2 ;
  float arg3 = (float) DEF_SR ;
  SpecSplit *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (SpecSplit *)new SpecSplit(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecSplit *)0;
  }
}


EXPORT void _wrap_delete_SpecSplit (SpecSplit *larg1) {
  SpecSplit *arg1 = (SpecSplit *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecSplit_DoProcess (SpecSplit *larg1) {
  short lresult = (short)0 ;
  SpecSplit *arg1 = (SpecSplit *) 0 ;
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


EXPORT void _wrap_SpecThresh_SetThreshold (SpecThresh *larg1, float larg2) {
  SpecThresh *arg1 = (SpecThresh *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetThreshold(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SpecThresh_Set (SpecThresh *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SpecThresh *arg1 = (SpecThresh *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT SpecThresh *_wrap_new_SpecThresh_empty () {
  SpecThresh * lresult = (SpecThresh *)0 ;
  SpecThresh *result = 0 ;
  
  try {
    result = (SpecThresh *)new SpecThresh();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecThresh *)0;
  }
}


EXPORT SpecThresh *_wrap_new_SpecThresh (float larg1, SndObj *larg2, int larg3, float larg4) {
  SpecThresh * lresult = (SpecThresh *)0 ;
  float arg1 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecThresh *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecThresh *)new SpecThresh(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecThresh *)0;
  }
}


EXPORT void _wrap_delete_SpecThresh (SpecThresh *larg1) {
  SpecThresh *arg1 = (SpecThresh *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecThresh_DoProcess (SpecThresh *larg1) {
  short lresult = (short)0 ;
  SpecThresh *arg1 = (SpecThresh *) 0 ;
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


EXPORT SpecVoc *_wrap_new_SpecVoc_empty () {
  SpecVoc * lresult = (SpecVoc *)0 ;
  SpecVoc *result = 0 ;
  
  try {
    result = (SpecVoc *)new SpecVoc();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecVoc *)0;
  }
}


EXPORT SpecVoc *_wrap_new_SpecVoc (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  SpecVoc * lresult = (SpecVoc *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecVoc *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecVoc *)new SpecVoc(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecVoc *)0;
  }
}


EXPORT void _wrap_delete_SpecVoc (SpecVoc *larg1) {
  SpecVoc *arg1 = (SpecVoc *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecVoc_DoProcess (SpecVoc *larg1) {
  short lresult = (short)0 ;
  SpecVoc *arg1 = (SpecVoc *) 0 ;
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


EXPORT SpecCart *_wrap_new_SpecCart_empty () {
  SpecCart * lresult = (SpecCart *)0 ;
  SpecCart *result = 0 ;
  
  try {
    result = (SpecCart *)new SpecCart();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecCart *)0;
  }
}


EXPORT SpecCart *_wrap_new_SpecCart (SndObj *larg1, int larg2, float larg3) {
  SpecCart * lresult = (SpecCart *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  int arg2 = (int) DEF_FFTSIZE ;
  float arg3 = (float) DEF_SR ;
  SpecCart *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (SpecCart *)new SpecCart(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecCart *)0;
  }
}


EXPORT void _wrap_delete_SpecCart (SpecCart *larg1) {
  SpecCart *arg1 = (SpecCart *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecCart_DoProcess (SpecCart *larg1) {
  short lresult = (short)0 ;
  SpecCart *arg1 = (SpecCart *) 0 ;
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


EXPORT SpecCombine *_wrap_new_SpecCombine_empty () {
  SpecCombine * lresult = (SpecCombine *)0 ;
  SpecCombine *result = 0 ;
  
  try {
    result = (SpecCombine *)new SpecCombine();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecCombine *)0;
  }
}


EXPORT SpecCombine *_wrap_new_SpecCombine (SndObj *larg1, SndObj *larg2, int larg3, float larg4) {
  SpecCombine * lresult = (SpecCombine *)0 ;
  SndObj *arg1 = (SndObj *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecCombine *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecCombine *)new SpecCombine(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecCombine *)0;
  }
}


EXPORT void _wrap_delete_SpecCombine (SpecCombine *larg1) {
  SpecCombine *arg1 = (SpecCombine *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SpecCombine_SetPhaseInput (SpecCombine *larg1, SndObj *larg2) {
  SpecCombine *arg1 = (SpecCombine *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPhaseInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SpecCombine_SetMagInput (SpecCombine *larg1, SndObj *larg2) {
  SpecCombine *arg1 = (SpecCombine *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetMagInput(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SpecCombine_Connect (SpecCombine *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SpecCombine *arg1 = (SpecCombine *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SpecCombine_DoProcess (SpecCombine *larg1) {
  short lresult = (short)0 ;
  SpecCombine *arg1 = (SpecCombine *) 0 ;
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


EXPORT SpecIn *_wrap_new_SpecIn_empty () {
  SpecIn * lresult = (SpecIn *)0 ;
  SpecIn *result = 0 ;
  
  try {
    result = (SpecIn *)new SpecIn();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecIn *)0;
  }
}


EXPORT SpecIn *_wrap_new_SpecIn (SndFIO *larg1, short larg2, int larg3, float larg4) {
  SpecIn * lresult = (SpecIn *)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  short arg2 = (short) 1 ;
  int arg3 = (int) DEF_FFTSIZE ;
  float arg4 = (float) DEF_SR ;
  SpecIn *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (SpecIn *)new SpecIn(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecIn *)0;
  }
}


EXPORT void _wrap_delete_SpecIn (SpecIn *larg1) {
  SpecIn *arg1 = (SpecIn *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SpecIn_SetInput (SpecIn *larg1, SndIO *larg2, short larg3) {
  SpecIn *arg1 = (SpecIn *) 0 ;
  SndIO *arg2 = (SndIO *) 0 ;
  short arg3 = (short) 1 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetInput(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SpecIn_Connect (SpecIn *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  SpecIn *arg1 = (SpecIn *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SpecIn_Set (SpecIn *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  SpecIn *arg1 = (SpecIn *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SpecIn_DoProcess (SpecIn *larg1) {
  short lresult = (short)0 ;
  SpecIn *arg1 = (SpecIn *) 0 ;
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


EXPORT char *_wrap_SpecIn_ErrorMessage (SpecIn *larg1) {
  char * lresult = (char *)0 ;
  SpecIn *arg1 = (SpecIn *) 0 ;
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


EXPORT Convol *_wrap_new_Convol_empty () {
  Convol * lresult = (Convol *)0 ;
  Convol *result = 0 ;
  
  try {
    result = (Convol *)new Convol();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Convol *)0;
  }
}


EXPORT Convol *_wrap_new_Convol (Table *larg1, SndObj *larg2, float larg3, int larg4, float larg5) {
  Convol * lresult = (Convol *)0 ;
  Table *arg1 = (Table *) 0 ;
  SndObj *arg2 = (SndObj *) 0 ;
  float arg3 = (float) 1.f ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  Convol *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (Convol *)new Convol(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (Convol *)0;
  }
}


EXPORT void _wrap_delete_Convol (Convol *larg1) {
  Convol *arg1 = (Convol *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_Convol_Connect (Convol *larg1, char *larg2, void *larg3) {
  int lresult = (int)0 ;
  Convol *arg1 = (Convol *) 0 ;
  char *arg2 = (char *) 0 ;
  void *arg3 = (void *) 0 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Connect(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_Convol_Set (Convol *larg1, char *larg2, float larg3) {
  int lresult = (int)0 ;
  Convol *arg1 = (Convol *) 0 ;
  char *arg2 = (char *) 0 ;
  float arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->Set(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_Convol_SetImpulse (Convol *larg1, Table *larg2, float larg3) {
  Convol *arg1 = (Convol *) 0 ;
  Table *arg2 = (Table *) 0 ;
  float arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetImpulse(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_Convol_DoProcess (Convol *larg1) {
  short lresult = (short)0 ;
  Convol *arg1 = (Convol *) 0 ;
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


EXPORT FILE *_wrap_SndFIO_GetFile (SndFIO *larg1) {
  FILE * lresult = (FILE *)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  FILE *result = 0 ;
  
  arg1 = larg1;
  try {
    result = (FILE *)(arg1)->GetFile();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (FILE *)0;
  }
}


EXPORT short _wrap_SndFIO_GetMode (SndFIO *larg1) {
  short lresult = (short)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->GetMode();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT void _wrap_SndFIO_SetPos_float (SndFIO *larg1, float larg2) {
  SndFIO *arg1 = (SndFIO *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPos(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndFIO_SetPos (SndFIO *larg1, long larg2) {
  SndFIO *arg1 = (SndFIO *) 0 ;
  long arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPos(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndFIO_Eof (SndFIO *larg1) {
  int lresult = (int)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->Eof();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT long _wrap_SndFIO_GetDataFrames (SndFIO *larg1) {
  long lresult = (long)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  long result;
  
  arg1 = larg1;
  try {
    result = (long)(arg1)->GetDataFrames();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (long)0;
  }
}


EXPORT float _wrap_SndFIO_GetPos (SndFIO *larg1) {
  float lresult = (float)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  float result;
  
  arg1 = larg1;
  try {
    result = (float)(arg1)->GetPos();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (float)0;
  }
}


EXPORT short _wrap_SndFIO_GetStatus (SndFIO *larg1) {
  short lresult = (short)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->GetStatus();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT SndFIO *_wrap_new_SndFIO (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8) {
  SndFIO * lresult = (SndFIO *)0 ;
  char *arg1 = (char *) 0 ;
  short arg2 ;
  short arg3 = (short) 1 ;
  short arg4 = (short) 16 ;
  SndObj **arg5 = (SndObj **) 0 ;
  float arg6 = (float) 0.f ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  SndFIO *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (SndFIO *)new SndFIO(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndFIO *)0;
  }
}


EXPORT void _wrap_delete_SndFIO (SndFIO *larg1) {
  SndFIO *arg1 = (SndFIO *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndFIO_Read (SndFIO *larg1) {
  short lresult = (short)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndFIO_Write (SndFIO *larg1) {
  short lresult = (short)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_SndFIO_ErrorMessage (SndFIO *larg1) {
  char * lresult = (char *)0 ;
  SndFIO *arg1 = (SndFIO *) 0 ;
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


EXPORT wave_head *_wrap_SndWave_GetHeader (SndWave *larg1) {
  wave_head * lresult = (wave_head *)0 ;
  SndWave *arg1 = (SndWave *) 0 ;
  wave_head result;
  
  arg1 = larg1;
  try {
    result = (arg1)->GetHeader();
    
    lresult = new wave_head(result);
    return lresult;
  } catch (...) {
    return (wave_head *)0;
  }
}


EXPORT SndWave *_wrap_new_SndWave (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8) {
  SndWave * lresult = (SndWave *)0 ;
  char *arg1 = (char *) 0 ;
  short arg2 = (short) OVERWRITE ;
  short arg3 = (short) 1 ;
  short arg4 = (short) 16 ;
  SndObj **arg5 = (SndObj **) 0 ;
  float arg6 = (float) 0.f ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  SndWave *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (SndWave *)new SndWave(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndWave *)0;
  }
}


EXPORT void _wrap_delete_SndWave (SndWave *larg1) {
  SndWave *arg1 = (SndWave *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndWave_Read (SndWave *larg1) {
  short lresult = (short)0 ;
  SndWave *arg1 = (SndWave *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndWave_Write (SndWave *larg1) {
  short lresult = (short)0 ;
  SndWave *arg1 = (SndWave *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT bool _wrap_SndWave_IsWave (SndWave *larg1) {
  bool lresult = (bool)0 ;
  SndWave *arg1 = (SndWave *) 0 ;
  bool result;
  
  arg1 = larg1;
  try {
    result = (bool)(arg1)->IsWave();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (bool)0;
  }
}


EXPORT char *_wrap_SndWave_ErrorMessage (SndWave *larg1) {
  char * lresult = (char *)0 ;
  SndWave *arg1 = (SndWave *) 0 ;
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


EXPORT SndWaveX *_wrap_new_SndWaveX (char *larg1, short larg2, short larg3, int larg4, short larg5, short larg6, SndObj **larg7, float larg8, int larg9, float larg10) {
  SndWaveX * lresult = (SndWaveX *)0 ;
  char *arg1 = (char *) 0 ;
  short arg2 = (short) OVERWRITE ;
  short arg3 = (short) 1 ;
  int arg4 = (int) 0 ;
  short arg5 = (short) 16 ;
  short arg6 = (short) PCM ;
  SndObj **arg7 = (SndObj **) 0 ;
  float arg8 = (float) 0.f ;
  int arg9 = (int) DEF_VECSIZE ;
  float arg10 = (float) DEF_SR ;
  SndWaveX *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  try {
    result = (SndWaveX *)new SndWaveX(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndWaveX *)0;
  }
}


EXPORT void _wrap_delete_SndWaveX (SndWaveX *larg1) {
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndWaveX_GetHeader (SndWaveX *larg1, WAVEFORMATEXTENSIBLE *larg2) {
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  WAVEFORMATEXTENSIBLE *arg2 = (WAVEFORMATEXTENSIBLE *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->GetHeader(arg2);
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndWaveX_GetChannelMask (SndWaveX *larg1) {
  int lresult = (int)0 ;
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetChannelMask();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT short _wrap_SndWaveX_Read (SndWaveX *larg1) {
  short lresult = (short)0 ;
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndWaveX_Write (SndWaveX *larg1) {
  short lresult = (short)0 ;
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT bool _wrap_SndWaveX_IsWaveExtensible (SndWaveX *larg1) {
  bool lresult = (bool)0 ;
  SndWaveX *arg1 = (SndWaveX *) 0 ;
  bool result;
  
  arg1 = larg1;
  try {
    result = (bool)(arg1)->IsWaveExtensible();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (bool)0;
  }
}


EXPORT SndPVOCEX *_wrap_new_SndPVOCEX (char *larg1, short larg2, int larg3, int larg4, short larg5, int larg6, short larg7, int larg8, SndObj **larg9, float larg10, int larg11, int larg12, float larg13) {
  SndPVOCEX * lresult = (SndPVOCEX *)0 ;
  char *arg1 = (char *) 0 ;
  short arg2 = (short) OVERWRITE ;
  int arg3 = (int) PVOC_AMP_FREQ ;
  int arg4 = (int) HANNING ;
  short arg5 = (short) 1 ;
  int arg6 = (int) 0 ;
  short arg7 = (short) 32 ;
  int arg8 = (int) PCM ;
  SndObj **arg9 = (SndObj **) 0 ;
  float arg10 = (float) 0.f ;
  int arg11 = (int) DEF_VECSIZE ;
  int arg12 = (int) DEF_FFTSIZE ;
  float arg13 = (float) DEF_SR ;
  SndPVOCEX *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  arg12 = larg12;
  arg13 = larg13;
  try {
    result = (SndPVOCEX *)new SndPVOCEX(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndPVOCEX *)0;
  }
}


EXPORT void _wrap_delete_SndPVOCEX (SndPVOCEX *larg1) {
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT int _wrap_SndPVOCEX_GetFFTSize (SndPVOCEX *larg1) {
  int lresult = (int)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetFFTSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndPVOCEX_GetHopSize (SndPVOCEX *larg1) {
  int lresult = (int)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetHopSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndPVOCEX_GetWindowType (SndPVOCEX *larg1) {
  int lresult = (int)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetWindowType();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndPVOCEX_GetWindowLength (SndPVOCEX *larg1) {
  int lresult = (int)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetWindowLength();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SndPVOCEX_GetHeader (SndPVOCEX *larg1, WAVEFORMATPVOCEX *larg2) {
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  WAVEFORMATPVOCEX *arg2 = (WAVEFORMATPVOCEX *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->GetHeader(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndPVOCEX_SetTimePos (SndPVOCEX *larg1, float larg2) {
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTimePos(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndPVOCEX_Read (SndPVOCEX *larg1) {
  short lresult = (short)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndPVOCEX_Write (SndPVOCEX *larg1) {
  short lresult = (short)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT bool _wrap_SndPVOCEX_IsPvocex (SndPVOCEX *larg1) {
  bool lresult = (bool)0 ;
  SndPVOCEX *arg1 = (SndPVOCEX *) 0 ;
  bool result;
  
  arg1 = larg1;
  try {
    result = (bool)(arg1)->IsPvocex();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (bool)0;
  }
}


EXPORT SndSinIO *_wrap_new_SndSinIO (char *larg1, int larg2, float larg3, int larg4, short larg5, short larg6, int larg7, short larg8, int larg9, SndObj **larg10, float larg11, int larg12, int larg13, float larg14) {
  SndSinIO * lresult = (SndSinIO *)0 ;
  char *arg1 = (char *) 0 ;
  int arg2 ;
  float arg3 = (float) 0.01f ;
  int arg4 = (int) HANNING ;
  short arg5 = (short) OVERWRITE ;
  short arg6 = (short) 1 ;
  int arg7 = (int) 0 ;
  short arg8 = (short) 32 ;
  int arg9 = (int) PCM ;
  SndObj **arg10 = (SndObj **) 0 ;
  float arg11 = (float) 0.f ;
  int arg12 = (int) DEF_VECSIZE ;
  int arg13 = (int) DEF_FFTSIZE ;
  float arg14 = (float) DEF_SR ;
  SndSinIO *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  arg10 = larg10;
  arg11 = larg11;
  arg12 = larg12;
  arg13 = larg13;
  arg14 = larg14;
  try {
    result = (SndSinIO *)new SndSinIO(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndSinIO *)0;
  }
}


EXPORT void _wrap_delete_SndSinIO (SndSinIO *larg1) {
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndSinIO_Write (SndSinIO *larg1) {
  short lresult = (short)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndSinIO_Read (SndSinIO *larg1) {
  short lresult = (short)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT int _wrap_SndSinIO_GetTrackID (SndSinIO *larg1, int larg2, int larg3) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int arg2 ;
  int arg3 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (int)(arg1)->GetTrackID(arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndSinIO_GetTracks (SndSinIO *larg1, int larg2) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int arg2 ;
  int result;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (int)(arg1)->GetTracks(arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndSinIO_GetFFTSize (SndSinIO *larg1) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetFFTSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndSinIO_GetHopSize (SndSinIO *larg1) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetHopSize();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndSinIO_GetWindowType (SndSinIO *larg1) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetWindowType();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT int _wrap_SndSinIO_GetMaxTracks (SndSinIO *larg1) {
  int lresult = (int)0 ;
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  int result;
  
  arg1 = larg1;
  try {
    result = (int)(arg1)->GetMaxTracks();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (int)0;
  }
}


EXPORT void _wrap_SndSinIO_GetHeader (SndSinIO *larg1, WAVEFORMATSINUSEX *larg2) {
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  WAVEFORMATSINUSEX *arg2 = (WAVEFORMATSINUSEX *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->GetHeader(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndSinIO_SetTimePos (SndSinIO *larg1, float larg2) {
  SndSinIO *arg1 = (SndSinIO *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetTimePos(arg2);
    
  } catch (...) {
    
  }
}


EXPORT bool _wrap_SndAiff_IsAiff (SndAiff *larg1) {
  bool lresult = (bool)0 ;
  SndAiff *arg1 = (SndAiff *) 0 ;
  bool result;
  
  arg1 = larg1;
  try {
    result = (bool)(arg1)->IsAiff();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (bool)0;
  }
}


EXPORT SndAiff *_wrap_new_SndAiff (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8) {
  SndAiff * lresult = (SndAiff *)0 ;
  char *arg1 = (char *) 0 ;
  short arg2 ;
  short arg3 = (short) 1 ;
  short arg4 = (short) 16 ;
  SndObj **arg5 = (SndObj **) 0 ;
  float arg6 = (float) 0.f ;
  int arg7 = (int) DEF_VECSIZE ;
  float arg8 = (float) DEF_SR ;
  SndAiff *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (SndAiff *)new SndAiff(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndAiff *)0;
  }
}


EXPORT void _wrap_delete_SndAiff (SndAiff *larg1) {
  SndAiff *arg1 = (SndAiff *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndAiff_Read (SndAiff *larg1) {
  short lresult = (short)0 ;
  SndAiff *arg1 = (SndAiff *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndAiff_Write (SndAiff *larg1) {
  short lresult = (short)0 ;
  SndAiff *arg1 = (SndAiff *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_SndAiff_ErrorMessage (SndAiff *larg1) {
  char * lresult = (char *)0 ;
  SndAiff *arg1 = (SndAiff *) 0 ;
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


EXPORT SndBuffer *_wrap_new_SndBuffer (short larg1, int larg2, SndObj **larg3, int larg4, float larg5) {
  SndBuffer * lresult = (SndBuffer *)0 ;
  short arg1 ;
  int arg2 = (int) DEF_VECSIZE ;
  SndObj **arg3 = (SndObj **) 0 ;
  int arg4 = (int) DEF_VECSIZE ;
  float arg5 = (float) DEF_SR ;
  SndBuffer *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (SndBuffer *)new SndBuffer(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndBuffer *)0;
  }
}


EXPORT void _wrap_delete_SndBuffer (SndBuffer *larg1) {
  SndBuffer *arg1 = (SndBuffer *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SndBuffer_Write (SndBuffer *larg1) {
  short lresult = (short)0 ;
  SndBuffer *arg1 = (SndBuffer *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Write();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT short _wrap_SndBuffer_Read (SndBuffer *larg1) {
  short lresult = (short)0 ;
  SndBuffer *arg1 = (SndBuffer *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->Read();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_SndBuffer_ErrorMessage (SndBuffer *larg1) {
  char * lresult = (char *)0 ;
  SndBuffer *arg1 = (SndBuffer *) 0 ;
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


EXPORT void _wrap_HarmTable_SetHarm (HarmTable *larg1, int larg2, int larg3) {
  HarmTable *arg1 = (HarmTable *) 0 ;
  int arg2 ;
  int arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetHarm(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_HarmTable_ErrorMessage (HarmTable *larg1) {
  char * lresult = (char *)0 ;
  HarmTable *arg1 = (HarmTable *) 0 ;
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


EXPORT short _wrap_HarmTable_MakeTable (HarmTable *larg1) {
  short lresult = (short)0 ;
  HarmTable *arg1 = (HarmTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT HarmTable *_wrap_new_HarmTable_empty () {
  HarmTable * lresult = (HarmTable *)0 ;
  HarmTable *result = 0 ;
  
  try {
    result = (HarmTable *)new HarmTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HarmTable *)0;
  }
}


EXPORT void _wrap_HarmTable_SetPhase (HarmTable *larg1, float larg2) {
  HarmTable *arg1 = (HarmTable *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetPhase(arg2);
    
  } catch (...) {
    
  }
}


EXPORT HarmTable *_wrap_new_HarmTable (long larg1, int larg2, int larg3, float larg4) {
  HarmTable * lresult = (HarmTable *)0 ;
  long arg1 ;
  int arg2 ;
  int arg3 ;
  float arg4 = (float) 0.f ;
  HarmTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (HarmTable *)new HarmTable(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HarmTable *)0;
  }
}


EXPORT void _wrap_delete_HarmTable (HarmTable *larg1) {
  HarmTable *arg1 = (HarmTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_UsrHarmTable_SetHarm (UsrHarmTable *larg1, int larg2, float *larg3) {
  UsrHarmTable *arg1 = (UsrHarmTable *) 0 ;
  int arg2 ;
  float *arg3 = (float *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetHarm(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_UsrHarmTable_ErrorMessage (UsrHarmTable *larg1) {
  char * lresult = (char *)0 ;
  UsrHarmTable *arg1 = (UsrHarmTable *) 0 ;
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


EXPORT short _wrap_UsrHarmTable_MakeTable (UsrHarmTable *larg1) {
  short lresult = (short)0 ;
  UsrHarmTable *arg1 = (UsrHarmTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT UsrHarmTable *_wrap_new_UsrHarmTable_empty () {
  UsrHarmTable * lresult = (UsrHarmTable *)0 ;
  UsrHarmTable *result = 0 ;
  
  try {
    result = (UsrHarmTable *)new UsrHarmTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (UsrHarmTable *)0;
  }
}


EXPORT UsrHarmTable *_wrap_new_UsrHarmTable (long larg1, int larg2, float *larg3) {
  UsrHarmTable * lresult = (UsrHarmTable *)0 ;
  long arg1 ;
  int arg2 ;
  float *arg3 = (float *) 0 ;
  UsrHarmTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (UsrHarmTable *)new UsrHarmTable(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (UsrHarmTable *)0;
  }
}


EXPORT void _wrap_delete_UsrHarmTable (UsrHarmTable *larg1) {
  UsrHarmTable *arg1 = (UsrHarmTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_TrisegTable_SetCurve (TrisegTable *larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, float larg9) {
  TrisegTable *arg1 = (TrisegTable *) 0 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  float arg6 ;
  float arg7 ;
  float arg8 ;
  float arg9 = (float) 0.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  try {
    (arg1)->SetCurve(arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_TrisegTable_SetCurve_points (TrisegTable *larg1, float *larg2, float larg3) {
  TrisegTable *arg1 = (TrisegTable *) 0 ;
  float *arg2 = (float *) 0 ;
  float arg3 = (float) 0.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetCurve(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_TrisegTable_ErrorMessage (TrisegTable *larg1) {
  char * lresult = (char *)0 ;
  TrisegTable *arg1 = (TrisegTable *) 0 ;
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


EXPORT short _wrap_TrisegTable_MakeTable (TrisegTable *larg1) {
  short lresult = (short)0 ;
  TrisegTable *arg1 = (TrisegTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT TrisegTable *_wrap_new_TrisegTable_empty () {
  TrisegTable * lresult = (TrisegTable *)0 ;
  TrisegTable *result = 0 ;
  
  try {
    result = (TrisegTable *)new TrisegTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (TrisegTable *)0;
  }
}


EXPORT TrisegTable *_wrap_new_TrisegTable (long larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, float larg9) {
  TrisegTable * lresult = (TrisegTable *)0 ;
  long arg1 ;
  float arg2 ;
  float arg3 ;
  float arg4 ;
  float arg5 ;
  float arg6 ;
  float arg7 ;
  float arg8 ;
  float arg9 = (float) 0.f ;
  TrisegTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  arg9 = larg9;
  try {
    result = (TrisegTable *)new TrisegTable(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (TrisegTable *)0;
  }
}


EXPORT TrisegTable *_wrap_new_TrisegTable_points (long larg1, float *larg2, float larg3) {
  TrisegTable * lresult = (TrisegTable *)0 ;
  long arg1 ;
  float *arg2 = (float *) 0 ;
  float arg3 = (float) 0.f ;
  TrisegTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (TrisegTable *)new TrisegTable(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (TrisegTable *)0;
  }
}


EXPORT void _wrap_delete_TrisegTable (TrisegTable *larg1) {
  TrisegTable *arg1 = (TrisegTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_EnvTable_SetEnvelope (EnvTable *larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6) {
  EnvTable *arg1 = (EnvTable *) 0 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    (arg1)->SetEnvelope(arg2,arg3,arg4,arg5,arg6);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_EnvTable_ErrorMessage (EnvTable *larg1) {
  char * lresult = (char *)0 ;
  EnvTable *arg1 = (EnvTable *) 0 ;
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


EXPORT short _wrap_EnvTable_MakeTable (EnvTable *larg1) {
  short lresult = (short)0 ;
  EnvTable *arg1 = (EnvTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT EnvTable *_wrap_new_EnvTable_empty () {
  EnvTable * lresult = (EnvTable *)0 ;
  EnvTable *result = 0 ;
  
  try {
    result = (EnvTable *)new EnvTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (EnvTable *)0;
  }
}


EXPORT EnvTable *_wrap_new_EnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6) {
  EnvTable * lresult = (EnvTable *)0 ;
  long arg1 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 = (float) 0.f ;
  EnvTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  try {
    result = (EnvTable *)new EnvTable(arg1,arg2,arg3,arg4,arg5,arg6);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (EnvTable *)0;
  }
}


EXPORT void _wrap_delete_EnvTable (EnvTable *larg1) {
  EnvTable *arg1 = (EnvTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_SndTable_SetInput (SndTable *larg1, long larg2, SndFIO *larg3, short larg4) {
  SndTable *arg1 = (SndTable *) 0 ;
  long arg2 ;
  SndFIO *arg3 = (SndFIO *) 0 ;
  short arg4 = (short) 1 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    (arg1)->SetInput(arg2,arg3,arg4);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_SndTable_ErrorMessage (SndTable *larg1) {
  char * lresult = (char *)0 ;
  SndTable *arg1 = (SndTable *) 0 ;
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


EXPORT short _wrap_SndTable_MakeTable (SndTable *larg1) {
  short lresult = (short)0 ;
  SndTable *arg1 = (SndTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT SndTable *_wrap_new_SndTable_empty () {
  SndTable * lresult = (SndTable *)0 ;
  SndTable *result = 0 ;
  
  try {
    result = (SndTable *)new SndTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndTable *)0;
  }
}


EXPORT SndTable *_wrap_new_SndTable (long larg1, SndFIO *larg2, short larg3) {
  SndTable * lresult = (SndTable *)0 ;
  long arg1 ;
  SndFIO *arg2 = (SndFIO *) 0 ;
  short arg3 = (short) 1 ;
  SndTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (SndTable *)new SndTable(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SndTable *)0;
  }
}


EXPORT void _wrap_delete_SndTable (SndTable *larg1) {
  SndTable *arg1 = (SndTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PlnTable_SetPln (PlnTable *larg1, int larg2, double *larg3, float larg4) {
  PlnTable *arg1 = (PlnTable *) 0 ;
  int arg2 ;
  double *arg3 = (double *) 0 ;
  float arg4 = (float) 1.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    (arg1)->SetPln(arg2,arg3,arg4);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_PlnTable_ErrorMessage (PlnTable *larg1) {
  char * lresult = (char *)0 ;
  PlnTable *arg1 = (PlnTable *) 0 ;
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


EXPORT short _wrap_PlnTable_MakeTable (PlnTable *larg1) {
  short lresult = (short)0 ;
  PlnTable *arg1 = (PlnTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT PlnTable *_wrap_new_PlnTable_empty () {
  PlnTable * lresult = (PlnTable *)0 ;
  PlnTable *result = 0 ;
  
  try {
    result = (PlnTable *)new PlnTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PlnTable *)0;
  }
}


EXPORT PlnTable *_wrap_new_PlnTable (long larg1, int larg2, double *larg3, float larg4) {
  PlnTable * lresult = (PlnTable *)0 ;
  long arg1 ;
  int arg2 ;
  double *arg3 = (double *) 0 ;
  float arg4 = (float) 1.f ;
  PlnTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (PlnTable *)new PlnTable(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PlnTable *)0;
  }
}


EXPORT void _wrap_delete_PlnTable (PlnTable *larg1) {
  PlnTable *arg1 = (PlnTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_HammingTable_SetParam (HammingTable *larg1, long larg2, float larg3) {
  HammingTable *arg1 = (HammingTable *) 0 ;
  long arg2 ;
  float arg3 = (float) .54 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetParam(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_HammingTable_ErrorMessage (HammingTable *larg1) {
  char * lresult = (char *)0 ;
  HammingTable *arg1 = (HammingTable *) 0 ;
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


EXPORT short _wrap_HammingTable_MakeTable (HammingTable *larg1) {
  short lresult = (short)0 ;
  HammingTable *arg1 = (HammingTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT HammingTable *_wrap_new_HammingTable_empty () {
  HammingTable * lresult = (HammingTable *)0 ;
  HammingTable *result = 0 ;
  
  try {
    result = (HammingTable *)new HammingTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HammingTable *)0;
  }
}


EXPORT HammingTable *_wrap_new_HammingTable (long larg1, float larg2) {
  HammingTable * lresult = (HammingTable *)0 ;
  long arg1 ;
  float arg2 ;
  HammingTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (HammingTable *)new HammingTable(arg1,arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (HammingTable *)0;
  }
}


EXPORT void _wrap_delete_HammingTable (HammingTable *larg1) {
  HammingTable *arg1 = (HammingTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_NoteTable_SetFreqInterval (NoteTable *larg1, float larg2, float larg3) {
  NoteTable *arg1 = (NoteTable *) 0 ;
  float arg2 ;
  float arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetFreqInterval(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_NoteTable_SetNoteInterval (NoteTable *larg1, short larg2, short larg3) {
  NoteTable *arg1 = (NoteTable *) 0 ;
  short arg2 ;
  short arg3 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetNoteInterval(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT NoteTable *_wrap_new_NoteTable_empty () {
  NoteTable * lresult = (NoteTable *)0 ;
  NoteTable *result = 0 ;
  
  try {
    result = (NoteTable *)new NoteTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (NoteTable *)0;
  }
}


EXPORT NoteTable *_wrap_new_NoteTable (short larg1, short larg2, float larg3, float larg4) {
  NoteTable * lresult = (NoteTable *)0 ;
  short arg1 ;
  short arg2 ;
  float arg3 ;
  float arg4 ;
  NoteTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  try {
    result = (NoteTable *)new NoteTable(arg1,arg2,arg3,arg4);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (NoteTable *)0;
  }
}


EXPORT void _wrap_delete_NoteTable (NoteTable *larg1) {
  NoteTable *arg1 = (NoteTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_NoteTable_MakeTable (NoteTable *larg1) {
  short lresult = (short)0 ;
  NoteTable *arg1 = (NoteTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT char *_wrap_NoteTable_ErrorMessage (NoteTable *larg1) {
  char * lresult = (char *)0 ;
  NoteTable *arg1 = (NoteTable *) 0 ;
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


EXPORT void _wrap_UsrDefTable_SetTable (UsrDefTable *larg1, long larg2, float *larg3) {
  UsrDefTable *arg1 = (UsrDefTable *) 0 ;
  long arg2 ;
  float *arg3 = (float *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    (arg1)->SetTable(arg2,arg3);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_UsrDefTable_ErrorMessage (UsrDefTable *larg1) {
  char * lresult = (char *)0 ;
  UsrDefTable *arg1 = (UsrDefTable *) 0 ;
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


EXPORT short _wrap_UsrDefTable_MakeTable (UsrDefTable *larg1) {
  short lresult = (short)0 ;
  UsrDefTable *arg1 = (UsrDefTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT UsrDefTable *_wrap_new_UsrDefTable_empty () {
  UsrDefTable * lresult = (UsrDefTable *)0 ;
  UsrDefTable *result = 0 ;
  
  try {
    result = (UsrDefTable *)new UsrDefTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (UsrDefTable *)0;
  }
}


EXPORT UsrDefTable *_wrap_new_UsrDefTable (long larg1, float *larg2) {
  UsrDefTable * lresult = (UsrDefTable *)0 ;
  long arg1 ;
  float *arg2 = (float *) 0 ;
  UsrDefTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    result = (UsrDefTable *)new UsrDefTable(arg1,arg2);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (UsrDefTable *)0;
  }
}


EXPORT void _wrap_delete_UsrDefTable (UsrDefTable *larg1) {
  UsrDefTable *arg1 = (UsrDefTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_LoPassTable_ErrorMessage (LoPassTable *larg1) {
  char * lresult = (char *)0 ;
  LoPassTable *arg1 = (LoPassTable *) 0 ;
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


EXPORT short _wrap_LoPassTable_MakeTable (LoPassTable *larg1) {
  short lresult = (short)0 ;
  LoPassTable *arg1 = (LoPassTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT void _wrap_LoPassTable_SetFreq (LoPassTable *larg1, float larg2) {
  LoPassTable *arg1 = (LoPassTable *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetFreq(arg2);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_LoPassTable_SetSr (LoPassTable *larg1, float larg2) {
  LoPassTable *arg1 = (LoPassTable *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT LoPassTable *_wrap_new_LoPassTable (int larg1, float larg2, float larg3) {
  LoPassTable * lresult = (LoPassTable *)0 ;
  int arg1 ;
  float arg2 ;
  float arg3 = (float) 44100 ;
  LoPassTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  try {
    result = (LoPassTable *)new LoPassTable(arg1,arg2,arg3);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LoPassTable *)0;
  }
}


EXPORT LoPassTable *_wrap_new_LoPassTable_empty () {
  LoPassTable * lresult = (LoPassTable *)0 ;
  LoPassTable *result = 0 ;
  
  try {
    result = (LoPassTable *)new LoPassTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (LoPassTable *)0;
  }
}


EXPORT void _wrap_delete_LoPassTable (LoPassTable *larg1) {
  LoPassTable *arg1 = (LoPassTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVEnvTable_SetEnvelope (PVEnvTable *larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7) {
  PVEnvTable *arg1 = (PVEnvTable *) 0 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 ;
  float arg7 = (float) 0.f ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    (arg1)->SetEnvelope(arg2,arg3,arg4,arg5,arg6,arg7);
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVEnvTable_SetSr (PVEnvTable *larg1, float larg2) {
  PVEnvTable *arg1 = (PVEnvTable *) 0 ;
  float arg2 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetSr(arg2);
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_PVEnvTable_ErrorMessage (PVEnvTable *larg1) {
  char * lresult = (char *)0 ;
  PVEnvTable *arg1 = (PVEnvTable *) 0 ;
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


EXPORT short _wrap_PVEnvTable_MakeTable (PVEnvTable *larg1) {
  short lresult = (short)0 ;
  PVEnvTable *arg1 = (PVEnvTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT PVEnvTable *_wrap_new_PVEnvTable_empty () {
  PVEnvTable * lresult = (PVEnvTable *)0 ;
  PVEnvTable *result = 0 ;
  
  try {
    result = (PVEnvTable *)new PVEnvTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVEnvTable *)0;
  }
}


EXPORT PVEnvTable *_wrap_new_PVEnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7, float larg8) {
  PVEnvTable * lresult = (PVEnvTable *)0 ;
  long arg1 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 = (float) 0.f ;
  float arg7 = (float) 44100.f ;
  float arg8 = (float) 0.f ;
  PVEnvTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (PVEnvTable *)new PVEnvTable(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVEnvTable *)0;
  }
}


EXPORT void _wrap_delete_PVEnvTable (PVEnvTable *larg1) {
  PVEnvTable *arg1 = (PVEnvTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_SpecEnvTable_MakeTable (SpecEnvTable *larg1) {
  short lresult = (short)0 ;
  SpecEnvTable *arg1 = (SpecEnvTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT SpecEnvTable *_wrap_new_SpecEnvTable_empty () {
  SpecEnvTable * lresult = (SpecEnvTable *)0 ;
  SpecEnvTable *result = 0 ;
  
  try {
    result = (SpecEnvTable *)new SpecEnvTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecEnvTable *)0;
  }
}


EXPORT SpecEnvTable *_wrap_new_SpecEnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7) {
  SpecEnvTable * lresult = (SpecEnvTable *)0 ;
  long arg1 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 = (float) 0.f ;
  float arg7 = (float) 0.f ;
  SpecEnvTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  try {
    result = (SpecEnvTable *)new SpecEnvTable(arg1,arg2,arg3,arg4,arg5,arg6,arg7);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (SpecEnvTable *)0;
  }
}


EXPORT void _wrap_delete_SpecEnvTable (SpecEnvTable *larg1) {
  SpecEnvTable *arg1 = (SpecEnvTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT void _wrap_PVTable_SetTable (PVTable *larg1, SndFIO *larg2, Table *larg3, float larg4, float larg5) {
  PVTable *arg1 = (PVTable *) 0 ;
  SndFIO *arg2 = (SndFIO *) 0 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 ;
  float arg5 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    (arg1)->SetTable(arg2,arg3,arg4,arg5);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_PVTable_MakeTable (PVTable *larg1) {
  short lresult = (short)0 ;
  PVTable *arg1 = (PVTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT PVTable *_wrap_new_PVTable_empty () {
  PVTable * lresult = (PVTable *)0 ;
  PVTable *result = 0 ;
  
  try {
    result = (PVTable *)new PVTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVTable *)0;
  }
}


EXPORT PVTable *_wrap_new_PVTable (int larg1, SndFIO *larg2, Table *larg3, float larg4, float larg5) {
  PVTable * lresult = (PVTable *)0 ;
  int arg1 ;
  SndFIO *arg2 = (SndFIO *) 0 ;
  Table *arg3 = (Table *) 0 ;
  float arg4 ;
  float arg5 ;
  PVTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  try {
    result = (PVTable *)new PVTable(arg1,arg2,arg3,arg4,arg5);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (PVTable *)0;
  }
}


EXPORT void _wrap_delete_PVTable (PVTable *larg1) {
  PVTable *arg1 = (PVTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


EXPORT char *_wrap_PVTable_ErrorMessage (PVTable *larg1) {
  char * lresult = (char *)0 ;
  PVTable *arg1 = (PVTable *) 0 ;
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


EXPORT void _wrap_ImpulseTable_SetWindow (ImpulseTable *larg1, Table *larg2) {
  ImpulseTable *arg1 = (ImpulseTable *) 0 ;
  Table *arg2 = (Table *) 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  try {
    (arg1)->SetWindow(arg2);
    
  } catch (...) {
    
  }
}


EXPORT short _wrap_ImpulseTable_MakeTable (ImpulseTable *larg1) {
  short lresult = (short)0 ;
  ImpulseTable *arg1 = (ImpulseTable *) 0 ;
  short result;
  
  arg1 = larg1;
  try {
    result = (short)(arg1)->MakeTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (short)0;
  }
}


EXPORT ImpulseTable *_wrap_new_ImpulseTable_empty () {
  ImpulseTable * lresult = (ImpulseTable *)0 ;
  ImpulseTable *result = 0 ;
  
  try {
    result = (ImpulseTable *)new ImpulseTable();
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ImpulseTable *)0;
  }
}


EXPORT ImpulseTable *_wrap_new_ImpulseTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, Table *larg7, float larg8) {
  ImpulseTable * lresult = (ImpulseTable *)0 ;
  long arg1 ;
  int arg2 ;
  float arg3 ;
  float *arg4 = (float *) 0 ;
  float *arg5 = (float *) 0 ;
  float arg6 = (float) 0.f ;
  Table *arg7 = (Table *) 0 ;
  float arg8 = (float) 0.f ;
  ImpulseTable *result = 0 ;
  
  arg1 = larg1;
  arg2 = larg2;
  arg3 = larg3;
  arg4 = larg4;
  arg5 = larg5;
  arg6 = larg6;
  arg7 = larg7;
  arg8 = larg8;
  try {
    result = (ImpulseTable *)new ImpulseTable(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
    
    lresult = result;
    return lresult;
  } catch (...) {
    return (ImpulseTable *)0;
  }
}


EXPORT void _wrap_delete_ImpulseTable (ImpulseTable *larg1) {
  ImpulseTable *arg1 = (ImpulseTable *) 0 ;
  
  arg1 = larg1;
  try {
    delete arg1;
    
  } catch (...) {
    
  }
}


