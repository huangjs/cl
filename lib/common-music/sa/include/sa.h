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


#ifndef SA_H
#define SA_H

#define EXPORT   extern "C" 

#ifndef NOPTHREAD
#include "SndThread.h" // needs pthread library
#endif  

// Base Classes
#include "SndObj.h"
#include "SndIO.h" 
#include "Table.h"

// SndObj-derived
#include "Oscil.h"    // Oscillators
#include "Oscilt.h"
#include "Oscili.h"
#include "FastOsc.h"
#include "Osc.h"
#include "Osci.h"
#include "PhOscili.h"

#include "SndIn.h"    // Sound input
#include "SndRead.h"  // audio file input

#if defined(OSS) || defined(SGI) || defined(WIN)
#include "MidiIn.h"   // Midi input
#include  "Bend.h"    // pitch bend
#include "MidiMap.h" 
#endif

#include "ADSR.h"     // Envelopes
#include "IADSR.h" 

#include "Buzz.h"     // Buzzer

#include "Balance.h"  // Balance of signals

#include "DelayLine.h" // Delay line
#include  "Tap.h"      // Truncating tap
#include  "Tapi.h"     // Time-varying tap
#include  "Comb.h"     // Comb filter
#include  "Allpass.h"  // Allpass filter
#include  "StringFlt.h" // String filter
#include   "Pluck.h"    // Karplus-Strong
#include   "VDelay.h"   // Variable delay
#include   "Pitch.h"    // Pitch transposer
#include   "Loop.h"     // Looping
#include   "Fir.h"      // direct convolution

#include   "Filter.h"    // Fixed-freq/BW reson
#include   "TpTz.h"      // Two-pole Two-zero
#include    "Reson.h"    // Variable reson
#include    "Lp.h"       // Variable LP with resonance
#include    "ButtBP.h"   // Butterworth filters
#include    "ButtBR.h" 
#include    "ButtHP.h" 
#include    "ButtLP.h" 
#include    "Ap.h"       // 2nd order all-pass
#include    "LowPass.h"  // 1st order LP
#include    "HiPass.h"   // 1st order HP

#include    "Hilb.h"     // Hilbert transformer

#include    "SyncGrain.h" // granular synthesis

#include    "Mix.h"      // Mixer
#include    "Pan.h"     // panning
#include    "Gain.h"    // gain 
#include    "Interp.h"  // curve segments
#include    "Phase.h"    // phase accumulator
#include    "Ring.h"     // general-purpose multiplier
#include    "Unit.h"     // test signals

#include   "Lookup.h"     // table lookup
#include   "Lookupi.h"  

#include  "Rand.h"     // Noise
#include  "Randh.h"    // Band-limited noise
#include  "Randi.h"    // interpolated

// Spectral stuff

#include "FFT.h"        // windowed overlapping FFT
#include "IFFT.h"       // overlap-add IFFT
#include "PVA.h"        // pvoc analysis/synthesis
#include "PVS.h" 
#include "PVRead.h"                
#include "IFGram.h"

#include "SinAnal.h"    // sinusoidal analysis
#include "SinSyn.h"     // sinusoidal resynthesis
#include "AdSyn.h"		// additive resynthesis
#include "ReSyn.h"      // additive resynthesis
#include  "IFAdd.h"      // additive resynthesis

#include  "SpecMult.h"    // spectral multiplication
#include  "SpecInterp.h"  // spectral interpolation
#include  "PVMask.h"	  // spectral masking
#include   "PVTransp.h"  // transposition
#include   "PVMix.h"     // mixing
#include   "PVBlur.h"    // blurring
#include   "PVFilter.h"    // mag filtering
#include  "PVMorph.h"     // pvoc morphing 
#include  "SpecPolar.h"  // polar converstion
#include  "SpecSplit.h"  // polar conversion & split
#include  "SpecThresh.h" // threshold filter
#include  "SpecVoc.h"    // impose magnitudes
#include  "SpecCart.h"   // cartesian conversion
#include  "SpecCombine.h" // combine phases & mags
#include   "SpecIn.h"   // spectral file input


#include "Convol.h"     // table-based convolution

// SndIO-derived
#include "SndFIO.h"   // Raw file IO 
#include "SndWave.h"  // RIFF-Wave
#include "SndWaveX.h" // waveformatextensible
#include "SndPVOCEX.h" // pvocex
#include "SndSinIO.h"  // sinusex
#include "SndAiff.h"  // AIFF
#include "SndBuffer.h" // memory buffer

#if defined(OSS) || defined(SGI) || defined(WIN)
#include "SndMidi.h"  // midi IO
#include "SndMidiIn.h"
#endif

#include "SndRTIO.h" // WinMME/OSS/SGI RT IO
#ifdef _MBCS
#include "SndAsio.h" // ASIO-driver IO
#endif
#ifdef JACK
#include "SndJackIO.h" // Jack IO
#endif
#ifdef MACOSX
#include "SndCoreAudio.h" // Core Audio support
#endif

// Table-derived 
#include "HarmTable.h"    // wavetables
#include "UsrHarmTable.h"
#include "TrisegTable.h"  // envelope
#include  "EnvTable.h"    // envelope curves
#include "SndTable.h"     // soundfile input
#include "PlnTable.h"     // Polynomials
#include "HammingTable.h" // Windows
#include "NoteTable.h"    // midi note conversion
#include "UsrDefTable.h"  // user-definable
#include "LoPassTable.h"    // lowpass impulse response 
#include "SpecEnvTable.h"    // spectral envelope
#include "PVEnvTable.h"      // PV envelope
#include "PVTable.h"         // PV  frame
#include "ImpulseTable.h"    // linear FIR coeffs 


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <strings.h>
#include <pthread.h>
#include <CoreServices/CoreServices.h>
#include <CoreAudio/CoreAudio.h>


#include "Maths.h"
#include "PVOCEXRead.h"
#include "RTOutput.h"
#include "RTAudioStream.h"
#include "BusWrite.h"
#include "BusRead.h"
#include "FloatSig.h"

/** Exports for main SWIG wrapper SndObj_wrap.cpp **/

EXPORT bool _wrap_SndObj_IsProcessing (SndObj *larg1);
EXPORT int _wrap_SndObj_GetError (SndObj *larg1);
EXPORT SndObj *_wrap_SndObj_SndObjEqualSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSumAssignSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSubtractAssignSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjMultiplyAssignSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSumAssignFloat (SndObj *larg1, float larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSubtractAssignFloat (SndObj *larg1, float larg2);
EXPORT SndObj *_wrap_SndObj_SndObjMultiplyAssignFloat (SndObj *larg1, float larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSumSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSubtractSndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjMultiplySndObj (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSumFloat (SndObj *larg1, float larg2);
EXPORT SndObj *_wrap_SndObj_SndObjSubtractFloat (SndObj *larg1, float larg2);
EXPORT SndObj *_wrap_SndObj_SndObjMultiplyFloat (SndObj *larg1, float larg2);
EXPORT void _wrap_SndObj_SndObjShiftLeftFloat (SndObj *larg1, float larg2);
EXPORT void _wrap_SndObj_SndObjShiftLeftFloatVector (SndObj *larg1, float *larg2);
EXPORT void _wrap_SndObj_SndObjShiftRightSndIO (SndObj *larg1, SndIO *larg2);
EXPORT void _wrap_SndObj_SndObjShiftLeftSndIO (SndObj *larg1, SndIO *larg2);
EXPORT int _wrap_SndObj_PushIn (SndObj *larg1, float *larg2, int larg3);
EXPORT int _wrap_SndObj_PopOut (SndObj *larg1, float *larg2, int larg3);
EXPORT int _wrap_SndObj_AddOut (SndObj *larg1, float *larg2, int larg3);
EXPORT void _wrap_SndObj_GetMsgList (SndObj *larg1, string *larg2);
EXPORT void _wrap_SndObj_Enable (SndObj *larg1);
EXPORT void _wrap_SndObj_Disable (SndObj *larg1);
EXPORT float _wrap_SndObj_Output (SndObj *larg1, int larg2);
EXPORT int _wrap_SndObj_GetVectorSize (SndObj *larg1);
EXPORT void _wrap_SndObj_SetVectorSize (SndObj *larg1, int larg2);
EXPORT float _wrap_SndObj_GetSr (SndObj *larg1);
EXPORT void _wrap_SndObj_SetSr (SndObj *larg1, float larg2);
EXPORT int _wrap_SndObj_Set (SndObj *larg1, char *larg2, float larg3);
EXPORT int _wrap_SndObj_Connect (SndObj *larg1, char *larg2, void *larg3);
EXPORT void _wrap_SndObj_SetInput (SndObj *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SndObj_GetInput (SndObj *larg1);
EXPORT SndObj *_wrap_new_SndObj (SndObj *larg1, int larg2, float larg3);
EXPORT SndObj *_wrap_new_SndObj_empty ();
EXPORT void _wrap_delete_SndObj (SndObj *larg1);
EXPORT char *_wrap_SndObj_ErrorMessage (SndObj *larg1);
EXPORT short _wrap_SndObj_DoProcess (SndObj *larg1);
EXPORT void _wrap_SndIO_m_sampsize_set (SndIO *larg1, short larg2);
EXPORT short _wrap_SndIO_m_sampsize_get (SndIO *larg1);
EXPORT float _wrap_SndIO_GetSr (SndIO *larg1);
EXPORT int _wrap_SndIO_GetVectorSize (SndIO *larg1);
EXPORT short _wrap_SndIO_GetChannels (SndIO *larg1);
EXPORT short _wrap_SndIO_GetSize (SndIO *larg1);
EXPORT float _wrap_SndIO_Output (SndIO *larg1, int larg2, int larg3);
EXPORT short _wrap_SndIO_SetOutput (SndIO *larg1, short larg2, SndObj *larg3);
EXPORT SndIO *_wrap_new_SndIO (short larg1, short larg2, SndObj **larg3, int larg4, float larg5);
EXPORT void _wrap_delete_SndIO (SndIO *larg1);
EXPORT short _wrap_SndIO_Read (SndIO *larg1);
EXPORT short _wrap_SndIO_Write (SndIO *larg1);
EXPORT char *_wrap_SndIO_ErrorMessage (SndIO *larg1);
EXPORT int _wrap_SndIO_Error (SndIO *larg1);
EXPORT long _wrap_Table_GetLen (Table *larg1);
EXPORT float *_wrap_Table_GetTable (Table *larg1);
EXPORT float _wrap_Table_Lookup (Table *larg1, int larg2);
EXPORT void _wrap_delete_Table (Table *larg1);
EXPORT char *_wrap_Table_ErrorMessage (Table *larg1);
EXPORT short _wrap_Table_MakeTable (Table *larg1);
EXPORT void _wrap_Oscil_SetSr (Oscil *larg1, float larg2);
EXPORT void _wrap_Oscil_m_factor_set (Oscil *larg1, float larg2);
EXPORT float _wrap_Oscil_m_factor_get (Oscil *larg1);
EXPORT Oscil *_wrap_new_Oscil_empty ();
EXPORT Oscil *_wrap_new_Oscil (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Oscil (Oscil *larg1);
EXPORT short _wrap_Oscil_SetPhase (Oscil *larg1, float larg2);
EXPORT void _wrap_Oscil_SetTable (Oscil *larg1, Table *larg2);
EXPORT void _wrap_Oscil_SetFreq (Oscil *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Oscil_SetAmp (Oscil *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Oscil_SetFreqSndObj (Oscil *larg1, SndObj *larg2);
EXPORT void _wrap_Oscil_SetAmpSndObj (Oscil *larg1, SndObj *larg2);
EXPORT int _wrap_Oscil_Connect (Oscil *larg1, char *larg2, void *larg3);
EXPORT int _wrap_Oscil_Set (Oscil *larg1, char *larg2, float larg3);
EXPORT short _wrap_Oscil_DoProcess (Oscil *larg1);
EXPORT Oscilt *_wrap_new_Oscilt_empty ();
EXPORT Oscilt *_wrap_new_Oscilt (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Oscilt (Oscilt *larg1);
EXPORT short _wrap_Oscilt_DoProcess (Oscilt *larg1);
EXPORT Oscili *_wrap_new_Oscili_empty ();
EXPORT Oscili *_wrap_new_Oscili (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Oscili (Oscili *larg1);
EXPORT short _wrap_Oscili_DoProcess (Oscili *larg1);
EXPORT FastOsc *_wrap_new_FastOsc_empty ();
EXPORT FastOsc *_wrap_new_FastOsc (Table *larg1, float larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_FastOsc (FastOsc *larg1);
EXPORT void _wrap_FastOsc_SetFreq (FastOsc *larg1, float larg2);
EXPORT void _wrap_FastOsc_SetAmp (FastOsc *larg1, float larg2);
EXPORT void _wrap_FastOsc_SetPhase (FastOsc *larg1, float larg2);
EXPORT void _wrap_FastOsc_SetTable (FastOsc *larg1, Table *larg2);
EXPORT int _wrap_FastOsc_Set (FastOsc *larg1, char *larg2, float larg3);
EXPORT int _wrap_FastOsc_Connect (FastOsc *larg1, char *larg2, void *larg3);
EXPORT void _wrap_FastOsc_SetSr (FastOsc *larg1, float larg2);
EXPORT short _wrap_FastOsc_DoProcess (FastOsc *larg1);
EXPORT Osc *_wrap_new_Osc_empty ();
EXPORT Osc *_wrap_new_Osc (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Osc (Osc *larg1);
EXPORT void _wrap_Osc_SetFreq (Osc *larg1, SndObj *larg2);
EXPORT void _wrap_Osc_SetAmp (Osc *larg1, SndObj *larg2);
EXPORT int _wrap_Osc_Connect (Osc *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Osc_DoProcess (Osc *larg1);
EXPORT Osci *_wrap_new_Osci_empty ();
EXPORT Osci *_wrap_new_Osci (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Osci (Osci *larg1);
EXPORT void _wrap_Osci_SetTable (Osci *larg1, Table *larg2);
EXPORT int _wrap_Osci_Connect (Osci *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Osci_DoProcess (Osci *larg1);
EXPORT SndIn *_wrap_new_SndIn_empty ();
EXPORT SndIn *_wrap_new_SndIn (SndIO *larg1, short larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SndIn (SndIn *larg1);
EXPORT void _wrap_SndIn_SetInput (SndIn *larg1, SndIO *larg2, short larg3);
EXPORT int _wrap_SndIn_Connect (SndIn *larg1, char *larg2, void *larg3);
EXPORT int _wrap_SndIn_Set (SndIn *larg1, char *larg2, float larg3);
EXPORT short _wrap_SndIn_DoProcess (SndIn *larg1);
EXPORT char *_wrap_SndIn_ErrorMessage (SndIn *larg1);
EXPORT SndObj *_wrap_SndRead_Outchannel (SndRead *larg1, int larg2);
EXPORT SndRead *_wrap_new_SndRead_empty ();
EXPORT SndRead *_wrap_new_SndRead (char *larg1, float larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_SndRead (SndRead *larg1);
EXPORT void _wrap_SndRead_SetInput (SndRead *larg1, char *larg2);
EXPORT void _wrap_SndRead_SetScale (SndRead *larg1, float larg2);
EXPORT void _wrap_SndRead_SetPitch (SndRead *larg1, float larg2);
EXPORT int _wrap_SndRead_Set (SndRead *larg1, char *larg2, float larg3);
EXPORT short _wrap_SndRead_DoProcess (SndRead *larg1);
EXPORT ADSR *_wrap_new_ADSR_empty ();
EXPORT ADSR *_wrap_new_ADSR (float larg1, float larg2, float larg3, float larg4, float larg5, float larg6, SndObj *larg7, int larg8, float larg9);
EXPORT void _wrap_delete_ADSR (ADSR *larg1);
EXPORT void _wrap_ADSR_SetSr (ADSR *larg1, float larg2);
EXPORT void _wrap_ADSR_SetMaxAmp (ADSR *larg1, float larg2);
EXPORT void _wrap_ADSR_Sustain (ADSR *larg1);
EXPORT void _wrap_ADSR_Release (ADSR *larg1);
EXPORT void _wrap_ADSR_Restart (ADSR *larg1);
EXPORT void _wrap_ADSR_SetADSR (ADSR *larg1, float larg2, float larg3, float larg4, float larg5);
EXPORT void _wrap_ADSR_SetDur (ADSR *larg1, float larg2);
EXPORT int _wrap_ADSR_Set (ADSR *larg1, char *larg2, float larg3);
EXPORT short _wrap_ADSR_DoProcess (ADSR *larg1);
EXPORT IADSR *_wrap_new_IADSR_empty ();
EXPORT IADSR *_wrap_new_IADSR (float larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, SndObj *larg9, int larg10, float larg11);
EXPORT void _wrap_delete_IADSR (IADSR *larg1);
EXPORT void _wrap_IADSR_SetInit (IADSR *larg1, float larg2);
EXPORT void _wrap_IADSR_SetEnd (IADSR *larg1, float larg2);
EXPORT int _wrap_IADSR_Set (IADSR *larg1, char *larg2, float larg3);
EXPORT short _wrap_IADSR_DoProcess (IADSR *larg1);
EXPORT Buzz *_wrap_new_Buzz_empty ();
EXPORT Buzz *_wrap_new_Buzz (float larg1, float larg2, short larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Buzz (Buzz *larg1);
EXPORT void _wrap_Buzz_SetFreq (Buzz *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Buzz_SetAmp (Buzz *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Buzz_SetSr (Buzz *larg1, float larg2);
EXPORT void _wrap_Buzz_SetHarm (Buzz *larg1, int larg2);
EXPORT int _wrap_Buzz_Set (Buzz *larg1, char *larg2, float larg3);
EXPORT int _wrap_Buzz_Connect (Buzz *larg1, char *larg2, void *larg3);
EXPORT char *_wrap_Buzz_ErrorMessage (Buzz *larg1);
EXPORT short _wrap_Buzz_DoProcess (Buzz *larg1);
EXPORT void _wrap_Balance_SetInput (Balance *larg1, SndObj *larg2, SndObj *larg3);
EXPORT void _wrap_Balance_SetLPFreq (Balance *larg1, float larg2);
EXPORT void _wrap_Balance_SetSr (Balance *larg1, float larg2);
EXPORT int _wrap_Balance_Set (Balance *larg1, char *larg2, float larg3);
EXPORT Balance *_wrap_new_Balance_empty ();
EXPORT Balance *_wrap_new_Balance (SndObj *larg1, SndObj *larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Balance (Balance *larg1);
EXPORT char *_wrap_Balance_ErrorMessage (Balance *larg1);
EXPORT short _wrap_Balance_DoProcess (Balance *larg1);
EXPORT int _wrap_Balance_Connect (Balance *larg1, char *larg2, void *larg3);
EXPORT DelayLine *_wrap_new_DelayLine_empty ();
EXPORT DelayLine *_wrap_new_DelayLine (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_DelayLine (DelayLine *larg1);
EXPORT float *_wrap_DelayLine_Buffer (DelayLine *larg1);
EXPORT long _wrap_DelayLine_GetWritePointerPos (DelayLine *larg1);
EXPORT float _wrap_DelayLine_GetDelayTime (DelayLine *larg1);
EXPORT void _wrap_DelayLine_SetSr (DelayLine *larg1, float larg2);
EXPORT void _wrap_DelayLine_Reset (DelayLine *larg1);
EXPORT void _wrap_DelayLine_SetDelayTime (DelayLine *larg1, float larg2);
EXPORT int _wrap_DelayLine_Set (DelayLine *larg1, char *larg2, float larg3);
EXPORT short _wrap_DelayLine_DoProcess (DelayLine *larg1);
EXPORT char *_wrap_DelayLine_ErrorMessage (DelayLine *larg1);
EXPORT Tap *_wrap_new_Tap_empty ();
EXPORT Tap *_wrap_new_Tap (float larg1, DelayLine *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Tap (Tap *larg1);
EXPORT void _wrap_Tap_SetDelayTime (Tap *larg1, float larg2);
EXPORT void _wrap_Tap_SetDelayTap (Tap *larg1, DelayLine *larg2);
EXPORT int _wrap_Tap_Set (Tap *larg1, char *larg2, float larg3);
EXPORT int _wrap_Tap_Connect (Tap *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Tap_DoProcess (Tap *larg1);
EXPORT char *_wrap_Tap_ErrorMessage (Tap *larg1);
EXPORT Tapi *_wrap_new_Tapi_empty ();
EXPORT Tapi *_wrap_new_Tapi (SndObj *larg1, DelayLine *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Tapi (Tapi *larg1);
EXPORT void _wrap_Tapi_SetDelayInput (Tapi *larg1, SndObj *larg2);
EXPORT int _wrap_Tapi_Connect (Tapi *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Tapi_DoProcess (Tapi *larg1);
EXPORT Comb *_wrap_new_Comb_empty ();
EXPORT Comb *_wrap_new_Comb (float larg1, float larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Comb (Comb *larg1);
EXPORT void _wrap_Comb_SetGain (Comb *larg1, float larg2);
EXPORT int _wrap_Comb_Set (Comb *larg1, char *larg2, float larg3);
EXPORT short _wrap_Comb_DoProcess (Comb *larg1);
EXPORT Allpass *_wrap_new_Allpass_empty ();
EXPORT Allpass *_wrap_new_Allpass (float larg1, float larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Allpass (Allpass *larg1);
EXPORT short _wrap_Allpass_DoProcess (Allpass *larg1);
EXPORT StringFlt *_wrap_new_StringFlt_empty ();
EXPORT StringFlt *_wrap_new_StringFlt (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT StringFlt *_wrap_new_StringFlt_decay (float larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_StringFlt (StringFlt *larg1);
EXPORT void _wrap_StringFlt_SetSr (StringFlt *larg1, float larg2);
EXPORT void _wrap_StringFlt_SetDecay (StringFlt *larg1, float larg2);
EXPORT void _wrap_StringFlt_SetFreq (StringFlt *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_StringFlt_SetFdbgain (StringFlt *larg1, float larg2);
EXPORT int _wrap_StringFlt_Set (StringFlt *larg1, char *larg2, float larg3);
EXPORT int _wrap_StringFlt_Connect (StringFlt *larg1, char *larg2, void *larg3);
EXPORT short _wrap_StringFlt_DoProcess (StringFlt *larg1);
EXPORT Pluck *_wrap_new_Pluck_empty ();
EXPORT Pluck *_wrap_new_Pluck (float larg1, float larg2, float larg3, SndObj *larg4, float larg5, int larg6, float larg7);
EXPORT Pluck *_wrap_new_Pluck_decay (float larg1, float larg2, SndObj *larg3, float larg4, float larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Pluck (Pluck *larg1);
EXPORT void _wrap_Pluck_RePluck (Pluck *larg1);
EXPORT int _wrap_Pluck_Set (Pluck *larg1, char *larg2, float larg3);
EXPORT void _wrap_Pluck_SetAmp (Pluck *larg1, float larg2, float larg3);
EXPORT short _wrap_Pluck_DoProcess (Pluck *larg1);
EXPORT VDelay *_wrap_new_VDelay_empty ();
EXPORT VDelay *_wrap_new_VDelay (float larg1, float larg2, float larg3, float larg4, SndObj *larg5, SndObj *larg6, SndObj *larg7, SndObj *larg8, SndObj *larg9, int larg10, float larg11);
EXPORT VDelay *_wrap_new_VDelay_delaytime (float larg1, float larg2, float larg3, float larg4, float larg5, SndObj *larg6, SndObj *larg7, SndObj *larg8, SndObj *larg9, SndObj *larg10, int larg11, float larg12);
EXPORT void _wrap_delete_VDelay (VDelay *larg1);
EXPORT int _wrap_VDelay_Set (VDelay *larg1, char *larg2, float larg3);
EXPORT int _wrap_VDelay_Connect (VDelay *larg1, char *larg2, void *larg3);
EXPORT void _wrap_VDelay_SetMaxDelayTime (VDelay *larg1, float larg2);
EXPORT void _wrap_VDelay_SetDelayTime (VDelay *larg1, float larg2);
EXPORT void _wrap_VDelay_SetVdtInput (VDelay *larg1, SndObj *larg2);
EXPORT void _wrap_VDelay_SetFdbgain (VDelay *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_VDelay_SetFwdgain (VDelay *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_VDelay_SetDirgain (VDelay *larg1, float larg2, SndObj *larg3);
EXPORT short _wrap_VDelay_DoProcess (VDelay *larg1);
EXPORT Pitch *_wrap_new_Pitch_empty ();
EXPORT Pitch *_wrap_new_Pitch (float larg1, SndObj *larg2, float larg3, int larg4, float larg5);
EXPORT Pitch *_wrap_new_Pitch_semitones (float larg1, SndObj *larg2, int larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Pitch (Pitch *larg1);
EXPORT void _wrap_Pitch_SetPitch (Pitch *larg1, float larg2);
EXPORT void _wrap_Pitch_SetPitch_semitones (Pitch *larg1, int larg2);
EXPORT int _wrap_Pitch_Set (Pitch *larg1, char *larg2, float larg3);
EXPORT short _wrap_Pitch_DoProcess (Pitch *larg1);
EXPORT SndLoop *_wrap_new_SndLoop_empty ();
EXPORT SndLoop *_wrap_new_SndLoop (float larg1, float larg2, SndObj *larg3, float larg4, int larg5, float larg6);
EXPORT void _wrap_delete_SndLoop (SndLoop *larg1);
EXPORT void _wrap_SndLoop_SetXFade (SndLoop *larg1, float larg2);
EXPORT void _wrap_SndLoop_SetPitch (SndLoop *larg1, float larg2);
EXPORT void _wrap_SndLoop_ReSample (SndLoop *larg1);
EXPORT int _wrap_SndLoop_Set (SndLoop *larg1, char *larg2, float larg3);
EXPORT short _wrap_SndLoop_DoProcess (SndLoop *larg1);
EXPORT int _wrap_FIR_Connect (FIR *larg1, char *larg2, void *larg3);
EXPORT int _wrap_FIR_Set (FIR *larg1, char *larg2, float larg3);
EXPORT FIR *_wrap_new_Fir_empty ();
EXPORT FIR *_wrap_new_FIR (Table *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT FIR *_wrap_new_FIR_impulse (float *larg1, int larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_FIR (FIR *larg1);
EXPORT void _wrap_FIR_SetTable (FIR *larg1, Table *larg2);
EXPORT void _wrap_FIR_SetImpulse (FIR *larg1, float *larg2, int larg3);
EXPORT void _wrap_FIR_SetDelayTime (FIR *larg1, float larg2);
EXPORT short _wrap_FIR_DoProcess (FIR *larg1);
EXPORT Filter *_wrap_new_Filter_empty ();
EXPORT Filter *_wrap_new_Filter (float larg1, float larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Filter (Filter *larg1);
EXPORT void _wrap_Filter_SetFreq (Filter *larg1, float larg2);
EXPORT void _wrap_Filter_SetBW (Filter *larg1, float larg2);
EXPORT int _wrap_Filter_Set (Filter *larg1, char *larg2, float larg3);
EXPORT void _wrap_Filter_SetSr (Filter *larg1, float larg2);
EXPORT char *_wrap_Filter_ErrorMessage (Filter *larg1);
EXPORT short _wrap_Filter_DoProcess (Filter *larg1);
EXPORT TpTz *_wrap_new_TpTz_empty ();
EXPORT TpTz *_wrap_new_TpTz (double larg1, double larg2, double larg3, double larg4, double larg5, SndObj *larg6, int larg7, float larg8);
EXPORT void _wrap_delete_TpTz (TpTz *larg1);
EXPORT void _wrap_TpTz_SetParam (TpTz *larg1, double larg2, double larg3, double larg4, double larg5, double larg6);
EXPORT int _wrap_TpTz_Set (TpTz *larg1, char *larg2, float larg3);
EXPORT short _wrap_TpTz_DoProcess (TpTz *larg1);
EXPORT Reson *_wrap_new_Reson_empty ();
EXPORT Reson *_wrap_new_Reson (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_Reson_SetFreq (Reson *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Reson_SetBW (Reson *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_delete_Reson (Reson *larg1);
EXPORT int _wrap_Reson_Connect (Reson *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Reson_DoProcess (Reson *larg1);
EXPORT Lp *_wrap_new_Lp_empty ();
EXPORT Lp *_wrap_new_Lp (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Lp (Lp *larg1);
EXPORT void _wrap_Lp_SetSr (Lp *larg1, float larg2);
EXPORT int _wrap_Lp_Set (Lp *larg1, char *larg2, float larg3);
EXPORT short _wrap_Lp_DoProcess (Lp *larg1);
EXPORT ButtBP *_wrap_new_ButtBP_empty ();
EXPORT ButtBP *_wrap_new_ButtBP (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_ButtBP (ButtBP *larg1);
EXPORT int _wrap_ButtBP_Set (ButtBP *larg1, char *larg2, float larg3);
EXPORT void _wrap_ButtBP_SetFreq (ButtBP *larg1, float larg2);
EXPORT void _wrap_ButtBP_SetBW (ButtBP *larg1, float larg2);
EXPORT void _wrap_ButtBP_SetFreq_mod (ButtBP *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_ButtBP_SetBW_mod (ButtBP *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_ButtBP_SetSr (ButtBP *larg1, float larg2);
EXPORT int _wrap_ButtBP_Connect (ButtBP *larg1, char *larg2, void *larg3);
EXPORT short _wrap_ButtBP_DoProcess (ButtBP *larg1);
EXPORT ButtBR *_wrap_new_ButtBR_empty ();
EXPORT ButtBR *_wrap_new_ButtBR (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_ButtBR (ButtBR *larg1);
EXPORT ButtHP *_wrap_new_ButtHP_empty ();
EXPORT ButtHP *_wrap_new_ButtHP (float larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_ButtHP (ButtHP *larg1);
EXPORT ButtLP *_wrap_new_ButtLP_empty ();
EXPORT ButtLP *_wrap_new_ButtLP (float larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_ButtLP (ButtLP *larg1);
EXPORT Ap *_wrap_new_Ap_empty ();
EXPORT Ap *_wrap_new_Ap (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Ap (Ap *larg1);
EXPORT void _wrap_Ap_SetFreq (Ap *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Ap_SetR (Ap *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Ap_SetSr (Ap *larg1, float larg2);
EXPORT int _wrap_Ap_Set (Ap *larg1, char *larg2, float larg3);
EXPORT int _wrap_Ap_Connect (Ap *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Ap_DoProcess (Ap *larg1);
EXPORT char *_wrap_Ap_ErrorMessage (Ap *larg1);
EXPORT LoPass *_wrap_new_LoPass_empty ();
EXPORT LoPass *_wrap_new_LoPass (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_LoPass (LoPass *larg1);
EXPORT void _wrap_LoPass_SetFreq (LoPass *larg1, float larg2);
EXPORT void _wrap_LoPass_SetSr (LoPass *larg1, float larg2);
EXPORT int _wrap_LoPass_Set (LoPass *larg1, char *larg2, float larg3);
EXPORT short _wrap_LoPass_DoProcess (LoPass *larg1);
EXPORT HiPass *_wrap_new_HiPass_empty ();
EXPORT HiPass *_wrap_new_HiPass (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_HiPass (HiPass *larg1);
EXPORT void _wrap_HiPass_SetFreq (HiPass *larg1, float larg2);
EXPORT void _wrap_HiPass_SetSr (HiPass *larg1, float larg2);
EXPORT int _wrap_HiPass_Set (HiPass *larg1, char *larg2, float larg3);
EXPORT void _wrap_Hilb_real_set (Hilb *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_Hilb_real_get (Hilb *larg1);
EXPORT void _wrap_Hilb_imag_set (Hilb *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_Hilb_imag_get (Hilb *larg1);
EXPORT Hilb *_wrap_new_Hilb_empty ();
EXPORT Hilb *_wrap_new_Hilb (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Hilb (Hilb *larg1);
EXPORT short _wrap_Hilb_DoProcess (Hilb *larg1);
EXPORT char *_wrap_Hilb_ErrorMessage (Hilb *larg1);
EXPORT SyncGrain *_wrap_new_SyncGrain_empty ();
EXPORT SyncGrain *_wrap_new_SyncGrain (Table *larg1, Table *larg2, float larg3, float larg4, float larg5, float larg6, float larg7, SndObj *larg8, SndObj *larg9, SndObj *larg10, SndObj *larg11, int larg12, int larg13, float larg14);
EXPORT void _wrap_delete_SyncGrain (SyncGrain *larg1);
EXPORT void _wrap_SyncGrain_Offset (SyncGrain *larg1, int larg2);
EXPORT void _wrap_SyncGrain_Offset_seconds (SyncGrain *larg1, float larg2);
EXPORT void _wrap_SyncGrain_SetWaveTable (SyncGrain *larg1, Table *larg2);
EXPORT void _wrap_SyncGrain_SetEnvelopeTable (SyncGrain *larg1, Table *larg2);
EXPORT void _wrap_SyncGrain_SetFreq (SyncGrain *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_SyncGrain_SetAmp (SyncGrain *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_SyncGrain_SetPitch (SyncGrain *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_SyncGrain_SetGrainSize (SyncGrain *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_SyncGrain_SetPointerRate (SyncGrain *larg1, float larg2);
EXPORT int _wrap_SyncGrain_Set (SyncGrain *larg1, char *larg2, float larg3);
EXPORT int _wrap_SyncGrain_Connect (SyncGrain *larg1, char *larg2, void *larg3);
EXPORT short _wrap_SyncGrain_DoProcess (SyncGrain *larg1);
EXPORT char *_wrap_SyncGrain_ErrorMessage (SyncGrain *larg1);
EXPORT Mixer *_wrap_new_Mixer_empty ();
EXPORT Mixer *_wrap_new_Mixer (int larg1, SndObj **larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Mixer (Mixer *larg1);
EXPORT int _wrap_Mixer_GetObjNo (Mixer *larg1);
EXPORT short _wrap_Mixer_AddObj (Mixer *larg1, SndObj *larg2);
EXPORT short _wrap_Mixer_DeleteObj (Mixer *larg1, SndObj *larg2);
EXPORT short _wrap_Mixer_DoProcess (Mixer *larg1);
EXPORT int _wrap_Mixer_Connect (Mixer *larg1, char *larg2, void *larg3);
EXPORT char *_wrap_Mixer_ErrorMessage (Mixer *larg1);
EXPORT void _wrap_Pan_left_set (Pan *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_Pan_left_get (Pan *larg1);
EXPORT void _wrap_Pan_right_set (Pan *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_Pan_right_get (Pan *larg1);
EXPORT Pan *_wrap_new_Pan_empty ();
EXPORT Pan *_wrap_new_Pan (float larg1, SndObj *larg2, SndObj *larg3, int larg4, int larg5, float larg6);
EXPORT void _wrap_delete_Pan (Pan *larg1);
EXPORT void _wrap_Pan_SetPan (Pan *larg1, float larg2, SndObj *larg3);
EXPORT int _wrap_Pan_Set (Pan *larg1, char *larg2, float larg3);
EXPORT int _wrap_Pan_Connect (Pan *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Pan_DoProcess (Pan *larg1);
EXPORT char *_wrap_Pan_ErrorMessage (Pan *larg1);
EXPORT Gain *_wrap_new_Gain_empty ();
EXPORT Gain *_wrap_new_Gain (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Gain (Gain *larg1);
EXPORT int _wrap_Gain_Set (Gain *larg1, char *larg2, float larg3);
EXPORT void _wrap_Gain_SetGain (Gain *larg1, float larg2);
EXPORT void _wrap_Gain_SetGainM (Gain *larg1, float larg2);
EXPORT float _wrap_Gain_dBToAmp (Gain *larg1, float larg2);
EXPORT short _wrap_Gain_DoProcess (Gain *larg1);
EXPORT Interp *_wrap_new_Interp_empty ();
EXPORT Interp *_wrap_new_Interp (float larg1, float larg2, float larg3, float larg4, int larg5, float larg6);
EXPORT void _wrap_delete_Interp (Interp *larg1);
EXPORT int _wrap_Interp_Set (Interp *larg1, char *larg2, float larg3);
EXPORT void _wrap_Interp_SetSr (Interp *larg1, float larg2);
EXPORT void _wrap_Interp_Restart (Interp *larg1);
EXPORT void _wrap_Interp_SetCurve (Interp *larg1, float larg2, float larg3, float larg4);
EXPORT void _wrap_Interp_SetDur (Interp *larg1, float larg2);
EXPORT short _wrap_Interp_DoProcess (Interp *larg1);
EXPORT Phase *_wrap_new_Phase_empty ();
EXPORT Phase *_wrap_new_Phase (float larg1, SndObj *larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Phase (Phase *larg1);
EXPORT void _wrap_Phase_SetFreq (Phase *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_Phase_SetPhase (Phase *larg1, float larg2);
EXPORT int _wrap_Phase_Set (Phase *larg1, char *larg2, float larg3);
EXPORT int _wrap_Phase_Connect (Phase *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Phase_DoProcess (Phase *larg1);
EXPORT Ring *_wrap_new_Ring_empty ();
EXPORT Ring *_wrap_new_Ring (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Ring (Ring *larg1);
EXPORT short _wrap_Ring_DoProcess (Ring *larg1);
EXPORT int _wrap_Ring_Connect (Ring *larg1, char *larg2, void *larg3);
EXPORT Unit *_wrap_new_Unit_empty ();
EXPORT Unit *_wrap_new_Unit (float larg1, short larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Unit (Unit *larg1);
EXPORT void _wrap_Unit_SetAmp (Unit *larg1, float larg2);
EXPORT void _wrap_Unit_SetStep (Unit *larg1, float larg2);
EXPORT void _wrap_Unit_SetMode (Unit *larg1, short larg2);
EXPORT int _wrap_Unit_Set (Unit *larg1, char *larg2, float larg3);
EXPORT short _wrap_Unit_DoProcess (Unit *larg1);
EXPORT Lookup *_wrap_new_Lookup_empty ();
EXPORT Lookup *_wrap_new_Lookup (Table *larg1, long larg2, SndObj *larg3, int larg4, int larg5, int larg6, float larg7);
EXPORT void _wrap_Lookup_SetMode (Lookup *larg1, int larg2, int larg3);
EXPORT void _wrap_delete_Lookup (Lookup *larg1);
EXPORT void _wrap_Lookup_Offset (Lookup *larg1, long larg2);
EXPORT void _wrap_Lookup_SetTable (Lookup *larg1, Table *larg2);
EXPORT int _wrap_Lookup_Set (Lookup *larg1, char *larg2, float larg3);
EXPORT int _wrap_Lookup_Connect (Lookup *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Lookup_DoProcess (Lookup *larg1);
EXPORT Lookupi *_wrap_new_Lookupi_empty ();
EXPORT Lookupi *_wrap_new_Lookupi (Table *larg1, long larg2, SndObj *larg3, int larg4, int larg5, int larg6, float larg7);
EXPORT void _wrap_delete_Lookupi (Lookupi *larg1);
EXPORT short _wrap_Lookupi_DoProcess (Lookupi *larg1);
EXPORT Rand *_wrap_new_Rand_empty ();
EXPORT Rand *_wrap_new_Rand (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Rand (Rand *larg1);
EXPORT void _wrap_Rand_SetAmp (Rand *larg1, float larg2, SndObj *larg3);
EXPORT int _wrap_Rand_Set (Rand *larg1, char *larg2, float larg3);
EXPORT int _wrap_Rand_Connect (Rand *larg1, char *larg2, void *larg3);
EXPORT short _wrap_Rand_DoProcess (Rand *larg1);
EXPORT PhOscili *_wrap_new_PhOscili_empty ();
EXPORT PhOscili *_wrap_new_PhOscili (Table *larg1, float larg2, float larg3, SndObj *larg4, SndObj *larg5, SndObj *larg6, int larg7, float larg8);
EXPORT void _wrap_delete_PhOscili (PhOscili *larg1);
EXPORT int _wrap_PhOscili_Connect (PhOscili *larg1, char *larg2, void *larg3);
EXPORT short _wrap_PhOscili_DoProcess (PhOscili *larg1);
EXPORT Randh *_wrap_new_Randh_empty ();
EXPORT Randh *_wrap_new_Randh (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_Randh (Randh *larg1);
EXPORT int _wrap_Randh_Connect (Randh *larg1, char *larg2, void *larg3);
EXPORT void _wrap_Randh_SetSr (Randh *larg1, float larg2);
EXPORT void _wrap_Randh_SetFreq (Randh *larg1, float larg2, SndObj *larg3);
EXPORT int _wrap_Randh_Set (Randh *larg1, char *larg2, float larg3);
EXPORT short _wrap_Randh_DoProcess (Randh *larg1);
EXPORT Randi *_wrap_new_Randi_empty ();
EXPORT Randi *_wrap_new_Randi (float larg1, float larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_Randi (Randi *larg1);
EXPORT short _wrap_Randi_DoProcess (Randi *larg1);
EXPORT FFT *_wrap_new_FFT_empty ();
EXPORT FFT *_wrap_new_FFT (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6);
EXPORT void _wrap_delete_FFT (FFT *larg1);
EXPORT int _wrap_FFT_GetFFTSize (FFT *larg1);
EXPORT int _wrap_FFT_GetHopSize (FFT *larg1);
EXPORT void _wrap_FFT_SetWindow (FFT *larg1, Table *larg2);
EXPORT int _wrap_FFT_Connect (FFT *larg1, char *larg2, void *larg3);
EXPORT int _wrap_FFT_Set (FFT *larg1, char *larg2, float larg3);
EXPORT void _wrap_FFT_SetScale (FFT *larg1, float larg2);
EXPORT void _wrap_FFT_SetFFTSize (FFT *larg1, int larg2);
EXPORT void _wrap_FFT_SetHopSize (FFT *larg1, int larg2);
EXPORT short _wrap_FFT_DoProcess (FFT *larg1);
EXPORT IFFT *_wrap_new_IFFT_empty ();
EXPORT IFFT *_wrap_new_IFFT (Table *larg1, SndObj *larg2, int larg3, int larg4, float larg5);
EXPORT void _wrap_delete_IFFT (IFFT *larg1);
EXPORT int _wrap_IFFT_GetFFTSize (IFFT *larg1);
EXPORT int _wrap_IFFT_GetHopSize (IFFT *larg1);
EXPORT void _wrap_IFFT_SetWindow (IFFT *larg1, Table *larg2);
EXPORT int _wrap_IFFT_Connect (IFFT *larg1, char *larg2, void *larg3);
EXPORT int _wrap_IFFT_Set (IFFT *larg1, char *larg2, float larg3);
EXPORT void _wrap_IFFT_SetFFTSize (IFFT *larg1, int larg2);
EXPORT void _wrap_IFFT_SetHopSize (IFFT *larg1, int larg2);
EXPORT short _wrap_IFFT_DoProcess (IFFT *larg1);
EXPORT PVA *_wrap_new_PVA_empty ();
EXPORT PVA *_wrap_new_PVA (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6);
EXPORT void _wrap_delete_PVA (PVA *larg1);
EXPORT float _wrap_PVA_Outphases (PVA *larg1, int larg2);
EXPORT int _wrap_PVA_Set (PVA *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVA_SetFFTSize (PVA *larg1, int larg2);
EXPORT void _wrap_PVA_SetHopSize (PVA *larg1, int larg2);
EXPORT short _wrap_PVA_DoProcess (PVA *larg1);
EXPORT PVS *_wrap_new_PVS_empty ();
EXPORT PVS *_wrap_new_PVS (Table *larg1, SndObj *larg2, int larg3, int larg4, float larg5);
EXPORT void _wrap_delete_PVS (PVS *larg1);
EXPORT int _wrap_PVS_Set (PVS *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVS_SetFFTSize (PVS *larg1, int larg2);
EXPORT void _wrap_PVS_SetHopSize (PVS *larg1, int larg2);
EXPORT short _wrap_PVS_DoProcess (PVS *larg1);
EXPORT SndObj *_wrap_PVRead_Outchannel (PVRead *larg1, int larg2);
EXPORT int _wrap_PVRead_Set (PVRead *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVRead_SetInput (PVRead *larg1, char *larg2);
EXPORT void _wrap_PVRead_SetTimescale (PVRead *larg1, float larg2);
EXPORT PVRead *_wrap_new_PVRead_empty ();
EXPORT PVRead *_wrap_new_PVRead (char *larg1, float larg2, int larg3, float larg4);
EXPORT void _wrap_delete_PVRead (PVRead *larg1);
EXPORT short _wrap_PVRead_DoProcess (PVRead *larg1);
EXPORT IFGram *_wrap_new_IFGram_empty ();
EXPORT IFGram *_wrap_new_IFGram (Table *larg1, SndObj *larg2, float larg3, int larg4, int larg5, float larg6);
EXPORT void _wrap_delete_IFGram (IFGram *larg1);
EXPORT int _wrap_IFGram_Set (IFGram *larg1, char *larg2, float larg3);
EXPORT int _wrap_IFGram_Connect (IFGram *larg1, char *larg2, void *larg3);
EXPORT void _wrap_IFGram_SetFFTSize (IFGram *larg1, int larg2);
EXPORT short _wrap_IFGram_DoProcess (IFGram *larg1);
EXPORT SinAnal *_wrap_new_SinAnal_empty ();
EXPORT SinAnal *_wrap_new_SinAnal (SndObj *larg1, float larg2, int larg3, int larg4, int larg5, float larg6);
EXPORT void _wrap_delete_SinAnal (SinAnal *larg1);
EXPORT int _wrap_SinAnal_GetTrackID (SinAnal *larg1, int larg2);
EXPORT int _wrap_SinAnal_GetTracks (SinAnal *larg1);
EXPORT int _wrap_SinAnal_Set (SinAnal *larg1, char *larg2, float larg3);
EXPORT int _wrap_SinAnal_Connect (SinAnal *larg1, char *larg2, void *larg3);
EXPORT void _wrap_SinAnal_SetThreshold (SinAnal *larg1, float larg2);
EXPORT void _wrap_SinAnal_SetIFGram (SinAnal *larg1, SndObj *larg2);
EXPORT void _wrap_SinAnal_SetMaxTracks (SinAnal *larg1, int larg2);
EXPORT short _wrap_SinAnal_DoProcess (SinAnal *larg1);
EXPORT SinSyn *_wrap_new_SinSyn_empty ();
EXPORT SinSyn *_wrap_new_SinSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, int larg5, float larg6);
EXPORT void _wrap_delete_SinSyn (SinSyn *larg1);
EXPORT void _wrap_SinSyn_SetTable (SinSyn *larg1, Table *larg2);
EXPORT void _wrap_SinSyn_SetMaxTracks (SinSyn *larg1, int larg2);
EXPORT void _wrap_SinSyn_SetScale (SinSyn *larg1, float larg2);
EXPORT int _wrap_SinSyn_Set (SinSyn *larg1, char *larg2, float larg3);
EXPORT int _wrap_SinSyn_Connect (SinSyn *larg1, char *larg2, void *larg3);
EXPORT short _wrap_SinSyn_DoProcess (SinSyn *larg1);
EXPORT ReSyn *_wrap_new_ReSyn_empty ();
EXPORT ReSyn *_wrap_new_ReSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, float larg5, float larg6, int larg7, float larg8);
EXPORT void _wrap_ReSyn_SetPitch (ReSyn *larg1, float larg2);
EXPORT void _wrap_ReSyn_SetTimeScale (ReSyn *larg1, float larg2);
EXPORT int _wrap_ReSyn_Set (ReSyn *larg1, char *larg2, float larg3);
EXPORT void _wrap_delete_ReSyn (ReSyn *larg1);
EXPORT short _wrap_ReSyn_DoProcess (ReSyn *larg1);
EXPORT AdSyn *_wrap_new_AdSyn_empty ();
EXPORT AdSyn *_wrap_new_AdSyn (SinAnal *larg1, int larg2, Table *larg3, float larg4, float larg5, int larg6, float larg7);
EXPORT void _wrap_delete_AdSyn (AdSyn *larg1);
EXPORT short _wrap_AdSyn_DoProcess (AdSyn *larg1);
EXPORT IFAdd *_wrap_new_IFAdd_empty ();
EXPORT IFAdd *_wrap_new_IFAdd (IFGram *larg1, int larg2, Table *larg3, float larg4, float larg5, float larg6, int larg7, float larg8);
EXPORT void _wrap_delete_IFAdd (IFAdd *larg1);
EXPORT short _wrap_IFAdd_DoProcess (IFAdd *larg1);
EXPORT SpecMult *_wrap_new_SpecMult_empty ();
EXPORT SpecMult *_wrap_new_SpecMult (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT SpecMult *_wrap_new_SpecMult_table (Table *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SpecMult (SpecMult *larg1);
EXPORT int _wrap_SpecMult_Connect (SpecMult *larg1, char *larg2, void *larg3);
EXPORT void _wrap_SpecMult_SetTable (SpecMult *larg1, Table *larg2);
EXPORT short _wrap_SpecMult_DoProcess (SpecMult *larg1);
EXPORT SpecInterp *_wrap_new_SpecInterp_empty ();
EXPORT SpecInterp *_wrap_new_SpecInterp (float larg1, SndObj *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_SpecInterp (SpecInterp *larg1);
EXPORT int _wrap_SpecInterp_Connect (SpecInterp *larg1, char *larg2, void *larg3);
EXPORT int _wrap_SpecInterp_Set (SpecInterp *larg1, char *larg2, float larg3);
EXPORT void _wrap_SpecInterp_SetInterp (SpecInterp *larg1, float larg2, SndObj *larg3);
EXPORT short _wrap_SpecInterp_DoProcess (SpecInterp *larg1);
EXPORT PVMask *_wrap_new_PVMask_empty ();
EXPORT PVMask *_wrap_new_PVMask (float larg1, SndObj *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT PVMask *_wrap_new_PVMask_table (float larg1, Table *larg2, SndObj *larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_PVMask (PVMask *larg1);
EXPORT int _wrap_PVMask_Connect (PVMask *larg1, char *larg2, void *larg3);
EXPORT int _wrap_PVMask_Set (PVMask *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVMask_SetMaskInput (PVMask *larg1, SndObj *larg2);
EXPORT void _wrap_PVMask_SetMaskTable (PVMask *larg1, Table *larg2);
EXPORT void _wrap_PVMask_SetMaskGain (PVMask *larg1, float larg2, SndObj *larg3);
EXPORT short _wrap_PVMask_DoProcess (PVMask *larg1);
EXPORT int _wrap_PVTransp_Set (PVTransp *larg1, char *larg2, float larg3);
EXPORT int _wrap_PVTransp_Connect (PVTransp *larg1, char *larg2, void *larg3);
EXPORT void _wrap_PVTransp_SetPitch (PVTransp *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_PVTransp_SetMode (PVTransp *larg1, int larg2);
EXPORT PVTransp *_wrap_new_PVTransp_empty ();
EXPORT PVTransp *_wrap_new_PVTransp (SndObj *larg1, float larg2, int larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_PVTransp (PVTransp *larg1);
EXPORT short _wrap_PVTransp_DoProcess (PVTransp *larg1);
EXPORT PVMix *_wrap_new_PVMix_empty ();
EXPORT PVMix *_wrap_new_PVMix (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_PVMix (PVMix *larg1);
EXPORT short _wrap_PVMix_DoProcess (PVMix *larg1);
EXPORT int _wrap_PVBlur_Set (PVBlur *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVBlur_SetBlurTime (PVBlur *larg1, float larg2);
EXPORT void _wrap_PVBlur_SetHopsize (PVBlur *larg1, int larg2);
EXPORT PVBlur *_wrap_new_PVBlur_empty ();
EXPORT PVBlur *_wrap_new_PVBlur (SndObj *larg1, float larg2, int larg3, int larg4, float larg5);
EXPORT void _wrap_delete_PVBlur (PVBlur *larg1);
EXPORT short _wrap_PVBlur_DoProcess (PVBlur *larg1);
EXPORT PVFilter *_wrap_new_PVFilter_empty ();
EXPORT PVFilter *_wrap_new_PVFilter (SndObj *larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6);
EXPORT PVFilter *_wrap_new_PVFilter_table (Table *larg1, SndObj *larg2, float larg3, SndObj *larg4, int larg5, float larg6);
EXPORT void _wrap_delete_PVFilter (PVFilter *larg1);
EXPORT int _wrap_PVFilter_Connect (PVFilter *larg1, char *larg2, void *larg3);
EXPORT int _wrap_PVFilter_Set (PVFilter *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVFilter_SetFilterInput (PVFilter *larg1, SndObj *larg2);
EXPORT void _wrap_PVFilter_SetFilterTable (PVFilter *larg1, Table *larg2);
EXPORT void _wrap_PVFilter_SetAmount (PVFilter *larg1, float larg2, SndObj *larg3);
EXPORT short _wrap_PVFilter_DoProcess (PVFilter *larg1);
EXPORT PVMorph *_wrap_new_PVMorph_empty ();
EXPORT PVMorph *_wrap_new_PVMorph (float larg1, float larg2, SndObj *larg3, SndObj *larg4, SndObj *larg5, SndObj *larg6, int larg7, float larg8);
EXPORT void _wrap_delete_PVMorph (PVMorph *larg1);
EXPORT int _wrap_PVMorph_Connect (PVMorph *larg1, char *larg2, void *larg3);
EXPORT int _wrap_PVMorph_Set (PVMorph *larg1, char *larg2, float larg3);
EXPORT void _wrap_PVMorph_SetFreqMorph (PVMorph *larg1, float larg2, SndObj *larg3);
EXPORT void _wrap_PVMorph_SetAmpMorph (PVMorph *larg1, float larg2, SndObj *larg3);
EXPORT short _wrap_PVMorph_DoProcess (PVMorph *larg1);
EXPORT SpecPolar *_wrap_new_SpecPolar_empty ();
EXPORT SpecPolar *_wrap_new_SpecPolar (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_SpecPolar (SpecPolar *larg1);
EXPORT short _wrap_SpecPolar_DoProcess (SpecPolar *larg1);
EXPORT void _wrap_SpecSplit_magnitude_set (SpecSplit *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SpecSplit_magnitude_get (SpecSplit *larg1);
EXPORT void _wrap_SpecSplit_phase_set (SpecSplit *larg1, SndObj *larg2);
EXPORT SndObj *_wrap_SpecSplit_phase_get (SpecSplit *larg1);
EXPORT SpecSplit *_wrap_new_SpecSplit_empty ();
EXPORT SpecSplit *_wrap_new_SpecSplit (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_SpecSplit (SpecSplit *larg1);
EXPORT short _wrap_SpecSplit_DoProcess (SpecSplit *larg1);
EXPORT void _wrap_SpecThresh_SetThreshold (SpecThresh *larg1, float larg2);
EXPORT int _wrap_SpecThresh_Set (SpecThresh *larg1, char *larg2, float larg3);
EXPORT SpecThresh *_wrap_new_SpecThresh_empty ();
EXPORT SpecThresh *_wrap_new_SpecThresh (float larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SpecThresh (SpecThresh *larg1);
EXPORT short _wrap_SpecThresh_DoProcess (SpecThresh *larg1);
EXPORT SpecVoc *_wrap_new_SpecVoc_empty ();
EXPORT SpecVoc *_wrap_new_SpecVoc (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SpecVoc (SpecVoc *larg1);
EXPORT short _wrap_SpecVoc_DoProcess (SpecVoc *larg1);
EXPORT SpecCart *_wrap_new_SpecCart_empty ();
EXPORT SpecCart *_wrap_new_SpecCart (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_SpecCart (SpecCart *larg1);
EXPORT short _wrap_SpecCart_DoProcess (SpecCart *larg1);
EXPORT SpecCombine *_wrap_new_SpecCombine_empty ();
EXPORT SpecCombine *_wrap_new_SpecCombine (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SpecCombine (SpecCombine *larg1);
EXPORT void _wrap_SpecCombine_SetPhaseInput (SpecCombine *larg1, SndObj *larg2);
EXPORT void _wrap_SpecCombine_SetMagInput (SpecCombine *larg1, SndObj *larg2);
EXPORT int _wrap_SpecCombine_Connect (SpecCombine *larg1, char *larg2, void *larg3);
EXPORT short _wrap_SpecCombine_DoProcess (SpecCombine *larg1);
EXPORT SpecIn *_wrap_new_SpecIn_empty ();
EXPORT SpecIn *_wrap_new_SpecIn (SndFIO *larg1, short larg2, int larg3, float larg4);
EXPORT void _wrap_delete_SpecIn (SpecIn *larg1);
EXPORT void _wrap_SpecIn_SetInput (SpecIn *larg1, SndIO *larg2, short larg3);
EXPORT int _wrap_SpecIn_Connect (SpecIn *larg1, char *larg2, void *larg3);
EXPORT int _wrap_SpecIn_Set (SpecIn *larg1, char *larg2, float larg3);
EXPORT short _wrap_SpecIn_DoProcess (SpecIn *larg1);
EXPORT char *_wrap_SpecIn_ErrorMessage (SpecIn *larg1);
EXPORT Convol *_wrap_new_Convol_empty ();
EXPORT Convol *_wrap_new_Convol (Table *larg1, SndObj *larg2, float larg3, int larg4, float larg5);
EXPORT void _wrap_delete_Convol (Convol *larg1);
EXPORT int _wrap_Convol_Connect (Convol *larg1, char *larg2, void *larg3);
EXPORT int _wrap_Convol_Set (Convol *larg1, char *larg2, float larg3);
EXPORT void _wrap_Convol_SetImpulse (Convol *larg1, Table *larg2, float larg3);
EXPORT short _wrap_Convol_DoProcess (Convol *larg1);
EXPORT FILE *_wrap_SndFIO_GetFile (SndFIO *larg1);
EXPORT short _wrap_SndFIO_GetMode (SndFIO *larg1);
EXPORT void _wrap_SndFIO_SetPos_float (SndFIO *larg1, float larg2);
EXPORT void _wrap_SndFIO_SetPos (SndFIO *larg1, long larg2);
EXPORT int _wrap_SndFIO_Eof (SndFIO *larg1);
EXPORT long _wrap_SndFIO_GetDataFrames (SndFIO *larg1);
EXPORT float _wrap_SndFIO_GetPos (SndFIO *larg1);
EXPORT short _wrap_SndFIO_GetStatus (SndFIO *larg1);
EXPORT SndFIO *_wrap_new_SndFIO (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8);
EXPORT void _wrap_delete_SndFIO (SndFIO *larg1);
EXPORT short _wrap_SndFIO_Read (SndFIO *larg1);
EXPORT short _wrap_SndFIO_Write (SndFIO *larg1);
EXPORT char *_wrap_SndFIO_ErrorMessage (SndFIO *larg1);
EXPORT wave_head *_wrap_SndWave_GetHeader (SndWave *larg1);
EXPORT SndWave *_wrap_new_SndWave (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8);
EXPORT void _wrap_delete_SndWave (SndWave *larg1);
EXPORT short _wrap_SndWave_Read (SndWave *larg1);
EXPORT short _wrap_SndWave_Write (SndWave *larg1);
EXPORT bool _wrap_SndWave_IsWave (SndWave *larg1);
EXPORT char *_wrap_SndWave_ErrorMessage (SndWave *larg1);
EXPORT SndWaveX *_wrap_new_SndWaveX (char *larg1, short larg2, short larg3, int larg4, short larg5, short larg6, SndObj **larg7, float larg8, int larg9, float larg10);
EXPORT void _wrap_delete_SndWaveX (SndWaveX *larg1);
EXPORT void _wrap_SndWaveX_GetHeader (SndWaveX *larg1, WAVEFORMATEXTENSIBLE *larg2);
EXPORT int _wrap_SndWaveX_GetChannelMask (SndWaveX *larg1);
EXPORT short _wrap_SndWaveX_Read (SndWaveX *larg1);
EXPORT short _wrap_SndWaveX_Write (SndWaveX *larg1);
EXPORT bool _wrap_SndWaveX_IsWaveExtensible (SndWaveX *larg1);
EXPORT SndPVOCEX *_wrap_new_SndPVOCEX (char *larg1, short larg2, int larg3, int larg4, short larg5, int larg6, short larg7, int larg8, SndObj **larg9, float larg10, int larg11, int larg12, float larg13);
EXPORT void _wrap_delete_SndPVOCEX (SndPVOCEX *larg1);
EXPORT int _wrap_SndPVOCEX_GetFFTSize (SndPVOCEX *larg1);
EXPORT int _wrap_SndPVOCEX_GetHopSize (SndPVOCEX *larg1);
EXPORT int _wrap_SndPVOCEX_GetWindowType (SndPVOCEX *larg1);
EXPORT int _wrap_SndPVOCEX_GetWindowLength (SndPVOCEX *larg1);
EXPORT void _wrap_SndPVOCEX_GetHeader (SndPVOCEX *larg1, WAVEFORMATPVOCEX *larg2);
EXPORT void _wrap_SndPVOCEX_SetTimePos (SndPVOCEX *larg1, float larg2);
EXPORT short _wrap_SndPVOCEX_Read (SndPVOCEX *larg1);
EXPORT short _wrap_SndPVOCEX_Write (SndPVOCEX *larg1);
EXPORT bool _wrap_SndPVOCEX_IsPvocex (SndPVOCEX *larg1);
EXPORT SndSinIO *_wrap_new_SndSinIO (char *larg1, int larg2, float larg3, int larg4, short larg5, short larg6, int larg7, short larg8, int larg9, SndObj **larg10, float larg11, int larg12, int larg13, float larg14);
EXPORT void _wrap_delete_SndSinIO (SndSinIO *larg1);
EXPORT short _wrap_SndSinIO_Write (SndSinIO *larg1);
EXPORT short _wrap_SndSinIO_Read (SndSinIO *larg1);
EXPORT int _wrap_SndSinIO_GetTrackID (SndSinIO *larg1, int larg2, int larg3);
EXPORT int _wrap_SndSinIO_GetTracks (SndSinIO *larg1, int larg2);
EXPORT int _wrap_SndSinIO_GetFFTSize (SndSinIO *larg1);
EXPORT int _wrap_SndSinIO_GetHopSize (SndSinIO *larg1);
EXPORT int _wrap_SndSinIO_GetWindowType (SndSinIO *larg1);
EXPORT int _wrap_SndSinIO_GetMaxTracks (SndSinIO *larg1);
EXPORT void _wrap_SndSinIO_GetHeader (SndSinIO *larg1, WAVEFORMATSINUSEX *larg2);
EXPORT void _wrap_SndSinIO_SetTimePos (SndSinIO *larg1, float larg2);
EXPORT bool _wrap_SndAiff_IsAiff (SndAiff *larg1);
EXPORT SndAiff *_wrap_new_SndAiff (char *larg1, short larg2, short larg3, short larg4, SndObj **larg5, float larg6, int larg7, float larg8);
EXPORT void _wrap_delete_SndAiff (SndAiff *larg1);
EXPORT short _wrap_SndAiff_Read (SndAiff *larg1);
EXPORT short _wrap_SndAiff_Write (SndAiff *larg1);
EXPORT char *_wrap_SndAiff_ErrorMessage (SndAiff *larg1);
EXPORT SndBuffer *_wrap_new_SndBuffer (short larg1, int larg2, SndObj **larg3, int larg4, float larg5);
EXPORT void _wrap_delete_SndBuffer (SndBuffer *larg1);
EXPORT short _wrap_SndBuffer_Write (SndBuffer *larg1);
EXPORT short _wrap_SndBuffer_Read (SndBuffer *larg1);
EXPORT char *_wrap_SndBuffer_ErrorMessage (SndBuffer *larg1);
EXPORT void _wrap_HarmTable_SetHarm (HarmTable *larg1, int larg2, int larg3);
EXPORT char *_wrap_HarmTable_ErrorMessage (HarmTable *larg1);
EXPORT short _wrap_HarmTable_MakeTable (HarmTable *larg1);
EXPORT HarmTable *_wrap_new_HarmTable_empty ();
EXPORT void _wrap_HarmTable_SetPhase (HarmTable *larg1, float larg2);
EXPORT HarmTable *_wrap_new_HarmTable (long larg1, int larg2, int larg3, float larg4);
EXPORT void _wrap_delete_HarmTable (HarmTable *larg1);
EXPORT void _wrap_UsrHarmTable_SetHarm (UsrHarmTable *larg1, int larg2, float *larg3);
EXPORT char *_wrap_UsrHarmTable_ErrorMessage (UsrHarmTable *larg1);
EXPORT short _wrap_UsrHarmTable_MakeTable (UsrHarmTable *larg1);
EXPORT UsrHarmTable *_wrap_new_UsrHarmTable_empty ();
EXPORT UsrHarmTable *_wrap_new_UsrHarmTable (long larg1, int larg2, float *larg3);
EXPORT void _wrap_delete_UsrHarmTable (UsrHarmTable *larg1);
EXPORT void _wrap_TrisegTable_SetCurve (TrisegTable *larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, float larg9);
EXPORT void _wrap_TrisegTable_SetCurve_points (TrisegTable *larg1, float *larg2, float larg3);
EXPORT char *_wrap_TrisegTable_ErrorMessage (TrisegTable *larg1);
EXPORT short _wrap_TrisegTable_MakeTable (TrisegTable *larg1);
EXPORT TrisegTable *_wrap_new_TrisegTable_empty ();
EXPORT TrisegTable *_wrap_new_TrisegTable (long larg1, float larg2, float larg3, float larg4, float larg5, float larg6, float larg7, float larg8, float larg9);
EXPORT TrisegTable *_wrap_new_TrisegTable_points (long larg1, float *larg2, float larg3);
EXPORT void _wrap_delete_TrisegTable (TrisegTable *larg1);
EXPORT void _wrap_EnvTable_SetEnvelope (EnvTable *larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6);
EXPORT char *_wrap_EnvTable_ErrorMessage (EnvTable *larg1);
EXPORT short _wrap_EnvTable_MakeTable (EnvTable *larg1);
EXPORT EnvTable *_wrap_new_EnvTable_empty ();
EXPORT EnvTable *_wrap_new_EnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6);
EXPORT void _wrap_delete_EnvTable (EnvTable *larg1);
EXPORT void _wrap_SndTable_SetInput (SndTable *larg1, long larg2, SndFIO *larg3, short larg4);
EXPORT char *_wrap_SndTable_ErrorMessage (SndTable *larg1);
EXPORT short _wrap_SndTable_MakeTable (SndTable *larg1);
EXPORT SndTable *_wrap_new_SndTable_empty ();
EXPORT SndTable *_wrap_new_SndTable (long larg1, SndFIO *larg2, short larg3);
EXPORT void _wrap_delete_SndTable (SndTable *larg1);
EXPORT void _wrap_PlnTable_SetPln (PlnTable *larg1, int larg2, double *larg3, float larg4);
EXPORT char *_wrap_PlnTable_ErrorMessage (PlnTable *larg1);
EXPORT short _wrap_PlnTable_MakeTable (PlnTable *larg1);
EXPORT PlnTable *_wrap_new_PlnTable_empty ();
EXPORT PlnTable *_wrap_new_PlnTable (long larg1, int larg2, double *larg3, float larg4);
EXPORT void _wrap_delete_PlnTable (PlnTable *larg1);
EXPORT void _wrap_HammingTable_SetParam (HammingTable *larg1, long larg2, float larg3);
EXPORT char *_wrap_HammingTable_ErrorMessage (HammingTable *larg1);
EXPORT short _wrap_HammingTable_MakeTable (HammingTable *larg1);
EXPORT HammingTable *_wrap_new_HammingTable_empty ();
EXPORT HammingTable *_wrap_new_HammingTable (long larg1, float larg2);
EXPORT void _wrap_delete_HammingTable (HammingTable *larg1);
EXPORT void _wrap_NoteTable_SetFreqInterval (NoteTable *larg1, float larg2, float larg3);
EXPORT void _wrap_NoteTable_SetNoteInterval (NoteTable *larg1, short larg2, short larg3);
EXPORT NoteTable *_wrap_new_NoteTable_empty ();
EXPORT NoteTable *_wrap_new_NoteTable (short larg1, short larg2, float larg3, float larg4);
EXPORT void _wrap_delete_NoteTable (NoteTable *larg1);
EXPORT short _wrap_NoteTable_MakeTable (NoteTable *larg1);
EXPORT char *_wrap_NoteTable_ErrorMessage (NoteTable *larg1);
EXPORT void _wrap_UsrDefTable_SetTable (UsrDefTable *larg1, long larg2, float *larg3);
EXPORT char *_wrap_UsrDefTable_ErrorMessage (UsrDefTable *larg1);
EXPORT short _wrap_UsrDefTable_MakeTable (UsrDefTable *larg1);
EXPORT UsrDefTable *_wrap_new_UsrDefTable_empty ();
EXPORT UsrDefTable *_wrap_new_UsrDefTable (long larg1, float *larg2);
EXPORT void _wrap_delete_UsrDefTable (UsrDefTable *larg1);
EXPORT char *_wrap_LoPassTable_ErrorMessage (LoPassTable *larg1);
EXPORT short _wrap_LoPassTable_MakeTable (LoPassTable *larg1);
EXPORT void _wrap_LoPassTable_SetFreq (LoPassTable *larg1, float larg2);
EXPORT void _wrap_LoPassTable_SetSr (LoPassTable *larg1, float larg2);
EXPORT LoPassTable *_wrap_new_LoPassTable (int larg1, float larg2, float larg3);
EXPORT LoPassTable *_wrap_new_LoPassTable_empty ();
EXPORT void _wrap_delete_LoPassTable (LoPassTable *larg1);
EXPORT void _wrap_PVEnvTable_SetEnvelope (PVEnvTable *larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7);
EXPORT void _wrap_PVEnvTable_SetSr (PVEnvTable *larg1, float larg2);
EXPORT char *_wrap_PVEnvTable_ErrorMessage (PVEnvTable *larg1);
EXPORT short _wrap_PVEnvTable_MakeTable (PVEnvTable *larg1);
EXPORT PVEnvTable *_wrap_new_PVEnvTable_empty ();
EXPORT PVEnvTable *_wrap_new_PVEnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7, float larg8);
EXPORT void _wrap_delete_PVEnvTable (PVEnvTable *larg1);
EXPORT short _wrap_SpecEnvTable_MakeTable (SpecEnvTable *larg1);
EXPORT SpecEnvTable *_wrap_new_SpecEnvTable_empty ();
EXPORT SpecEnvTable *_wrap_new_SpecEnvTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, float larg7);
EXPORT void _wrap_delete_SpecEnvTable (SpecEnvTable *larg1);
EXPORT void _wrap_PVTable_SetTable (PVTable *larg1, SndFIO *larg2, Table *larg3, float larg4, float larg5);
EXPORT short _wrap_PVTable_MakeTable (PVTable *larg1);
EXPORT PVTable *_wrap_new_PVTable_empty ();
EXPORT PVTable *_wrap_new_PVTable (int larg1, SndFIO *larg2, Table *larg3, float larg4, float larg5);
EXPORT void _wrap_delete_PVTable (PVTable *larg1);
EXPORT char *_wrap_PVTable_ErrorMessage (PVTable *larg1);
EXPORT void _wrap_ImpulseTable_SetWindow (ImpulseTable *larg1, Table *larg2);
EXPORT short _wrap_ImpulseTable_MakeTable (ImpulseTable *larg1);
EXPORT ImpulseTable *_wrap_new_ImpulseTable_empty ();
EXPORT ImpulseTable *_wrap_new_ImpulseTable (long larg1, int larg2, float larg3, float *larg4, float *larg5, float larg6, Table *larg7, float larg8);
EXPORT void _wrap_delete_ImpulseTable (ImpulseTable *larg1);


///* sa_wrap.cpp */
//
EXPORT Acos *_wrap_new_Acos (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Acos (Acos *larg1);
EXPORT short _wrap_Acos_DoProcess (Acos *larg1);

EXPORT Asin *_wrap_new_Asin (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Asin (Asin *larg1);
EXPORT short _wrap_Asin_DoProcess (Asin *larg1);

EXPORT Atan *_wrap_new_Atan (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Atan (Atan *larg1);
EXPORT short _wrap_Atan_DoProcess (Atan *larg1);

EXPORT Atan2 *_wrap_new_Atan2 (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Atan2 (Atan2 *larg1);
EXPORT short _wrap_Atan2_DoProcess (Atan2 *larg1);

EXPORT void _wrap_delete_Cos (Cos *larg1);
EXPORT short _wrap_Cos_DoProcess (Cos *larg1);

EXPORT Sin *_wrap_new_Sin (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Sin (Sin *larg1);
EXPORT short _wrap_Sin_DoProcess (Sin *larg1);

EXPORT Tan *_wrap_new_Tan (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Tan (Tan *larg1);
EXPORT short _wrap_Tan_DoProcess (Tan *larg1);

EXPORT Acosh *_wrap_new_Acosh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Acosh (Acosh *larg1);
EXPORT short _wrap_Acosh_DoProcess (Acosh *larg1);

EXPORT Asinh *_wrap_new_Asinh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Asinh (Asinh *larg1);
EXPORT short _wrap_Asinh_DoProcess (Asinh *larg1);

EXPORT Atanh *_wrap_new_Atanh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Atanh (Atanh *larg1);
EXPORT short _wrap_Atanh_DoProcess (Atanh *larg1);

EXPORT Cosh *_wrap_new_Cosh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Cosh (Cosh *larg1);
EXPORT short _wrap_Cosh_DoProcess (Cosh *larg1);

EXPORT Sinh *_wrap_new_Sinh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Sinh (Sinh *larg1);
EXPORT short _wrap_Sinh_DoProcess (Sinh *larg1);

EXPORT Tanh *_wrap_new_Tanh (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Tanh (Tanh *larg1);
EXPORT short _wrap_Tanh_DoProcess (Tanh *larg1);

EXPORT Exp *_wrap_new_Exp (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Exp (Exp *larg1);
EXPORT short _wrap_Exp_DoProcess (Exp *larg1);


EXPORT Exp2 *_wrap_new_Exp2 (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Exp2 (Exp2 *larg1);
EXPORT short _wrap_Exp2_DoProcess (Exp2 *larg1);

EXPORT Log *_wrap_new_Log (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Log (Log *larg1);
EXPORT short _wrap_Log_DoProcess (Log *larg1);

EXPORT Log10 *_wrap_new_Log10 (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Log10 (Log10 *larg1);
EXPORT short _wrap_Log10_DoProcess (Log10 *larg1);

EXPORT Log2 *_wrap_new_Log2 (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Log2 (Log2 *larg1);
EXPORT short _wrap_Log2_DoProcess (Log2 *larg1);

EXPORT Log1p *_wrap_new_Log1p (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Log1p (Log1p *larg1);
EXPORT short _wrap_Log1p_DoProcess (Log1p *larg1);

EXPORT Logb *_wrap_new_Logb (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Logb (Logb *larg1);
EXPORT short _wrap_Logb_DoProcess (Logb *larg1);

EXPORT Fabs *_wrap_new_Fabs (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Fabs (Fabs *larg1);
EXPORT short _wrap_Fabs_DoProcess (Fabs *larg1);

EXPORT Cbrt *_wrap_new_Cbrt (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Cbrt (Cbrt *larg1);
EXPORT short _wrap_Cbrt_DoProcess (Cbrt *larg1);

EXPORT Hypot *_wrap_new_Hypot (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Hypot (Hypot *larg1);
EXPORT short _wrap_Hypot_DoProcess (Hypot *larg1);
EXPORT Hypot *_wrap_new_Hypotf (SndObj *larg1, float larg2, int larg3, float larg4);


EXPORT Pow *_wrap_new_Pow (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Pow (Pow *larg1);
EXPORT short _wrap_Pow_DoProcess (Pow *larg1);
EXPORT Pow *_wrap_new_Powf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Sqrt *_wrap_new_Sqrt (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Sqrt (Sqrt *larg1);
EXPORT short _wrap_Sqrt_DoProcess (Sqrt *larg1);

EXPORT Ceil *_wrap_new_Ceil (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Ceil (Ceil *larg1);
EXPORT short _wrap_Ceil_DoProcess (Ceil *larg1);

EXPORT Floor *_wrap_new_Floor (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Floor (Floor *larg1);
EXPORT short _wrap_Floor_DoProcess (Floor *larg1);

EXPORT Fdim *_wrap_new_Fdim (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Fdim (Fdim *larg1);
EXPORT short _wrap_Fdim_DoProcess (Fdim *larg1);

EXPORT Fmax *_wrap_new_Fmax (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Fmax (Fmax *larg1);
EXPORT short _wrap_Fmax_DoProcess (Fmax *larg1);

EXPORT Fmax *_wrap_new_Fmaxf (SndObj *larg1, float larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Fmaxf (Fmax *larg1);
EXPORT short _wrap_Fmaxf_DoProcess (Fmax *larg1);


EXPORT Fmin *_wrap_new_Fmin (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Fmin (Fmin *larg1);
EXPORT short _wrap_Fmin_DoProcess (Fmin *larg1);
EXPORT Fmin *_wrap_new_Fminf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Multiply *_wrap_new_Multiply (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Multiply (Multiply *larg1);
EXPORT short _wrap_Multiply_DoProcess (Multiply *larg1);
EXPORT Multiply *_wrap_new_Multiplyf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Divide *_wrap_new_Divide (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Divide (Divide *larg1);
EXPORT short _wrap_Divide_DoProcess (Divide *larg1);
EXPORT Divide *_wrap_new_Dividef (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Add *_wrap_new_Add (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Add (Add *larg1);
EXPORT short _wrap_Add_DoProcess (Add *larg1);
EXPORT Add *_wrap_new_Addf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Subtract *_wrap_new_Subtract (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Subtract (Subtract *larg1);
EXPORT short _wrap_Subtract_DoProcess (Subtract *larg1);
EXPORT Subtract *_wrap_new_Subtractf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT GreaterThan *_wrap_new_GreaterThan (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_GreaterThan (GreaterThan *larg1);
EXPORT short _wrap_GreaterThan_DoProcess (GreaterThan *larg1);
EXPORT GreaterThan *_wrap_new_GreaterThanf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT GreaterThanEqual *_wrap_new_GreaterThanEqual (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_GreaterThanEqual (GreaterThanEqual *larg1);
EXPORT short _wrap_GreaterThanEqual_DoProcess (GreaterThanEqual *larg1);
EXPORT GreaterThanEqual *_wrap_new_GreaterThanEqualf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT LessThan *_wrap_new_LessThan (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_LessThan (LessThan *larg1);
EXPORT short _wrap_LessThan_DoProcess (LessThan *larg1);
EXPORT LessThan *_wrap_new_LessThanf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT LessThanEqual *_wrap_new_LessThanEqual (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_LessThanEqual (LessThanEqual *larg1);
EXPORT short _wrap_LessThanEqual_DoProcess (LessThanEqual *larg1);
EXPORT LessThanEqual *_wrap_new_LessThanEqualf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT And *_wrap_new_And (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_And (And *larg1);
EXPORT short _wrap_And_DoProcess (And *larg1);
EXPORT And *_wrap_new_Andf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Or *_wrap_new_Or (SndObj *larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_Or (Or *larg1);
EXPORT short _wrap_Or_DoProcess (Or *larg1);
EXPORT Or *_wrap_new_Orf (SndObj *larg1, float larg2, int larg3, float larg4);

EXPORT Not *_wrap_new_Not (SndObj *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_Not (Not *larg1);
EXPORT short _wrap_Not_DoProcess (Not *larg1);

EXPORT If *_wrap_new_If (SndObj *larg1, SndObj *larg2, SndObj *larg3, int larg4, float larg5);
EXPORT void _wrap_delete_If (If *larg1);
EXPORT short _wrap_If_DoProcess (If *larg1);
EXPORT If *_wrap_new_Iffs (SndObj *larg1, float larg2, SndObj *larg3, int larg4, float larg5);
EXPORT If *_wrap_new_Ifsf (SndObj *larg1, SndObj *larg2, float larg3, int larg4, float larg5);
EXPORT If *_wrap_new_Ifff (SndObj *larg1, float larg2, float larg3, int larg4, float larg5);



EXPORT RTOutput *_wrap_new_RTOutput_empty ();
EXPORT RTOutput *_wrap_new_RTOutput (int larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_RTOutput (RTOutput *larg1);
EXPORT void _wrap_RTOutput_SetChan (RTOutput *larg1, int larg2);
EXPORT short _wrap_RTOutput_DoProcess (RTOutput *larg1);
EXPORT BusWrite *_wrap_new_BusWrite_empty ();
EXPORT BusWrite *_wrap_new_BusWrite (int larg1, SndObj *larg2, int larg3, float larg4);
EXPORT void _wrap_delete_BusWrite (BusWrite *larg1);
EXPORT void _wrap_BusWrite_SetBus (BusWrite *larg1, int larg2);
EXPORT short _wrap_BusWrite_DoProcess (BusWrite *larg1);
EXPORT BusRead *_wrap_new_BusRead_empty ();
EXPORT BusRead *_wrap_new_BusRead (int larg1, int larg2, float larg3);
EXPORT void _wrap_delete_BusRead (BusRead *larg1);
EXPORT void _wrap_BusRead_SetBus (BusRead *larg1, int larg2);
EXPORT short _wrap_BusRead_DoProcess (BusRead *larg1);
EXPORT SndObj *_wrap_PVOCEXRead_Outchannel (PVOCEXRead *larg1, int larg2);
EXPORT PVOCEXRead *_wrap_new_PVOCEXRead_empty ();
EXPORT PVOCEXRead *_wrap_new_PVOCEXRead (char *larg1, int larg2, float larg3);
EXPORT void _wrap_delete_PVOCEXRead (PVOCEXRead *larg1);
EXPORT void _wrap_PVOCEXRead_SetInput (PVOCEXRead *larg1, char *larg2);
EXPORT short _wrap_PVOCEXRead_DoProcess (PVOCEXRead *larg1);
EXPORT FloatSig *_wrap_new_FloatSig_empty ();
EXPORT FloatSig *_wrap_new_FloatSig (float larg1, int larg2, float larg3);
EXPORT void _wrap_FloatSig_SetVal (FloatSig *larg1, float larg2);
EXPORT short _wrap_FloatSig_DoProcess (FloatSig *larg1);
EXPORT void *_wrap_RTRunAudio (void *larg1);
EXPORT int _wrap_paCallback (void *larg1, void *larg2, unsigned long larg3, PaStreamCallbackTimeInfo *larg4, PaStreamCallbackFlags *larg5, void *larg6);
EXPORT Synth *_wrap_new_Synth_empty ();
EXPORT Synth *_wrap_new_Synth (int larg1, SndObj **larg2, long larg3, void (*larg4)(SndObj *), int larg5, float larg6);
EXPORT void _wrap_delete_Synth (Synth *larg1);
EXPORT void _wrap_Synth_SetDuration (Synth *larg1, long larg2);
EXPORT long _wrap_Synth_GetDuration (Synth *larg1);
EXPORT int _wrap_Synth_GetObjNo (Synth *larg1);
EXPORT short _wrap_Synth_DoProcess (Synth *larg1);
EXPORT char *_wrap_Synth_ErrorMessage (Synth *larg1);
EXPORT int _wrap_Synth_Add (Synth *larg1);
EXPORT int _wrap_Synth_Free (Synth *larg1);
EXPORT Synth *_wrap_Synth_Next (Synth *larg1);
EXPORT Synth *_wrap_Synth_Previous (Synth *larg1);
EXPORT RTAudioStream *_wrap_RTAudioStream_Instance (float larg1, int larg2, int larg3, int larg4, int larg5);
EXPORT int _wrap_RTAudioStream_Start (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_Stop (RTAudioStream *larg1);
EXPORT float _wrap_RTAudioStream_GetSampleRate (RTAudioStream *larg1);
EXPORT int _wrap_RTAudioStream_GetVectorSize (RTAudioStream *larg1);
EXPORT PaStream *_wrap_RTAudioStream_GetPAStream (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_sampleRate_set (RTAudioStream *larg1, float larg2);
EXPORT float _wrap_RTAudioStream_m_sampleRate_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_inputChannels_set (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_m_inputChannels_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_outputChannels_set (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_m_outputChannels_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_busses_set (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_m_busses_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_bufferSize_set (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_m_bufferSize_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_synthHead_set (RTAudioStream *larg1, Synth *larg2);
EXPORT Synth *_wrap_RTAudioStream_m_synthHead_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_outputs_set (RTAudioStream *larg1, SndObj **larg2);
EXPORT SndObj **_wrap_RTAudioStream_m_outputs_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_inputs_set (RTAudioStream *larg1, SndObj **larg2);
EXPORT SndObj **_wrap_RTAudioStream_m_inputs_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_inbusses_set (RTAudioStream *larg1, SndObj **larg2);
EXPORT SndObj **_wrap_RTAudioStream_m_inbusses_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_outbusses_set (RTAudioStream *larg1, SndObj **larg2);
EXPORT SndObj **_wrap_RTAudioStream_m_outbusses_get (RTAudioStream *larg1);
EXPORT void _wrap_RTAudioStream_m_status_set (RTAudioStream *larg1, int larg2);
EXPORT int _wrap_RTAudioStream_m_status_get (RTAudioStream *larg1);
 





#endif
