#include <stdlib.h>

#define RANDOM_MAX 0x7FFFFFFF

unsigned long rts_random_seed (unsigned long seed) {
  srandom(seed);
  return seed;
}

int rts_between (int a, int b) {
  int w = (b-a);
  return (w<1) ? a : a+(int)(w*(float)random()/(RANDOM_MAX + 1.0));
}

int rts_random (int r) {
  return (int)(r*(float)random()/(RANDOM_MAX + 1.0));
}

int rts_rescale (int x, int x1, int x2, int y1, int y2) {
  float r1 = (float)(x2-x1);
  float r2 = (float)(y2-y1);
  return y1+(int)((r2/r1)*(float)(x-x1));
}

int rts_rhythm2msec (int rhy, int tempo) {
  return (int)(1000.0*((float)rhy/480.0)*(60.0/(float)tempo));
}

