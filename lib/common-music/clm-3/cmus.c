/* C support code for CLM */

#include "cmus.h"

sigfnc *clm_signal(int signo, sigfnc *fnc) 
{
  return(signal(signo, fnc));
}

static void mus_error2clm(int type, char *str)
{
  /* there's apparently no way to get printout from C in ACL on windows --aclprintf, fprintf,
   *   and lisp_call_address back to lisp all fail.  The folks at Franz had no suggestions.
   */
  fprintf(stdout, str); 
  fflush(stdout);
}

void initialize_cmus(void)
{
  init_mus_module();
  mus_error_set_handler(mus_error2clm);
}

void clm_mix(const char *outfile, const char *infile, int out_start, int out_frames, int in_start)
{
  mus_mix(outfile, infile, (off_t)out_start, (off_t)out_frames, (off_t)in_start, NULL, NULL);
}

#define MIX_BUFFER_SIZE 8192
int clm_scale_file(char *outfile, char *infile, double scaler, int out_format, int out_header)
{
  int i, j, k, ofd, ifd, chans, datsize, frames;
  mus_sample_t **obufs;
  chans = mus_sound_chans(infile);
  obufs = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++) 
    obufs[i] = (mus_sample_t *)calloc(MIX_BUFFER_SIZE, sizeof(mus_sample_t));
  ifd = mus_sound_open_input(infile);
  mus_file_seek_frame(ifd, 0);
  mus_file_read(ifd, 0, MIX_BUFFER_SIZE - 1, chans, obufs);
  ofd = mus_sound_open_output(outfile, mus_sound_srate(infile), chans, out_format, out_header, NULL);
  datsize = mus_bytes_per_sample(out_format);
  frames = mus_sound_frames(infile);
  if (chans == 1)
    {
      for (k = 0, j = 0; k < frames; k++, j++)
	{
	  if (j == MIX_BUFFER_SIZE)
	    {
	      mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      mus_file_read(ifd, 0, MIX_BUFFER_SIZE - 1, chans, obufs);
	    }
	  obufs[0][j] = (mus_sample_t)(scaler * obufs[0][j]);
	}
    }
  else
    {
      for (k = 0, j = 0; k < frames; k++, j++)
	{
	  if (j == MIX_BUFFER_SIZE)
	    {
	      mus_file_write(ofd, 0, j - 1, chans, obufs);
	      j = 0;
	      mus_file_read(ifd, 0, MIX_BUFFER_SIZE - 1, chans, obufs);
	    }
	  for (i = 0; i < chans; i++) 
	    obufs[i][j] = (mus_sample_t)(obufs[i][j] * scaler);
	}
    }
  if (j > 0)
    mus_file_write(ofd, 0, j - 1, chans, obufs);
  mus_sound_close_output(ofd, frames * chans * datsize);
  mus_sound_close_input(ifd);
  for (i = 0; i < chans; i++)
    FREE(obufs[i]);
  FREE(obufs);
  return(frames);
}

/* sndplay as function call, added 14-Jul-00, 
 *                           fixed up mus_file_read trailing chunk business 26-Jan-01 (but it was broken again later?)
 *                           added device arg 8-Feb-01
 */

#if MUS_MAC_OSX
  #define BUFFER_SIZE 256
#else
  #define BUFFER_SIZE 4096
#endif

int sl_dac(char *name, int output_device)
{
  int fd, afd, i, j, n, k, chans, srate, frames, outbytes, curframes;
  mus_sample_t **bufs;
  short *obuf;
  mus_sound_initialize();

  afd = -1;
  fd = mus_sound_open_input(name);
  if (fd != -1)
    {
      chans = mus_sound_chans(name);
      srate = mus_sound_srate(name);
      frames = mus_sound_frames(name);
      outbytes = BUFFER_SIZE * chans * 2;
      bufs = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
      for (i = 0; i < chans; i++) 
	bufs[i] = (mus_sample_t *)calloc(BUFFER_SIZE, sizeof(mus_sample_t));
      obuf = (short *)calloc(BUFFER_SIZE * chans, sizeof(short));
      /* assume for lafs that our DAC wants 16-bit integers */
      for (i = 0; i < frames; i += BUFFER_SIZE)
	{
	  if ((i + BUFFER_SIZE) <= frames)
	    curframes = BUFFER_SIZE;
	  else curframes = frames - i;
	  mus_file_read(fd, 0, curframes - 1, chans, bufs); 
	  if (chans == 1)
	    {
	      for (k = 0; k < curframes; k++) 
		obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
	    }
	  else
	    {
	      if (chans == 2)
		{
		  for (k = 0, n = 0; k < curframes; k++, n += 2) 
		    {
		      obuf[n] = MUS_SAMPLE_TO_SHORT(bufs[0][k]); 
		      obuf[n + 1] = MUS_SAMPLE_TO_SHORT(bufs[1][k]);
		    }
		}
	      else
		{
		  for (k = 0, j = 0; k < curframes; k++, j += chans)
		    for (n = 0; n < chans; n++) 
		      obuf[j + n] = MUS_SAMPLE_TO_SHORT(bufs[n][k]);
		}
	    }
	  if (afd == -1)
	    {
#if defined(MUS_LINUX) && defined(PPC)
	      afd = mus_audio_open_output(output_device, srate, chans, MUS_AUDIO_COMPATIBLE_FORMAT, 0);
#else
	      afd = mus_audio_open_output(output_device, srate, chans, MUS_AUDIO_COMPATIBLE_FORMAT, outbytes);
#endif
	      if (afd == -1) return(-1);
	    }
	  outbytes = curframes * chans * 2;
	  mus_audio_write(afd, (char *)obuf, outbytes);
	}
      if (afd != -1) mus_audio_close(afd);
      mus_sound_close_input(fd);
      for (i = 0; i < chans; i++) FREE(bufs[i]);
      FREE(bufs);
      FREE(obuf);
    }
  else return(-2);
  return(0);
}

#ifndef MUS_INTERP_LINEAR
  #define MUS_INTERP_LINEAR 0
#endif

/* type conversions for lisp FFI's that can't handle off_t */

/* ffi.lisp */
int clm_sound_samples(const char *arg) 
{
  return((int)mus_sound_samples(arg));
}

int clm_sound_frames(const char *arg) 
{
  return((int)mus_sound_frames(arg));
}

int clm_sound_data_location(const char *arg) 
{
  return((int)mus_sound_data_location(arg));
}

int clm_sound_length(const char *arg) 
{
  return((int)mus_sound_length(arg));
}

char *clm_sound_comment(const char *arg)
{
  char *comment;
  comment = mus_sound_comment(arg);
  if ((comment) && (*comment))
    return(comment);
  return(" ");
}

int clm_sound_reopen_output(const char *arg, int chans, int format, int type, int data_loc)
{
  return(mus_sound_reopen_output(arg, chans, format, type, (off_t)data_loc));
}

int clm_sound_close_output(int fd, int bytes_of_data) 
{
  return(mus_sound_close_output(fd, (off_t)bytes_of_data));
}

/* sndlib2clm.lisp */
int clm_sound_comment_start(const char *arg) 
{
  return((int)mus_sound_comment_start(arg));
}

int clm_sound_comment_end(const char *arg) 
{
  return((int)mus_sound_comment_end(arg));
}

int clm_header_samples(void) 
{
  return((int)mus_header_samples());
}

int clm_header_data_location(void) 
{
  return((int)mus_header_data_location());
}

int clm_header_comment_start(void) 
{
  return((int)mus_header_comment_start());
}

int clm_header_comment_end(void) 
{
  return((int)mus_header_comment_end());
}

int clm_header_true_length(void) 
{
  return((int)mus_header_true_length());
}

int clm_samples_to_bytes(int format, int size) 
{
  return(mus_samples_to_bytes(format, (off_t)size));
}

int clm_bytes_to_samples(int format, int size) 
{
  return(mus_bytes_to_samples(format, (off_t)size));
}

int clm_header_write(const char *name, int type, int srate, int chans, int loc, int size_in_samples, int format, const char *comment, int len)
{
  return((int)mus_header_write(name, type, srate, chans, (off_t)loc, (off_t)size_in_samples, format, comment, len));
}

int clm_header_aux_comment_start(int n) 
{
  return((int)mus_header_aux_comment_start(n));
}

int clm_header_aux_comment_end(int n) 
{
  return((int)mus_header_aux_comment_end(n));
}

int clm_file2array(char *filename, int chan, int start, int samples, double *array)
{
  return(mus_file_to_float_array(filename, chan, (off_t)start, samples, array));
}

static double *clm_clisp_samples = NULL;
static int clm_clisp_samples_size = 0;
int clm_clisp_file2array_init(char *filename, int chan, int start, int samples)
{
  if (clm_clisp_samples) FREE(clm_clisp_samples);
  clm_clisp_samples = (double *)CALLOC(samples, sizeof(double));
  clm_clisp_samples_size = samples;
  return(clm_file2array(filename, chan, start, samples, clm_clisp_samples));
}

double clm_clisp_file2array(int sample)
{
  if ((clm_clisp_samples) &&
      (sample < clm_clisp_samples_size))
    return(clm_clisp_samples[sample]);
  return(0.0);
}

int clm_clisp_write_ints(int fd, int *buf, int n) {return(write(fd, (char *)buf, n));}
int clm_clisp_write_floats(int fd, double *buf, int n) {return(write(fd, (char *)buf, n));}
int clm_clisp_close(int fd) {return(close(fd));}
int clm_clisp_lseek(int fd, int loc, int type) {return((int)lseek(fd, loc, type));}

static double *clm_clisp_doubles = NULL;
static int clm_clisp_doubles_size = 0;
int clm_clisp_doubles_init(int fd, int n)
{
  if (clm_clisp_doubles) FREE(clm_clisp_doubles);
  clm_clisp_doubles = (double *)CALLOC(n, sizeof(double));
  clm_clisp_doubles_size = n;
  return(read(fd, (char *)clm_clisp_doubles, sizeof(double) * n));
}
double clm_clisp_double(int n)
{
  if ((clm_clisp_doubles) &&
      (n < clm_clisp_doubles_size))
    return(clm_clisp_doubles[n]);
  return(0.0);
}

static int *clm_clisp_ints = NULL;
static int clm_clisp_ints_size = 0;
int clm_clisp_ints_init(int fd, int n)
{
  if (clm_clisp_ints) FREE(clm_clisp_ints);
  clm_clisp_ints = (int *)CALLOC(n, sizeof(int));
  clm_clisp_ints_size = n;
  return(read(fd, (char *)clm_clisp_ints, sizeof(int) * n));
}
int clm_clisp_int(int n)
{
  if ((clm_clisp_ints) &&
      (n < clm_clisp_ints_size))
    return(clm_clisp_ints[n]);
  return(0);
}

void clm_array2file(char *filename, double *ddata, int len, int srate, int channels)
{
  mus_float_array_to_file(filename, ddata, len, srate, channels);
}

int clm_audio_mixer_read(int dev, int field, int chan, double *val, int len)
{
  float *vals;
  int i, res;
  vals = (float *)malloc(len * sizeof(float));
  for (i = 0; i < len; i++) vals[i] = val[i];
  res = mus_audio_mixer_read(dev, field, chan, vals);
  for (i = 0; i < len; i++) val[i] = vals[i];
  free(vals);
  return(res);
}

int clm_audio_mixer_write(int dev, int field, int chan, double *val, int len)
{
  float *vals;
  int i, res;
  vals = (float *)malloc(len * sizeof(float));
  for (i = 0; i < len; i++) vals[i] = val[i];
  res = mus_audio_mixer_write(dev, field, chan, vals);
  for (i = 0; i < len; i++) val[i] = vals[i];
  free(vals);
  return(res);
}

int clm_sound_maxamp(const char *ifile, int chans, double *vals, int *times)
{
  off_t *ts;
  mus_sample_t *xs;
  int i;
  int res = -1;
  ts = (off_t *)calloc(chans, sizeof(off_t));
  xs = (mus_sample_t *)calloc(chans, sizeof(mus_sample_t));
  res = mus_sound_maxamps(ifile, chans, xs, ts);
  for (i = 0; i < chans; i++) 
    {
      times[i] = (int)ts[i]; 
      vals[i] = (double)xs[i];
    }
  free(ts);
  free(xs);
  return(res);
}


/* this kludge is for Clisp */
static mus_sample_t *maxamp_xs = NULL;
static off_t *maxamp_ts = NULL;
static int maxamp_size = 0;

int clm_sound_maxamp_init(const char *ifile, int chans)
{
  if (maxamp_xs) free(maxamp_xs);
  if (maxamp_ts) free(maxamp_ts);
  maxamp_size = chans;
  maxamp_ts = (off_t *)calloc(chans, sizeof(off_t));
  maxamp_xs = (mus_sample_t *)calloc(chans, sizeof(mus_sample_t));
  return(mus_sound_maxamps(ifile, chans, maxamp_xs, maxamp_ts));
}

int clm_sound_maxamp_time(int chan)
{
  if ((maxamp_ts) &&
      (chan < maxamp_size))
    return(maxamp_ts[chan]);
  return(-1);
}

double clm_sound_maxamp_amp(int chan)
{
  if ((maxamp_xs) &&
      (chan < maxamp_size))
    return(maxamp_xs[chan]);
  return((double)(-1.0));
}


/* endian converters (clm1.lisp) */
void swap_int_array(int *arr, int size);
void swap_int_array(int *arr, int size)
{
  int i, lim;
  unsigned char *p;
  unsigned char temp;
  p = (unsigned char *)arr;
  lim = size * 4;
  for (i = 0; i < lim; i += 4)
    {
      temp = p[i + 0];
      p[i + 0] = p[i + 3]; 
      p[i + 3] = temp;
      temp = p[i + 1];
      p[i + 1] = p[i + 2]; 
      p[i + 2] = temp;
    }
}

void swap_double_array(double *arr, int size);
void swap_double_array(double *arr, int size)
{
  int i, lim;
  unsigned char *p;
  unsigned char temp;
  p = (unsigned char *)arr;
  lim = size * 8;
  for (i = 0; i < lim; i += 8)
    {
      temp = p[i + 0];
      p[i + 0] = p[i + 7]; 
      p[i + 7] = temp;
      temp = p[i + 1];
      p[i + 1] = p[i + 6]; 
      p[i + 6] = temp;
      temp = p[i + 2];
      p[i + 2] = p[i + 5]; 
      p[i + 5] = temp;
      temp = p[i + 3];
      p[i + 3] = p[i + 4];
      p[i + 4] = temp;
    }
}


/* ---------------- CLM.C support ---------------- */

#define INITIAL_GENBAG_SIZE 8

typedef struct {
  int size;
  int top;
  mus_any **gens;
} genbag;

void *clm_make_genbag(void)
{
  genbag *gb;
  gb = (genbag *)calloc(1, sizeof(genbag));
  gb->size = INITIAL_GENBAG_SIZE;
  gb->top = 0;
  gb->gens = (mus_any **)calloc(gb->size, sizeof(mus_any *));
  return((void *)gb);
}

mus_any *clm_add_gen_to_genbag(void *bag, mus_any *gen)
{
  genbag *gb = (genbag *)bag;
  if (gb->top == gb->size)
    {
      int i;
      gb->size += INITIAL_GENBAG_SIZE;
      gb->gens = (mus_any **)realloc((void *)(gb->gens), gb->size * sizeof(mus_any *));
      for (i = gb->top; i < gb->size; i++) gb->gens[i] = NULL;
    }
  gb->gens[gb->top++] = gen;
  return(gen);
}

void *clm_free_genbag(void *bag)
{
  int i;
  genbag *gb = (genbag *)bag;
  if (gb)
    {
      if (gb->top > 0)
	for (i = 0; i < gb->top; i++)
	  if (gb->gens[i])
	    mus_free(gb->gens[i]);
      FREE(gb);
    }
  return(NULL);
}

Float clm_as_needed_input(void *arg, int direction)
{
  /* assume readin as arg */
  mus_any *gen = (mus_any *)arg;
  if (direction != (int)mus_increment(gen))
    mus_set_increment(gen, (Float)direction);
  return(mus_readin(gen));
}

static mus_any *clm_output_stream = NULL, *clm_reverb_stream = NULL;

bool clm_make_output(const char *filename, int chans, int out_format, int out_type, const char *comment)
{
  if (clm_output_stream) return(false);
  clm_output_stream = mus_make_frame_to_file_with_comment(filename, chans, out_format, out_type, comment);
  return(true);
}

bool clm_make_reverb(const char *filename, int chans, int out_format, int out_type, const char *comment)
{
  if (clm_reverb_stream) return(false);
  clm_reverb_stream = mus_make_frame_to_file_with_comment(filename, chans, out_format, out_type, comment);
  return(true);
}

bool clm_continue_output(const char *filename)
{
  if (clm_output_stream) fprintf(stdout,"output active?");
  clm_output_stream = mus_continue_frame_to_file(filename);
  return(true);
}

bool clm_continue_reverb(const char *filename)
{
  if (clm_reverb_stream) fprintf(stdout,"reverb active?");
  clm_reverb_stream = mus_continue_frame_to_file(filename);
  return(true);
}

mus_any *clm_output(void)
{
  return(clm_output_stream);
}

mus_any *clm_reverb(void)
{
  return(clm_reverb_stream);
}

bool clm_close_output(void)
{
  if (!clm_output_stream) return(false); 
  mus_close_file(clm_output_stream); /* is this needed? */
  mus_free(clm_output_stream);
  clm_output_stream = NULL;
  return(true);
}

bool clm_close_reverb(void)
{
  if (!clm_reverb_stream) return(false); 
  mus_close_file(clm_reverb_stream); /* is this needed? */
  mus_free(clm_reverb_stream);
  clm_reverb_stream = NULL;
  return(true);
}

off_t clm_to_off_t(int *data, int loc)
{
  /* lisp can't always pass us 32-bit quantities, so try 24 + 24 + sign */
  int sign = 1;
  if (data[loc + 1] & 0x2000000) sign = -1;
  /* bizarre!! all these "off_t"'s are needed */
  return((off_t)(((off_t)data[loc] & 0xffffff) + (off_t)(((off_t)data[loc + 1] & 0xffffff) << 24)) * (off_t)sign);
}

/* specializations of some generic functions for the CL-CLM context */
static off_t clm_location(mus_any *ptr)
{
  mus_any *closure;
  if (ptr)
    {
      if (((ptr->core)->location) && (!mus_src_p(ptr))) /* avoid infinite loop if mus_channel(src) */
	return(((*(ptr->core)->location))(ptr));        /* not mus_channel here -- another way to loop */
      closure = (mus_any *)mus_environ(ptr);            /* safe in this context because we set it to either NULL or a readin gen */
      if ((closure) &&
	  (mus_readin_p(closure)))
	return(((*(closure->core)->location))(closure));
    }
  return(0);
}

static off_t clm_set_location(mus_any *ptr, off_t val)
{
  mus_any *closure;
  if (ptr)
    {
      if (((ptr->core)->set_location) && (!mus_src_p(ptr)))
	return((*((ptr->core)->set_location))(ptr, val));
      closure = (mus_any *)mus_environ(ptr);
      if ((closure) &&
	  (mus_readin_p(closure)))
	return((*((closure->core)->set_location))(closure, val));
    }
  return(0);
}

static int clm_channel(mus_any *ptr)
{
  mus_any *closure;
  if (ptr)
    {
      if (((ptr->core)->channel) && (!mus_src_p(ptr)))
	return(((*(ptr->core)->channel))(ptr));
      closure = (mus_any *)mus_environ(ptr);
      if ((closure) &&
	  (mus_readin_p(closure)))
	return(((*(closure->core)->channel))(closure));
    }
  return(0);
}

mus_any *clm_make_src(Float (*input)(void *arg, int direction), Float srate, int width, void *closure)
{
  mus_any *gen;
  gen = mus_make_src(input, srate, width, closure);
  if (gen)
    {
      gen->core->location = &clm_location;
      gen->core->set_location = &clm_set_location;
      gen->core->channel = &clm_channel;
    }
  return(gen);
}
