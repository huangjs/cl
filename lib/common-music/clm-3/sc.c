/* control Snd program through X properties */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

static Window compare_window(Display *display, Window window, Atom id)
{
  Atom type;
  int format;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  Window found = (Window)None;
  if (((XGetWindowProperty(display, window, id, 0L, (long)BUFSIZ, False,
			   XA_STRING, &type, &format, &nitems, &bytesafter,
			   (unsigned char **)version)) == Success) && 
      (type != None))
    {
      found = window;
      if (version[0]) XFree((char *)(version[0]));
    }
  return(found);
}

static Window find_window(Display *display, Window starting_window, Atom id, Window (*compare_func)())
{
  Window rootwindow, window_parent;
  int i = 0;
  unsigned int num_children = 0;
  Window *children = NULL;
  Window window = (compare_func)(display, starting_window, id);
  if (window != (Window)None)return (window);
  if ((XQueryTree(display, starting_window, &rootwindow, &window_parent, &children, &num_children)) == 0) return ((Window)None);
  while ((i < num_children) && (window == (Window)None))
    window = find_window(display, children[i++], id, compare_func);
  if (children) XFree((char *)children);
  return(window);
}

static Atom snd_v,snd_c, clm_v, clm_c;
static Display *dpy = NULL;
static Window wn;

int clm_start_snd(void);
int clm_start_snd(void)
{
  if (!(fork()))
    return(execl("/bin/sh", "/bin/sh", "-c", "snd -nostdin", NULL));
  return(0); /* make compiler happy */
}

#ifndef FALSE
  #define FALSE 0
#endif

void clm_init_x(char *version);
void clm_init_x(char *version)
{
  dpy = XOpenDisplay(NULL);
  wn = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy), 0, 0, 10, 10, 1, CopyFromParent, CopyFromParent);
  snd_v = XInternAtom(dpy, "SND_VERSION", FALSE);
  snd_c = XInternAtom(dpy, "SND_COMMAND", FALSE);
  clm_v = XInternAtom(dpy, "CLM_VERSION", FALSE);
  clm_c = XInternAtom(dpy, "CLM_COMMAND", FALSE);
  XChangeProperty(dpy, wn, clm_v, XA_STRING, 8, PropModeReplace, (unsigned char *)version, strlen(version) + 1);
  XFlush(dpy);
}

int clm_send_snd(char *command);
int clm_send_snd(char *command)
{
  Window window;
  if (dpy == NULL) clm_init_x("?");
  if ((window = find_window(dpy, DefaultRootWindow(dpy), snd_v, compare_window)))
    {
      XChangeProperty(dpy, window, snd_c, XA_STRING, 8, PropModeReplace, (unsigned char *)command, strlen(command) + 1);
      XFlush(dpy);
      return(0);
    }
  return(-1);
}

static char lisp_buf[4096];
char *clm_receive_snd(void);
char *clm_receive_snd(void)
{
  /* we're forced to poll for the goddamn thing (attempt to get property changed event in lisp => segfaults) */
  int format;
  Atom type;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  if (dpy == NULL) clm_init_x("?");
  if (((XGetWindowProperty(dpy, wn, clm_c, 0L, (long)BUFSIZ, False,
			   XA_STRING, &type, &format, &nitems, &bytesafter,
			   (unsigned char **)version)) == Success) && 
      (type != None))
    {
      if (version[0]) 
	{
	  /* fprintf(stdout,"got %s ",version[0]); fflush(stdout); */
	  strcpy(lisp_buf, (char *)(version[0]));
	  XFree(version[0]);
	  XChangeProperty(dpy, wn, clm_c, XA_STRING, 8, PropModeReplace, (unsigned char *)"", 1);
	  return(lisp_buf);
	}
    }
  return("");
}

/* cc sc.c -g -o sc -L/usr/X11R6/lib -lX11 */
