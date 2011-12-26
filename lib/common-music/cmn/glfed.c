/* 
  glyph editor

Input is in the form of cmn-glyph.lisp draw function bodies,
assumed to be saved in test.data.  After editing (upon exit),
the new lisp expressions are saved in new.data along with
the glyph bounding box.  For example, place:

  (moveto score -0.096 0.062)
  (curveto score -0.130 0.104 -0.124 0.208 -0.048 0.214)
  (curveto score 0.060 0.204 0.154 0.040 0.258 0.010)
  (curveto score 0.384 -0.028 0.524 0.116 0.418 0.220)
  (curveto score 0.372 0.266 0.264 0.262 0.278 0.180)
  (curveto score 0.310 0.122 0.370 0.154 0.394 0.196)
  (curveto score 0.432 0.144 0.428 0.054 0.342 0.044)
  (curveto score 0.242 0.054 0.148 0.216 0.046 0.248)
  (curveto score -0.080 0.296 -0.220 0.142 -0.114 0.036)
  (curveto score -0.070 -0.008 0.040 -0.004 0.028 0.080)
  (curveto score -0.002 0.142 -0.066 0.102 -0.096 0.062)

in test.data, then (if glfed is not already built)
 
LINUX: cc glfed.c -o glfed -L/usr/X11R6/lib -lXm -lXp -lXt -lXext -lX11
SGI: cc glfed.c -o glfed -lXm -lXt -lX11 -lPW
 
run glfed, exit, and check out new.data.  To make the 
displayed form bigger, increase "size" below; similarly,
change "top" and "left" to move it around in the window.

*/

#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdarg.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <Xm/XmAll.h>


int mx,my;

typedef struct {
  int x,y;
} pt;

pt points[1024];

#if 0
typedef struct {
  int x,y,n;
} bpt;

bpt backup[4096];
int backup_loc = -1;
#endif

#define MOVETO 0
#define LINETO 1
#define CURVETO 2
#define RLINETO 3
#define RMOVETO 4

int curves[1024];

Display *dpy;
Drawable win;
GC gc,igc,rgc;
Pixel red;

int size = 800;
int top = 700;
int left = 300;

int curve_len = 0;
int total_points = 0;

int string_equal(char *s0, char *s1)
{
  int len = 0,i;
  if ((s1 == NULL) || (s0 == NULL)) return((s1 == NULL) && (s0 == NULL));
  len = strlen(s1);
  for (i=0;i<len;i++)
    {
      if (s0[i] != s1[i]) return(0);
    }
  return(1);
}

static int round(float x)
{
  int i;
  i=x;
  if ((x-i) > 0.5) return(i+1);
  return(i);
}

void read_file(char *name)
{
  int fd,i,curve,point;
  int bytes;
  float fx0,fy0,fx1,fy1,fx2,fy2;
  char *buf,*sp;
  fd = open(name,O_RDONLY,0);
  if (fd == -1) {fprintf(stderr,"can't open %s",name); exit(0);}
  buf = (char *)calloc(65536,sizeof(char));
  bytes = read(fd,buf,65536);
  close(fd);
  i=0;
  curve = 0;
  point = 0;
  while (i < bytes)
    {
      if (string_equal((char *)(buf+i),"moveto"))
	{
	  i+=13;
	  curves[curve++] = MOVETO;
	  sp = (char *)(buf+i);
	  sscanf(sp,"%f %f",&fx0,&fy0);
	  points[point].x = round(size * fx0);
	  points[point].y = round(size * fy0);
	  point++;
	}
      else
	{
	  if (string_equal((char *)(buf+i),"rmoveto"))
	    {
	      i+=14;
	      curves[curve++] = RMOVETO;
	      sp = (char *)(buf+i);
	      sscanf(sp,"%f %f",&fx0,&fy0);
	      points[point].x = round(size * fx0);
	      points[point].y = round(size * fy0);
	      point++;
	    }
	  else
	    {
	      if (string_equal((char *)(buf+i),"lineto"))
		{
		  i+=13;
		  curves[curve++] = LINETO;
		  sp = (char *)(buf+i);
		  sscanf(sp,"%f %f",&fx0,&fy0);
		  points[point].x = round(size * fx0);
		  points[point].y = round(size * fy0);
		  point++;
		}
	      else
		{
		  if (string_equal((char *)(buf+i),"rlineto"))
		    {
		      i+=14;
		      curves[curve++] = RLINETO;
		      sp = (char *)(buf+i);
		      sscanf(sp,"%f %f",&fx0,&fy0);
		      points[point].x = round(size * fx0);
		      points[point].y = round(size * fy0);
		      point++;
		    }
		  else
		    {
		      if (string_equal((char *)(buf+i),"curveto"))
			{
			  i+=14;
			  curves[curve++] = CURVETO;
			  sp = (char *)(buf+i);
			  sscanf(sp,"%f %f %f %f %f %f",&fx0,&fy0,&fx1,&fy1,&fx2,&fy2);
			  points[point].x = round(size * fx0);
			  points[point].y = round(size * fy0);
			  point++;
			  points[point].x = round(size * fx1);
			  points[point].y = round(size * fy1);
			  point++;
			  points[point].x = round(size * fx2);
			  points[point].y = round(size * fy2);
			  point++;
			}
		    }
		}
	    }
	}
      i++;
    }
  curve_len = curve;
  total_points = point;
  free(buf);
}

int minx,maxx,miny,maxy;

void write_file(char *name)
{
  int fd;
  int i,n;
  char *buf;
  fd=creat(name,0666);
  if (fd == -1) {fprintf(stderr,"can't write %s",name); exit(0);}
  n=0;
  buf = (char *)calloc(1024,sizeof(char));
  for (i=0;i<curve_len;i++)
    {
      switch(curves[i])
	{
	case MOVETO: 
	  sprintf(buf,"  (moveto score %.3f %.3f)\n",(float)(points[n].x)/(float)size,(float)(points[n].y)/(float)size);
	  n++;
	  break;
	case RMOVETO: 
	  sprintf(buf,"  (rmoveto score %.3f %.3f)\n",(float)(points[n].x)/(float)size,(float)(points[n].y)/(float)size);
	  n++;
	  break;
	case LINETO:
	  sprintf(buf,"  (lineto score %.3f %.3f)\n",(float)(points[n].x)/(float)size,(float)(points[n].y)/(float)size);
	  n++;
	  break;
	case RLINETO:
	  sprintf(buf,"  (rlineto score %.3f %.3f)\n",(float)(points[n].x)/(float)size,(float)(points[n].y)/(float)size);
	  n++;
	  break;
	case CURVETO:
	  sprintf(buf,"  (curveto score %.3f %.3f %.3f %.3f %.3f %.3f)\n",
		  (float)(points[n].x)/(float)size,(float)(points[n].y)/(float)size,
		  (float)(points[n+1].x)/(float)size,(float)(points[n+1].y)/(float)size,
		  (float)(points[n+2].x)/(float)size,(float)(points[n+2].y)/(float)size);
	  n+=3;
	  break;
	}
      write(fd,buf,strlen(buf));
    }
  sprintf(buf,";; (bounds %.3f %.3f %.3f %.3f)\n",
	  (float)minx/(float)size,
	  (float)miny/(float)size,
	  (float)maxx/(float)size,
	  (float)maxy/(float)size);
  write(fd,buf,strlen(buf));
  close(fd);
  free(buf);
}

void XDrawBezier(Display *dpy, Drawable win, GC gc, int x0, int y0, int x1, int y1, int x2, int y2, int x3, int y3, int n)
{
  int ax,ay,bx,by,cx,cy,i,x,y,nx,ny;
  float incr,val;
  cx = 3 * (x1 - x0);
  cy = 3 * (y1 - y0);
  bx = 3 * (x2 - x1) - cx;
  by = 3 * (y2 - y1) - cy;
  ax = x3 - (x0 + cx + bx);
  ay = y3 - (y0 + cy + by);
  incr = 1.0 / (float)n;
  val = 0.0;
  x = x0; y = y0;
  for (i=0;i<n;i++)
    {
      if (x>maxx) maxx=x; if (x<minx) minx=x; if (y>maxy) maxy=y; if (y<miny) miny=y;
      nx = (x0 + val * (cx + (val * (bx + (val * ax)))));
      ny = (y0 + val * (cy + (val * (by + (val * ay)))));
      XDrawLine(dpy,win,gc,left + x,top - y,left+nx,top-ny);
      x = nx;
      y = ny;
      val += incr;
    }
}

int square_size = 8;

void draw_ps_curve (int len, int erase, int clear)
{
  int i,n;
  GC ogc;
  char strbuf[16];
  int x,y,x0,y0;
  n=0; x=0; y=0;
  maxx=0; minx=0; maxy=0; miny=0;
  if (clear) XClearWindow(dpy,win);
  if (erase) ogc=igc; else ogc=gc;
  for (i=0;i<len;i++)
    {
      x0 = x; y0 = y;
      sprintf(strbuf,"%d",i);
      switch (curves[i])
	{
	case MOVETO: 
	  x = points[n].x; y = points[n].y; n++; 
	  XDrawRectangle(dpy,win,rgc,left+x-(square_size/2),(top - y)-(square_size/2),square_size,square_size);
	  break;
	case RMOVETO: 
	  x += points[n].x; y += points[n].y; n++; 
	  XDrawRectangle(dpy,win,rgc,left+x-(square_size/2),(top - y)-(square_size/2),square_size,square_size);
	  break;
	case LINETO: 
	  x = points[n].x; y = points[n].y; n++; 
	  XDrawLine(dpy,win,ogc,left+x0,top - y0,left+x,top - y); 
	  XDrawRectangle(dpy,win,rgc,left+x-(square_size/2),(top - y)-(square_size/2),square_size,square_size);
	  break;
	case RLINETO: 
	  x += points[n].x; y += points[n].y; n++; 
	  XDrawLine(dpy,win,ogc,left+x0,top - y0,left+x,top - y); 
	  XDrawRectangle(dpy,win,rgc,left+x-(square_size/2),(top - y)-(square_size/2),square_size,square_size);
	  break;
	case CURVETO: 
	  XDrawBezier(dpy,win,ogc,x0,y0,points[n].x,points[n].y,points[n+1].x,points[n+1].y,points[n+2].x,points[n+2].y,50);
	  x = points[n+2].x; y = points[n+2].y;
	  XDrawRectangle(dpy,win,ogc,left+points[n].x-(square_size/2),(top - points[n].y)-(square_size/2),square_size,square_size);
	  XDrawRectangle(dpy,win,ogc,left+points[n+1].x-(square_size/2),(top - points[n+1].y)-(square_size/2),square_size,square_size);
	  XDrawRectangle(dpy,win,rgc,left+points[n+2].x-(square_size/2),(top - points[n+2].y)-(square_size/2),square_size,square_size);
	  n+=3;
	  break;
	}
      XDrawString(dpy,win,gc,x+left,top-y,strbuf,strlen(strbuf));
      if (x>maxx) maxx=x; if (x<minx) minx=x; if (y>maxy) maxy=y; if (y<miny) miny=y;
    }
}

int current_point = 0;

static void graph_button_press(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  int i,x0,x1,y0,y1;
  XButtonEvent *ev = (XButtonEvent *)event;
  mx = ev->x;
  my = ev->y;
  current_point = -1;
  for (i=0;i<total_points;i++)
    {
      x0 = left+points[i].x - (square_size/2);
      x1 = left+points[i].x + (square_size/2);
      y0 = (top - points[i].y) - (square_size/2);
      y1 = (top - points[i].y) + (square_size/2);
      if ((mx >= x0) && (mx <= x1) && (my >= y0) && (my <= y1)) 
	{
	  current_point = i;
	  break;
	}
    }
}

static void graph_button_release(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int dx,dy;
  if ((current_point >= 0) && (current_point < total_points))
    {
      dx = (ev->x - mx);
      dy = (ev->y - my);
      points[current_point].x += dx;
      points[current_point].y -= dy;
      draw_ps_curve(curve_len,0,1);
    }
}

static void graph_button_motion(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  Time mouse_time;
  int time_interval;
  int dx,dy;
  XMotionEvent *ev = (XMotionEvent *)event;
  if ((current_point >= 0) && (current_point < total_points))
    {
      draw_ps_curve(curve_len,1,0);
      dx = (ev->x - mx);
      dy = (ev->y - my);
      points[current_point].x += dx;
      points[current_point].y -= dy;
      draw_ps_curve(curve_len,0,0);
      points[current_point].x -= dx;
      points[current_point].y += dy;
    }
}

static void resize(Widget w,XtPointer clientData,XtPointer callData)
{
  if (curve_len) draw_ps_curve(curve_len,0,1);
}

void save_results(Widget w,XtPointer clientData,XtPointer callData)
{
  write_file("new.data");
}

int main(int argc, char **argv)
{
  XtAppContext app;     
  Widget shell,pane;
  Arg args[32];
  XGCValues gv;
  int n;
  int scr;
  Atom wm_delete_window;
  Colormap cmap;
  XColor tmp_color,ignore;
  XFontStruct *nfont;

  shell = XtVaOpenApplication(&app,"mainshell",NULL,0,&argc,argv,NULL,applicationShellWidgetClass,
			      XmNallowShellResize,TRUE,
			      XmNwidth,500,XmNheight,500,
			      NULL);
  dpy=XtDisplay(shell);
  scr=DefaultScreen(dpy);
  cmap=DefaultColormap(dpy,scr);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNallowResize,TRUE); n++;
  XtSetArg(args[n],XmNbackground,WhitePixel(dpy,scr)); n++;
  XtSetArg(args[n],XmNforeground,BlackPixel(dpy,scr)); n++;
  pane = XtCreateManagedWidget("mainpane",xmDrawingAreaWidgetClass,shell,args,n);

  XtAddEventHandler(pane,ButtonPressMask,FALSE,graph_button_press,NULL);
  XtAddEventHandler(pane,ButtonMotionMask,FALSE,graph_button_motion,NULL);
  XtAddEventHandler(pane,ButtonReleaseMask,FALSE,graph_button_release,NULL);

  XtRealizeWidget(shell);

  XAllocNamedColor(dpy,cmap,"red",&tmp_color,&ignore);
  red = tmp_color.pixel;

  gv.function = GXcopy;
  XtVaGetValues(pane,XmNbackground, &gv.background,XmNforeground, &gv.foreground,NULL);
  gc = XtGetGC(pane, GCForeground | GCFunction, &gv);
  gv.function = GXcopy;
  XtVaGetValues(pane,XmNbackground, &gv.foreground,XmNforeground, &gv.background,NULL);
  igc = XtGetGC(pane, GCBackground | GCForeground | GCFunction, &gv);
  win = XtWindow(pane);
  gv.foreground = red;
  rgc = XtGetGC(pane, GCBackground | GCForeground | GCFunction, &gv);

  XtAddCallback(pane,XmNresizeCallback,resize,NULL);
  XtAddCallback(pane,XmNexposeCallback,resize,NULL);
  
  nfont = XLoadQueryFont(dpy,"-*-courier-medium-r-normal-*-14-*-*-*-*-*-iso8859-1");
  XSetFont(dpy,gc,nfont->fid);

  wm_delete_window = XmInternAtom(dpy,"WM_DELETE_WINDOW",FALSE);
  XmAddWMProtocolCallback(shell,wm_delete_window,save_results,NULL);

  read_file("test.data");
  XtAppMainLoop(app);
}
