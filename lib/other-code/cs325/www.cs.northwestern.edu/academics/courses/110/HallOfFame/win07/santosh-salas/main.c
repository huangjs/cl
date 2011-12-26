/* EECS 110 Game Graphics Design Project
   The goal of this game is to stop the speeding cars. Right-click with your mouse to draw a stop sign. 
   When cars overlap with or collide into the stop sign, the cars stop.
   Stop all the cars to win the game!
   
   Written by Parvathi Santhosh-Kumar and Rodrigo Salas
   Last Updated: March 07, 2007
*/

#include "introGlutLib.h"		//include the basic drawing library
#include <time.h>				// for MS Windows time-finding functions
#include <stdlib.h>				// for malloc and free
#include <string.h>				// for strcpy and other string fcns.

#define GREY 0x1C1C1C
#define BLACK 0x000000
#define WHITE 0xFFFFFF
#define STREET_COLOR 0x7A7A7A
#define BLUE 0x00B2EE
#define YELLOW 0xFFFF00
#define RED 0xFF0000
#define GREEN 0x00611C

#define MIN_CARS 5
#define MAX_CARS 7
#define MIN_CLOUDS 3
#define MAX_CLOUDS 8
#define MIN_TRUCKS 2
#define MAX_TRUCKS 4

#define MIN_VEHICLES 5
#define MAX_VEHICLES 10

#define GO_RIGHT 1
#define GO_LEFT -1

#define STREET_HEIGHT 0.7

#define STOP_SIGN_RADIUS 20

typedef enum {CARS_TYPE, TRUCKS_TYPE} VEHICLE_TYPE;

typedef struct {
    VEHICLE_TYPE type;    
    double x_pos, y_pos, width, speed;
    unsigned long color;
    int dir;
} VEHICLE;

int num_vehicles = 0;
VEHICLE vehicles[MAX_VEHICLES];

void InitializeCars(void);
void InitializeCar(int index);
void drawRandomVehicles(void);
void MoveVehicles(void);
void MoveCars(VEHICLE *car);
void drawCar(double x_start, double y_start, double width, unsigned long color, int direction);

void drawBackground(void);
void drawSky(void);
void drawSun(void);

typedef struct {
    double x_pos_cloud, y_pos_cloud, radius_cloud, speed_cloud;
    int dir_cloud;
} CLOUDS; 

int num_clouds = 0;
CLOUDS clouds[MAX_CLOUDS];

void InitializeClouds();
void InitializeCloud(int index);
void drawRandomClouds(void);
void MoveClouds(void);
void drawCloud(double x_start, double y_start, double radius, int direction);

typedef struct {
    int on, x, y; // on = 1: draw stop sign, on = 0: don't draw stop sign.
} STOP_SIGN;

STOP_SIGN stop_sign = {0,0 - STOP_SIGN_RADIUS, 0 - STOP_SIGN_RADIUS} ;

void drawStopSign(double x_start, double y_start, double radius);
void TheEnd();

unsigned long PickRandomRGB(void);
int PickRandomInt(int low, int high);

void InitializeTrucks();
void InitializeTruck(int index);
void MoveTrucks(VEHICLE *car);
void drawTruck(double x_start, double y_start, double width, unsigned long color, int direction);

                   
// ========================== Callback Functions ============================
// The following functions must be defined for this file to compile.
// They are called by the GLUT main loop started by InitGraphics().
//
//   myDisplay(): will be called repeatedly to update the display.
//   myMouse(): will be called when the mouse is clicked.
//   myKeyboard(): will be called when a key is pressed.
//   mySpecialKey(): will be called when a special key, e.g., a cursor key, is pressed.
//
// Do NOT call these functions yourself. 

// =================================Main:=======================================				
int main()
{
	srand(time(NULL));
	InitializeClouds();
    InitializeCars();
    InitializeTrucks();
    InitGraphics();			// start GLUT/OpenGL
	return 0;
}

/***************************************************************
 myDisplay()
 
 Selects a grey background color and draws race cars of various random colors and a background of clouds and the sun.
***************************************************************/
void myDisplay(void)
{
       SetBackgndColorHex(STREET_COLOR);
       ClearWindow();
       drawBackground();
       drawRandomVehicles();
       MoveVehicles();
       
       STOP_SIGN *stopsign = &stop_sign;
       if (stopsign->on == 1)
       {
            drawStopSign(stopsign->x, stopsign->y, STOP_SIGN_RADIUS);
       }
       TheEnd();
}

/****************************************************
Below are the functions called to draw cars:
      InitalizeCars() sets up the initial random data to draw cars.
      InitializeCar(index) creates one car.
      drawRandomObjects() draws all objects.
      MoveCars() moves cars from left to right. When cars hit the side of the screen, they turn around.
      drawCar() draws all the part of the cars. 
*****************************************************/

void InitializeCars() {
     num_vehicles = PickRandomInt(MIN_CARS, MAX_CARS);
     
     for (int i = 0; i < num_vehicles; ++i) {
         InitializeCar(i);
     }
} 

void InitializeTrucks() {
     num_vehicles = PickRandomInt(MIN_TRUCKS, MAX_TRUCKS);
     
     for (int i = 0; i < num_vehicles; ++i) {
         InitializeTruck(i);
         }
}

void InitializeCar(int index) {
     int sz = PickRandomInt(50, 300);
     VEHICLE *car = &vehicles[index];
     car->type = CARS_TYPE;
     car->width = sz;
     car->x_pos = PickRandomInt(sz, NU_SCREENWIDTH - sz);
     car->y_pos = PickRandomInt(0, 300); //sz, NU_SCREENHEIGHT*STREET_HEIGHT -  sz*0.274); // w*8/29 is the height of one car  
     car->speed = 10 - sz*10/300;
     car->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
     car->color = PickRandomRGB();
}

void InitializeTruck(int index) {
     int sz = PickRandomInt(70, 200);
     VEHICLE *truck = &vehicles[index];
     truck->type = TRUCKS_TYPE;
     truck->width = sz;
     truck->x_pos = PickRandomInt(sz, NU_SCREENWIDTH - sz);
     truck->y_pos = PickRandomInt(0, 200);
     truck->speed = 2-0.01*sz;
     truck->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
     truck->color = PickRandomRGB();
}

void drawRandomVehicles()
{
     num_vehicles = 10;
     for (int i = 0; i < num_vehicles; ++i) {
         VEHICLE *vehicle = &vehicles[i];
         if (vehicle->type == CARS_TYPE)
         {
              drawCar(vehicle->x_pos, vehicle->y_pos, vehicle->width, vehicle->color, vehicle->dir);
         }
         else if (vehicle->type == TRUCKS_TYPE)
         {
              drawTruck(vehicle->x_pos, vehicle->y_pos, vehicle->width, vehicle->color, vehicle->dir);
         }     
     }
}

void MoveVehicles() 
{
     for (int i = 0; i < num_vehicles; ++i) {
         VEHICLE *vehicle = &vehicles[i];
         STOP_SIGN *stopsign = &stop_sign;
         switch (vehicle->type)
         {
            case 0: MoveCars(vehicle); break;
            case 1: MoveTrucks(vehicle); break;
         }
     }
}

void MoveCars(VEHICLE *car) 
{
    STOP_SIGN *stopsign = &stop_sign;
    /***************************************************
    The following if statement stops cars if they are in the same space as a stop sign that is drawn when the mouse is clicked.
    ****************************************************/
    if ((   ( (car->dir == 1 ) && (stopsign->x > car->x_pos)              && (stopsign->x  < car->x_pos + car->width) )   // checks if they hit in the x axis for dir = 1
       ||   ( (car->dir == -1) && (stopsign->x > car->x_pos - car->width) && (stopsign->x  < car->x_pos             ) ) )  // checks if they hit in the x axis for dir = -1
       &&   ( (stopsign->y  > car->y_pos)                                 && (stopsign->y  < car->y_pos + car->width * 8/29) ) ) // cheks if they hit in the y axis any direction
     {
          car->speed = 0;
     }
     else
     {
         if (car->x_pos < 0 || car->x_pos > NU_SCREENWIDTH)
         {
              car->dir *= -1;
         } 
         car->x_pos += car->speed * car->dir;
     }
}

void MoveTrucks(VEHICLE *truck)
{
    STOP_SIGN *stopsign = &stop_sign;
    if ((   ( (truck->dir == 1 ) && (stopsign->x > truck->x_pos)              && (stopsign->x  < truck->x_pos + truck->width*1.5) )   // checks if they hit in the x axis for dir = 1
       ||   ( (truck->dir == -1) && (stopsign->x > truck->x_pos - truck->width*1.5) && (stopsign->x  < truck->x_pos             ) ) )  // checks if they hit in the x axis for dir = -1
       &&   ( (stopsign->y  > truck->y_pos)                                 && (stopsign->y  < truck->y_pos + truck->width * 1/2) ) ) // cheks if they hit in the y axis any direction
     {
          truck->speed = 0;
     }
     else
     {
         if (truck->x_pos < 0 || truck->x_pos > NU_SCREENWIDTH)
         {
              truck->dir *= -1;
         } 
         truck->x_pos += truck->speed * truck->dir;
     }
}

void drawTruck(double x_start, double y_start, double width, unsigned long color, int direction)
{
     double height = width * 1/2;
     
     /*BODY*/
     SetPenColorHex(color);
     DrawFillBox(x_start + direction,
                 y_start + 0.25*height,
                 x_start + direction*width,
                 y_start + height);
                 
     /*WHEELS*/
     SetPenColorHex(BLACK);
     DrawFillCircle(x_start + direction*0.2*width,
                   y_start + 0.25*height,
                   0.1*width);
     
    SetPenColorHex(WHITE);
    DrawFillCircle(x_start + direction*0.2*width,
                   y_start + 0.25*height,
                   0.06*width);
    
    SetPenColorHex(BLACK);
    DrawFillCircle(x_start + direction*0.8*width,
                   y_start + 0.25*height,
                   0.1*width);
    
    SetPenColorHex(WHITE);
    DrawFillCircle(x_start + direction*0.8*width,
                   y_start + 0.25*height,
                   0.06*width);
    
    
    /*HEAD OF SEMI-TRUCK*/
    SetPenColorHex(color);
    DrawFillBox(x_start + direction*width,
                y_start + 0.25*height,
                x_start + direction*1.05*width,
                y_start + 0.5*height);
    
    SetPenColorHex(color);
    DrawFillBox(x_start + direction*1.05*width,
                y_start + 0.25*height,
                x_start + direction*1.2*width,
                y_start + height);
                
    DrawPieArc(x_start + direction*1.25*width,
               y_start + 0.25*height,
               width*0.2,
               0,
               180);
    
    SetPenColorHex(BLACK);
    DrawFillBox(x_start + direction*1.1*width,
                y_start + 0.8*height,
                x_start + direction*1.2*width,
                y_start + 0.9*height);  

    SetPenColorHex(BLACK);
    DrawFillCircle(x_start + direction*1.2*width,
                   y_start + 0.25*height,
                   0.1*width);
    
    SetPenColorHex(WHITE);
    DrawFillCircle(x_start + direction*1.2*width,
                   y_start + 0.25*height,
                   0.06*width);
}

void drawCar(double x_start, double y_start, double width, unsigned long color, int direction)    
 { 
     double height = width * 8/29;
	 
	/*The Racecar Driver*/
    SetPenColorHex(GREY);
    DrawFillCircle(x_start + direction * width * 13.5/29,
                   y_start + height * 5.5/8,
                   width * 1.5/29);
     
    /*The Body of the Car*/
    SetPenColorHex(color);  
	DrawFillBox(x_start + direction * width * 6/29,
	            y_start + height * 1/8,
	            x_start + direction * width * 18/29,
	            y_start + height * 5/8);
	            
    /*The Head of the Car*/
    DrawFillTriangle(x_start + direction * width * 18/29,
                     y_start + height * 1/8,
                     x_start + direction * width * 18/29,
                     y_start + height * 6/8,
                     x_start + direction * width,
                     y_start + height * 1/8);
    
    /*The Arc of the Car*/
    DrawPieArc (x_start + direction * width * 11/29,
             y_start + height * 5/8,
             width * 3/29,
             45 * (1+direction),
             90 * (2+direction));
             
    /*The RearBox*/
    DrawFillBox(x_start,
                y_start + height * 5/8,
                x_start + direction * width * 3/29,
                y_start + height * 7/8);
                
    /*The Rear Wheel*/
    SetPenColorHex(BLACK);
    DrawFillCircle(x_start + direction * width * 4/29,
                   y_start + height * 3/8,
                   width * 3/29);
   
   /*The Rear Wheel Interior*/
   SetPenColorHex(WHITE);
    DrawFillCircle(x_start + direction * width * 4/29,
                   y_start + height * 3/8,
                   width * 2/29);
    
    /*The Front Wheel*/
    SetPenColorHex(BLACK);
    DrawFillCircle(x_start + direction * width * 23/29,
                   y_start + height * 2/8,
                   width * 2/29);
    
    /*The Front Wheel Interior*/
    SetPenColorHex(WHITE);
     DrawFillCircle(x_start + direction * width * 23/29,
                   y_start + height * 2/8,
                   width * 1/29);
    
    /*Skid Marks*/
    SetPenColorHex(BLACK);
    SetDottedLines();
    DrawLine(x_start + direction * 0.1*width, y_start, x_start - direction * 0.12*width, y_start);
    DrawLine(x_start + direction * 0.8*width, y_start, x_start + direction * 0.6*width, y_start);
    SetSolidLines(); 
}

/*********************************************************
The following functions draw the background scene.
drawSky() draws a sky blue box to represent the sky.
drawSun() draws a circle to represent the sun.
InitializeClouds() and InitializeCloud() create the initial data
drawRandomClouds() draws a set of random clouds.
MoveClouds() moves clouds from left to right.
drawCloud() draws one cloud.
*********************************************************/

void drawBackground(void) {
     drawSky();
     drawSun();
     drawRandomClouds();
     MoveClouds();
}

void drawSky(void) {
     SetPenColorHex(BLUE);
     DrawFillBox(0,
                 STREET_HEIGHT*NU_SCREENHEIGHT,
                 NU_SCREENWIDTH,
                 NU_SCREENHEIGHT);
}

void drawSun(void) {
     SetPenColorHex(YELLOW);
     DrawFillCircle(0.9*NU_SCREENWIDTH,
                    0.9*NU_SCREENHEIGHT,
                    0.05*NU_SCREENWIDTH);
}

void InitializeClouds() {
     num_clouds = PickRandomInt(MIN_CLOUDS, MAX_CLOUDS);
     for (int i = 0; i < num_clouds; ++i) {
         InitializeCloud(i);
     }
} 

void InitializeCloud(int index) {
     int size = PickRandomInt(1, 5);
     CLOUDS *cloud = &clouds[index];
     cloud->radius_cloud = (double)size/100 * NU_SCREENWIDTH;
     cloud->x_pos_cloud = PickRandomInt(0, NU_SCREENWIDTH);
     cloud->y_pos_cloud = PickRandomInt(cloud->radius_cloud + STREET_HEIGHT * NU_SCREENHEIGHT, NU_SCREENHEIGHT - cloud->radius_cloud);  
     cloud->speed_cloud = 1 - size*15/400;
     cloud->dir_cloud = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
}
   
void drawRandomClouds() {
     for (int i = 0; i < num_clouds; ++i) {
         CLOUDS *cloud = &clouds[i];
         drawCloud(cloud->x_pos_cloud, cloud->y_pos_cloud, cloud->radius_cloud, cloud->dir_cloud);
     }
}

void MoveClouds() 
{
    for (int i = 0; i < num_clouds; ++i) {
        CLOUDS *cloud = &clouds[i];
        if (cloud->x_pos_cloud < 0 || cloud->x_pos_cloud > NU_SCREENWIDTH)
        {
            cloud->dir_cloud *= -1;
        }
        cloud->x_pos_cloud += cloud->speed_cloud * cloud->dir_cloud;
    }
}

void drawCloud(double x_start, double y_start, double radius, int direction) {
     SetPenColorHex(WHITE);
     DrawFillCircle(x_start + direction, y_start, radius);
     DrawFillCircle(x_start + direction + radius, y_start, radius*0.75);
     DrawFillCircle(x_start - direction - radius , y_start, radius*0.75);
}

/********************************************************************
drawStopsign() draws a stop sign when the mouse is clicked.
TheEnd() shows a green box when all the cars are stopped and the game is over.
********************************************************************/

void drawStopSign(double x_start, double y_start, double radius)
{
     SetPenColorHex(RED);
     DrawFillCircle(x_start, y_start, radius);
     SetPenColorHex(WHITE);
     DrawText2D(helv12, x_start - 0.8*radius, y_start - 0.2*radius, "STOP");
}

void TheEnd()
{
    int num_stopped = 0;
    for (int i = 0; i < num_vehicles; ++i) {
        VEHICLE *car = &vehicles[i];
        if (car->speed == 0) {
            num_stopped++;
        }
    }

     if (num_stopped >= num_vehicles)
     {
         SetPenColorHex(GREEN);
         DrawFillBox(0.4*NU_SCREENWIDTH, 0.3*NU_SCREENHEIGHT, 0.75*NU_SCREENWIDTH, 0.5*NU_SCREENHEIGHT);
         SetPenColorHex(WHITE);
         DrawText2D(helv18, 0.4*NU_SCREENWIDTH, 0.45*NU_SCREENHEIGHT, "              Congratulations!\n\n You stopped the speeding cars!");
         DrawText2D(helv12, 0.4*NU_SCREENWIDTH, 0.32*NU_SCREENHEIGHT, "                  (Press any key to quit)");
     }
}

/**********************************************************************
 myMouse(button, state, x, y)

Draws a stop sign when the mouse is clicked.
***********************************************************************/			
void myMouse(int button, int state, int x, int y) 
{
	if ((button == GLUT_LEFT_BUTTON) && (state == GLUT_UP)) 
	{
        STOP_SIGN *stopsign = &stop_sign;
        stopsign->on = 1;
        stopsign->x = x;
        stopsign->y = y;
    }      
}

/**********************************************************************
 myKeyboard(key, x, y)
 
 Exit the program if a normal key is pressed
***********************************************************************/
void myKeyboard(unsigned char key, int x, int y)
{
	exit(0);
}

/**********************************************************************
 mySpecialKey(key, x, y)
 
 Exit the program if a special key is pressed
***********************************************************************/
void mySpecialKey(int key, int x, int y)
{
	exit(0);
}

/**********************************************************************
PickRandomRGB(void) outputs a random color.
PickRandomInt(int low, int high) outputs a random integer.
**********************************************************************/
unsigned long PickRandomRGB(void)
{
    int red = rand() % 255;
    int green = rand() % 255;
    int blue = rand() % 255;
    return red * 256 * 256 + green * 256 + blue;
} 

int PickRandomInt(int low, int high) 
{
    return low + rand() % (high - low + 1);
}
