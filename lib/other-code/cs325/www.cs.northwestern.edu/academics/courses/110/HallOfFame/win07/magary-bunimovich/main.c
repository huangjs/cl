/* This program is a game much like Frogger. The player is in charge of getting 
Smiley and his kid across the Autobahn. He can use the up and down keys to 
accelerate Smiley. But beware: his kid may lag behind. The left and right arrow keys 
are used to avoid cars. To increase the challenge, the user can press the 'a' key 
to accelerate the vehicles, or press the 'd' key to slow them down. 
Creators: Karin Magary & Ilya Bunimovich
Date: 03/09/07
-- Main.c includes commonly needed header files.
-- It defines mouse and keyboard functions to control Smiley.
-- It defines myDisplay() to draw cars and tanks. 
*/

#include "introGlutLib.h"             //include the basic drawing library
#include <time.h>                    // for MS Windows time-finding functions
#include <stdlib.h>                 // for rand() and srand()
#define helv18 GLUT_BITMAP_HELVETICA_18 //for font on tank

/* Cars can only go left or right. 1 and -1 are used 
so that new_x = old_x + smiley_dir * speed 
*/
#define GO_RIGHT 1
#define GO_LEFT -1

#define MAX_CARS 6
#define MIN_CARS 3
#define MIN_TANKS 3
#define MAX_TANKS 5
#define MAX_OBJECTS 13


typedef struct
{
     int type;
     double x_pos, y_pos, size, speed;
     unsigned long color;
     int dir;
} OBJECT;

/* Global object data */

int num_objects = 0;
int smiley_dir = 0; 

OBJECT objects[MAX_OBJECTS];

const unsigned long BLACK = 0x000000;
const unsigned long WHITE = 0xFFFFFF;
const unsigned long RED = 0xFF0000;
const unsigned long TIRE_COLOR = 0x774377;
const unsigned long TANK_COLOR = 0x666600;
const unsigned long EXHAUST = 0x333333; //exhaust color
const unsigned long ANTENNA = 0xBBBBBB; //antenna color
const unsigned long BACKGROUND = 0x000000;
const unsigned long YELLOW = 0xFFFF00;

/* InitializeCars() creates the initial random car data
    InitializeCar(index) creates one car 
    DrawObjects() draws all the objects
    MoveObjects() moves all the objects
*/

int InitializeCars(int start_index);
void InitializeCar(int index);
int InitializeTanks(int start_index);
void InitializeTank(int index);
int InitializeSmileys(int start_index);
void InitializeSmiley(int index);
void InitializeKids(int start_index);
void InitializeKid(int index);

void DrawObjects(void);
void DrawCar(double car_x, double car_y, double size, long int color);
void DrawTank(double tank_x, double tank_y, double t);
void DrawSmiley(double smiley_x, double smiley_y);
void DrawKid(double eyes_x, double eyes_y);

void MoveObjects(void);
void MoveCar(OBJECT *car);
void MoveTank(OBJECT *tank);
void MoveSmiley(OBJECT *smiley);
void MoveKid(OBJECT *eye);

/*Drawing Tools*/
void SetDottedLines(void);
void SetSolidLines(void);

/*DrawText2D inserts a string of text*/
void DrawText2D(void * font, double x0, double y0, const char* pString);

/* returns random integer between low and high */
int PickRandomInt(int low, int high);

/* pickRandomRGB() generates random hexadecimal color value */
unsigned long pickRandomRGB(void);

//=================================Main:=======================================                       
int main()
{
    srand(time(NULL));
    int num_cars = InitializeCars(0);
    int num_tanks = InitializeTanks(num_cars);
    InitializeSmileys(num_tanks + num_cars);
    InitializeKids(num_tanks + num_cars + 1);
    num_objects = num_tanks + num_cars + 2;
    InitGraphics(); // start GLUT/OpenGL
    return 0;
}

/***************************************************************
 myDisplay()
 
Clear the screen and draw the moving vehicles. 
***************************************************************/
void myDisplay(void){
     SetBackgndColorHex(BACKGROUND);
     ClearWindow();
     DrawObjects();
     MoveObjects();
}

/***************************************************************
   Initialize all vehicles
***************************************************************/

int InitializeCars(int start_index) {
     int num_cars = PickRandomInt(MIN_CARS, MAX_CARS);
     for (int i = start_index; i < num_cars; ++i) {
         InitializeCar(i);
     }
     return num_cars;
}

int InitializeTanks(int start_index) {
     int num_tanks = PickRandomInt(MIN_TANKS, MAX_TANKS);
     for (int i = start_index; i < start_index + num_tanks; ++i) {
         InitializeTank(i);
     }
     return num_tanks;
}

int InitializeSmileys(int start_index) {
     InitializeSmiley(start_index);
}

void InitializeKids(int start_index) {
     InitializeKid(start_index);
}

void InitializeCar(int index) {
     int sz = PickRandomInt(50, 70);
     OBJECT *car = &objects[index];
     car->type = 0;
     car->size = sz;
     car->x_pos = PickRandomInt(sz, NU_SCREENWIDTH - sz);
     car->y_pos = PickRandomInt(sz, NU_SCREENHEIGHT - sz);
     car->speed = 5.7 - 0.055 * sz;
     car->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
     car->color = pickRandomRGB();
}

void InitializeTank(int index) {
        int sz = 25;
        OBJECT *tank = &objects[index];
        tank->type = 1;
        tank->size = sz;
        tank->x_pos = PickRandomInt(sz, NU_SCREENWIDTH - sz);
        tank->y_pos = PickRandomInt(sz, NU_SCREENHEIGHT - sz);
        tank->speed = .15;
        tank->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
}

void InitializeSmiley(int index) {
        OBJECT *smiley = &objects[index];
        smiley->type = 2;
        smiley->x_pos = NU_SCREENWIDTH/2;
        smiley->y_pos = 5;
        smiley->speed = .5;
}

void InitializeKid(int index) {
        OBJECT *eye = &objects[index];
        eye->type = 3;
        eye->x_pos = 445;
        eye->y_pos = 5;
        eye->speed = .5;  
        eye->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT; 
} 
     
/***************************************************************
   Draw all objects
***************************************************************/
void DrawObjects() {
     for (int i = 0; i < num_objects; ++i){
         OBJECT *object = &objects[i];
         if (object->type == 0)
             {
             DrawCar(object->x_pos, object->y_pos, object->size, object->color);
             }
         else if (object->type == 1)
             {   
             DrawTank(object->x_pos, object->y_pos, object->size);
             }
         else if (object->type ==2)
             {
              DrawSmiley(object->x_pos, object->y_pos);
             } 
         else if (object->type ==3)
             {
              DrawKid(object->x_pos, object->y_pos);
             }     
     }
}
/***************************************************************
   Move objects
***************************************************************/

void MoveObjects() {
     for (int i = 0; i < num_objects; ++i){
         OBJECT *object = &objects[i];
         switch (object->type)
         {
             case 0: MoveCar(object); break;
             case 1: MoveTank(object); break;
             case 2: MoveSmiley(object); break;
             case 3: MoveKid(object); break;
         }
     }
}

void MoveCar(OBJECT *car) {
         if (car->x_pos < 0 || car->x_pos > NU_SCREENWIDTH)
         {
             car->dir *= -1;
         }
         car->x_pos += car->speed * car->dir;
}

void MoveTank(OBJECT *tank) {
         if (tank->x_pos < 0 || tank->x_pos > NU_SCREENWIDTH)
         {
             tank->dir *= -1;
         }
         tank->x_pos += tank->speed * tank->dir;
}
    
void MoveSmiley(OBJECT *smiley) {
         if (smiley->y_pos < NU_SCREENHEIGHT)
         {
         smiley->y_pos += smiley->speed;
         smiley->x_pos += smiley->speed * smiley_dir;  
         }
}

void MoveKid(OBJECT *eye) {
         if (eye->x_pos < 395 || eye->x_pos > 405 )
         {
             eye->dir *= -1;
         }
         eye->y_pos += eye->speed;
         eye->x_pos += eye->speed * smiley_dir;
}
     
/***************************************************************
   Car drawing functions
***************************************************************/
void DrawCar(double car_x, double car_y, double size, long int color){
   double height = size/4;
// exhaust pipe
   SetPenColorHex(EXHAUST);
   DrawFillBox(car_x + size/4, car_y + size/10, car_x, car_y + size * 1/16);
// car main body
   SetPenColorHex(color);
   DrawFillBox(car_x + height, car_y, car_x + size * 5/4, car_y + height);   
// car top
   DrawFillBox(car_x + size/2, car_y + height, car_x + size, car_y + size/2);
// car back
   DrawFillTriangle(car_x, car_y, car_x + size/4, car_y, car_x + height, car_y + height);
// car front
   DrawFillTriangle(car_x + size * 3/2, car_y, car_x + size * 5/4, car_y, car_x + size * 5/4, car_y + height);
// back windshield
   DrawFillTriangle(car_x + size * 3/8, car_y + size/4, car_x + size/2, car_y + size/4, car_x + size/2, car_y + size/2);
// front windshield
   DrawFillTriangle(car_x + size, car_y + height, car_x + size * 9/8, car_y + height, car_x + size, car_y + size/2);
// draw fender
   DrawFillTriangle(car_x + size, car_y + height, car_x + size/2, car_y + size * 7/16, car_x - size/16, car_y + size/6);
// back wheel
   SetPenColorHex(TIRE_COLOR);
   DrawFillCircle(car_x + size * 3/8, car_y, size/8); 
// front wheel
   DrawFillCircle(car_x + size * 9/8, car_y, size/8); 
// window
   SetPenColorHex(WHITE);
   DrawFillBox(car_x + size/2, car_y + size * 5/16, car_x + size, car_y + size * 7/16);
}

/***************************************************************
   Tank drawing functions
***************************************************************/
void DrawTank (double tank_x, double tank_y, double t){
//body 1    
     SetPenColorHex(TANK_COLOR);
     DrawFillBox(tank_x, tank_y, tank_x + t * 4, tank_y + t * 3/4);
//body 2
     DrawFillBox(tank_x - t/2, tank_y + t * 3/4, tank_x + t * 3, tank_y + t * 5/4); 
//turret box
     DrawFillBox(tank_x + t * 3/4, tank_y + t * 3/4, tank_x + t * 11/4, tank_y + t * 2);
//gun box
     DrawFillBox(tank_x + t * 11/4, tank_y + t * 3/2, tank_x + t * 7/2, tank_y + t * 2);
//gun box support
     DrawFillTriangle(tank_x + t * 11/4, tank_y + t * 5/4, tank_x + t * 3, tank_y + t * 3/2, tank_x + t * 11/4, tank_y + t * 3/2);
//back point
     DrawFillTriangle(tank_x - t/2, tank_y + t * 3/4, tank_x - t/2, tank_y + t * 5/4, tank_x - t * 3/4, tank_y + t);
//back bumper
     DrawFillTriangle(tank_x, tank_y, tank_x, tank_y + t * 3/4, tank_x - t/2, tank_y + t * 3/4);
//back hood
     DrawFillBox(tank_x, tank_y + t * 3/2, tank_x + t * 3/4, tank_y + t * 7/4);
//back hatch
     DrawFillTriangle(tank_x, tank_y + t * 7/4, tank_x + t * 3/4, tank_y + t * 7/4, tank_x + t * 3/4, tank_y + t * 2);
//front point
     DrawFillBox(tank_x + t * 4, tank_y + t/2, tank_x + t * 9/2, tank_y + t * 3/4);
//front bumper
     DrawFillTriangle(tank_x + t * 4, tank_y, tank_x + t * 9/2, tank_y + t/2, tank_x + t * 4, tank_y + t/2);
//front hood
     DrawFillTriangle(tank_x + t * 3, tank_y + t * 3/4, tank_x + t * 4, tank_y + t * 3/4, tank_x + t * 3, tank_y + t * 5/4);
//gun barrel
     SetLineWidth(8);
     SetSolidLines();
     DrawLine(tank_x + t * 11/4, tank_y + t * 7/4, tank_x + t * 21/4, tank_y + t * 7/4);
//antennae
     SetPenColorHex(ANTENNA);
     SetLineWidth(4);
     SetSolidLines();
     DrawLine(tank_x + t * 5/4, tank_y + t * 2, tank_x + t * 5/4, tank_y + t * 7/2);
     DrawLine(tank_x + t * 3/2, tank_y + t * 2, tank_x + t * 3/2, tank_y + t * 3);
//wheels
     SetPenColorHex(WHITE); 
     DrawFillCircle(tank_x + t/2, tank_y - t/4, t/4);
     DrawFillCircle(tank_x + t, tank_y - t/4, t/4);
     DrawFillCircle(tank_x + t*3/2, tank_y - t/4, t/4);
     DrawFillCircle(tank_x + t*2, tank_y - t/4, t/4);
     DrawFillCircle(tank_x + t*11/4, tank_y - t/4, t/4);
     DrawFillCircle(tank_x + t*7/2, tank_y - t/4, t/4);
//treads
     SetLineWidth(3);
     SetDottedLines();
     DrawLine(tank_x, tank_y, tank_x + t/4, tank_y - t * 3/5);
     DrawLine(tank_x + t/4, tank_y - t * 3/5, tank_x + t * 15/4, tank_y - t * 3/5);
     DrawLine(tank_x + t * 15/4, tank_y - t * 3/5, tank_x + t * 4, tank_y);
//text
     SetPenColorHex(RED);
     DrawText2D(helv18, tank_x + t * 5/4, tank_y + t/2, "ARMY");
}

/***************************************************************
   Smiley drawing functions
***************************************************************/
void DrawSmiley(double smiley_x, double smiley_y)
{
     SetPenColorHex(YELLOW);
     DrawFillCircle(smiley_x, smiley_y, 20); //face
     SetPenColorHex(BLACK);
     DrawFillCircle(smiley_x - 7, smiley_y + 5, 4.7); //left eye socket
     DrawFillCircle(smiley_x + 7, smiley_y + 5, 4.7); //right eye socket
     SetPenColorHex(WHITE);
     DrawFillCircle(smiley_x - 7, smiley_y + 4, 1.5); //left eye ball
     DrawFillCircle (smiley_x + 6.5, smiley_y + 4, 1.5); //right eye ball
     SetSolidLines();
     SetLineWidth(4); // mouth
     DrawLine(smiley_x - 12, smiley_y - 8, smiley_x + 12, smiley_y - 8);
}

/***************************************************************
   Draw Kid
***************************************************************/

void DrawKid(double kid_x, double kid_y)    
{
     SetPenColorHex(YELLOW);
     DrawFillCircle(kid_x, kid_y, 10);
     SetPenColorHex(BLACK);
     DrawFillCircle(kid_x - 4, kid_y + 2, 2.7); 
     DrawFillCircle(kid_x + 4, kid_y + 2, 2.7); 
     SetPenColorHex(WHITE);
     DrawFillCircle(kid_x - 4, kid_y + 3, 1.5); 
     DrawFillCircle (kid_x + 4, kid_y + 3, 1.5); 
     SetSolidLines();
     SetLineWidth(2);
     DrawLine(kid_x - 6, kid_y - 6, kid_x + 6, kid_y - 6);
}
/*Utilities*/

/* returns random number between low and high */
int PickRandomInt(int low, int high){
     return low + rand() % (high - low + 1);
}

unsigned long pickRandomRGB(){
     int red = rand() % 255;
     int green = rand() % 255;
     int blue = rand() % 255;
     return red * 256 * 256 + green * 256 + blue;
}

/**********************************************************************
 myMouse(button, state, x, y)
 
***********************************************************************/
void myMouse(int button, int state, int x, int y) 
{
}

/**********************************************************************
 myKeyboard(key, x, y)
 
 Pressing the 'a' key accelerates the vehicles.
 Pressing the 'd' key decelerates the vehicles.
 'q' exits program.
***********************************************************************/
void myKeyboard(unsigned char key, int x, int y)
{
    if (key == 'a'){
       for (int i = 0; i < num_objects; ++i) {
           OBJECT *object = &objects[i];
           switch (object->type)
           {
              case 0: object->speed *= 1.5; break;
              case 1: object->speed *= 1.5; break;
           }
       }
    }
    else if (key == 'd'){
       for (int i = 0; i < num_objects; ++i) {
         OBJECT *object = &objects[i];
         switch (object->type)
         {
             case 0: object->speed /= 1.5; break;
             case 1: object->speed /= 1.5; break;
         }
         }
    }
    else if (key == 'q'){
         exit (0);
    }
}

/**********************************************************************
 mySpecialKey(key, x, y)
 
 Up and down arrow keys control the smileys' speed. 
 Left and right arrow keys control direction. 
***********************************************************************/

void mySpecialKey(int key, int x, int y)
{
     if(key == GLUT_KEY_LEFT)
            smiley_dir = -1;
     else if (key == GLUT_KEY_RIGHT)
            smiley_dir = 1; 
     if(key == GLUT_KEY_UP){
             for (int i = 0; i < num_objects; ++i) {
                 OBJECT *object = &objects[i];
                 if (object->speed < 1.04){
                    switch (object->type)
                    {
                        case 2: object->speed *= 1.5; break;
                        case 3: object->speed *= 1.18; break;
                    }
                 }
             }
     }
     if(key == GLUT_KEY_DOWN){
             for (int i = 0; i < num_objects; ++i) {
                 OBJECT *object = &objects[i];
                 if (object->speed > .5){
                    switch (object->type)
                    {
                        case 2: object->speed *= .5; break;
                        case 3: object->speed *= .5; break;
                    }
                 }
             }
     }
}
