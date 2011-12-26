/***********************************************************************************
Virginia Palmer and Megan Calhoun
3/09/2007
Ships Graphic
This program draws a random number of ships in a random location with randomly
colored sails. The ships sail from one side of the screen to the other, with smaller
ships sailing faster than larger ones. When the left mouse is clicked, a shark fin 
appears. The arrow keys can be used to move the shark to try to sink the ships.
***********************************************************************************/


#include "introGlutLib.h" 
#include <time.h>    
#include <stdlib.h>  
#include <string.h> 

/* Ships can only go left or right. 1 and -1 are used so
  that new_x = old_x + direction * speed */
 
#define GO_RIGHT  1
#define GO_LEFT   -1

#define MinShips 2
#define MaxShips 4
#define MaxObjects 5

#define oceancolor 0x00B2EE
#define wakecolor 0xC1F0F6
#define skycolor 0x87CEEB
#define suncolor 0xFFC125
#define cloudcolor 0xF0F8FF
#define fincolor 0x696969

/*Global ship data*/

typedef struct
{
    double x_pos;
    double y_pos;
    double size; 
    int dir;
    int type;
    unsigned long sailcolor;
    unsigned long woodcolor;
    double speed;
} OBJECT;

int NumObjects = 0;

OBJECT objects[MaxObjects];

/*Function Prototypes*/

/* InitializeShips() creates the initial random ship data
    InitializeShip(index) creates one ship
    DrawShips() draws all the ship*/
void InitializeShips (void);
void InitializeShip (int index);
void CreateFin(int x, int y);
void DrawObjects(void);
void MoveObjects();
void MoveShips(OBJECT *ship);
void MoveFin(OBJECT *fin);

/*Function to draw the picture*/
void DrawSky(void);
void DrawShips(int x_pos, int y_pos, int size, int direction, int speed);
void DrawShip (double ShipX, double ShipY, double ShipSize, int direction, unsigned long sailcolor, unsigned long woodcolor);
void DrawFillSail(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3);
void DrawFin(double x, double y, double size);

/*Returns a random hexadecimal color*/
unsigned long PickRandomRGB(void);

/*Returns a random brown hexadecimal color*/
unsigned long PickRandomBrown(void);

/* Returns a random integer between low and high */
int PickRandomInt(int low, int high);

// =================================Main:=======================================     ;
int main()
{
    srand(time(NULL));
    InitializeShips();
    InitGraphics();   // start GLUT/OpenGL
    return 0;
}

/**********************************************************************
 myMouse(button, state, x, y)
 
 Exit the program if the user clicks the mouse.
***********************************************************************/

void myMouse(int button, int state, int x, int y)
{
     static int firstTime = 0;
     if (firstTime == 0)
     {        
         if (button == GLUT_LEFT_BUTTON && state == GLUT_UP)
         {
               CreateFin(x, y);
               firstTime = 1;
         }
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

    int stepX = 0;
    int stepY = 0;
    if (key == GLUT_KEY_LEFT)
       {stepX= -5;}
    else if (key == GLUT_KEY_RIGHT)
       {stepX = 5;}
    else if (key == GLUT_KEY_UP)
       {stepY = 5;}
    else if (key == GLUT_KEY_DOWN)
       {stepY = -5;}
    int i = 0;
    for (int i = 0; i <= NumObjects; ++i) 
    {
         OBJECT *object = &objects[i];
         if (object->type == 1)
         {
             object->x_pos = object->x_pos + stepX;
             object->y_pos = object->y_pos + stepY;
         }
     }
    
}

/***************************************************************
 myDisplay()
 
 When called the first time, draws a random number of ships
 in a random location, with randomly colored sails.
  Does nothing when called subsequently.
***************************************************************/

void myDisplay()
{
    static int first_time = 1;
    if (first_time)
    {
        ClearWindow();
        first_time = 0;
    }
    else
    {
        SetBackgndColorHex(oceancolor);
        DrawSky();
        DrawObjects();
        MoveObjects();
    }
}

/***************************************************************
   Initialize all ships
***************************************************************/

void InitializeShips ()
{
     NumObjects = PickRandomInt(MinShips, MaxShips);
     int i = 0;
     for (i = 0; i < NumObjects; ++i)
     {
         InitializeShip(i);
     }
}

void InitializeShip(int index)
{
     int sz = PickRandomInt(25, 100);
     OBJECT *ship = &objects [index];
     ship->size = sz;
     ship->x_pos = PickRandomInt(sz, NU_SCREENWIDTH - sz);
     ship->y_pos = PickRandomInt(sz, 3*NU_SCREENHEIGHT/4 - sz);
     ship->speed = 1.0 - 0.005 * sz;
     ship->dir = PickRandomInt(0, 1) ? GO_LEFT : GO_RIGHT;
     ship->sailcolor = PickRandomRGB();
     ship->woodcolor = PickRandomBrown();
}

/***************************************************************
   Draw all objects
***************************************************************/

void DrawObjects() {
     for (int i = 0; i <= NumObjects; ++i) 
     {
         OBJECT *object = &objects[i];
         if (object->type == 0)
         {
            DrawShips(object->x_pos, object->y_pos, object->size, object->dir, object->speed);
         }
         else if (object->type == 1)
         {
             DrawFin(object->x_pos, object->y_pos, object->size);
         }
     }
}

/***************************************************************
   Move every ship
***************************************************************/


void MoveObjects()
{
     for (int i = 0; i < NumObjects; ++i)
     {
         OBJECT *object = &objects[i];
         if (object->type == 0 )
         {
            MoveShips(object);
         }
     }
}

void MoveShips(OBJECT *ship)
{
     if (ship->x_pos < 0 || ship->x_pos > NU_SCREENWIDTH)
     {
         ship->dir *= -1;
     }
     ship->x_pos += ship->speed * ship->dir;
}

 

/***************************************************************
   Fin creation
***************************************************************/

void CreateFin(int x, int y)
{
     if (NumObjects < MaxShips)
     {
         OBJECT *fin = &objects[NumObjects];
         fin->type = 1;
         fin->size = 20;
         fin->x_pos = x;
         fin->y_pos = y;
     }
}

/***************************************************************
   Sky drawing function
***************************************************************/

void DrawSky()
{
     SetPenColorHex(skycolor); //Draws sky
     DrawFillBox(0, 3*NU_SCREENHEIGHT/4, NU_SCREENWIDTH, NU_SCREENHEIGHT);
    
     SetPenColorHex(suncolor); //Draws sun
     DrawPieArc(150, 450, 25, 0, 360);
    
     SetPenColorHex(cloudcolor); //Draws cloud
     DrawFillBox(500, 425, 550, 445);
     DrawPieArc(500, 425, 20, 180, 360);
     DrawPieArc(500, 425, 20, 90, 270);
     DrawPieArc(490, 440, 18, 0, 270);
     DrawPieArc(520, 435, 20, 0, 180);
     DrawPieArc(540, 440, 16, 0, 360);
     DrawPieArc(535, 425, 17, 0, 360);
     DrawPieArc(525, 420, 20, 0, 360);
}

/***************************************************************
   Ship drawing funtions
***************************************************************/
void DrawShips (int x_pos, int y_pos, int size, int direction, int speed)
{
     OBJECT *fin = &objects[NumObjects];
     if (fin->type == 1)
     {
         
         int i = 0;
         for (i = 0; i < NumObjects; ++i)
         {
             OBJECT *ship = &objects[i];
             int xDistance = 0;
             int yDistance = 0;
             double TotalDistance = 0;
             xDistance = (fin->x_pos - ship->x_pos)*(fin->x_pos - ship->x_pos);
             yDistance = (fin->y_pos - ship->y_pos)*(fin->y_pos - ship->y_pos);
             TotalDistance = sqrt(xDistance + yDistance);
             OBJECT *object = &objects[i];
             if (2*(ship->size) >= TotalDistance)
             {
                 ship->type = 2;
             }
         }
     }
     for (int i = 0; i < NumObjects; ++i)
         {
              OBJECT *ship = &objects[i];
              if (ship->type == 0)
              {
                  DrawShip(ship->x_pos, ship->y_pos, ship->size, ship->dir, ship->sailcolor, ship->woodcolor);
                }
         }
}

void DrawShip (double ShipX, double ShipY, double ShipSize, int direction, unsigned long sailcolor, unsigned long woodcolor)
{    
    SetPenColorHex(woodcolor);
    double width = 3*ShipSize/4;
    double height = ShipSize/2;
    double mastctr1 = ShipX + direction*width/2;
    double mastctr2 = ShipX - direction*width/4;
   
    DrawFillBox(ShipX - width, ShipY, ShipX + width, ShipY - height); //The bottom of the ship.
    DrawFillTriangle(ShipX - width, ShipY, ShipX - 2*width, ShipY, ShipX - width, ShipY - height);
    DrawFillTriangle(ShipX + width, ShipY, ShipX + 2*width, ShipY, ShipX + width, ShipY - height);
   
    DrawFillBox(mastctr1 - direction*width/24, ShipY, mastctr1 + direction*width/24, ShipY + 2*width); //Rightmost mast.
    DrawFillBox(mastctr2 - direction*width/24, ShipY, mastctr2 + direction*width/24, ShipY + 2*width);//Middle mast.
   
    SetPenColorHex(sailcolor);
   
    DrawFillSail(mastctr1 + direction*width/24, ShipY + 1*width/4, //Rightmost sail.
               mastctr1 + direction*10*width/6, ShipY + width/2,
               mastctr1 + direction*8*width/12, ShipY + 2*width,
               mastctr1 + direction*width/24, ShipY + 3*width/2); 
    DrawFillSail(mastctr2 + direction*width/24, ShipY + 1*width/4, //Middle sail.
               mastctr2 + direction*7*width/10, ShipY + width/3,
               mastctr2 + direction*2*width/6, ShipY + 7*width /4,
               mastctr2 + direction*width/24, ShipY + 4*width/3); 
    DrawFillTriangle(mastctr2 - direction*width/6, ShipY + 3*width/10, //The  middle sail.
               mastctr2 - direction*4*width/5, ShipY + 3*width/10,
               mastctr2 - direction*width/6, ShipY + 3*width/2);    
    DrawFillTriangle(mastctr2 - direction*7*width/10, ShipY + width/2, //The  leftmost sail.
               mastctr2 - direction*3*width/2, ShipY + width/2,
               mastctr2 - direction*width/6, ShipY + 8*width/5);     
    DrawLine(mastctr2 - direction*width/6, ShipY + 3*width/10, mastctr2 - direction*width/24, ShipY); //Sail lines.
    DrawLine(mastctr2 - direction*width/6, ShipY + 3*width/2, mastctr2 - direction*width/24, ShipY + 27*width/16);
    DrawLine(mastctr2 - direction*3*width/2, ShipY + width/2, ShipX - direction*2*width, ShipY);
    DrawLine(mastctr2 - direction*width/6, ShipY + 8*width/5, mastctr2 - direction*width/24, ShipY + 28*width/16);
    DrawLine(mastctr2 - direction*4*width/5, ShipY + 3*width/10, ShipX - direction*2*width, ShipY);
    DrawLine(mastctr2 + direction*2*width/6, ShipY + 7*width/4, mastctr2 + direction*width/24, ShipY + 30*width/16);
    DrawLine(mastctr1 + direction*8*width/12, ShipY + 2*width, mastctr1 + direction*width/24, ShipY + 30*width/16);

    SetPenColorHex(wakecolor);
    DrawPieArc(ShipX + 5.0/6*direction*width, ShipY - height, ShipSize/5 , 0, 180);
    DrawPieArc(ShipX + 3.0/6*direction*width, ShipY - height, ShipSize/6 , 0, 180);
    DrawPieArc(ShipX + 1.0/6*direction*width, ShipY - height, ShipSize/7, 0, 180);
}

/**********************************************************************
 Fin drawing function
***********************************************************************/

void DrawFin(double x, double y, double size)
{
     SetPenColorHex (fincolor);
     DrawFillTriangle (x - size, y, x + size, y, x, y + 7/4*size);
}

/* Utilities */

unsigned long PickRandomRGB()
{
     int red = rand() % 255;
     int green = rand() % 255;
     int blue = rand() % 255;
     return red * 256 * 256 + green * 256 + blue;
}

void DrawFillSail(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3)
{
     glBegin(GL_POLYGON);   // Draw a connected line from
     glVertex2d(x0,y0);    // corner to
     glVertex2d(x1,y1);    // lcorner to
     glVertex2d(x2,y2);    // corner to
     glVertex2d(x3,y3);    // corner to
     glVertex2d(x0,y0);    // corner,
     glEnd();         // then stop--we're finished.
     glFlush();      // Finish any pending drawing comm
}

unsigned long PickRandomBrown()
{
     int red = rand() % 80 + 100;
     int green = rand() % 25 + 60;
     int blue = rand() % 20 + 40;
     return red * 256 * 256 + green * 256 + blue;
}
    
int PickRandomInt(int low, int high)
{
     return low + rand() % (high - low + 1);
} 
