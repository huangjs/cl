/***********************************************************
 *  ASTERIODS: A Northwestern University Graphics Project  *
 *  Authors: Jesse Benck and Kristine Elsener              * 
 *  Version 1.0: Monday, January 15                        *
 *  Version 1.1: Sunday, January 21                        *
 *  Version 2.0: Sunday, January 28                        *
 *  Version 2.1: Tuesday, February 6                       *
 *  Version 3.0: Sunday, February 18                       *
 *  Version 4.0: Sunday, March 4                           *
 *  Version 4.1: Wednesday, March 7                        *
 *  Version 4.2: Friday, March 9                           *
 *                                                         *
 *                                                         *
 *               **Instructions**                          *
 *                                                         *
 *  To shoot bullets (up to 3 at a time), press z or Z.    *
 *  To generate a shield (up to 1), click the left mouse   *
 *    button in the window.                                *q
 *  To move the ship left and right, press the left and    *
 *    right arrow keys, respectively.                      *
 *  To quit the game, press q or Q.                        *
 ***********************************************************/

#define MAXSTARS 100             // maximum number of stars in the background
#define MINSTARS 50              // minimum number of stars in the background
#define MAXAST 15                // maximum number of asteroids
#define MINAST 12                // minimum number of asteroids
#define MAXBULLETS 3             // maximum number of bullets
#define BULLETHEIGHT 15
#define BULLETWRATIO 0.3
#define BULLETCOLOR 0xFF0000
#define VBULLET 8.0
#define SHIPVINC 0.6
#define SHIPVMAX 4.0
#include "introGlutLib.h"		 // include the basic drawing library
#include <time.h>			     // for MS Windows time-finding functions
#include <stdlib.h>				 // for malloc and free
#include <string.h>				 // for strcpy and other string fcns.

/* Function Prototypes */
/* Ship Drawing */
void drawShip(double x, double y, double h);
double newx(double x, double dx,  double h);
double newy(double y, double dy, double h);

void setShip(void);
void moveShip(void);

/* Asteroid Drawing */
void drawAst(double x, double y, double h, unsigned long c);
double newxa(double x, double dx,  double h);
double newya(double y, double dy, double h);

/* Random Asteroids */
void setAst(void);
void drawrandAst(void);
void moveAst(void);

/* Random Stars */
void setStars(void);
void drawStars(void);

/* Bullets */
void drawBullets(void);
//void setBullets(void);
void moveBullets(void);

/* Check for asteroid/ship collision */
void shipCollision(void);

/* Check for bullet/asteroid collision*/
void astCollision(void);

/* Structure definitions */
typedef struct {
    double x;
    double y;
    double h;
    double vx;
    int shield;
    } SHIP;
    
typedef struct {
    double x;
    double y;
    double h;
    unsigned long c;
    double vx;
    double vy;
    } AST;
    
typedef struct {
    int x;
    int y;
    int r;
    } STAR;   

typedef struct {
    double x;
    double y;
    double h;
    double v;
    } BULLET;

/* Define global variables for random object locations */


int numstars;          // Number of stars
STAR star[MAXSTARS];
AST ast[MAXAST];
int numast;               // Number of Asteroids                                                  
SHIP ship;
BULLET bullet[MAXBULLETS];
int bulletnum;
char *endmessage = "                     ";
char scoremessage[10];
char *yourscoremessage = "                                         ";
int score = 0;

int main()
{
    srand(time(NULL));
    numstars = (rand () % (MAXSTARS - MINSTARS)) + MINSTARS;
    numast = (rand () % (MAXAST - MINAST)) + MINAST;
    bulletnum = 0;
    setStars();
    setAst();
    setShip();
    //setBullets();
    InitGraphics();			// start GLUT/OpenGL
	


    return 0;
}

/**********************************************************************
 myDisplay()
 
***********************************************************************/

void myDisplay(void)
{
    SetBackgndColorHex(0X000000);
    ClearWindow();
    drawStars();
    drawBullets();
    drawShip(ship.x,ship.y,ship.h);
    drawrandAst(); 
    
    /* Print end message */
    SetPenColorHex(0xFF0000); // Red
    DrawText2D(helv18,0.5*NU_SCREENWIDTH-48,0.5*NU_SCREENHEIGHT,endmessage);   // Center on screen
    DrawText2D(helv18,0.5*NU_SCREENWIDTH-80,0.5*NU_SCREENHEIGHT-20,yourscoremessage);
    DrawText2D(helv18,0.5*NU_SCREENWIDTH+50,0.5*NU_SCREENHEIGHT-20,scoremessage);
    
    shipCollision();
    astCollision();
    
    moveAst();
    moveShip();
    moveBullets();
}



/**********************************************************************
 myMouse(button, state, x, y)
 
 Shield the ship when the player clicks the screen up to one time
 
***********************************************************************/
			
void myMouse(int button, int state, int x, int y) 
{
     if(button == GLUT_LEFT_BUTTON && state == GLUT_UP && ship.shield == 0)
     {
         ship.shield = 1;                    
     }
}



/**********************************************************************
 myKeyboard(key, x, y)
 
 Exit program if q is pressed
 Fire bullet if z is pressed
***********************************************************************/

void myKeyboard(unsigned char key, int x, int y)
{
	BULLET *tempbullet = &bullet[bulletnum];
        switch(key)
        {
            case 'z': case 'Z':
            if(ship.h != 0)
            {
                tempbullet->x = ship.x;
                tempbullet->y = ship.y;
                tempbullet->h = BULLETHEIGHT;
                tempbullet->v = VBULLET;
                bulletnum++;
                if(bulletnum > (MAXBULLETS-1))
                { bulletnum = 0; }
            }
            break;
            case 'q': case 'Q':
                 exit(0);
                 break;
            default:
            break;
        }
}



/**********************************************************************
 mySpecialKey(key, x, y)
 
 Accelerate the ship left or right when the arrow keys are pressed
 
***********************************************************************/

void mySpecialKey(int key, int x, int y)
{

    switch(key) {
        case GLUT_KEY_LEFT: ship.vx = -1 * (ship.vx - SHIPVINC) > SHIPVMAX ? -1 * SHIPVMAX: ship.vx - SHIPVINC; break;
        case GLUT_KEY_RIGHT: ship.vx = (ship.vx + SHIPVINC) > SHIPVMAX ? SHIPVMAX: ship.vx + SHIPVINC; break;
        }
    
    
}



/*********************************************************************  
drawShip(x, y, h)

Draw the ship at center point (x,y) with height h

**********************************************************************/

void drawShip(double x, double y, double h)
{
    double h0 = 210.0;
    
    if(ship.shield == 1)
    {
        SetPenColorHex(0xFFFF00);
        DrawFillCircle(newx(x,0,h), newy(y,-20,h), h * .60);              
    }
    
    /* Ship */
    SetPenColorHex(0x4A85CE);
    DrawFillTriangle(newx(x,-25,h),newy(y,-40,h),
                     newx(x,25,h),newy(y,-40,h),
                     newx(x,-25,h),newy(y,40,h));
    DrawFillTriangle(newx(x,-25,h),newy(y,40,h),
                     newx(x,25,h),newy(y,-40,h),
                     newx(x,25,h),newy(y,40,h));
    DrawFillTriangle(newx(x,-25,h),newy(y,40,h),
                     newx(x,0,h),newy(y,80,h),
                     newx(x,25,h),newy(y,40,h));
    
    SetPenColorHex(0xCF3E3E);
    DrawFillTriangle(newx(x,-50,h),newy(y,-30,h),
                     newx(x,-25,h),newy(y,-25,h),
                     newx(x,-25,h),newy(y,20,h));
    DrawFillTriangle(newx(x,50,h),newy(y,-30,h),
                     newx(x,25,h),newy(y,-25,h),
                     newx(x,25,h),newy(y,20,h));
    DrawFillTriangle(newx(x,-15,h),newy(y,-55,h),
                     newx(x,-15,h),newy(y,-40,h),
                     newx(x,15,h),newy(y,-55,h));
    DrawFillTriangle(newx(x,-15,h),newy(y,-40,h),
                     newx(x,15,h),newy(y,-55,h),
                     newx(x,15,h),newy(y,-40,h));
    DrawFillTriangle(newx(x,-20,h),newy(y,-55,h),
                     newx(x,-15,h),newy(y,-55,h),
                     newx(x,-15,h),newy(y,-40,h));
    DrawFillTriangle(newx(x,20,h),newy(y,-55,h),
                     newx(x,15,h),newy(y,-55,h),
                     newx(x,15,h),newy(y,-40,h));
    
    SetPenColorHex(0xFFFF00);
    DrawFillCircle(newx(x,0,h),newy(y,20,h),10*h/h0);
    SetPenColorHex(0xDDD0B4);
    DrawFillCircle(newx(x,0,h),newy(y,18,h),8*h/h0);
    SetPenColorHex(0x000000);
    SetLineWidth(2.0*h/h0);
    DrawArc(newx(x,0,h),newy(y,19,h),5*h/h0,220,320);
    DrawFillCircle(newx(x,0,h),newy(y,18,h),1*h/h0);
    DrawFillCircle(newx(x,-3,h),newy(y,21,h),1*h/h0);
    DrawFillCircle(newx(x,3,h),newy(y,21,h),1*h/h0);

    
    /* Flames */
    SetPenColorHex(0xFF9900);
    DrawFillTriangle(newx(x,-15,h),newy(y,-80,h),
                     newx(x,-15,h),newy(y,-100,h),
                     newx(x,-25,h),newy(y,-95,h));
    DrawFillTriangle(newx(x,-15,h),newy(y,-100,h),
                     newx(x,-25,h),newy(y,-95,h),
                     newx(x,-25,h),newy(y,-115,h));
    DrawFillTriangle(newx(x,15,h),newy(y,-80,h),
                     newx(x,15,h),newy(y,-100,h),
                     newx(x,25,h),newy(y,-95,h));
    DrawFillTriangle(newx(x,15,h),newy(y,-100,h),
                     newx(x,25,h),newy(y,-95,h),
                     newx(x,25,h),newy(y,-115,h));
    DrawFillTriangle(newx(x,0,h),newy(y,-90,h),
                     newx(x,-7,h),newy(y,-110,h),
                     newx(x,7,h),newy(y,-110,h));
    DrawFillTriangle(newx(x,-7,h),newy(y,-110,h),
                     newx(x,7,h),newy(y,-110,h),
                     newx(x,0,h),newy(y,-130,h));
}



/*********************************************************************
newx(x,dx,h)

Calculate the new x coordinate for the ship

**********************************************************************/

double newx(double x, double dx, double h)
{
    double w0 = 100.0;
    double w = 100.0/210.0*h;
    return (x+dx*w/w0);     
}



/*********************************************************************
newy(y,dy,h)

Calculate the new y coordinate for the ship

**********************************************************************/

double newy(double y, double dy, double h)
{
    double h0 = 210.0;  
    return (y+dy*h/h0);     
}


/*********************************************************************
setShip()

Places the ship and sets its initial velocity

**********************************************************************/

void setShip(void)
{
     ship.x = 400;
     ship.y = 50;
     ship.h = 80;     
     ship.vx = 0;
     ship.shield = 0;
}


/*********************************************************************
moveShip()

Moves the ship across the screen and loops it back

**********************************************************************/

void moveShip(void)
{
     double shipw = 100.0/210.0*ship.h;   // Ship width to height proportion = 10:21
     ship.x = ship.x + ship.vx;
     if(ship.x - 0.5 * shipw > NU_SCREENWIDTH)
     {
         ship.x = ship.x - NU_SCREENWIDTH - shipw;
     }
     if(ship.x + 0.5 * shipw < 0)
     {ship.x = ship.x + NU_SCREENWIDTH + shipw;
     }
}


/*********************************************************************
drawAst(x, y, h)

Draw an asteroid at center point (x,y) with height h

**********************************************************************/

void drawAst(double x, double y, double h, unsigned long c)
{
        /* Asteroid */
    SetPenColorHex(c);
    DrawFillBox(newxa(x,-50,h),newya(y,-50,h),
                newxa(x,50,h),newya(y,50,h));
    DrawFillBox(newxa(x,50,h),newya(y,-50,h),
                newxa(x,80,h),newya(y,-10,h));
    DrawFillTriangle(newxa(x,50,h),newya(y,50,h),
                newxa(x,80,h),newya(y,-10,h),
                newxa(x,50,h),newya(y,-10,h));
    DrawFillTriangle(newxa(x,80,h),newya(y,-50,h),
                newxa(x,-10,h),newya(y,-50,h),
                newxa(x,-10,h),newya(y,-75,h));
    DrawFillTriangle(newxa(x,-10,h),newya(y,-50,h),
                newxa(x,-10,h),newya(y,-75,h),
                newxa(x,-60,h),newya(y,-50,h));
    DrawFillBox(newxa(x,-60,h),newya(y,-50,h),
                newxa(x,-50,h),newya(y,-40,h));
    DrawFillTriangle(newxa(x,-60,h),newya(y,-40,h),
                newxa(x,-50,h),newya(y,-40,h),
                newxa(x,-50,h),newya(y,50,h));
    DrawFillTriangle(newxa(x,-60,h),newya(y,-40,h),
                newxa(x,-50,h),newya(y,50,h),
                newxa(x,-70,h),newya(y,20,h));
    DrawFillTriangle(newxa(x,-50,h),newya(y,50,h),
                newxa(x,50,h),newya(y,50,h),
                newxa(x,15,h),newya(y,65,h));    
}    



/*********************************************************************
newxa(x,dx,h)

Calculate the new x coordinate for the asteroid

**********************************************************************/

double newxa(double x, double dx, double h)
{
    double w0 = 140.0;
    double w = 140.0/140.0*h;   
    return (x+dx*w/w0);     
}



/*********************************************************************
newya(y,dy,h)

Calculate the new y coordinate for the asteroid

**********************************************************************/

double newya(double y, double dy, double h)
{
    double h0 = 140.0;   
    return (y+dy*h/h0);     
}



/*********************************************************************
setAst()

Randomly sets the center, height, and color of the asteroids 

**********************************************************************/

void setAst(void)
{
    
    int n;
    for(n=0; n<numast; n++)
    {
         AST *tempast = &ast[n];
         tempast->x = rand() % NU_SCREENWIDTH;
         tempast->y = rand() % NU_SCREENHEIGHT + NU_SCREENHEIGHT + 75;
         tempast->h = (rand() % 100) + 50;    
         tempast->vy = -2.5*(0.65 - 0.003*tempast->h);
         tempast->vx = (rand() % 5 - 2) * 0.35 * tempast->vy;
         switch (rand() % 4)
         {
            case 0: tempast->c = 0x15A2BB;  // Blue-Green
                 break;
            case 1: tempast->c = 0xE495C1;  // Pink
                 break;
            case 2: tempast->c = 0x8F772E;  // Brown
                 break;
            case 3: tempast->c = 0x73D449;  // Green
                 break;
         }      
    }
}   



/*********************************************************************
drawrandAst()

Draws the asteroids defined by setAst()

**********************************************************************/

void drawrandAst(void)
{
    int n;
    for(n = 0; n<numast; n++)
    {
        drawAst(ast[n].x, ast[n].y, ast[n].h, ast[n].c);
    }      
}
 

/*********************************************************************
moveAst()

Moves the asteroids and loops them back to the top of the screen

**********************************************************************/

void moveAst(void)
{
    for(int n = 0; n<numast; n++)
    {
         AST *tempast = &ast[n];
         tempast->y = tempast->y + tempast->vy;
         tempast->x = tempast->x + tempast->vx;
         if (tempast->x + .5 * tempast->h < 0)
         {
            tempast->x = tempast->x + NU_SCREENWIDTH + tempast->h;
         }   
         if (tempast->x - .5 * tempast->h > NU_SCREENWIDTH)
         {
            tempast->x = tempast->x - NU_SCREENWIDTH - tempast->h;
         }
         if((tempast->y + .5 * tempast->h) < 0)
         {
            tempast->y = tempast->y + NU_SCREENHEIGHT + tempast->h;
         }   
    } 
     
}


/*********************************************************************
setStars()

Randomly sets the locations and radii of the stars

**********************************************************************/

void setStars(void)
{
     int n;
     for(n=0; n<numstars; n++)
     {
         STAR *tempstar = &star[n];     
         tempstar->x = rand() % NU_SCREENWIDTH;
         tempstar->y = rand() % NU_SCREENHEIGHT;
         tempstar->r = (rand() % 2) + 1;     
     }     
}



/*********************************************************************
drawStars()

Draws the stars defined by setStars()

**********************************************************************/

void drawStars(void)
{
    int n;
    SetPenColorHex(0xFFFFFF);
    for(n = 1; n<=numstars; n++)
    {
        DrawFillCircle(star[n].x,star[n].y,star[n].r); 
    }          
}

/*********************************************************************
drawBullets()

Draws the bullets

**********************************************************************/

void drawBullets(void)
{
    SetPenColorHex(BULLETCOLOR);
    for(int n = 0; n < MAXBULLETS; n++)
    {
            BULLET *tempbullet = &bullet[n];
            double x0 = bullet[n].x - 0.5*BULLETWRATIO*bullet[n].h;
            double x1 = bullet[n].x + 0.5*BULLETWRATIO*bullet[n].h;
            double y0 = bullet[n].y - 0.5*bullet[n].h;
            double y1 = bullet[n].y + 0.5*bullet[n].h;
            DrawFillBox(x0,y0,x1,y1);  
    }  
}

/*********************************************************************
setBullets()

Sets the initial positions of the bullets

**********************************************************************/

void setBullets(void)
{
     for(int n = 0; n < MAXBULLETS; n++)
     {
             BULLET * tempbullet = &bullet[n];
             tempbullet->x = 0;
             tempbullet->y = 0;
             tempbullet->h = 0; 
             tempbullet->v = 0;                    
     }     
     
}

/*********************************************************************
moveBullets()

Moves the bullets up the screen

**********************************************************************/

void moveBullets(void)
{
     for(int n = 0; n < MAXBULLETS; n++)
     {
             BULLET *tempbullet = &bullet[n];
             tempbullet->y += tempbullet->v;
             if(tempbullet->y > NU_SCREENHEIGHT)
             {
                 tempbullet->h = 0;            
             }
     }
}

/*********************************************************************
shipCollision()

Detects collisions between the ship and the asteroids and ends the
game if they collide
**********************************************************************/

void shipCollision(void)
{
     double shipHalfWidth = 0.5*50.0/210.0*ship.h;
     double shipFinWidth = 0.5*100.0/210.0*ship.h;
     double shipBoxHeight = 80.0/210.0*ship.h;
     
     for(int n = 0; n < numast; n++)
     {
         AST *tempast = &ast[n];
         double radius = tempast->h * 0.5;
         double dist1 = sqrt(pow(ship.y+shipBoxHeight-tempast->y,2) + pow(ship.x+shipHalfWidth-tempast->x,2));  // Upper Right Corner
         double dist2 = sqrt(pow(ship.y+shipBoxHeight-tempast->y,2) + pow(ship.x-shipHalfWidth-tempast->x,2));  // Upper Left corner
         double dist3 = sqrt(pow(ship.y-shipBoxHeight-tempast->y,2) + pow(ship.x+shipFinWidth-tempast->x,2));   // Lower Right Fin
         double dist4 = sqrt(pow(ship.y-shipBoxHeight-tempast->y,2) + pow(ship.x-shipFinWidth-tempast->x,2));   // Lower Left Fin
         if(dist1 < radius || dist2 < radius || dist3 < radius || dist4 <radius)
         {
                if(ship.shield == 0 || ship.shield == 2)
                { 
                      ship.h = 0;  
                      endmessage = "Game Over!";
                      yourscoremessage = "Your score: ";
                      sprintf(scoremessage, "%d", score);
                }
                else if(ship.shield ==1)
                {
                     tempast->h = 0;
                     ship.shield = 2;
                }
         }       
     }    
}

/*********************************************************************
astCollision()

Detects collisions between the bullets and asteroids and destroys and 
resets the asteroids if they collide
**********************************************************************/

void astCollision(void)
{
     for(int n = 0; n < numast; n++)
     {
         AST *tempast = &ast[n];
         double radius = tempast->h * 0.5;
         for(int m = 0; m < MAXBULLETS; m++)
         {
              BULLET *tempbullet = &bullet[m];
              double tempy = tempbullet->y;
              double dist = sqrt(pow((tempast->y - tempbullet->y), 2) + pow((tempast->x - tempbullet->x), 2));
                 
              if(tempbullet->h != 0)
              {
                  if(dist <= radius)
                  {
                      tempast->x = 0;
                      tempast->y = 0;
                      tempast->h = 0;
                      tempbullet->h = 0;    
                      
                      score = score+50;
                      
                      AST *tempast = &ast[n];
                      tempast->x = rand() % NU_SCREENWIDTH;
                      tempast->y = rand() % NU_SCREENHEIGHT + NU_SCREENHEIGHT + 75;
                      tempast->h = (rand() % 100) + 50;    
                      tempast->vy = -2.5*(0.65 - 0.003*tempast->h);
                      tempast->vx = (rand() % 5 - 2) * 0.35 * tempast->vy;
                      switch (rand() % 4)
                      {
                        case 0: tempast->c = 0x15A2BB;  // Blue-Green
                             break;
                        case 1: tempast->c = 0xE495C1;  // Pink
                             break;
                        case 2: tempast->c = 0x8F772E;  // Brown
                             break;
                        case 3: tempast->c = 0x73D449;  // Green
                             break;
                                                                     
                      }  
                  }        
              }
         }
     }
}

