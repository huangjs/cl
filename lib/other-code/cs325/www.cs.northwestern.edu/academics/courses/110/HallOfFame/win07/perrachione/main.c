/*
** 21-January-2007
** The following code is a modified version of that provided in CS110 for 
** drawing graphics.  This code will draw an ambulance of any size using the 
** function DrawAmbulance()
**
** 8-February-2007
** The code has been modified for PA3 to now include random other cars. In addition
** to the ambulance (whose size and position are static), the program will draw 
** a random number (at least 3, and at most 12) cars at random positions with 
** random body colors.  These cars are modeled on 1980's style station wagons
** with genuine wood-panelled exteriors!
**
** 21-February-2007
** Updated for PA4 so that now there are arrays to hold relevant values about the 
** cars and Ambulance.  Additionally, these arrays allow for animation, such that
** the cars and Ambulance drive from left to right across the screen, and the
** lights on the ambulance flash as though it were heading to an emergency.  The
** cars will move to get out of the way of the ambulance while its lights are 
** flashing, and the ambulance will also move to avoid the cars.
**
** 11-March-2007
** Now this program is interactive!  You can control the motion of the ambulance
** with the keyboard. Pressing [i] moves the ambulance up.  Pressing [k] moves 
** the ambulance down.  Pressing [j] moves ambulance left; [l] moves ambulance 
** right.  If the ambulance runs into a car, that counts as an accident.  After 
** four accidents, the game is over!!  The longer you play before you exceed four
** accidents, the higher your score will be.  Your score and the number of
** accidents are displayed on the top of the screen. Try to get the highest 
** score you can!!
**
** Created by Tyler Perrachione
** for EECS 110-0
** 11-March-2007
*/

#include "introGlutLib.h"		//include the basic drawing library
#include <stdio.h>              // standard input and output
#include <time.h>				// for MS Windows time-finding functions
#include <stdlib.h>				// for malloc and free
#include <string.h>				// for strcpy and other string fcns.
#include <ctype.h>              // for character whatnot

#define DARKGREY 0x606060
#define WHITE 0xFFFFFF
#define TRUEBLUE 0x0000FF
#define ORANGE 0xFF8840
#define TRUERED 0xFF0000
#define BLACK 0x000000
#define YELLOW 0xFFFF00
#define LIGHTBLUE  0xAAFFFF
#define DARKBROWN 0x804000

/*
** Declare global variables, etc, for GamePlay
*/ 
double score = 0;
int num_accidents = 0;
double lane_marks[7];

/*
** Declare Global Variables and constants for the Cars
*/
typedef struct {
        double xpos;
        int ypos;
        double speed;
        int color;
        } carT;
carT allcars[11];
int numcars = 0;
const double car_width = 400;
const int car_height = 300;

/*
** Declare global variables + constants for the ambulance
*/
const double ambulance_width = 800;
const double ambulance_height = 600;
double ambulance_x = 0;
double ambulance_y = 200;
double ambulance_speed = 2;

/* 
** DrawAmbulance(x, y, width, height) draws all the parts of an ambulance with 
** relative dimensions based on a starting x position, a starting y position
** and ratios of the width and height specified
*/

void DrawAmbulance(double x, double y, double width, double height);

void DrawWoodPanelCar(double x, double y, double width, double height, unsigned long color);

/* FlashLights() makes the ambulance lights flash as in an emergency*/ 
int FlashLights();

/*  
 ** pickRandomRGB() generates a random color value
 ** originally from rand_rect.c on the course website
 */

unsigned long pickRandomRGB(void);
void DrawAccident(double x, double y);
void Game_Over(void);


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
    numcars = rand() % 3 + 3;
    InitGraphics();			// start GLUT/OpenGL
    return 0;
}


/***************************************************************
 myDisplay()

 Does the drawing, what with the lines and the rectangles and so on
 ***************************************************************/

void myDisplay(void)
{
    static int first_time = 1;
    /* Draw the Background: sky, highway, grass */
    ClearWindow();    
    SetPenColorHex(LIGHTBLUE);
    DrawFillBox(0,400,800,600);
    SetPenColorHex(0x00AA00);
    DrawFillBox(0,300,800,450);
    DrawFillBox(0,0,800,50);
    SetPenColorHex(DARKGREY);
    DrawFillBox(0,50,800,300);
    
    /* Display information about gameplay: score and number of accidents */
    SetPenColorHex(BLACK);
    char your_score[80];
    sprintf(your_score, "Your score is %d", (int)score);
    DrawText2D(helv18, 20, 475, your_score);
    char your_accidents[80];
    sprintf(your_accidents, "You have had %d accidents", (int)num_accidents);
    SetPenColorHex(TRUERED);
    DrawText2D(helv18, 200, 475, your_accidents);
    score += 0.1;
    
    if (first_time) {
    /* Initialize and draw the highway lines */
    SetPenColorHex(YELLOW);
    SetLineWidth(4);
    for (int b = 0; b <= 7; b++) {
    lane_marks[b] = b * 140;
    DrawLine(lane_marks[b], 135, lane_marks[b] + 80, 135);
    DrawLine(lane_marks[b] + 40, 225, lane_marks[b] + 120, 225);
    } 
        
    /* Initialize the starting values for the cars */
       for (int i = 0; i <= numcars; ++i) {        
            allcars[i].color = pickRandomRGB();
            
            /* Determine the Y-coord and speed of each car */      
            switch (rand() % 3) {
                   case 0: allcars[i].ypos = 75;
                           allcars[i].speed = 1.0;
                           break;
                   case 1: allcars[i].ypos = 175;
                           allcars[i].speed = 1.50;
                           break;
                   case 2: allcars[i].ypos = 250;
                           allcars[i].speed = 2.0;
                           break;
                   } 
                   
            /* Determine the x-coord of each car and try to prevent any overlap */
            allcars[i].xpos = rand() % 400;
            for (int p = 0; p <= i; ++p) {
                if (allcars[i].ypos == allcars[p].ypos) {
                   if (allcars[i].xpos > allcars[p].xpos && allcars[i].xpos < allcars[p].xpos + 100) {
                      allcars[p].xpos += 200;
                   }
                   if (allcars[i].xpos < allcars[p].xpos && allcars[i].xpos > allcars[p].xpos - 100) {
                       allcars[p].xpos -= 200;
                   }
                }
             } 
            DrawWoodPanelCar(allcars[i].xpos, allcars[i].ypos, car_width, car_height, allcars[i].color);
        }
        
        /* Draw the first ambulance */
        ambulance_x = 100;
        DrawAmbulance(ambulance_x, ambulance_y, ambulance_width, ambulance_height);
        first_time = 0;

    } else {
    
         /* For all subsequent redrawings after the first time */                     

         /* Draw the Highway Lanes */ 
         SetPenColorHex(YELLOW);
         SetLineWidth(4);
         for (int q = 1; q <= 7; q++) {
          DrawLine(lane_marks[q], 135, lane_marks[q] + 80, 135);
          DrawLine(lane_marks[q] + 40, 225, lane_marks[q] + 120, 225);
          lane_marks[q]--;
          if (lane_marks[q] == -120) {
            lane_marks[q] = 840;
            }
          }
                   
         /* Draw the cars and update their coordinates */
         for (int k = 0; k <= numcars; ++k) {
             DrawWoodPanelCar(allcars[k].xpos, allcars[k].ypos, car_width, car_height, allcars[k].color);
             allcars[k].xpos -= allcars[k].speed;
             
             /* Determine whether the ambulance ran into a car*/
                if (allcars[k].xpos > ambulance_x + 250 && allcars[k].xpos < ambulance_x + 300) {
                if (allcars[k].ypos > ambulance_y && allcars[k].ypos < ambulance_y + 50) {
                   num_accidents += 1;
                   allcars[k].xpos = 900;
                   if (num_accidents >= 4) {
                   Game_Over();
                   }
                   }
                if (allcars[k].ypos < ambulance_y && allcars[k].ypos > ambulance_y - 50) {
                   num_accidents += 1;
                   DrawAccident(allcars[k].xpos, allcars[k].ypos);
                   allcars[k].xpos = 900;
                   if (num_accidents >= 4) {
                   Game_Over();
                   }
                   }
                }
             
             
             /* If the cars go off the screen, have them go back to the other side
                with a new starting location */
             if (allcars[k].xpos < 0) {
                 allcars[k].xpos = 900;
                 switch (rand() % 3) {
                   case 0: allcars[k].ypos = 75;
                           allcars[k].speed = 1;
                           break;
                   case 1: allcars[k].ypos = 175;
                           allcars[k].speed = 1.5;
                           break;
                   case 2: allcars[k].ypos = 250;
                           allcars[k].speed = 2;
                           break;
                   } 
                }
         }    
         
         /* Draw the ambulance as it moves across the screen */ 
         DrawAmbulance(ambulance_x, ambulance_y, ambulance_width, ambulance_height);         
    }

}


/**********************************************************************
 myMouse(button, state, x, y)
 
 Exit the program if the user clicks the mouse.
***********************************************************************/
			
void myMouse(int button, int state, int x, int y) 
{
	exit(0);
}


/**********************************************************************
 myKeyboard(key, x, y)
 
 Exit the program if a normal key is pressed
***********************************************************************/

void myKeyboard(unsigned char key, int x, int y)
{

switch (tolower(key)) {
       case 105: if (ambulance_y <= 250) {
                 ambulance_y += 8;
                 }
                 break;
       case 107: if (ambulance_y >= 50) {
                 ambulance_y -= 8;
                 } 
                 break;
       case 106: if (ambulance_x > 100) {
                ambulance_x -= 8;
                }
                break;
       case 108: if (ambulance_x < 600) {
                ambulance_x += 8;
                }
                break;
       case 113: exit(0);
                 break;
   }
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
 DrawAmbulance(x, y, width, height)
 
 Draws an ambulance in the graphics window from lower left coordinates
 (x,y) with other dimensions based on ratios of the values width and height
 passed to the function.
***********************************************************************/

void DrawAmbulance (double x, double y, double width, double height) { 

     //Ambulance Carriage
     SetPenColorHex(WHITE);
     DrawFillBox(x,
                 y,
                 x + 7 * width / 40,
                 y + 3 * height / 24);

     //Ambulance Roof
     SetPenColorHex(WHITE);
     DrawFillBox(x + width / 120,
                 y + 3 * height / 24 + height / 60,
                 x + 7 * width / 40 - width / 120,
                 y + 8 * height / 75);
     SetPenColorHex(BLACK);
     DrawLine(x,
              y + 8 * height / 75,
              x + 7 * width / 40,
              y + 8 * height / 75);

     //Flashing Lights

     int siren = FlashLights();
     SetPenColorHex(siren);
     DrawFillCircle(x + width / 120,
                    y + 3 * height / 24,
                    width / 80);
     siren = FlashLights();
     SetPenColorHex(siren);
     DrawFillCircle(x + 7 * width / 40 - width / 120,
                    y + 3 * height / 24,
                    width / 80);
  
     //Passenger Door
     SetPenColorHex(BLACK);
     SetLineWidth(height / 200);
     DrawLine(x + 7 * width / 60,
              y + 8 * height / 75,
              x + 7 * width / 60,
              y);
     DrawLine(x + 7 * width / 40,
              y + 8 * height / 75,
              x + 7 * width / 40,
              y);
     SetLineWidth(1);
     SetPenColorHex(LIGHTBLUE);
 	 DrawFillBox(x + 5 * width / 40,
                 y + 7.5 * height / 75,
                 x + 6.5 * width / 40,
                 y + 5 * height / 75);    
 	 SetPenColorHex(BLACK);
 	 SetLineWidth(height / 200);
 	 DrawLine(x + 5 * width / 40,
	  	  	  y + 4 * height / 75,
              x + 5.5 * width / 40,
              y + 4 * height / 75);
 	 SetLineWidth(1);     
 
     //Windshield, Engine and Headlight
     SetPenColorHex(LIGHTBLUE);
     DrawFillTriangle(x + 7 * width / 40,
                      y + 8 * height / 75,
                      x + 7 * width / 40,
                      y + 4 * height / 75,
                      x + 17 * width / 80,
                      y + 4 * height / 75);
     SetPenColorHex(WHITE);
     DrawFillBox(x + 7 * width / 40,
                 y + 4 * height / 75,
                 x + 9 * width / 40,
                 y);      
     SetPenColorHex(YELLOW);         
     DrawPieArc(x + 9 * width / 40,
                y + height / 30,
                height / 75,
                90,
                270);   
                 
 	 //Wheels and Tires
 	 SetPenColorHex(BLACK);
     DrawFillCircle(x + 7 * width / 40,
                    y,
                    width / 45);
     DrawFillCircle(x + 3 * width / 80,
                    y,
                    width / 45);
                    
     //EMT Caduceus Symbol
  	 SetPenColorHex(TRUEBLUE);     
     SetLineWidth(height / 75);
     DrawLine(x + 4 / 3 * 6.5 * height / 75,
              y + 6.5 * height / 75,
              x + 4 / 3 * 2 * height / 75,
              y + 4 * height / 75);
     DrawLine(x + 4 / 3 * 2 * height / 75,
              y + 6.5 * height / 75,
              x + 4 / 3 * 6.5 * height / 75,
              y + 4 * height / 75);   
     DrawLine(x + 4 / 3 * 4.25 * height / 75,
              y + 7.5 * height / 75,
              x + 4 / 3 * 4.25 * height / 75,
              y + 3 * height / 75);         
     SetLineWidth(1);     
     
}
/**********************************************************************
 pickRandomRGB()

 Returns a random RGB color, with the value for each primary color determined
 separately, allowing for broad samples across the spectrum.  Taken from the code
 used in class provided in the rand_rect.c file on the course website.
***********************************************************************/

unsigned long pickRandomRGB()
{
    int red = rand() % 255;
    int green = rand() % 255;
    int blue = rand() % 255;
    return red * 256 * 256 + green * 256 + blue;
}

/**********************************************************************
 DrawWoodPanelCar(x, y, width, height)
 
 Draws a regular ol' automobile in the graphics window from lower left 
 coordinates (x,y) with other dimensions based on ratios of the values 
 width and height passed to the function.
***********************************************************************/

void DrawWoodPanelCar(double x, double y, double width, double height, unsigned long color) {
     //Draw the car body
     SetPenColorHex(color);
     DrawFillTriangle(-400 + x + 7 * width / 10,
                      y + 8 * height / 50,
                      -400 + x + 7 * width / 10,
                      y + 4 * height / 50,
                      -400 + x + 6.5 * width / 10,
                      y + 4 * height / 50);
     DrawFillBox(-400 + x + 7 * width / 10,
                 y + 4 * height / 25,
                 -400 + x + 8.5 * width / 10,
                 y); 
     DrawFillBox(-400 + x + 7 * width / 10,
                 y + 2 * height / 25,
                 -400 + x + 6.4 * width / 10,
                 y);   
     DrawFillBox(-400 + x + 9.5 * width / 10,
                 y + 2 * height / 50,
                 -400 + x + 9 * width / 10,
                 y); 
     DrawFillBox(-400 + x + 9 * width / 10,
                 y + 4 * height / 50,
                 -400 + x + 8.5 * width / 10,
                 y);                     
     DrawFillTriangle(-400 + x + 9 * width / 10,
                      y + 4 * height / 50,
                      -400 + x + 9 * width / 10,
                      y + 2 * height / 50,
                      -400 + x + 9.5 * width / 10,
                      y + 2 * height / 50);      
     DrawFillTriangle(-400 + x + 8.5 * width / 10,
                      y + 8 * height / 50,
                      -400 + x + 8.5 * width / 10,
                      y + 4 * height / 50,
                      -400 + x + 9 * width / 10,
                      y + 4 * height / 50);      
                      
     //Draw the windows and windshield
     SetPenColorHex(LIGHTBLUE);
     SetLineWidth(height / 75);   
     DrawLine(-400 + x + 8.5 * width / 10,
              y + 7.5 * height / 50,
              -400 + x + 9 * width / 10,
              y + 4 * height / 50);    
     DrawLine(-400 + x + 6.5 * width / 10,
              y + 4 * height / 50,
              -400 + x + 7 * width / 10,
              y + 7.5 * height / 50);    
     SetLineWidth(1);     
     DrawFillBox(-400 + x + 7.2 * width / 10,
                 y + 3.75 * height / 25,
                 -400 + x + 7.6 * width / 10,
                 y + 2.25 * height / 25);
     DrawFillTriangle(-400 + x + 7.2 * width / 10,
                      y + 3.75 * height / 25,
                      -400 + x + 7.2 * width / 10,
                      y + 2.25 * height / 25,
                      -400 + x + 6.9 * width / 10,
                      y + 2.25 * height / 25);
     DrawFillBox(-400 + x + 7.9 * width / 10,
                 y + 3.75 * height / 25,
                 -400 + x + 8.3 * width / 10,
                 y + 2.25 * height / 25);
     DrawFillTriangle(-400 + x + 8.3 * width / 10,
                      y + 3.75 * height / 25,
                      -400 + x + 8.3 * width / 10,
                      y + 2.25 * height / 25,
                      -400 + x + 8.7 * width / 10,
                      y + 2.25 * height / 25);

     //Draw the genuine wood panelling!
     SetPenColorHex(DARKBROWN);
     DrawFillBox(-400 + x + 6.8 * width / 10,
                 y + 2 * height / 25,
                 -400 + x + 8.75 * width / 10,
                 y + 0.5 * height / 25);
     DrawFillTriangle(-400 + x + 6.8 * width / 10,
                      y + 2 * height / 25,
                      -400 + x + 6.8 * width / 10,
                      y + 0.5 * height / 25,
                      -400 + x + 6.6 * width / 10,
                      y + 0.5 * height / 25);

     //Draw the headlight and taillight
     SetPenColorHex(YELLOW);         
     DrawPieArc(-400 + x + 9.5 * width / 10,
                y + height / 40,
                height / 50,
                90,
                270);   
     SetPenColorHex(TRUERED);         
     DrawPieArc(-400 + x + 6.4 * width / 10,
                y + height / 30,
                height / 50,
                -90,
                90); 
     
     //Draw the wheels and door frames
     SetPenColorHex(BLACK);
     DrawLine(-400 + x + 7.75 * width / 10,
              y + 7 * height / 50,
              -400 + x + 7.75 * width / 10,
              y); 
     DrawLine(-400 + x + 8.75 * width / 10,
              y + 4.5 * height / 50,
              -400 + x + 8.75 * width / 10,
              y);  
     DrawFillCircle(-400 + x + 8.75 * width / 10,
                    y,
                    width / 30);
     DrawFillCircle(-400 + x + 7.1 * width / 10,
                    y,
                    width / 30);
                                     
}
     
/**********
FlahsLights() will make the ambulance lights flash 
so cars will know to get out of its way!
**********/
int FlashLights() {
     int lightcolor = rand() % 4;
     switch (lightcolor) {
            case 0: lightcolor = TRUERED;
                 break;
            case 1: lightcolor = ORANGE;
                 break;
            case 2: lightcolor = TRUERED;
                 break;
            case 3: lightcolor = TRUEBLUE;
                 break;
            default: lightcolor = TRUERED;
            }
     return lightcolor; 
}

void DrawAccident(double x, double y) {
     SetPenColorHex(TRUERED);
     DrawFillCircle(x, y, 20);   
     SetPenColorHex(YELLOW);
     DrawFillCircle(x-20, y+10, 30);   
     SetPenColorHex(ORANGE);
     DrawFillCircle(x+10, y-30, 10);   
     SetPenColorHex(YELLOW);
     DrawFillCircle(x-5, y+15, 15);   
     SetPenColorHex(TRUERED);
     DrawFillCircle(x+15, y-20, 20);       
}


void Game_Over(void) {
    printf("\n\n\n\nYou had too many accidents!  Game Over!\n\n\n Your final score was: %.0f\n\n\n", score);
    exit(0);
} 
