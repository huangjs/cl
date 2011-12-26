/* This file is a CS110 graphic program.
   This is a code written by Matt Markezin-Press and Esra Belkis Saydam
   
   -- Random Objects
   -- A giraffe 
   -- Animated Birds
   -- Interactive, animated sun, river and the neck of the giraffe
   Mouse clicks:
         Left button-up will make the neck of the giraffe turn back and forth
         Right button-down will make the neck of the giraffe go down towards the river 
         to drink water.
   Special keys:
         Key_down will make the sun go down from upper-left to lower-right and
         the moon will appear as the sky get darker
         Key_right will make the river flow diagonally from left to right
           
*/


#include "introGlutLib.h"		//include the basic drawing library
#include <time.h>				// for MS Windows time-finding functions
#include <stdlib.h>				// for malloc and free
#include <string.h>				// for strcpy and other string fcns.

#define max_clouds 20
#define min_clouds 10
#define max_trees 5
#define min_trees 2
#define max_birds 30
#define max_objects 50
#define min_birds 15
#define go_left -1
#define go_right 1

typedef struct
{
        int type;
        double x_pos, y_pos, size, speed;
        unsigned long color;
        int dir;
} ELEMENT;

int num_elements = 0;

ELEMENT elements[max_birds];
/*******************************************************************************
Prototypes of the Functions
*******************************************************************************/

void drawGiraffe(double x_start, double y_start, double width, unsigned long color);
unsigned long random_rgb(void);
void draw_headneck (double x_neck, double y_neck, double size, int dir);
void init_headneck();
void init_clouds();
void init_cloud(int callC);
void draw_cloud(double x_cloud1, double y_cloud1, double size_cloud1);
int totalclouds = 0;
void draw_sky();
void draw_grass();
void draw_rand_grass();
void draw_river(double x, double y, double size);
void init_river();
void move_river(ELEMENT *river);
void draw_drink(double x_neck, double y_neck, double size);
void init_drink();
void initialize_sun();
void init_sun(int callS);
void move_sun(ELEMENT *sun);
void draw_sun(double x_sun, double y_sun, double sun_size, unsigned long sun_color);
void init_sky(int day);
int night1();
int night2();
int night3();
int night;
void draw_moon();
void call_trees();
void draw_tree(double x_tree1, double y_tree1, double size_tree1);
void init_trees();
void init_tree(int callT);
int totaltrees = 0;
double x_tree[max_trees];
double y_tree[max_trees];
double size_tree[max_trees];
void init_birds();
void init_bird(int call);
void draw_bird(double x_bird, double y_bird, double size_bird, double direction, long int colorb);
void draw_bird_head(double x_bird, double y_bird, double size_bird, double direction, long int colorb);
void draw_bird_wing(double x_bird, double y_bird, double size_bird, double direction, long int colorb);
void draw_bird_tail(double x_bird, double y_bird, double size_bird, double direction, long int colorb);
void draw_bird_beek(double x_bird, double y_bird, double size_bird, double direction, long int colorb);
void move_birds(ELEMENT *bird);

void move_elements();
void draw_elements();

int randint(int low, int high);

const unsigned long BEEK = 0x663333;
const unsigned long WHITE = 0xFFFFFF;
const unsigned long GREEN = 0x006600;
const unsigned long BROWN = 0x996600;
const unsigned long ORANGE = 0xFF9900;
const unsigned long RED = 0xEE0000;
const unsigned long DAY = 0x66CCFF;
const unsigned long NIGHT1 = 0x0000FF;
const unsigned long NIGHT2 = 0x330066;
const unsigned long NIGHT3 = 0x330033;
const unsigned long MOON = 0xFFFFCC;


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
    int day;
    srand(time(NULL));    
    init_birds();
    init_clouds();
    init_trees();
    init_headneck();
	InitGraphics();			// start GLUT/OpenGL
	return 0;
}


void myDisplay(void)
{
     ClearWindow();
     draw_sky();
     draw_grass();
     call_trees();    
     draw_elements();
     move_elements();
     drawGiraffe(300, 100, 200, 0xFFCC00);     
}

/*Sun*/
void initialize_sun()
{
    for(int i = 1; i < 2; ++i)
      {
      init_sun(i);
      }
}

void init_sun(int callS)
{
    ELEMENT *sun = &elements[1];
    sun->type = 6;
    sun->x_pos = 150;
    sun->y_pos = 450;
    sun->size = 40;
    sun->color = 0xFFFF33;
}

void draw_sun(double x_sun, double y_sun, double sun_size, unsigned long sun_color)
{
     SetPenColorHex(sun_color);
     DrawFillCircle(x_sun, y_sun, sun_size);
}

void init_sky(int day)
{
     ELEMENT *sky = &elements [47];
     sky->type = 5;
     sky->color = day;    
}

int night1()
{   
    night = 1;
    return 1;   
}

int night2()
{
    night = 2;
    return 1;
}

int night3()
{
    night = 3;
    return 1;
}

void draw_moon()
{    
     night = 4;
}    

void draw_sky()
{
     switch(night)
     {
     case 4: 
          SetPenColorHex(NIGHT3);
          DrawFillBox(0, 200, 800, 500);
          SetPenColorHex(MOON);
          DrawFillCircle(200, 400, 40);
          SetPenColorHex(NIGHT3);
          DrawFillCircle(220, 400, 35);
          break;
     case 3:     
          SetPenColorHex(NIGHT3);
          DrawFillBox(0, 200, 800,500);
          break;
     case 2:           
          SetPenColorHex(NIGHT2);
          DrawFillBox(0, 200, 800,500);
          break;
     case 1:
          SetPenColorHex(NIGHT1);
          DrawFillBox(0, 200, 800,500);
          break;
     default:
          SetPenColorHex(DAY);
          DrawFillBox(0, 200, 800, 500);   
     }
}

/*Trees*/
void init_trees()
{
    totaltrees = randint(min_trees, max_trees);    
    for (int i = 0; i < totaltrees; ++i)
        {
        init_tree(i);
        }
}

void init_tree(int callT)
{
     int big_tree = randint(50, 75);
     size_tree[callT] = big_tree;
     y_tree[callT] = randint(100, 200);
     x_tree[callT] = randint(50, 750);
}

void call_trees()
{
     for(int i = 0; i < totaltrees; ++i)
        {
        draw_tree(x_tree[i], y_tree[i], size_tree[i]);
        }
}


void draw_tree(double x_tree1, double y_tree1, double size_tree1)
{
     int x_bot_tree = x_tree1;
     int y_bot_tree = y_tree1;
     int width = x_tree1 + size_tree1 * .30;
     int height = y_tree1 + size_tree1;
     int leafL = x_bot_tree - size_tree1 * .33;
     int leafR = x_bot_tree + size_tree1 * .66;
     SetPenColorHex(BROWN);
     DrawFillBox(x_bot_tree, y_bot_tree, width, height);
     SetPenColorHex(GREEN);
     DrawFillTriangle(leafL, height, leafR, height, x_bot_tree + size_tree1 * .15, height + size_tree1 * 1.2);
     SetPenColorHex(RED);
     DrawFillCircle(leafL + 20, height + 10, size_tree1 * .1);   
     DrawFillCircle(leafL + 30, height + 12, size_tree1 * .07);
     DrawFillCircle(leafR - 25, height + 28, size_tree1 * .12);
}

void init_headneck()
{
     ELEMENT *headneck = &elements [4];
     headneck->type = 3;
     headneck->speed = 3;
     headneck->x_pos = 300;
     headneck->y_pos = 100;
     headneck->size = 200;
     headneck->dir = 0.35;
     headneck->dir = randint(0,1) ? go_left : go_right;
}


void draw_headneck (double x_neck, double y_neck, double size, int dir)
{
    double height = 100;
/* The Head */
    size = size * dir;
    SetPenColorHex (0xFFCC00);              
    DrawFillTriangle(x_neck - size * 3/4, y_neck + height * 5/2,
                     x_neck - size/4, y_neck + height * 5/2,
                     x_neck - size * 3/16, y_neck + height * 13/4);
    DrawFillCircle (x_neck - size * 41/64, y_neck + height * 41/16, height * 9/40);
/* The Nose */
    SetPenColorHex (0x996600);
    DrawFillBox (x_neck - size * 40/64, y_neck + height * 40/16, x_neck - size * 38/64, y_neck + height * 42/16);
    DrawFillBox (x_neck - size * 47/64, y_neck + height * 41/16, x_neck - size * 45/64, y_neck + height * 43/16);
/*The Neck*/
    SetPenColorHex(0xFFCC00);
    DrawFillTriangle(x_neck, y_neck + height/2, 
                     x_neck, y_neck + height, 
                     x_neck - size/2 , y_neck + height * 5/2);
    DrawFillTriangle(x_neck - size/2, y_neck + height * 5/2,
                     x_neck - size/4, y_neck + height * 5/2,
                     x_neck, y_neck + height);
/* The Ears */
    SetPenColorHex (0xFFCC00);
    DrawFillTriangle (x_neck - size * 3/16, y_neck + height * 48/15,
                     x_neck - size * 1/4, y_neck + height * 7/2,
                     x_neck - size * 5/16, y_neck + height * 40/15);             
    SetPenColorHex (0xFFCC00);
    DrawFillTriangle(x_neck - size * 5/16, y_neck + height * 44/15,
                     x_neck - size * 3/8, y_neck + height * 50/15,
                     x_neck - size * 14/32, y_neck + height *44/15);
/* The Inner Parts Of The Ears */
    SetPenColorHex (0x996600);
    DrawFillTriangle (x_neck - size * 13/64, y_neck + height * 47/15,
                     x_neck - size * 1/4, y_neck + height * 7/2,
                     x_neck - size * 8/32, y_neck + height * 47/15);                     
    DrawFillTriangle (x_neck - size * 23/64, y_neck + height * 45/15,
                      x_neck - size * 3/8, y_neck + height * 49/15,  
                      x_neck - size * 26/64, y_neck + height * 45/15);
/* The Horns */
    SetPenColorHex (0xFFCC00);
    DrawFillBox (x_neck - size * 29/96, 
                 y_neck + height * 11/4, 
                 x_neck - size * 27/96, 
                 y_neck + height * 53/16);
    DrawFillBox (x_neck - size * 32/96, 
                 y_neck + height * 11/4, 
                 x_neck - size * 30/96, 
                 y_neck + height * 52/16);
    SetPenColorHex (0x663300);
    DrawFillCircle (x_neck - size * 28/96, 
                    y_neck + height * 53/16,
                    height/30);
    DrawFillCircle (x_neck - size * 31/96, 
                    y_neck + height * 52/16, 
                    height/30);
/* Spots On The Neck*/
    DrawFillCircle(x_neck - size * 3/32, y_neck + height * 5/4, height * 1/12);
    DrawFillCircle(x_neck - size * 5/32, y_neck + height * 13/8, height * 1/12);
    DrawFillCircle(x_neck - size * 10/32, y_neck + height * 17/8, height * 1/6);

/*The Eyes*/
    SetPenColorHex (0xFFFFCC);
    DrawFillCircle (x_neck - size * 6/16, y_neck + height * 42/15, height * 1/16);
    SetPenColorHex (0x000000); 
    DrawFillCircle(x_neck - size *6/16, y_neck + height * 42/15, height * 1/32);
    
    /*The Mouth*/
   SetPenColorHex (0xFF3332);
   DrawFillTriangle(x_neck - size * 23/32, y_neck + height * 48/20,
                    x_neck - size * 21/32, y_neck + height * 48/20,
                    x_neck - size * 16/32, y_neck + height * 38/15);
}
     
/*Birds*/
void init_birds()
{
     num_elements = randint(min_birds, max_birds);
     for (int i = totalclouds; i < num_elements; ++i)
         {
         init_bird(i);
         }
}

void init_bird(int call)
{
     int big = randint(20, 200);
     ELEMENT *bird = &elements [call];
     bird->size = big;
     bird->type = 1;
     bird->x_pos = randint(big, 800 - big);
     bird->y_pos = randint(200, 500 - big);
     bird->speed = (10 - .045 * big);
     bird->dir = randint(0,1) ? go_left : go_right;
     bird->color = random_rgb();
}

/*Clouds*/
void init_clouds()
{
    totalclouds = randint(min_clouds, max_clouds);    
    for (int i = 2; i < totalclouds; ++i)
        {
        init_cloud(i);
        }
}

void init_cloud(int callC)
{
     int big_cloud = randint(25, 100);
     ELEMENT *cloud = &elements[callC];
     cloud->type = 5;
     cloud->size = big_cloud;
     cloud->x_pos = randint(350, 750);
     cloud->y_pos = randint(300, 790);
}

void draw_cloud(double x_cloud1, double y_cloud1, double size_cloud1)
{
     int cloud_centX = x_cloud1;
     int cloud_centY = y_cloud1;
     int cloud_rad = size_cloud1;
     SetPenColorHex(WHITE);
     DrawFillCircle(cloud_centX, cloud_centY , cloud_rad * .6);
     DrawFillCircle(cloud_centX - 40, cloud_centY + cloud_rad * .1, cloud_rad * .3);
     DrawFillCircle(cloud_centX + 40, cloud_centY + cloud_rad * .12 , cloud_rad * .3);     
}

void draw_river(double x, double y, double size)
{    
     SetPenColorHex(0x0000FF);
     DrawFillTriangle(x, y+100, x+size/15, y +150, x+size/2, y-90);   
}

void init_river()
{ 
     if (num_elements < max_objects)
         {
         ELEMENT *river = &elements[3];
         river->type = 4;
         river->size = 1500;
         river->x_pos = -1000;
         river->y_pos = 180;
         river->speed = 6;
         river->color = 0x9C661F;
         num_elements++;
         } 
}

void init_drink()
{    
     ELEMENT *drink = &elements [4];
     drink->type = 2;     
     drink->x_pos = 300;
     drink->y_pos = 100;
     drink->size = 200;
}

void draw_drink(double x_neck, double y_neck, double size)
{
    double height = 100;
/* The Head */
    SetPenColorHex (0xFFCC00);              
    DrawFillTriangle(x_neck - size * 3/4 - 80, y_neck +  height * 5/2 -330,
                     x_neck - size/4 - 80, y_neck +  height * 5/2 - 340,
                     x_neck - size * 3/16 - 80, y_neck +  height * 13/4 - 350);
    DrawFillCircle (x_neck - size * 41/64 - 80, y_neck  +  height * 41/16 - 340, height * 9/40);
/* The Nose */
SetPenColorHex (0x996600);
    DrawFillBox (x_neck - size * 40/64 - 80, y_neck + height * 40/16 - 340, x_neck - size * 38/64 - 80, y_neck + height * 42/16 - 340);
    DrawFillBox (x_neck - size * 47/64 - 80, y_neck + height * 41/16 - 340, x_neck - size * 45/64 - 80, y_neck + height * 43/16 - 340);
/* The Ears */
    SetPenColorHex (0xFFCC00);
    DrawFillTriangle (x_neck - size * 3/16 - 80, y_neck + height * 48/15 - 340,
                     x_neck - size * 1/4 - 80, y_neck + height * 7/2 - 340,
                     x_neck - size * 5/16 - 80, y_neck + height * 40/15 - 340);                 
    SetPenColorHex (0xFFCC00);
    DrawFillTriangle(x_neck - size * 5/16 - 80, y_neck + height * 44/15 - 340,
                     x_neck - size * 3/8 - 80, y_neck + height * 50/15 - 340,
                     x_neck - size * 14/32 - 80, y_neck + height *44/15 - 340);
/* The Inner Parts Of The Ears*/
    SetPenColorHex (0x996600);
    DrawFillTriangle (x_neck - size * 13/64 - 80, y_neck + height * 47/15 - 340,
                     x_neck - size * 1/4 - 80, y_neck + height * 7/2 - 340,
                     x_neck - size * 8/32 - 80, y_neck + height * 47/15 - 340);                     
    DrawFillTriangle (x_neck - size * 23/64 - 80, y_neck + height * 45/15 - 340,
                      x_neck - size * 3/8 - 80, y_neck + height * 49/15 - 340,  
                      x_neck - size * 26/64 - 80, y_neck + height * 45/15 - 340); 
/* The Horns */
    SetPenColorHex (0xFFCC00);
    DrawFillBox (x_neck - size * 29/96 - 80, 
                 y_neck + height * 11/4 - 340, 
                 x_neck - size * 27/96 - 80, 
                 y_neck + height * 53/16 - 340);
    DrawFillBox (x_neck - size * 32/96 - 80, 
                 y_neck + height * 11/4 - 340, 
                 x_neck - size * 30/96 - 80, 
                 y_neck + height * 52/16 - 340);
    SetPenColorHex (0x663300);
    DrawFillCircle (x_neck - size * 28/96 - 80, 
                    y_neck + height * 53/16 - 340,
                    height/30);
    DrawFillCircle (x_neck - size * 31/96 - 80, 
                    y_neck + height * 52/16 - 340, 
                    height/30);
/*The Eyes*/
    SetPenColorHex (0xFFFFCC);
    DrawFillCircle (x_neck - size * 6/16 - 80, y_neck + height * 42/15 - 340, height * 1/16);
    SetPenColorHex (0x000000); 
    DrawFillCircle(x_neck - size *6/16 - 80, y_neck + height * 42/15 - 340, height * 1/32);
    
/*The Mouth*/
   SetPenColorHex (0xFF3332);
   DrawFillTriangle(x_neck - size * 23/32 - 80, y_neck + height * 48/20 - 340,
                     x_neck - size * 21/32 - 80, y_neck + height * 48/20 - 340,
                     x_neck - size * 16/32 - 80, y_neck + height * 38/15 - 340);  
/*The Neck*/
    SetPenColorHex(0xFFCC00);
    DrawFillTriangle(x_neck, y_neck + height, 
                     x_neck, y_neck + height * 0.8, 
                     x_neck - size * 0.6 , y_neck - height * 0.8); 
    DrawFillTriangle(x_neck, y_neck+ height,
                     x_neck -  size * 0.8, y_neck - 0.8 * height,
                     x_neck - size * 0.6, y_neck - 0.8 * height);
/*Spots On The Neck*/
SetPenColorHex (0x663300);
DrawFillCircle(x_neck - size * 0.5, y_neck - height * 0.3, height * 1/8);
    DrawFillCircle(x_neck - size * 0.35, y_neck + height * 0.1, height * 1/12);
    DrawFillCircle(x_neck - size * 0.15, y_neck + height * 0.5, height * 1/12);
}

void draw_elements() 
{     
     for (int i = 0; i < max_objects; ++i)
         {
         ELEMENT *element = &elements[i];
            if (element->type == 6)
               {
               draw_sun(element->x_pos, element->y_pos, element->size, element->color);    
               }
            if (element->type == 2) 
               {
               draw_drink(element->x_pos, element->y_pos, element->size);
               }
            if (element->type == 1)
               {
               draw_bird(element->x_pos, element->y_pos, element->size, element->dir, element-> color);
               }
            if (element->type ==4) 
               {
               draw_river(element->x_pos, element->y_pos, element->size);
               }
            if (element->type == 5)
               {
               draw_cloud(element->x_pos, element->y_pos, element->size);
               }
            if (element->type == 3)
               {
               draw_headneck(element->x_pos, element->y_pos, element->size, element->dir);
               }   
         }
}

void move_elements() {
      for (int i = 0; i < num_elements; ++i) 
          {
          ELEMENT *element = &elements[i];
             switch(element->type)
                    {
                    case 1: move_birds(element); break;
                    case 6: move_sun(element); break;
                    case 4: move_river(element); break;
                    }
           }
}

void move_river(ELEMENT *river)
{
    if (river->x_pos < -100)
        {
        river->x_pos += river->speed;
        river->y_pos =river->y_pos - river->speed/4;
        }
}

void move_sun(ELEMENT *sun)
{
     if(sun->y_pos > -20000)
        {
        sun->y_pos -= 1.6;
        sun->x_pos += 5;
        night = 0;
        }
     
     if(sun->x_pos > 800)
         {
         night1();
         }
          
     if(sun->x_pos > 1000)
         {
         night2();
         }
     
     if(sun->x_pos > 1500)
         {
         night3();
         }
     
     if(sun->x_pos > 1500)
         {
         draw_moon();
         }     
}

void move_birds(ELEMENT *bird)
{
     if(bird->x_pos < 5 ||bird-> x_pos > 795)
         {
         bird->dir *= -1;
         }
     bird->x_pos += bird->speed * bird->dir * -1;
}

void draw_bird(double x_bird, double y_bird, double size_bird, double direction, long int colorb)
{
     draw_bird_head(x_bird, y_bird, size_bird, direction, colorb);
     draw_bird_wing(x_bird, y_bird, size_bird, direction, colorb);
     draw_bird_tail(x_bird, y_bird, size_bird, direction, colorb);
     draw_bird_beek(x_bird, y_bird, size_bird, direction, colorb);
}

void draw_bird_head(double x_bird, double y_bird, double size_bird, double direction, long int colorb)
{
     double x_head = x_bird;
     double y_head = y_bird;
     double rad = size_bird * .05;
     SetPenColorHex(WHITE);
     DrawFillCircle(x_head - (direction * .35 * size_bird), y_head, rad + 3);
     SetPenColorHex(colorb);
     DrawFillCircle(x_head - ((direction * .35 * size_bird) + 1), y_head, rad);     
}

void draw_bird_wing(double x_bird, double y_bird, double size_bird, double direction, long int colorb)
{
     double wing_size = size_bird * .3;
     double wing_base = x_bird - (direction * .29 * wing_size);
     double wing_base2 = x_bird - (direction * wing_size);
     double wing_root = y_bird;
     double wing_botX = x_bird + (direction * .5 * wing_size);
     double wing_botY = y_bird + (direction * wing_size);
     SetPenColorHex(colorb);
     DrawFillTriangle(wing_base, wing_root, wing_base2, wing_root, wing_botX, wing_botY);
     DrawFillTriangle(wing_base, wing_root, wing_base2, wing_root, wing_base2, wing_root + size_bird * .2);
}

void draw_bird_tail(double x_bird, double y_bird, double size_bird, double direction, long int colorb)
{
     double tail_size = size_bird * .1;
     double tail_baseY = y_bird;
     double tail_baseX = x_bird - (direction * tail_size);
     double tail_top = y_bird + (direction * tail_size * .7);
     double tail_bot = y_bird - (direction * tail_size * .7);
     double tail_x = x_bird; 
     SetPenColorHex(colorb);
     DrawFillTriangle(tail_baseX, y_bird, tail_x, tail_top, tail_x, tail_bot);
}

void draw_bird_beek(double x_bird, double y_bird, double size_bird, double direction, long int colorb)
{
     SetPenColorHex(colorb);
     double beekX = x_bird - (direction * .3 * size_bird);
     double beekX2 = x_bird - (direction * .4 * size_bird);
     double beek_top = y_bird + size_bird * .04;
     double beek_bot = y_bird - size_bird * .04;
     double beek_pointX = x_bird + direction * size_bird * .12;
     double beek_pointX2 = x_bird - direction * size_bird * .62;
     double beek_pointY = y_bird;
     double beek_pointY2 = y_bird;
     DrawFillTriangle(beekX, beek_top, beekX, beek_bot, beek_pointX, beek_pointY);
     SetPenColorHex(ORANGE);
     DrawFillTriangle(beekX2, beek_top, beekX2, beek_bot, beek_pointX2, beek_pointY2);
}


/*Background Grass*/
void draw_grass()
{
     SetPenColorHex(0xFFFF00);
     DrawFillBox(0,0,800,200);
}

/*******************************************************************************
The Giraffe
*******************************************************************************/
     void drawGiraffe(double x_start, double y_start, double width, unsigned long color)    
{
    double height = 100;
/*The Body*/
	SetPenColorHex(0xFFCC00);	
	DrawFillBox(x_start, y_start, x_start + width, y_start + height);
/*The Legs*/
    SetPenColorHex (color);
    DrawFillBox(x_start, y_start, x_start + width/10, y_start - height * 3/4);
    DrawFillBox(x_start + width *2/10, y_start, x_start + width * 3/10, y_start - height * 5/8);
    DrawFillBox(x_start + width, y_start, x_start + width * 9/10, y_start - height * 3/4);
    DrawFillBox(x_start + width * 7/10, y_start, x_start + width * 15/20, y_start - height * 1/2);
/* The Feet */
    SetPenColorHex (0x000000);
    DrawFillBox(x_start, y_start - height * 3/4, x_start + width/10, y_start - height * 7/8);
    DrawFillBox(x_start + width *2/10, y_start - height * 5/8, x_start + width * 3/10, y_start - height * 11/16);
    DrawFillBox(x_start + width, y_start - height * 3/4, x_start + width * 9/10, y_start - height * 7/8);
    DrawFillBox(x_start + width * 7/10, y_start - height * 1/2, x_start + width * 15/20, y_start - height * 9/16);
/*The Tail*/
    SetPenColorHex(color);
    DrawFillTriangle(x_start + width, y_start + height/2, 
    x_start + width, y_start + height * 1/4,
    x_start + width * 5/4, y_start - height * 1/2);
/* The Tail Edge*/    
    SetPenColorHex (0x000000);
    DrawFillCircle (x_start + width * 5/4, y_start - height * 1/2, height/15);
/* The Tail Spots */    
    SetPenColorHex(0x663300);
    DrawFillCircle(x_start + width * 33/32, y_start + height/4, height/19);
    DrawFillCircle(x_start + width * 9/8, y_start - height * 1/16, height/29);        
/*Spots On Body*/
    SetPenColorHex(0x663300);
    DrawFillCircle(x_start + width/8, y_start + height/4, height/10);
    DrawFillCircle(x_start + width/3, y_start + height/6, height/7);
    DrawFillCircle(x_start + width * 3/4, y_start + height * 6/7, height/15);
    DrawFillCircle(x_start + width * 1/2, y_start + height * 3/7, height/11);
    DrawFillCircle(x_start + width/7, y_start + height/4, height/8);
    DrawFillCircle(x_start + width * 6/11, y_start + height * 3/5, height/8);
    DrawFillCircle(x_start + width * 1/8, y_start + height * 3/4, height * 1/8);
    DrawFillCircle(x_start + width * 13/16, y_start + height * 3/4, height * 1/4);
    DrawFillCircle(x_start + width * 13/16, y_start + height * 1/4, height * 1/6);
/* The Spots On The Leg */
    SetPenColorHex(0x663300);
    DrawFillCircle(x_start + width/20, y_start - height/2, height/12);
    DrawFillCircle(x_start + width/20, y_start - height * 2/8, height/10);
    DrawFillCircle(x_start + width/4, y_start - height * 3/8, height/11);
    DrawFillCircle(x_start + width/4, y_start - height * 1/8, height/22);
    DrawFillCircle(x_start + width * 15/16, y_start - height * 1/16, height/18);
    DrawFillCircle(x_start + width * 23/32, y_start - height * 3/8, height/26);
    DrawFillCircle(x_start + width * 23/32, y_start - height * 1/8, height/25);
    DrawFillCircle(x_start + width * 47/64, y_start - height/4, height/27);
    DrawFillCircle(x_start + width * 15/16, y_start - height * 5/8, height/15);
/* Name the Giraffe*/
    SetPenColorHex (0x003333);
    DrawText2D(helv18, x_start + width * 1/2, y_start + height * 3/2, 
                       "Hi, my name is Frank the Giraffe!!!");    
}

/**********************************************************************
 myMouse(button, state, x, y)
 
 Exit the program if the user clicks the mouse.
***********************************************************************/
			
void myMouse(int button, int state, int x, int y) 
{
    if (button == GLUT_LEFT_BUTTON && state == GLUT_UP)
        {
        init_headneck();    
        }    
    if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN)
        {
        init_drink();
        }
    if (button == GLUT_RIGHT_BUTTON && state == GLUT_UP)
        {
        init_headneck();
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
     if (key == GLUT_KEY_DOWN)
        {
        initialize_sun();          
        }
     if (key == GLUT_KEY_RIGHT)
        {
        init_river();
        }
}

unsigned long random_rgb()
{
     int red = rand() % 255;
     int green = rand() % 255;
     int blue = rand() % 255;
     return red * 256 * 256 * 150 + green * 200 * 50 + blue * 0;
}

int randint(int low, int high)
{
    return low + rand() % (high - low + 1);
}


