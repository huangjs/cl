/* EECS 110: Intro to Computer Programming
   Programming Assignment 2: Problem #2: Graphics Project
   
   Josh Stroud
   Due 02.21.07
*/

/* Correction Log
   01.17.07 Built program
   01.22.07 Placed house drawing into a function, definite function's variables
   
   01.29.07 Changed house to appear in a random location, with random size and color
   01.30.07 Fixed color functions and called arrays
   01.31.07 Made random functions very simple
   
   02.18.07 Created arrays
   02.19.07 Made houses move
   02.20.07 Cleaned up code
   
   03.07.07 Put in structures, determined largest house, made interactive, attempted intro and win screens
   03.08.07 Fixed LargestHouse computation and calls to restart game, message screens still not working
*/


#include "introGlutLib.h"		//include the basic drawing library
#include <time.h>				// for MS Windows time-finding functions
#include <stdlib.h>				// for malloc and free
#include <string.h>				// for strcpy and other string fcns.

// Constant Defines
#define START_NUM_HOUSES 5
#define MAX_HOUSES 10
#define MIN_HOUSES 3
#define MAX_HOUSE_SIZE 100
#define MIN_HOUSE_SIZE 40

#define SPEED_B 4.65
#define SPEED_M 0.033

#define GO_LEFT -1
#define GO_RIGHT 1
#define GO_DOWN -1
#define GO_UP 1

#define YES 1
#define NO 0

#define COLOR_CONST 255
#define COLOR_HOUSE_BACKGROUND 0x00FF66
#define COLOR_TEXT 0x2F4F4F
#define COLOR_BOX 0xFDF8FF
#define COLOR_CHIMNEY 0x660000
#define COLOR_SMOKE 0xCCCCCC
#define COLOR_WINDOW_GLASS 0xFEF0DB
#define COLOR_WINDOW_FRAME 0x000000

#define PAUSE_INTRO 5000
#define PAUSE_STANDARD 2000
#define TEXT_X 50
#define TEXT_Y NU_SCREENHEIGHT - 50

// Display Type Enumeration
typedef enum{INTRO, PAUSE, RUN} DISPLAY_TYPE;

DISPLAY_TYPE display_type = INTRO;

// House Structure
typedef struct
{
     double size, x_pos, y_pos, speed;
     unsigned long color_body, color_roof, color_door;
     int x_dir, y_dir;
} HOUSE;

// Global Data
int number_houses = START_NUM_HOUSES;
int largest_house;
long int score = 0;

HOUSE houses[MAX_HOUSES];

// Global Fuctions
void GenerateRandomHouses(void);
void GenerateRandomHouse(int i);
void LargestHouse(void);
void FindLargestHouse(int cur);
void DrawRandomHouses(void);
void MoveHouses(void);
void ChangeXPos(int i);
void ChangeYPos(int i);
int RandomInt(int min, int max);
int RandomColor(void);

void DrawHouse(int xorigin, int yorigin, int width, unsigned long colorbody, unsigned long colorroof, unsigned long colordoor);

void IntroMessage(void);
void DrawIntroMessage(void);
void WinMessage(void);

void NewGame(void);
void IsIntersection(int x, int y);
int CheckX(int x);
int CheckY(int y);
void DecreaseHouses(void);
void IncreaseHouses(void);

/***************************************************************
 Main function
***************************************************************/			
int main()
{
	srand(time(NULL));
	NewGame();
    InitGraphics();
	return 0;
}

/***************************************************************
 Creates a new game
***************************************************************/

void NewGame(void)
{
    GenerateRandomHouses();
	LargestHouse();
}

/***************************************************************
 Clears screne and draws multiple houses.
***************************************************************/

void GenerateRandomHouses()
{     
     for(int i = 0; i < number_houses; i++)
     {
         GenerateRandomHouse(i); 
     }
}

void GenerateRandomHouse(int i)
{
     HOUSE *house = &houses[i];
     house->size = RandomInt(MIN_HOUSE_SIZE, MAX_HOUSE_SIZE);
     house->x_pos = RandomInt(house->size, (NU_SCREENWIDTH - house->size * 2));
     house->y_pos = RandomInt(house->size, (NU_SCREENHEIGHT - house->size * 2));
     
     house->color_body = RandomColor();
     house->color_roof = RandomColor();
     house->color_door = RandomColor();
     
     house->speed = SPEED_B - SPEED_M * house->size;
     house->x_dir = rand() % 1 ? GO_LEFT : GO_RIGHT;
     house->y_dir = RandomInt(0,1) ? GO_DOWN : GO_UP;
}

/***************************************************************
 Determines the largest house
***************************************************************/

void LargestHouse(void)
{
    largest_house = 0;
    
    for(int cur = 1; cur < number_houses; ++cur)
    {
        FindLargestHouse(cur);
    }
}

void FindLargestHouse(int cur)
{
    if(houses[cur].size > houses[largest_house].size)
    {
        largest_house = cur;
    }
}

/***************************************************************
 myDisplay()
 
 Draws multiple houses in random locations.
***************************************************************/

void myDisplay(void)
{
    if(display_type == INTRO)
    {
        static int intro_count = 0;
        if(intro_count == 0)
        {
            IntroMessage();
            intro_count++;
        }
        else if(intro_count == 1)
        {
            Pause(PAUSE_INTRO);
            intro_count++;
            display_type = RUN;
        }
    }
    else if(display_type == PAUSE)
    {
        Pause(PAUSE_STANDARD);
        display_type = RUN;
    }
    else if(display_type == RUN)
    {
        ClearWindow();
    	SetBackgndColorHex(COLOR_HOUSE_BACKGROUND);
        DrawRandomHouses();
        MoveHouses();
    }
     
    return;
}

/***************************************************************
 Displays Intro Message at start of game
          "Click on the largest house to win!"
***************************************************************/

void IntroMessage(void)
{
    ClearWindow();
    DrawIntroMessage();
}

void DrawIntroMessage(void)
{
    SetBackgndColorHex(COLOR_BOX);
    SetPenColorHex(COLOR_TEXT);
    DrawText2D(helv18, TEXT_X, TEXT_Y, "INSTRUCTIONS:");
    DrawText2D(helv18, TEXT_X, TEXT_Y * 2, "--Click on the largest house to win!!");
    DrawText2D(helv18, TEXT_X, TEXT_Y * 3, "--Press the right or left arrows for more or less houses");
    DrawText2D(helv18, TEXT_X, TEXT_Y * 4, "--Press any other key to exit");
}

/***************************************************************
 Draws multiple houses in randoms locations.
***************************************************************/

void DrawRandomHouses(void)
{
     for(int i = 0; i < number_houses; ++i)
     {
         HOUSE *house = &houses[i];
         DrawHouse(house->x_pos, house->y_pos, house->size, house->color_body, house->color_roof, house->color_door);
     }
}

/***************************************************************
 Move all houses along both x and y axes, as well as flucuate size.
***************************************************************/

void MoveHouses(void)
{
     for(int i = 0; i < number_houses; ++i)
     {
         ChangeXPos(i);
         ChangeYPos(i);
     }
}

void ChangeXPos(int i)
{
     HOUSE *house = &houses[i];
     if(house->x_pos < 0 || house->x_pos > NU_SCREENWIDTH)
     {
         house->x_dir *= -1;
     }
     
     house->x_pos += house->speed * house->x_dir;
}

void ChangeYPos(int i)
{
     HOUSE *house = &houses[i];
     if(house->y_pos < 0 || house->y_pos > NU_SCREENHEIGHT)
     {
         house->y_dir *= -1;
     }
     
     house->y_pos += house->speed * house->y_dir;
}

/***************************************************************
 Generate random integers and colors.
***************************************************************/

int RandomInt(int min, int max)
{
    int random_number = rand() % ((max + 1) - min) + min;
    return random_number;
}
        
int RandomColor(void)
{
     int red = rand() % COLOR_CONST;
     int green = rand() % COLOR_CONST;
     int blue = rand() % COLOR_CONST;
     return red * (COLOR_CONST + 1) * (COLOR_CONST + 1) + green * (COLOR_CONST + 1) + blue;
}

/***************************************************************
 Draws a house.
***************************************************************/

void DrawHouse(int xorigin, int yorigin, int width, unsigned long colorbody, unsigned long colorroof, unsigned long colordoor)
{
    double height = width * 4 / 5;
    double leftside = xorigin - width / 2;
    double rightside = xorigin + width / 2;
    double bottomside = yorigin - height / 2;
    double topside = yorigin + height / 2;
    
	// filled body square of house
	SetPenColorHex(colorbody);
	DrawFillBox(leftside, bottomside, rightside, topside);
	
	// filled chimney rectangle of house
	SetPenColorHex(COLOR_CHIMNEY);
    DrawFillBox(xorigin + (width * 1.8/10), topside, rightside - (width * 1.8/10), yorigin + height);
    
    // smoke above chimney
	SetPenColorHex(COLOR_SMOKE);
	DrawFillCircle(xorigin + (width * 1/4), yorigin + height * 8/7, width / 20);
	DrawFillCircle(xorigin + (width * 1/5), yorigin + height * 8/7, width / 30);
	DrawFillCircle(xorigin + (width * 1/3.5), yorigin + height * 8/7, width / 30);
    
   	// filled roof triangle of house
	SetPenColorHex(colorroof);
	DrawFillTriangle(xorigin - width * 6 / 10, topside, xorigin + width * 6 / 10, topside, xorigin, yorigin + height);
		
	// filled door square of house
	SetPenColorHex(colordoor);
	DrawFillBox(leftside + (width / 12), bottomside, xorigin - (width / 15), topside - (height / 5));
    
    // filled window square of house
    SetPenColorHex(COLOR_WINDOW_GLASS);
    DrawFillBox(xorigin + (width / 10), bottomside + (height / 4), rightside - (width / 10), topside - (height / 2.5));
    
    // filled window circle of house
    DrawFillCircle(xorigin, yorigin + height * 3/4, width / 10);
    
    // frame of window square of house
    SetPenColorHex(COLOR_WINDOW_FRAME);
    DrawBox(xorigin + (width / 10), bottomside + (height / 4), rightside - (width / 10), topside - (height / 2.5));
    
    // frame of window circle of house
    DrawCircle(xorigin, yorigin + height * 3/4, width / 10);
    
    // window panes on window square of house
    DrawLine((xorigin + (width / 10) + rightside - (width / 10)) / 2, topside - (height / 2.5), (xorigin + (width / 10) + rightside - (width / 10)) / 2, bottomside + (height / 4));
    DrawLine(xorigin + (width / 10), (bottomside + (height / 4) + topside - (height / 2.5)) / 2, rightside - (width / 10), (bottomside + (height / 4) + topside - (height / 2.5)) / 2);
    
	// window panes on circle window of house
	DrawLine(xorigin, (yorigin + height * 3/4) + (width / 10), xorigin, (yorigin + height * 3/4) - (width / 10));
	DrawLine(xorigin + (width / 10), yorigin + height * 3/4, xorigin - (width / 10), yorigin + height * 3/4);
	
	// doorknob on door of house
	DrawFillCircle(xorigin - (width / 10), (bottomside + (topside - (height / 5))) / 2, width / 50);
}

/**********************************************************************
 Check if clicked mouse button is touching largest house
***********************************************************************/

void IsIntersection(int x, int y)
{
    int win_x = CheckX(x);
    int win_y = CheckY(y);
    
    if(win_x == YES && win_y == YES)
    {
        WinMessage();
        display_type = PAUSE;
        NewGame();
    }
}

int CheckX(int x)
{
    HOUSE *house = &houses[largest_house];
    if(x >= (house->x_pos - house->size / 2) && x <= (house->x_pos + house->size / 2))
    {
        return YES;
    }
    
    return NO;
}

int CheckY(int y)
{
    HOUSE *house = &houses[largest_house];
    if(y >= (house->y_pos - house->size / 2) && y <= (house->y_pos + house->size))
    {
        return YES;
    }
    
    return NO;
}

/**********************************************************************
 Displays "You Win"
***********************************************************************/    

void WinMessage(void)
{    
    SetPenColorHex(COLOR_BOX);
    DrawFillBox(TEXT_X * 3, TEXT_Y * 5, TEXT_X * 8, TEXT_Y);
    SetPenColorHex(COLOR_TEXT);
    DrawBox(TEXT_X * 3, TEXT_Y * 5, TEXT_X * 8, TEXT_Y);
    DrawText2D(helv18, TEXT_X * 4, TEXT_Y * 2, "********************");
    DrawText2D(helv18, TEXT_X * 4, TEXT_Y * 3, "****You Win!!!****");
    DrawText2D(helv18, TEXT_X * 4, TEXT_Y * 4, "********************");
}

/**********************************************************************
 Makes a new game with less or more houses
***********************************************************************/ 

void DecreaseHouses(void)
{
    if(number_houses > MIN_HOUSES)
    {
        number_houses--;
        NewGame();
    }
}

void IncreaseHouses(void)
{
    if(number_houses < MAX_HOUSES)
    {
        number_houses++;
        NewGame();
    }
}

/**********************************************************************
 myMouse(button, state, x, y)
 
 Exit the program if the user clicks the mouse.
***********************************************************************/
			
void myMouse(int button, int state, int x, int y) 
{
    if(button == GLUT_LEFT_BUTTON && state == GLUT_UP)
    {
        IsIntersection(x, y);
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
	if(key == GLUT_KEY_LEFT)
    {
        DecreaseHouses();
    }
    else if(key == GLUT_KEY_RIGHT)
    {
        IncreaseHouses();
    }
}
