
Here is some information on the test programs available in this directory.
You must first load the Graphic-Forms systems as described in the top-level
README file.


How To Run             Purpose
------------------------------------------------------------------------------

(gft:drawing-tester)   Provides a test case for each major drawing operation,
                       with variations for pen style, pen width, foreground,
                       and background color settings. Text mode also shows
                       features like tab expansion, mnemonics, and transparent
                       text.

(gft:event-tester)     Verifies that events are being delivered and processed.
                       Mouse events are visible if you press any button while
                       moving the mouse (of course, mouse move events do not
                       require a button press). Keyboard events are shown if
                       you type keys. Mousing over the menus and menu items
                       shows menu arming events. Also, this is one of the
                       test cases for setting menu options via DEFMENU.

(gft:hello-world)      A very basic sanity check that we are able to create
                       a window and repaint it when needed.

(gft:image-tester)     Tests the display of bitmaps in various sizes and
                       color depths, also showing transparency masks and the
                       resulting transparent backgrounds. Also, if you've
                       loaded the ImageMagick plugin as described in the
                       top-level README, you'll see a PNG image as well.

(gft:layout-tester)    Tests the flow layout manager. Explore the menu tree
                       to see what operations are possible. Try changing
                       layout settings and then resize the window.

(gft:scroll-tester)    Provides test cases for scrolling. Simple grid mode
                       displays a numbered, scrollable grid, where the
                       step size is 1 pixel. Text mode tests integral
                       scrolling and resizing support. When switching from
                       the simple grid to text mode, clicking on the window
                       border gets the window to reset to the proper boundary.

(gft:widget-tester)    This is a start at a generic widget-testing program.
                       Currently displays buttons, a check box, list boxes,
                       sliders, and scrollbars. This is also a test case for
                       heap layout.

(gft:windlg)           Provides a way to exercise custom dialogs, system
                       dialogs, and several window styles. Note that the
                       Borderless window is dismissed when you click within
                       it using the mouse.


[the end]
