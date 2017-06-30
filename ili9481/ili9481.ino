#include "UTFT.h"

UTFT myGLCD(A5, A4, A3, A2);

extern uint8_t SmallCDU[];
extern uint8_t CDU[];

void setup() {
  myGLCD.InitLCD(PORTRAIT);
}

uint8_t x1 = 025;
uint8_t x2 = 180;

#define FONT_SIZE 20
#define POINT_OFFSET 4
#define POINT_SIZE (FONT_SIZE - 2 * POINT_OFFSET)

void printValue(uint16_t value, uint8_t n, int x, int y) {
    uint32_t divider = 1;

    for (uint8_t i = 0; i < n; i++) {
        char ch = value % (divider * 10) / divider;
        myGLCD.printChar(ch + '0', x + FONT_SIZE * (n - i - 1), y);
        divider*= 10;
    }
}

void printFrequency(uint16_t value1, uint16_t value2, int n, int x, int y) {
    myGLCD.printChar('.', x + FONT_SIZE * 3 - POINT_OFFSET, y);
    printValue(value1, 3, x, y);
    printValue(value2, n, x + FONT_SIZE * 3 + POINT_SIZE, y);
}

void printVertical(char *st, int x, int y) {
    int stl, i;

    stl = strlen(st);

    for (i = 0; i < stl; i++)
        myGLCD.printChar(*st++, x, y + i * 25);
}

void loop() {
  myGLCD.clrScr();

  myGLCD.setBackColor(VGA_BLACK);
  myGLCD.setColor(VGA_WHITE);
  myGLCD.setFont(SmallCDU);

  printVertical("VHF1", 5, 50);
  printVertical("VHF2", 165, 50);
  
  myGLCD.print("VHF1", x1 - 10, 10);
  myGLCD.print("NAV1", x1 - 10, 170);
  myGLCD.print("ADF1", x1 - 10, 330);
  myGLCD.print("VHF2", x2 - 10, 10);
  myGLCD.print("NAV2", x2 - 10, 170);
  myGLCD.print("ATC1", x2 - 10, 330);
  myGLCD.drawLine(160,0,160,479);
  myGLCD.drawLine(161,0,161,479);
  myGLCD.drawLine(0,160,319,160);
  myGLCD.drawLine(0,161,319,161);
  myGLCD.drawLine(0,320,319,320);
  myGLCD.drawLine(0,321,319,321);

  myGLCD.setFont(CDU);
  // active freq
  myGLCD.setColor(VGA_LIME);
  printFrequency(108, 250, 3, x1, 50);
  printFrequency(118, 700, 3, x1, 210);
  printFrequency(226, 0, 1, x1, 370);
  printFrequency(108, 250, 3, x2, 50);
  printFrequency(114, 600, 3, x2, 210);
  myGLCD.print("12 00", x2, 370);

  // standby freq
  myGLCD.setColor(VGA_AQUA);
  printFrequency(103, 100, 3, x1, 110);
  printFrequency(109, 300, 3, x1, 270);
  printFrequency(127, 3, 1, x1, 430);
  printFrequency(103, 100, 3, x2, 110);
  printFrequency(109, 300, 3, x2, 270);
  myGLCD.print("TA/RA", x2, 430);

  // selected freq
  myGLCD.setColor(VGA_WHITE);
  myGLCD.setBackColor(VGA_GRAY);
  for (uint16_t i = 113; i < 226; i++) {
    for (uint8_t j = 0; j < 10; j++) {
      printFrequency(i, j, 1, x1, 430);
      delay(50);
    }
  }

  delay(30000);
}
