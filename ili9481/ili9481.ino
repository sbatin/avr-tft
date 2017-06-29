#include "UTFT.h"

UTFT myGLCD(A5, A4, A3, A2);

extern uint8_t BigFont[];
extern uint8_t CDU[];

void setup() {
  myGLCD.InitLCD(PORTRAIT);
}

uint8_t x1 = 010;
uint8_t x2 = 170;

void loop() {
  myGLCD.clrScr();

  myGLCD.setBackColor(VGA_BLACK);
  myGLCD.setColor(VGA_WHITE);
  myGLCD.setFont(BigFont);
  myGLCD.print("VHF1",5,5);
  myGLCD.print("NAV1",5,165);
  myGLCD.print("ADF1",5,325);
  myGLCD.print("VHF2",165,5);
  myGLCD.print("NAV2",165,165);
  myGLCD.print("ATC1",165,325);
  myGLCD.drawLine(160,0,160,479);
  myGLCD.drawLine(161,0,161,479);
  myGLCD.drawLine(0,160,319,160);
  myGLCD.drawLine(0,161,319,161);
  myGLCD.drawLine(0,320,319,320);
  myGLCD.drawLine(0,321,319,321);

  myGLCD.setFont(CDU);
  // active freq
  myGLCD.setColor(VGA_LIME);
  myGLCD.print("108.25",x1,40);
  myGLCD.print("118.70",x1,200);
  myGLCD.print(" 226.0",x1,360);
  myGLCD.print("108.25",x2,40);
  myGLCD.print("118.70",x2,200);
  myGLCD.print(" 12 00",x2,360);

  // standby freq
  myGLCD.setColor(VGA_AQUA);
  myGLCD.print("103.10",x1,100);
  myGLCD.print("109.30",x1,260);
  myGLCD.print(" 127.3",x1,420);
  myGLCD.print("103.10",x2,100);
  myGLCD.print("109.30",x2,260);
  myGLCD.print(" TA/RA",x2,420);

  // selected freq
  myGLCD.setColor(VGA_WHITE);
  myGLCD.setBackColor(VGA_SILVER);
  for (int i = 113; i < 226; i++) {
    myGLCD.print(String(i),10,360);
    delay(100);
  }

  delay(30000);
}
