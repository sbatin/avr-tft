#ifndef UTFT_h
#define UTFT_h

#define PORTRAIT  0
#define LANDSCAPE 1

// VGA color palette
#define VGA_BLACK	0x0000
#define VGA_WHITE	0xFFFF
#define VGA_RED		0xF800
#define VGA_GREEN	0x0400
#define VGA_BLUE	0x001F
#define VGA_SILVER	0xC618
#define VGA_GRAY	0x8410
#define VGA_MAROON	0x8000
#define VGA_YELLOW	0xFFE0
#define VGA_OLIVE	0x8400
#define VGA_LIME	0x07E0
#define VGA_AQUA	0x07FF
#define VGA_TEAL	0x0410
#define VGA_NAVY	0x0010
#define VGA_FUCHSIA	0xF81F
#define VGA_PURPLE	0x8010

#define P_RS  PORTC
#define P_WR  PORTC
#define P_CS  PORTC
#define P_RST PORTC

#define R_RS  DDRC
#define R_WR  DDRC
#define R_CS  DDRC
#define R_RST DDRC

#define B_RS  (1 << PC5)
#define B_WR  (1 << PC4)
#define B_CS  (1 << PC3)
#define B_RST (1 << PC2)

#include "Arduino.h"

#define cbi(reg, bitmask) reg &= ~bitmask
#define sbi(reg, bitmask) reg |= bitmask
#define pulse_high(reg, bitmask) sbi(reg, bitmask); cbi(reg, bitmask);
#define pulse_low(reg, bitmask) cbi(reg, bitmask); sbi(reg, bitmask);

#define swap(type, i, j) {type t = i; i = j; j = t;}

#define fontbyte(x) pgm_read_byte(&cfont.font[x])

struct _current_font {
	uint8_t* font;
	uint8_t x_size;
	uint8_t y_size;
	uint8_t offset;
	uint8_t numchars;
};

class UTFT {
	public:
		UTFT(int RS, int WR, int CS, int RST);
		void InitLCD(byte orientation=LANDSCAPE);
		void clrScr();
		void drawPixel(int x, int y);
		void drawLine(int x1, int y1, int x2, int y2);
		void fillScr(byte r, byte g, byte b);
		void fillScr(word color);
		void drawRect(int x1, int y1, int x2, int y2);
		void fillRect(int x1, int y1, int x2, int y2);
		void setColor(byte r, byte g, byte b);
		void setColor(word color);
		void setBackColor(byte r, byte g, byte b);
		void setBackColor(uint32_t color);
		void print(char *st, int x, int y);
		void print(String st, int x, int y);
		void setFont(uint8_t* font);

        private:
		byte		fch, fcl, bch, bcl;
		byte		orient;
		long		disp_x_size, disp_y_size;
		_current_font	cfont;

		void LCD_Writ_Bus(char VH, char VL);
		void LCD_Write_COM(char VL);
		void LCD_Write_DATA(char VH, char VL);
		void LCD_Write_DATA(char VL);
		void setPixel(word color);
		void drawHLine(int x, int y, int l);
		void drawVLine(int x, int y, int l);
		void printChar(byte c, int x, int y);
		void setXY(word x1, word y1, word x2, word y2);
		void clrXY();
		void _fast_fill_16(char ch, char cl, long pix);
};

#endif
