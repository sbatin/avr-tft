#include "UTFT.h"
#include <avr/pgmspace.h>

inline void write_data(char high, char low) {
    PORTD = high;
    PORTC &= 0xFC;
    PORTC |= (low >> 6) & 0x03;
    PORTB = low & 0x3F;
}

void UTFT::LCD_Writ_Bus(char VH, char VL) {
    write_data(VH, VL);
    pulse_low(P_WR, B_WR);
}

void UTFT::_fast_fill_16(char ch, char cl, long pix) {
    long blocks = pix / 16;

    write_data(ch, cl);

    for (int i = 0; i < blocks; i++) {
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
        pulse_low(P_WR, B_WR);
    }

    if ((pix % 16) != 0)
        for (int i = 0; i < (pix % 16) + 1; i++) {
            pulse_low(P_WR, B_WR);
        }
}

UTFT::UTFT(int RS, int WR, int CS, int RST) {
    disp_x_size = 319;
    disp_y_size = 479;

    DDRD = 0xFF;
    DDRB |= 0x3F;
    DDRC |= 0x03;
    R_RS |= B_RS;
    R_WR |= B_WR;
    R_CS |= B_CS;
    R_RST |= B_RST;
}

void UTFT::LCD_Write_COM(char VL) {
    cbi(P_RS, B_RS);
    LCD_Writ_Bus(0x00, VL);
}

void UTFT::LCD_Write_DATA(char VH, char VL) {
    sbi(P_RS, B_RS);
    LCD_Writ_Bus(VH, VL);
}

void UTFT::LCD_Write_DATA(char VL) {
    sbi(P_RS, B_RS);
    LCD_Writ_Bus(0x00, VL);
}

void UTFT::InitLCD(byte orientation) {
    orient = orientation;

    sbi(P_RST, B_RST);
    delay(5);
    cbi(P_RST, B_RST);
    delay(15);
    sbi(P_RST, B_RST);
    delay(15);

    cbi(P_CS, B_CS);

    // exit sleep mode
    LCD_Write_COM(0x11);
    delay(20);

    // power setting
    LCD_Write_COM(0xD0);
    LCD_Write_DATA(0x07);
    LCD_Write_DATA(0x42);
    LCD_Write_DATA(0x18);

    // VCOM control
    LCD_Write_COM(0xD1);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x07);
    LCD_Write_DATA(0x10);

    // power setting for normal mode
    LCD_Write_COM(0xD2);
    LCD_Write_DATA(0x01);
    LCD_Write_DATA(0x02);

    // panel driving setting
    LCD_Write_COM(0xC0);
    LCD_Write_DATA(0x10);
    LCD_Write_DATA(0x3B);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x02);
    LCD_Write_DATA(0x11);

    // frame rate and inversion control
    LCD_Write_COM(0xC5);
    LCD_Write_DATA(0x03);

    // gamma seting
    LCD_Write_COM(0xC8);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x32);
    LCD_Write_DATA(0x36);
    LCD_Write_DATA(0x45);
    LCD_Write_DATA(0x06);
    LCD_Write_DATA(0x16);
    LCD_Write_DATA(0x37);
    LCD_Write_DATA(0x75);
    LCD_Write_DATA(0x77);
    LCD_Write_DATA(0x54);
    LCD_Write_DATA(0x0C);
    LCD_Write_DATA(0x00);

    // set address mode
    LCD_Write_COM(0x36);
    LCD_Write_DATA(0x0A);

    // set pixel format
    LCD_Write_COM(0x3A);
    LCD_Write_DATA(0x55);

    // set column address
    LCD_Write_COM(0x2A);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x01);
    LCD_Write_DATA(0x3F);

    // set page address
    LCD_Write_COM(0x2B);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x00);
    LCD_Write_DATA(0x01);
    LCD_Write_DATA(0xE0);
    delay(120);

    // set display on
    LCD_Write_COM(0x29);

    sbi(P_CS, B_CS);

    setColor(255, 255, 255);
    setBackColor(0, 0, 0);
    cfont.font=0;
}

void UTFT::setXY(word x1, word y1, word x2, word y2) {
    if (orient == LANDSCAPE) {
        swap(word, x1, y1);
        swap(word, x2, y2);
        y1 = disp_y_size - y1;
        y2 = disp_y_size - y2;
        swap(word, y1, y2);
    }

    LCD_Write_COM(0x2a);
    LCD_Write_DATA(x1 >> 8);
    LCD_Write_DATA(x1);
    LCD_Write_DATA(x2 >> 8);
    LCD_Write_DATA(x2);
    LCD_Write_COM(0x2b);
    LCD_Write_DATA(y1 >> 8);
    LCD_Write_DATA(y1);
    LCD_Write_DATA(y2 >> 8);
    LCD_Write_DATA(y2);
    LCD_Write_COM(0x2c);
}

void UTFT::clrXY() {
    if (orient == PORTRAIT)
        setXY(0, 0, disp_x_size, disp_y_size);
    else
        setXY(0, 0, disp_y_size, disp_x_size);
}

void UTFT::drawRect(int x1, int y1, int x2, int y2) {
    if (x1 > x2) {
        swap(int, x1, x2);
    }

    if (y1 > y2) {
        swap(int, y1, y2);
    }

    drawHLine(x1, y1, x2 - x1);
    drawHLine(x1, y2, x2 - x1);
    drawVLine(x1, y1, y2 - y1);
    drawVLine(x2, y1, y2 - y1);
}

void UTFT::fillRect(int x1, int y1, int x2, int y2) {
    if (x1 > x2) {
        swap(int, x1, x2);
    }

    if (y1 > y2) {
        swap(int, y1, y2);
    }

    cbi(P_CS, B_CS);
    setXY(x1, y1, x2, y2);
    sbi(P_RS, B_RS);
    _fast_fill_16(fch, fcl, (long(x2 - x1) + 1) * (long(y2 - y1) + 1));
    sbi(P_CS, B_CS);
}

void UTFT::clrScr() {
    cbi(P_CS, B_CS);
    clrXY();
    sbi(P_RS, B_RS);
    _fast_fill_16(0, 0, (disp_x_size + 1) * (disp_y_size + 1));
    sbi(P_CS, B_CS);
}

void UTFT::fillScr(byte r, byte g, byte b) {
    word color = ((r & 248) << 8 | (g & 252) << 3 | (b & 248) >> 3);
    fillScr(color);
}

void UTFT::fillScr(word color) {
    char ch = byte(color >> 8);
    char cl = byte(color & 0xFF);

    cbi(P_CS, B_CS);
    clrXY();
    sbi(P_RS, B_RS);
    _fast_fill_16(ch, cl, (disp_x_size + 1) * (disp_y_size + 1));
    sbi(P_CS, B_CS);
}

void UTFT::setColor(byte r, byte g, byte b) {
    fch = (r & 248) | g >> 5;
    fcl = (g & 28) << 3 | b >> 3;
}

void UTFT::setColor(word color) {
    fch = byte(color >> 8);
    fcl = byte(color & 0xFF);
}

void UTFT::setBackColor(byte r, byte g, byte b) {
    bch = (r & 248) | g >> 5;
    bcl = (g & 28) << 3| b >> 3;
}

void UTFT::setBackColor(uint32_t color) {
    bch=byte(color >> 8);
    bcl=byte(color & 0xFF);
}

void UTFT::setPixel(word color) {
    LCD_Write_DATA(color >> 8, color & 0xFF); // rrrrrggggggbbbbb
}

void UTFT::drawPixel(int x, int y) {
    cbi(P_CS, B_CS);
    setXY(x, y, x, y);
    setPixel((fch << 8) | fcl);
    sbi(P_CS, B_CS);
    clrXY();
}

void UTFT::drawLine(int x1, int y1, int x2, int y2) {
	if (y1==y2)
		drawHLine(x1, y1, x2-x1);
	else if (x1==x2)
		drawVLine(x1, y1, y2-y1);
	else {
		unsigned int	dx = (x2 > x1 ? x2 - x1 : x1 - x2);
		short			xstep =  x2 > x1 ? 1 : -1;
		unsigned int	dy = (y2 > y1 ? y2 - y1 : y1 - y2);
		short			ystep =  y2 > y1 ? 1 : -1;
		int				col = x1, row = y1;

		cbi(P_CS, B_CS);
		if (dx < dy) {
			int t = - (dy >> 1);
			while (true) {
				setXY (col, row, col, row);
				LCD_Write_DATA(fch, fcl);
				if (row == y2)
					return;
				row += ystep;
				t += dx;
				if (t >= 0) {
					col += xstep;
					t   -= dy;
				}
			}
		} else {
			int t = - (dx >> 1);
			while (true) {
				setXY (col, row, col, row);
				LCD_Write_DATA(fch, fcl);
				if (col == x2)
					return;
				col += xstep;
				t += dy;
				if (t >= 0) {
					row += ystep;
					t   -= dx;
				}
			}
		}
		sbi(P_CS, B_CS);
	}
	clrXY();
}

void UTFT::drawHLine(int x, int y, int l) {
    if (l < 0) {
        l = -l;
        x -= l;
    }

    cbi(P_CS, B_CS);
    setXY(x, y, x + l, y);
    sbi(P_RS, B_RS);
    _fast_fill_16(fch, fcl, l);
    sbi(P_CS, B_CS);
    clrXY();
}

void UTFT::drawVLine(int x, int y, int l) {
    if (l < 0) {
        l = -l;
        y -= l;
    }

    cbi(P_CS, B_CS);
    setXY(x, y, x, y + l);
    sbi(P_RS, B_RS);
    _fast_fill_16(fch, fcl, l);
    sbi(P_CS, B_CS);
    clrXY();
}

// this method supports only PORTRAIT orientation now
void UTFT::printChar(byte c, int x, int y) {
    byte i, ch;
    word j, n, k, pos;

    cbi(P_CS, B_CS);
    setXY(x, y, x + cfont.x_size - 1, y + cfont.y_size - 1);

    n = cfont.x_size * cfont.y_size / 8;
    pos = 4 + (c - cfont.offset) * n;

    for (j = 0; j < n; j++) {
        ch = pgm_read_byte(&cfont.font[pos]);

        for (i = 0; i < 8; i++) {
            if ((ch & (1 << (7 - i))) != 0) {
                setPixel((fch << 8) | fcl);
            } else {
                setPixel((bch << 8) | bcl);
            }
        }
        pos++;
    }

    sbi(P_CS, B_CS);
    clrXY();
}

/*void UTFT::printChar(byte c, int x, int y) {
    byte i,ch;
    word j;
    word temp;

    cbi(P_CS, B_CS);

    if (orient==LANDSCAPE) {
        temp=((c-cfont.offset)*((cfont.x_size/8)*cfont.y_size))+4;

        for(j=0;j<((cfont.x_size/8)*cfont.y_size);j+=(cfont.x_size/8)) {
            setXY(x,y+(j/(cfont.x_size/8)),x+cfont.x_size-1,y+(j/(cfont.x_size/8)));
            for (int zz=(cfont.x_size/8)-1; zz>=0; zz--) {
                ch=pgm_read_byte(&cfont.font[temp+zz]);
                for(i=0;i<8;i++) {
                    if((ch&(1<<i))!=0) {
                        setPixel((fch<<8)|fcl);
                    } else {
                        setPixel((bch<<8)|bcl);
                    }
                }
            }
            temp+=(cfont.x_size/8);
        }
    }

    sbi(P_CS, B_CS);
    clrXY();
}*/

void UTFT::print(char *st, int x, int y) {
    int stl, i;

    stl = strlen(st);

    for (i = 0; i < stl; i++)
        printChar(*st++, x + i*(cfont.x_size), y);
}

void UTFT::print(String st, int x, int y) {
    char buf[st.length()+1];

    st.toCharArray(buf, st.length()+1);
    print(buf, x, y);
}

void UTFT::setFont(uint8_t* font) {
    cfont.font = font;
    cfont.x_size = fontbyte(0);
    cfont.y_size = fontbyte(1);
    cfont.offset = fontbyte(2);
    cfont.numchars = fontbyte(3);
}