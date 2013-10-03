/*
encoding: UTF-8

各グリッドの中央部の平均値が算出可能。
*/

#include "AnalyzePacket.h"
#include <stdio.h>
#include <iostream>

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

RGB::RGB(char _r, char _g, char _b)
{
    r = _r;
    g = _g;
    b = _b;
}

float RGB::Brightness()
{
    return ((float)r + (float)g + (float)b) / 256 / 3;
}

float RGB::Dot(RGB x)
{
    return ((float)r * (float)x.r + (float)g * (float)x.g + (float)b * (float)x.b) / 256;
}
float RGB::Distance(RGB x)
{
    float dr = (float)(r - x.r), dg = (float)(g - x.g), db = (float)(b - x.b);
    return dr * dr + dg * dg + db * db;
}

PacketImage::PacketImage(void){
	image = Dialog::OpenImage();
    calculateCriteria();
}

PacketImage::PacketImage(Image src){
	image = src;
    calculateCriteria();
}

void PacketImage::calculateCriteria()
{
    // TODO: Actually calculate
    criterion1 = RGB(255, 0, 0);
    criterion2 = RGB(255, 255, 255);
    criterion5 = RGB(0, 0, 0);
}

RGB PacketImage::colorAverage(int tx, int ty, int radius){

	unsigned long int accumR = 0, accumG = 0, accumB = 0;

    int n = 0;

	for(int x= -radius; x < radius; x++){
		for(int y = -radius; y < radius; y++){
            if (x * x + y * y < radius * radius)
            {
                Color p = image.getPixel(ty + y, tx + x); // Note that getPixel takes (y, x)
                accumR += p.r;
                accumG += p.g;
                accumB += p.b;
                n++;
            }
		}

	}
	return RGB((char)(accumR / n), (char)(accumG / n), (char)(accumB / n));
}


int PacketImage::decideRoll(RGB average){

    float d1 = average.Distance(criterion1), d2 = average.Distance(criterion2), d5 = average.Distance(criterion5);
	//result = Format() + result + "\nave.r = " + average.r + "\nave.g = " + average.g + "\nave.b = " + average.b;
    if(d1 < d2){
        return (d5 < d1) ? 5 : 1;
	}
	else {
        return (d5 < d2) ? 5 : 2;
	}
}

std::vector<std::pair<Coord, int>> PacketImage::analyzePacket(const int left, const int top, int right, int bottom){
	const int packetWidth = right - left;
	const int packetHeight = bottom - top;

	double mediumSize = (double)packetHeight / 10.0;
	double largeSize  = mediumSize * 1.6; 

	//result = Format() + result + L"\nlefttopX = " + lefttopX + " mSize = " + mediumSize;

	double DiceSize;
	int DiceColumns;
	const int DiceRows = 7;
	double y = (double)top;
    std::vector<std::pair<Coord, int>> result;

	for(int i=0; i<DiceRows; i++, y+=DiceSize){
		if(i < 5){
			DiceSize = largeSize;
			DiceColumns = 9;
		}
		else if(i >= 5){
			DiceSize = mediumSize;
			DiceColumns = 14;
		}
		double x = (double)top;
		for(int j=0; j<DiceColumns; j++, x+=DiceSize){
			result.push_back(std::pair<Coord, int>(Coord(x, y), (decideRoll( colorAverage(x + (int)(DiceSize / 2), (int)(y + DiceSize / 2), (int)(DiceSize/3.0))))));
		}
	}
    return result;

}



//デバッグ用メイン (out of date?)
/*
void Main()
{
	writer.open(L"debug.txt");
	const Font font(7);
	PacketImage packet;
	packet.analyzePacket(371, 129, 1563, 945);
	printf("GO!\n");
	while(System::Update())
	{

		font.draw(result);
	}
}
*/