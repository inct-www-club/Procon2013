#include "AnalyzePacket.h"
#include <stdio.h>
#include <iostream>

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

RGB::RGB(unsigned char _r, unsigned char _g, unsigned char _b)
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
    float dr = (float)r - (float)x.r, dg = (float)g - (float)x.g, db = (float)b - (float)x.b;
    return dr * dr + dg * dg + db * db;
}

OneDicePoint::OneDicePoint(int tlX, int tlY, int brX, int brY){
	topLeftX = tlX;
	topLeftY = tlY;
	bottomRightY = brY;
	bottomRightX = brX;
}

OneDicePoint::OneDicePoint(double tlX, double tlY, double brX, double brY){
	topLeftX = (int)tlX;
	topLeftY = (int)tlY;
	bottomRightY = (int)brY;
	bottomRightX = (int)brX;
}

PacketImage::PacketImage(void){
	//image = Dialog::OpenImage();
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

void PacketImage::calculateCriteria(int redX, int redY, int whiteX, int whiteY, int blackX, int blackY)
{
    const int r = 8;
    // TODO: Actually calculate
    criterion1 = colorAverage(redX, redY, r);
	criterion2 = colorAverage(whiteX, whiteY, r);
	criterion5 = RGB(0, 0, 0);
    //criterion5 = colorAverage(blackX, blackY, r);
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
	return RGB((unsigned char)(accumR / n), (unsigned char)(accumG / n), (unsigned char)(accumB / n));
}


int PacketImage::decideRoll(RGB average){

    float d1 = average.Distance(criterion1), d2 = average.Distance(criterion2), d5 = average.Distance(criterion5);

    if(d1 < d2){
        if(d1 < d5){
			return 1;
		}
		else{
			return 5;
		}
	}
	else {
        if(d2 < d5){
			return 2;
		}
		else{
			return 5;
		}
	}
}

OneDicePoint* PacketImage::analyzePacket(const int left, const int top, int right, int bottom){
	const int packetWidth = right - left;
	const int packetHeight = bottom - top;

	OneDicePoint *first=NULL, *now;

	double largeSize  = (double)packetWidth / 9.0;
	double mediumSize = largeSize/1.6;

	double DiceSize;
	int DiceColumns;
	const int DiceRows = 7;
	double y = (double)top;

	for(int i=0; i<DiceRows; i++, y+=DiceSize){
		if(i < 5){
			DiceSize = largeSize;
			DiceColumns = 9;
		}
		else if(i >= 5){
			DiceSize = mediumSize;
			DiceColumns = 14;
		}

		double x = (double)left;

		for(int j=0; j<DiceColumns; j++, x+=DiceSize){
			now = new OneDicePoint(x, y, x+DiceSize, y+DiceSize);
			if(first == NULL){
				first = now;
			}
			now->diceRool = decideRoll(colorAverage((int)(x + DiceSize/2.0), (int)(y + DiceSize/2.0), 8));
		}
	}

    return first;

}
