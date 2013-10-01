/*
encoding: UTF-8

テスト実施済み。各グリッドの中央部の平均値が算出可能。判定の閾値は要検討 2013/8/11
各種命名で、単に"roll"と出てきたら、それはサイコロの出目を意味する。
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

PacketImage::PacketImage(void){
	image = Dialog::OpenImage();
	resetRoll();
}

PacketImage::PacketImage(Image src){
	image = src;
	resetRoll();
}

RGB PacketImage::colorAverage(int xCoordinate, int yCoordinate, int diceSize){

	unsigned long int accumR = 0, accumG = 0, accumB = 0;

    const int numPixels = diceSize * diceSize;

	//result = Format() + "\nDiceSize = " + diceSize + "\nx=" + xCoordinate + " y=" + yCoordinate;
	const int xLimitPixel = xCoordinate + diceSize;
	const int yLimitPixel = yCoordinate + diceSize;
	//result = Format() + result + "\nxLim=" + xLimitPixel + "yLim=" + yLimitPixel; 
	for(int x=xCoordinate; x < xLimitPixel; x++){

		for(int y=yCoordinate; y<yLimitPixel; y++){
            p = image.getPixel(y, x); // Note that getPixel takes (y, x)
            accumR += p.red;
            accumG += p.green;
            accumB += p.blue;
		}

	}
	return RGB((char)(accumR / numPixels), (char)(accumG / numPixels), (char)(accumB / numPixels));
}


int PacketImage::decideRoll(RGB average){

	//result = Format() + result + "\nave.r = " + average.r + "\nave.g = " + average.g + "\nave.b = " + average.b;
	if(average.r>120 && average.g>120 && average.b>120){
		return 5;
	}
	else if(average.r>120){
		return 1;
	}
	else{
		return 2;
	}

	return -1;
}

std::vector<int> PacketImage::analyzePacket(const int left, const int top, int right, int bottom){
	const int packetWidth = right - left;
	const int packetHeight = bottom - top;

	double mediumSize = (double)packetHeight / 10.0;
	double largeSize  = mediumSize * 1.6; 

	//result = Format() + result + L"\nlefttopX = " + lefttopX + " mSize = " + mediumSize;

	double DiceSize;
	int DiceColumns;
	const int DiceRows = 7;
	double y = (double)top;
    std::vector<int> result(90);

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
			int measureX = (int)( x + DiceSize/3.0 ); // magic number?
			int measureY = (int)( y + DiceSize/3.0 );
			//result = Format() + result + "\nx=" + measureX + " y=" +measureY;
			result.push_back(decideRoll( colorAverage(measureX, measureY, (int)(DiceSize/3.0) )));
            
			//break;//debug code
		}
		//break;//debug code
	}
    return result;

}



//デバッグ用メイン
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