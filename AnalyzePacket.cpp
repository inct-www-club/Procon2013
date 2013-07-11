﻿/*
character code: UTF-8

各メソッドのテスト未実施。
各種命名で、単に"roll"と出てきたら、それはサイコロの出目を意味する。
*/

#include<Siv3D.hpp>

class RGB{
	public:
		unsigned long int r, g, b;
		RGB();
		void plusColor(Color color);
		RGB divideColor(int a);
};

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

void RGB::plusColor(Color color){
	r += (unsigned long int)color.r;
	g += (unsigned long int)color.g;
	b += (unsigned long int)color.b;
}


RGB RGB::divideColor(int a){
	r /= (unsigned long int)a;
	g /= (unsigned long int)a;
	b /= (unsigned long int)a;

	//この戻り値は可能である。追記2013/7/9
	return *this;
}

//パケット画像関連の情報はここに詰め込む。変数追加の余地あり。
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
		int rollofDice[90];
		void resetRoll(void);
		RGB colerAve(int xCoordinate, int yCoordinate, int diceWidth, int diceHeight);
		int decideRoll(RGB average);
		void analyzePacket(int leftupX, int leftupY, int rightbottomX, int rightbottomY);
};

PacketImage::PacketImage(void){
	image = Dialog::OpenImage();
	resetRoll();
}

PacketImage::PacketImage(Image src){
	image = src;
	resetRoll();
}

void PacketImage::resetRoll(void){
	for(int i=0; i<90; i++){
		rollofDice[i]=0;
	}
	return;
}

RGB PacketImage::colerAve(int xCoordinate, int yCoordinate, int diceWidth, int diceHeight){
	
	RGB sumColor;

	for(int x=0; x<diceWidth; x++){

		for(int y=0; y<diceHeight; y++){
			sumColor.plusColor( image.getPixel(x, y) );
		}

	}

	return sumColor.divideColor(diceWidth * diceHeight);
}

//各色の閾値は要検討。臨機応変に変えられるよう、閾値の設定は別途テキストファイル等で行う予定
int PacketImage::decideRoll(RGB average){

	if(average.r>140 && average.g>140 && average.b>140){
		return 5;
	}
	else if(average.r>140){
		return 1;
	}
	else{
		return 2;
	}

	return -1;
}

//TODO:１つのサイコロ全面でなく、中央部のみ測定するように書き直す。配置変更に伴う書き直しをする。
void PacketImage::analyzePacket(int leftupX, int leftupY, int rightbottomX, int rightbottomY){
	int packetWidth = rightbottomX - leftupX;
	int packetHeight = rightbottomY - leftupY;

	double mediumSize = (double)packetHeight / 10.0;
	double largeSize  = mediumSize * 1.6; 
	for(double x = -largeSize, double y=0.0, double size, int i=0; i<90; i++){

		if(i%16 == 0){
			x += largeSize;
			y=0.0;
			size = mediumSize;
		}
		else if(i%16 == 10){
			x += mediumSize;
			y = 0.0;
			size = largeSize;
		}

		rollofDice[i] = decideRoll( colerAve((int)x, (int)y, (int)size, (int)size) ); 

		y += size;

	}
}