/*
encoding: UTF-8

テスト実施済み。各グリッドの中央部の平均値が算出可能。判定の閾値は要検討 2013/8/11
各種命名で、単に"roll"と出てきたら、それはサイコロの出目を意味する。
*/

#include"AnalyzePacket.h"
#include<stdio.h>

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

void RGB::plusColor(Color color){

	r += (unsigned long int)color.r;
	g += (unsigned long int)color.g;
	b += (unsigned long int)color.b;
	//String rrr;
	//rrr = Format() + L"(" + color.r + L"," + color.g + L"," + color.b + L")\n";
	//writer.write(rrr);
}


RGB RGB::divideColor(int a){
	r /= (unsigned long int)a;
	g /= (unsigned long int)a;
	b /= (unsigned long int)a;

	//この戻り値は可能である。追記2013/7/9
	return *this;
}




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

RGB PacketImage::colerAve(int xCoordinate, int yCoordinate, int diceSize){

	RGB sumColor;

	//result = Format() + "\ndiseSize = " + diceSize + "\nx=" + xCoordinate + " y=" + yCoordinate;
	const int xLimitPixsel = xCoordinate + diceSize;
	const int yLimitPixsel = yCoordinate + diceSize;
	//result = Format() + result + "\nxLim=" + xLimitPixsel + "yLim=" + yLimitPixsel; 
	for(int x=xCoordinate; x<xLimitPixsel; x++){

		for(int y=yCoordinate; y<yLimitPixsel; y++){
			sumColor.plusColor( image.getPixel(y, x) ); //引数の順番に注意！
		}

	}

	return sumColor.divideColor(diceSize * diceSize);
}

//各色の閾値は要検討。臨機応変に変えられるよう、閾値の設定は別途テキストファイル等で行う予定
int PacketImage::decideRoll(RGB average){

	//result = Format() + result + "\nave.r = " + average.r + "\nave.g = " + average.g + "\nave.b = " + average.b;
	if(average.r>120 && average.g>120 && average.b>120){
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
//上記実装済み2013/8/11
void PacketImage::analyzePacket(int lefttopX, int lefttopY, int rightbottomX, int rightbottomY){
	int packetWidth = rightbottomX - lefttopX;
	int packetHeight = rightbottomY - lefttopY;

	double mediumSize = (double)packetHeight / 10.0;
	double largeSize  = mediumSize * 1.6; 

	//result = Format() + result + L"\nlefttopX = " + lefttopX + " mSize = " + mediumSize;

	double diseSize;
	int diseRankLimit;
	int diseCountUp = 0;
	const int diseLineNum = 7;
	double y=(double)lefttopY;

	for(int i=0; i<diseLineNum; i++, y+=diseSize){
		if(i==0){
			diseSize = largeSize;
			diseRankLimit = 9;
		}
		else if(i==5){
			diseSize = mediumSize;
			diseRankLimit = 14;
		}
		double x = (double)lefttopX;
		for(int j=0; j<diseRankLimit; j++, x+=diseSize, diseCountUp++){
			int mesureX = (int)( x + diseSize/3.0 ) ;
			int mesureY = (int)( y + diseSize/3.0 );
			//result = Format() + result + "\nx=" + mesureX + " y=" +mesureY;
			rollofDice[diseCountUp] = decideRoll( colerAve(mesureX, mesureY, (int)(diseSize/3.0) ));
			//break;//debag code
		}
		//break;//debag code
	}

	resultWrite();
}

void PacketImage::resultWrite(void){
	writer = TextWriter(L"result.txt");
	for(int i=0; i<90; i++){
		String result = Format() + rollofDice[i];
		writer.writeln(result);
	}
	writer.close();
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