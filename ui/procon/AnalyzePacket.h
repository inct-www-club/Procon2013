#ifndef AnalyzePacket_H
#define AnalyzePacket_H

#include<Siv3D.hpp>

class RGB{
	public:
		char r, g, b;
		RGB();
        RGB(char r, char g, char b);
};

//�p�P�b�g�摜�֘A�̏��͂����ɋl�ߍ��ށB�ϐ��ǉ��̗]�n����B
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
        
		void resetRoll(void);
		RGB colorAverage(int xCoordinate, int yCoordinate, int diceSize);
		int decideRoll(RGB average);
		void analyzePacket(int leftupX, int leftupY, int rightbottomX, int rightbottomY);
};

#endif