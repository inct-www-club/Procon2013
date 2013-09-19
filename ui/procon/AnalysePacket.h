#ifndef AnalysePacket_H
#define AnalysePacket_H

#include<Siv3D.hpp>

class RGB{
	public:
		unsigned long int r, g, b;
		RGB();
		void plusColor(Color color);
		RGB divideColor(int a);
};

//�p�P�b�g�摜�֘A�̏��͂����ɋl�ߍ��ށB�ϐ��ǉ��̗]�n����B
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
		int rollofDice[90];
		void resetRoll(void);
		RGB colerAve(int xCoordinate, int yCoordinate, int diceSize);
		int decideRoll(RGB average);
		void analyzePacket(int leftupX, int leftupY, int rightbottomX, int rightbottomY);
		TextWriter writer;
		void resultWrite(void);
};

#endif