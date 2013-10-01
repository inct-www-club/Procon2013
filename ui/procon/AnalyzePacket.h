#ifndef AnalyzePacket_H
#define AnalyzePacket_H

#include<Siv3D.hpp>

class RGB{
	public:
		char r, g, b;
		RGB();
        RGB(char r, char g, char b);
        float Brightness();
        float Dot(RGB x);
        float Distance(RGB x);
};

//�p�P�b�g�摜�֘A�̏��͂����ɋl�ߍ��ށB�ϐ��ǉ��̗]�n����B
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
        
        RGB criterion1;
        RGB criterion2;
        RGB criterion5;

        void calculateCriteria();
		RGB colorAverage(int xCoordinate, int yCoordinate, int diceSize);
		int decideRoll(RGB average);
		std::vector<int> analyzePacket(const int left, const int top, const int right, const int bottom);
};

#endif