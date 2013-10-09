#ifndef AnalyzePacket_H
#define AnalyzePacket_H

#include<Siv3D.hpp>

class RGB{
	public:
		unsigned char r, g, b;
		RGB();
        RGB(unsigned char r, unsigned char g, unsigned char b);
        float Brightness();
        float Dot(RGB x);
        float Distance(RGB x);
};

typedef std::pair< int, int>  Coord;

//パケチE��画像関連の惁E��はここに詰め込む。変数追加の余地あり、E
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
        
        RGB criterion1;
        RGB criterion2;
        RGB criterion5;

        void calculateCriteria();
        void calculateCriteria(int redX, int redY, int whiteX, int whiteY, int blackX, int blackY);
		RGB colorAverage(int xCoordinate, int yCoordinate, int diceSize);
		int decideRoll(RGB average);
		std::vector<std::pair<s3d::Rect, RGB>> analyzePacket(const int left, const int top, const int right, const int bottom);
};

#endif