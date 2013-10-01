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

typedef std::pair< int, int>  Coord;

//パケット画像関連の情報はここに詰め込む。変数追加の余地あり。
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
		std::vector<std::pair<Coord, int>> analyzePacket(const int left, const int top, const int right, const int bottom);
};

#endif