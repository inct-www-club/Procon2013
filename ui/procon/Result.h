#ifndef RESULT_H
#define RESULT_H

#include<Siv3D.hpp>
#include"AnalyzePacket.h"

class Die{
public:
	int Roll;
	int ChangedRoll;
	Point LeftTop, RightBottom;
	Font font;

	Die(){
		Roll = 0;
		ChangedRoll = 0;
		LeftTop = Point(0,0);			;
		RightBottom = Point(0,0);
	}

	Die(int roll, Point lefttop, Point rightbottom){
		Roll = roll;
		ChangedRoll = Roll;
		LeftTop = lefttop;
		RightBottom = rightbottom;
	};

	void DrawRoll(){
		font.draw(ChangedRoll, LeftTop.x ,LeftTop.y, Palette::Green);
	};
};

class Result{
public:
	Die BigDice[5][9], SmallDice[2][14];

	void Result_Set(int *AnalyzedPacket){
		int *Packet = AnalyzedPacket;
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				Die(Packet[i*9 + j], Point(50+j*50,i*50), Point(0,0));
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				Die(Packet[i*14 + j - 45], Point(50+j*50, (i+5)*50), Point(0,0));
			}
		}
	}

	void DrawPacket(){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j].DrawRoll();
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j].DrawRoll();
			}
		}
	};

};

#endif