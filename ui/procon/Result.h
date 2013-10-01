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
		LeftTop = Point(0,0);
		RightBottom = Point(0,0);
	};

	Die(int roll, Point lefttop, Point rightbottom){
		Roll = roll;
		ChangedRoll = Roll;
		LeftTop = lefttop;
		RightBottom = rightbottom;
	};

	void DrawRoll(){
		font.draw(Format()+ChangedRoll, LeftTop.x ,LeftTop.y, Palette::Green);
	};
};

class Result{
public:
	Die BigDice[5][9], SmallDice[2][14];

	Result(){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j] = Die();
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j] = Die();
			}
		}
	};

	void Result_Set(std::vector<std::pair<Coord, int>> result){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				Die(result[i*9 + j].second, Point(result[i*9 + j].first.first, result[i*9 + j].first.second), Point(0,0)); //位置設定は座標情報が無いため適当
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				Die(result[i*9 + j].second, Point(result[i*9 + j].first.first, result[i*9 + j].first.second), Point(0,0));
			}
		}
	};

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

	void DrawPacket2(){
		int counter = 0;
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j].font = Font(12);
				BigDice[i][j].font.draw(Format()+BigDice[i][j].Roll, 50+counter*10, 0, Palette::Green);
				counter++;
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j].font = Font(12);
				SmallDice[i][j].font.draw(Format()+SmallDice[i][j].Roll, 50+counter*10, 0, Palette::Green);
				counter++;
			}
		}
	};

	void TestDraw(){
		Rect(100,100,200,200).draw(Palette::Brown);
		Font().draw(L"Hello", 100,100, Palette::Green); 
	};

};

#endif