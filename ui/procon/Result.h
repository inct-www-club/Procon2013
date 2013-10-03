﻿#ifndef RESULT_H
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
		LeftTop = Point(50,0);
		RightBottom = Point(50,0);
		font = Font(12);
	};

	void SetDie(int roll, Point lefttop, Point rightbottom){
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
				BigDice[i][j].SetDie(i*9+j, Point(50+j*26,i*26), Point(0,0));
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j] = Die();
				SmallDice[i][j].SetDie(i*14+j+45, Point(50+j*26,(i+5)*26), Point(0,0));
			}
		}
	};

	void Result_Set(std::vector<std::pair<Coord, int>> result){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				//BigDice[i][j].SetDie(result[i*9 + j].second, Point(result[i*9 + j].first.first, result[i*9 + j].first.second), Point(0,0));
				BigDice[i][j].SetDie(result[i*9 + j].second, Point(50+j*26,i*26), Point(0,0));
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				//SmallDice[i][j].SetDie(result[i*14 + j + 45].second, Point(result[i*14 + j + 45].first.first, result[i*14 + j + 45].first.second), Point(0,0));
				SmallDice[i][j].SetDie(result[i*14 + j + 45].second, Point(50+j*26,(i+5)*26), Point(0,0));
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

	// デバッグ用
	void DrawPacket2(){
		int counter = 0;
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j].font.draw(Format()+BigDice[i*9][j].ChangedRoll, 50+counter*20, 0, Palette::Green);
				counter++;
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j].font.draw(Format()+SmallDice[i*14][j].ChangedRoll, 50+counter*20, 0, Palette::Green);
				counter++;
			}
		}
	};


};

#endif