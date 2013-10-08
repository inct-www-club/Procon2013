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
	bool ResultDraw;

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
		ResultDraw = false;
	};

	void Result_Set(std::vector<std::pair<s3d::Rect, int>> result, Point PacketLeftTop){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j].SetDie(result[i*9 + j].second, 
					Point(result[i*9 + j].first.tl.x/2 + PacketLeftTop.x -50, result[i*9 + j].first.tl.y/2),
					Point(result[i*9 + j].first.br.x/2 + PacketLeftTop.x -50, result[i*9 + j].first.br.y/2));
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j].SetDie(result[i*14 + j + 45].second,
					Point(result[i*14 + j + 45].first.tl.x/2 + PacketLeftTop.x -50, result[i*14 + j + 45].first.tl.y/2),
					Point(result[i*14 + j + 45].first.br.x/2 + PacketLeftTop.x -50, result[i*14 + j + 45].first.br.y/2) );
			}
		}
		ResultDraw = true;
	};

	void DrawPacket(){
		if(ResultDraw){
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

			for(int i = 0; i < 5; i++){
				for(int j= 0; j < 9; j++){
					int xsize = BigDice[i][j].RightBottom.x - BigDice[i][j].LeftTop.x,
						ysize = BigDice[i][j].RightBottom.y - BigDice[i][j].LeftTop.y;

					Rect(BigDice[i][j].LeftTop.x, BigDice[i][j].LeftTop.y, xsize, ysize).drawFrame(1,1,Color(i*30,j*30,(i+j)*20));
						
					Circle(Point(BigDice[i][j].LeftTop.x + xsize/2,  BigDice[i][j].LeftTop.y + ysize/2), 3).draw(Palette::Orange);
				}
			}
			for(int i = 0; i < 2; i++){
				for(int j = 0; j < 14; j++){
					int xsize = SmallDice[i][j].RightBottom.x - SmallDice[i][j].LeftTop.x,
						ysize = SmallDice[i][j].RightBottom.y - SmallDice[i][j].LeftTop.y;

					Rect(SmallDice[i][j].LeftTop.x, SmallDice[i][j].LeftTop.y, xsize, ysize).drawFrame(1,1,Palette::Chocolate);
						
					Circle(Point(SmallDice[i][j].LeftTop.x + xsize/2,  SmallDice[i][j].LeftTop.y + ysize/2), 3).draw(Palette::Orange);
				}
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