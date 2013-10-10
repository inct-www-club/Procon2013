#ifndef SideButton_H
#define SideButton_H

#include<Siv3D.hpp>
#include"BaseInfo.h"
#include"GridChoice.h"
#include"AnalyzePacket.h"

class SideButton
{
public:
	Rect OpenButton, AnalyzeButton;
	Rect Criterion1, Criterion2, Criterion5; 
	Font font;

	SideButton(){
		font = Font(10);
		OpenButton = Rect(0,0,50,50);
		AnalyzeButton = Rect(0, 50, 50, 50);

		Criterion1 = Rect(0,300,30,30);
		Criterion2 = Rect(0,330,30,30);
		Criterion5 = Rect(0,360,30,30);
	};

	bool OpenButtonClick(){
		if(OpenButton.leftClicked){
			return true;
		}
		return false;
	};

	bool AnalyzeButtonClick(){
		if(AnalyzeButton.leftClicked){
			return true;
		}
		return false;
	};

	void Draw(GridChoice choice){
		OpenButton.draw(Palette::Brown);
		font.draw(L"画像を\n開く", 2,2, Palette::Azure);
		AnalyzeButton.draw(Palette::Red);
		font.draw(L"画像を\n解析", 2, 52, Palette::Azure);
	};

	void DrawCriterion(RGB red, RGB white, RGB black){
		Criterion1.draw(Color(red.r, red.g, red.b));
		Criterion2.draw(Color(white.r, white.g, white.b));
		Criterion5.draw(Color(black.r, black.g, black.b));
	};
};

#endif