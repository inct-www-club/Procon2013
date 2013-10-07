#ifndef SideButton_H
#define SideButton_H

#include<Siv3D.hpp>
#include"BaseInfo.h"
#include"GridChoice.h"

class SideButton
{
public:
	Rect OpenButton, AnalyzeButton;
	Font font;

	SideButton(){
		font = Font(10);
		OpenButton = Rect(0,0,50,50);
		AnalyzeButton = Rect(0, 50, 50, 50);
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
};

#endif