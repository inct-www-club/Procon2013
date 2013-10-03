#ifndef SideButton_H
#define SideButton_H

#include<Siv3D.hpp>
#include"BaseInfo.h"
#include"GridChoice.h"

class SideButton
{
public:
	Rect OpenButton, AnalyzeButton, ChoiceButton;
	Font font;

	SideButton(){
		font = Font(10);
		OpenButton = Rect(0,0,50,50);
		AnalyzeButton = Rect(0, 50, 50, 50);
		ChoiceButton = Rect(0, 100, 50, 50);
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

	bool ChoiceButtonClick(){
		if(ChoiceButton.leftClicked){
			return true;
		}
		return false;
	};

	void Draw(GridChoice choice){
		OpenButton.draw(Palette::Brown);
		font.draw(L"画像を\n開く", 2,2, Palette::Azure);
		AnalyzeButton.draw(Palette::Red);
		font.draw(L"画像を\n解析", 2, 52, Palette::Azure);
		
		if(choice.ChoiceMode == 0){
			ChoiceButton.draw(Palette::Blueviolet);
			font.draw(L"色を\n選択", 2, 102, Palette::Azure);
		}else if(choice.ChoiceMode == 1){
			ChoiceButton.draw(Palette::Beige);
			font.draw(L"範囲を\n選択", 2, 102, Palette::Azure);
		}
	};
};

#endif