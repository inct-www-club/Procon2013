#ifndef OPENIMAGE_H
#define OPENIMAGE_H

#include<Siv3D.hpp>

class OpenImage
{
public:
	Image image;
	Texture texture;
	Font font;
	Rect rect, AnalyzeButton;
	bool tex;

	OpenImage(){
		font = Font(10);
		rect = Rect(0,0,50,50);
		AnalyzeButton = Rect(0, 50, 50, 50);
		tex = false;
	};

	bool ButtonClicke(){
		if(rect.leftClicked){
			image = Dialog::OpenImage();
			texture = Texture(image);
			tex = true;
			return false;
		}
		else if(AnalyzeButton.leftClicked){
			return true;
		}
		return false;
	};

	void Draw(){
		rect.draw(Palette::Brown);
		font.draw(L"画像を\n開く", 2,2, Palette::Azure);
		AnalyzeButton.draw(Palette::Red);
		font.draw(L"画像を\n解析", 2, 52, Palette::Azure);
		
		if(tex && texture != NULL)
			texture.scale(0.5).draw(50,0);
	};
};

#endif