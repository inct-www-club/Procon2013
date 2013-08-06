#ifndef OPENIMAGE_H
#define OPENIMAGE_H

#include<Siv3D.hpp>

class OpenImage
{
public:
	Texture texture;
	Font font;
	Rect rect;
	bool tex;

	OpenImage(){
		font = Font(10);
		rect = Rect(0,0,50,50);
		tex = false;
	};

	void ImageOpen(){
		if(rect.leftClicked){
			texture = Dialog::OpenTexture();
			tex = true;
		}
	};

	void Draw(){
		rect.draw(Palette::Brown);
		font.draw(L"‰æ‘œ‚ð\nŠJ‚­", 2,2, Palette::Azure);
		
		if(tex && texture != NULL)
			texture.scale(0.5).draw(50,0);
	};
};

#endif