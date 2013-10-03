#ifndef OPENIMAGE_H
#define OPENIMAGE_H

#include<Siv3D.hpp>
#include"SideButton.h"

class OpenImage
{
public:
	Image image;
	Texture texture;
	bool tex;

	OpenImage(){
		tex = false;
	};

	void ClickedOpenImage(SideButton Button){
		if(Button.OpenButtonClick()){
			image = Dialog::OpenImage();
			texture = Texture(image);
			tex = true;
		}
	};

	void Draw(){		
		if(tex && texture != NULL)
			texture.scale(0.5).draw(50,0);
	};
};

#endif