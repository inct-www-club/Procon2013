#include<Siv3D.hpp>

Texture texture;

void OI_Open(){
	texture = Dialog::OpenTexture();
}

void OI_Draw(){
	if(texture != NULL)
		texture.draw(50,0);
}

/*bool Image_Judge(){
	const Image 
	return texture.leftPressed
}*/