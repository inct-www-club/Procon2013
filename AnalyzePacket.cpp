#include<Siv3D.hpp>
//�e�X�g�����{


class RGB{
	public:
		unsigned long int r, g, b;
		RGB();
		void plusColor(Color color);
		RGB divideColor(int a);
};

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

void RGB::plusColor(Color color){
	r += (unsigned long int)color.r;
	g += (unsigned long int)color.g;
	b += (unsigned long int)color.b;
}


RGB RGB::divideColor(int a){
	r /= (unsigned long int)a;
	g /= (unsigned long int)a;
	b /= (unsigned long int)a;

	//���̖߂�l�͋��e�����̂��s���B
	return *this;
}

//�p�P�b�g�摜�֘A�̏��͂����ɋl�ߍ��ށB�ϐ��ǉ��̗]�n����B
class PacketImage{
	public:
		Image image;
		RGB colerAve(int xCoordinate, int yCoordinate, int diceWidth, int diceHeight);
};

RGB PacketImage::colerAve(int xCoordinate, int yCoordinate, int diceWidth, int diceHeight){
	
	RGB sumColor;

	for(int x=0; x<diceWidth; x++){

		for(int y=0; y<diceHeight; y++){
			sumColor.plusColor( image.getPixel(x, y) );
		
		}

	}

	return sumColor.divideColor(diceWidth * diceHeight);
}