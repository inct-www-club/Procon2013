#include <Siv3D.hpp>
#include"OpenImage.h"

void Main()
{	
	const Font font(10);
	const Rect rect(0,0,50,50);
	const Rect backrect(50,0,700,500);
	bool tex = false;
	Point a,b;

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(700,500);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		if(rect.leftPressed){
			OI_Open();
			tex = true;
		}

		if(backrect.leftPressed)
			a = Mouse::Pos();
		if(backrect.rightPressed)
			b = Mouse::Pos();

		backrect.draw(Palette::Black);

		rect.draw(Palette::Brown);
		font.draw(L"‰æ‘œ‚ð\nŠJ‚­", 2,2, Palette::Azure);
		
		if(tex) OI_Draw();

		if(a.x != NULL) Circle(a, 10);
		if(b.x != NULL) Circle(b, 10);
		if(a.x != NULL && b.x != NULL)
			Rect(a.x, a.y, b.x, b.y).drawFrame(1,0,Palette::Red);
	}
}
