#include <Siv3D.hpp>
#include"OpenImage.h"
#include"GridChoice.h"


void Main()
{
	OpenImage appOpenImage = OpenImage();
	GridChoice appGridChoice = GridChoice();

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(700,500);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		appOpenImage.ImageOpen();
		appGridChoice.Position();


		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appGridChoice.DrawGrid();
	}
}

void Update(){
}

void Draw(){
}