#include <Siv3D.hpp>
#include"BaseInfo.h"
#include"OpenImage.h"
#include"GridChoice.h"


void Main()
{
	OpenImage appOpenImage = OpenImage();
	GridChoice appGridChoice = GridChoice();

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(WindowWidth,WindowHeight);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		appOpenImage.ImageOpen();
		appGridChoice.Position2();

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appGridChoice.DrawGridCoordinate();
		appGridChoice.DrawGrid();
	}
}