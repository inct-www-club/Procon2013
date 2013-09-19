#include <Siv3D.hpp>
#include"BaseInfo.h"
#include"OpenImage.h"
#include"GridChoice.h"
#include"AnalysePacket.h"


void Main()
{
	OpenImage appOpenImage = OpenImage();
	GridChoice appGridChoice = GridChoice();
	//PacketImage packet;

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(WindowWidth,WindowHeight);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		if(appOpenImage.ButtonClicke() == true){
			PacketImage packet = PacketImage(appOpenImage.image);
			packet.analyzePacket(appGridChoice.Ra.x, appGridChoice.Ra.y, appGridChoice.Rb.x, appGridChoice.Rb.y);
		}

		appGridChoice.Position2();

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appGridChoice.DrawGridCoordinate();
		appGridChoice.DrawGrid();
	}
}