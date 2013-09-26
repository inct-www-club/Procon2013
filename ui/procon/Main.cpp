#include <Siv3D.hpp>
#include"BaseInfo.h"
#include"OpenImage.h"
#include"GridChoice.h"
#include"AnalyzePacket.h"
#include"Result.h"


void Main()
{
	OpenImage appOpenImage = OpenImage();
	GridChoice appGridChoice = GridChoice();
	Result appResult = Result();
	//PacketImage packet;

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(WindowWidth,WindowHeight);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		if(appOpenImage.ButtonClicke() == true && appGridChoice.PointRight()){
			PacketImage packet = PacketImage(appOpenImage.image);
			packet.analyzePacket(appGridChoice.Ra.x, appGridChoice.Ra.y, appGridChoice.Rb.x, appGridChoice.Rb.y);
			appResult.Result_Set(packet.rollofDice);
		}

		appGridChoice.Position2();

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appGridChoice.DrawGridCoordinate();
		appResult.DrawPacket();
		appGridChoice.DrawGrid();
	}
}