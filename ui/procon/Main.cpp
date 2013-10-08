#include <Siv3D.hpp>
#include"BaseInfo.h"
#include"OpenImage.h"
#include"GridChoice.h"
#include"AnalyzePacket.h"
#include"Result.h"
#include"SideButton.h"


void Main()
{
	OpenImage appOpenImage = OpenImage();
	GridChoice appGridChoice = GridChoice();
	Result appResult = Result();
	SideButton appSideButton = SideButton();

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(WindowWidth,WindowHeight);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		if(appSideButton.AnalyzeButtonClick() == true && appGridChoice.PointRight()){
			PacketImage packet = PacketImage(appOpenImage.image);
            std::vector<std::pair<Coord, int>> result 
				= packet.analyzePacket(appGridChoice.Rlt.x, appGridChoice.Rlt.y, appGridChoice.Rrb.x, appGridChoice.Rrb.y);
            
			Point a, b;
			appGridChoice.ThrowGridPoint(&a, &b);
            appResult.Result_Set(result, a);
		
        }
		
		appGridChoice.SetChoiceColor();
		appGridChoice.ColorPosition();
		if(appGridChoice.ChoiceColor < 0 || appGridChoice.ChoiceColor > 2) appGridChoice.Position();

		appOpenImage.ClickedOpenImage(appSideButton);

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appSideButton.Draw(appGridChoice);

		appGridChoice.DrawGridCoordinate();
		appGridChoice.DrawColorGrid();

		appResult.DrawPacket();
		
		appGridChoice.DrawGrid();

	}
}