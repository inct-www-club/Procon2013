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
	PacketImage packet = PacketImage();
	std::vector<std::pair<s3d::Rect, RGB>> result;

	OneDicePoint *resultFirst = NULL;

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(WindowWidth,WindowHeight);

	Graphics::SetBackGround(Slategray);
	
	Resource::RegisterFont(L"Grid", 10);

	while(System::Update())
	{
		if(appSideButton.AnalyzeButtonClick() == true && appGridChoice.PointRight()){
			packet = PacketImage(appOpenImage.image);
			packet.calculateCriteria(appGridChoice.RColorGrid[0].x, appGridChoice.RColorGrid[0].y,
				appGridChoice.RColorGrid[1].x, appGridChoice.RColorGrid[1].y,
				appGridChoice.RColorGrid[2].x, appGridChoice.RColorGrid[2].y);

			resultFirst = packet.analyzePacket(appGridChoice.Rlt.x, appGridChoice.Rlt.y, appGridChoice.Rrb.x, appGridChoice.Rrb.y);
            
			//Point a, b;
			//appGridChoice.ThrowGridPoint(&a, &b);
            //appResult.Result_Set(result, a, packet);
		
        }
		
		appGridChoice.SetChoiceColor();
		appGridChoice.ColorPosition();

		if(appGridChoice.ChoiceColor < 0 || appGridChoice.ChoiceColor > 2) appGridChoice.Position();

		appOpenImage.ClickedOpenImage(appSideButton);

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appSideButton.Draw(appGridChoice);
		appSideButton.DrawCriterion(packet.criterion1, packet.criterion2, packet.criterion5);

		appGridChoice.DrawGridCoordinate();
		appGridChoice.DrawColorGrid();

		//appResult.DrawPacket(result);
		for(OneDicePoint *now = resultFirst; now != NULL; now = now->next){
			now->rect->drawFrame(2, 2, Palette::Red);
		}

		appGridChoice.DrawGrid();
	}
}