#ifndef GRIDCHOICE_H
#define GRIDCHOICE_H

#include<Siv3D.hpp>
#include"BaseInfo.h"
#include"SideButton.h"

class GridChoice
{
public:
	Rect backrect;
	int ChoiceMode;

	Point lt,rt,lb,rb; //画像選択のツール上での座標
	Point Rlt,Rrt,Rlb,Rrb; //画像選択の画像上での座標
	bool GridSetting; //座標を設定中
	int GridFixing; //座標をドラッグで修正中　<< 0:lt 1:rt 2:lb 3:rb >>

	int ChoiceColor; //選択する色 <<0:r 1:w 2:b>>
	Point ColorGrid[3]; //色選択のツール上での座標 <<0:r 1:w 2:b>>
	Point RColorGrid[3]; //色選択の画像上での座標 

	Point RealPositionSet(Point a){
		return Point((a.x-50)*2, a.y*2); 
	};

	GridChoice(){
		ChoiceMode = 0;
		backrect = Rect(50,0,WindowWidth,WindowHeight);
		GridSetting = false;
		lt = Point(50,0); rt = Point(50,0); lb = Point(50,0); rb = Point(50,0);
		Rlt = Point(0,0); Rrt = Point(0,0); Rlb = Point(0,0); Rrb = Point(0,0);
		GridFixing = -1;

		ChoiceColor = -1;
		for(int i = 0; i < 3 ;i++) ColorGrid[i] = Point(50+20+10*i,10);
		for(int i = 0; i < 3 ;i++) RColorGrid[i] = RealPositionSet(ColorGrid[i]);
	};

	void ChangeChoiceMode(bool clicked){
		if(clicked){
			if(ChoiceMode == 0) ChoiceMode = 1;
			else ChoiceMode = 0;
		}
	};

	int GetChoiceMode(){
		return ChoiceMode;
	};

	void Position(){
		if(GridSetting == false){
			if(Circle(lt, 5).leftPressed) GridFixing = 0;
			else if(Circle(rt, 5).leftPressed) GridFixing = 1;
			else if(Circle(lb, 5).leftPressed) GridFixing = 2;
			else if(Circle(rb, 5).leftPressed) GridFixing = 3;
		}

		if(GridFixing >= 0 && GridFixing <= 3){
			if(backrect.leftReleased) GridFixing = -1;
			else{
				if(GridFixing == 0) lt = Mouse::Pos();
				else if(GridFixing == 1) rt = Mouse::Pos();
				else if(GridFixing == 2) lb = Mouse::Pos();
				else if(GridFixing == 3) rb = Mouse::Pos();
			}
		}
		else{
			if(backrect.leftClicked){
				lt = Mouse::Pos();
				Rlt = RealPositionSet(lt);
				GridSetting = true;
			}
			if(GridSetting){
				if(backrect.leftPressed == false) GridSetting = false;
				else {
					rb = Mouse::Pos();
					Rrb = RealPositionSet(rb);

					rt = Point(rb.x, lt.y);
					lb = Point(lt.x, rb.y);
				}
			}
		}

		Rrt = RealPositionSet(rt);
		Rlb = RealPositionSet(lb);
	};

	void ThrowGridPoint(Point *A, Point *B){
		*A = lt; *B = rb;
	};

	void ThrowGridPointR(Point *A, Point *B){
		*A = Rlt; *B = Rrb;
	};

	void ColorPosition(){
		if(backrect.leftPressed){
			ColorGrid[ChoiceColor] = Mouse::Pos();
			RColorGrid[ChoiceColor].x = (ColorGrid[ChoiceColor].x-50)*2;
			RColorGrid[ChoiceColor].y = ColorGrid[ChoiceColor].y*2;
			//leftPress = true;
		}
	};

	void SetChoiceColor(){
		if(ChoiceColor == -1){
			if(Circle(ColorGrid[0], 8).leftPressed) ChoiceColor = 0;
			else if(Circle(ColorGrid[1], 8).leftPressed) ChoiceColor = 1;
			else if(Circle(ColorGrid[2], 8).leftPressed) ChoiceColor = 2;
		}else{
			if(backrect.leftReleased) ChoiceColor = -1;
		}

	};

	void ThrowColorGridR(Point *R, Point *W, Point *B){
		*R = RColorGrid[0]; *W = RColorGrid[1]; *B = RColorGrid[2];
	};

	void DrawColorGrid(){
		Color c;
		for(int i = 2; i >= 0; i--){
			if(i == 2) c = Color(Palette::Black);
			else if(i == 1) c = Color(Palette::White);
			else if(i == 0) c = Color(Palette::Red);

			Circle(ColorGrid[i], 8).draw(Palette::Green);
			Circle(ColorGrid[i], 6).draw(c);
			Line(Point(ColorGrid[i].x, ColorGrid[i].y-10), Point(ColorGrid[i].x, ColorGrid[i].y+10)).draw(1,Palette::Green);
			Line(Point(ColorGrid[i].x-10, ColorGrid[i].y), Point(ColorGrid[i].x+10, ColorGrid[i].y)).draw(1,Palette::Green);
		}
	};

	void DrawBack(){
		backrect.draw(Palette::Black);
	};

	void DrawGrid(){
		Circle(lt, 5).draw(Palette::Yellow);
		Circle(rb, 5).draw(Palette::Blue);
		Circle(rt, 5).draw(Palette::Aqua);
		Circle(lb, 5).draw(Palette::Greenyellow);
		
		Line(lt, rt).draw(2, Palette::Yellow);
		Line(lb, rb).draw(2, Palette::Blue);
		Line(rt, rb).draw(2, Palette::Aqua);
		Line(lt, lb).draw(2, Palette::Greenyellow);
	};
	
	void DrawGridCoordinate(){
		const Font CooltF(10), CoortF(10), CoolbF(10), CoorbF(10);
		
		CooltF.draw(Format() + L"(" + Rlt.x + L"," + Rlt.y + L")", 2, WindowHeight-80, Palette::Yellow);
		CoortF.draw(Format() + L"(" + Rrt.x + L"," + Rrt.y + L")", 2, WindowHeight-60, Palette::Aqua);
		CoolbF.draw(Format() + L"(" + Rlb.x + L"," + Rlb.y + L")", 2, WindowHeight-40, Palette::Greenyellow);
		CoorbF.draw(Format() + L"(" + Rrb.x + L"," + Rrb.y + L")", 2, WindowHeight-20, Palette::Blue);
	};

	bool PointRight(){
		if(lt.x < 0 || lt.y < 0 || rb.x > WindowWidth || rb.y > WindowHeight || 
			(lt == rb) || lt.x > rb.x || lt.y > rb.y) return false;
		else return true;
	};
	
};

#endif