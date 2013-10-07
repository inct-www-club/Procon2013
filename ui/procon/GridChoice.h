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

	Point a,b; //画像選択のツール上での座標
	Point Ra, Rb; //画像選択の画像上での座標

	int ChoiceColor; //選択する色 <<0:r 1:w 2:b>>
	Point ColorGrid[3]; //色選択のツール上での座標 <<0:r 1:w 2:b>>
	Point RColorGrid[3]; //色選択の画像上での座標 

	bool leftPress;

	GridChoice(){
		ChoiceMode = 0;
		backrect = Rect(50,0,WindowWidth,WindowHeight);
		leftPress = false;
		a = Point(50,0); b = Point(50,0);
		Ra = Point(0,0); Rb = Point(0,0);

		ChoiceColor = -1;
		for(int i = 0; i < 3 ;i++) ColorGrid[i] = Point(50+20+10*i,10);
		for(int i = 0; i < 3 ;i++) RColorGrid[i] = Point((ColorGrid[i].x-50)*2,ColorGrid[i].y*2);
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
		if(backrect.leftClicked){
			a = Mouse::Pos();
			Ra.x = (a.x-50)*2;
			Ra.y = a.y*2;
			leftPress = true;
		}
		if(leftPress){
			if(backrect.leftPressed == false) leftPress = false;
			else {
				b = Mouse::Pos();
				Rb.x = (b.x-50)*2;
				Rb.y = b.y*2;
			}
		}
	};

	void ThrowGridPoint(Point *A, Point *B){
		*A = a; *B = b;
	};

	void ThrowGridPointR(Point *A, Point *B){
		*A = Ra; *B = Rb;
	};

	void ColorPosition(){
		if(backrect.leftPressed){
			ColorGrid[ChoiceColor] = Mouse::Pos();
			RColorGrid[ChoiceColor].x = (ColorGrid[ChoiceColor].x-50)*2;
			RColorGrid[ChoiceColor].y = ColorGrid[ChoiceColor].y*2;
			leftPress = true;
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
		Circle(ColorGrid[2], 8).draw(Palette::Green);
		Circle(ColorGrid[2], 6).draw(Palette::Black);

		Circle(ColorGrid[1], 8).draw(Palette::Green);
		Circle(ColorGrid[1], 6).draw(Palette::White);

		Circle(ColorGrid[0], 8).draw(Palette::Green);
		Circle(ColorGrid[0], 6).draw(Palette::Red);
	};

	void DrawBack(){
		backrect.draw(Palette::Black);
	};

	void DrawGrid(){
		Circle(a, 5).draw(Palette::Yellow);
		Circle(b, 5).draw(Palette::Blue);
		
		Rect(a.x, a.y, b.x-a.x, b.y-a.y).drawFrame(1,0,Palette::Yellow);
		Rect(a.x-1, a.y-1, b.x-a.x+2, b.y-a.y+2).drawFrame(1,0,Palette::Blue);
	};
	
	void DrawGridCoordinate(){
		const Font CooaF(10), CoobF(10);
		String CooaS = Format() + L"(" + Ra.x + "," + Ra.y + L")";
		String CoobS = Format() + L"(" + Rb.x + "," + Rb.y + L")";
		
		CooaF.draw(CooaS, 2, WindowHeight-40, Palette::Yellow);
		CooaF.draw(CoobS, 2, WindowHeight-20, Palette::Blue);
	};

	bool PointRight(){
		if(a.x < 0 || a.y < 0 || b.x > WindowWidth || b.y > WindowHeight || 
			(a == b) || a.x > b.x || a.y > b.y) return false;
		else return true;
	};
	
};

#endif