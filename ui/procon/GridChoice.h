#ifndef GRIDCHOICE_H
#define GRIDCHOICE_H

#include<Siv3D.hpp>
#include"BaseInfo.h"

class GridChoice
{
public:
	Rect backrect;
	Point a,b;
	Point Ra, Rb;
	bool leftPress;

	GridChoice(){
		backrect = Rect(50,0,WindowWidth,WindowHeight);
		leftPress = false;
	};

	void Position1(){
		if(backrect.leftPressed){
			a = Mouse::Pos();
			Ra.x = (a.x-50)*2;
			Ra.y = a.y*2;
		}
		if(backrect.rightPressed){
			b = Mouse::Pos();
			Rb.x = (b.x-50)*2;
			Rb.y = b.y*2;
		}
	};

	void Position2(){
		if(backrect.leftClicked){
			a = Mouse::Pos();
			Ra.x = (a.x-50)*2;
			Ra.y = a.y*2;
			leftPress = true;
		}
		if(leftPress){
			b = Mouse::Pos();
			Rb.x = (b.x-50)*2;
			Rb.y = b.y*2;
			if(backrect.leftPressed == false) leftPress = false;
		}		
	};

	void DrawBack(){
		backrect.draw(Palette::Black);
	};

	void DrawGrid(){
		/*if(a.x != NULL)*/ Circle(a, 5).draw(Palette::Red);
		/*if(b.x != NULL)*/ Circle(b, 5).draw(Palette::Blue);
		/*if(a.x != NULL && b.x != NULL)*/
		Rect(a.x, a.y, b.x-a.x, b.y-a.y).drawFrame(1,0,Palette::Red);
		Rect(a.x-1, a.y-1, b.x-a.x+2, b.y-a.y+2).drawFrame(1,0,Palette::Blue);
	};
	
	void DrawGridCoordinate(){
		const Font CooaF(10), CoobF(10);
		String CooaS = Format() + L"(" + Ra.x + "," + Ra.y + L")";
		String CoobS = Format() + L"(" + Rb.x + "," + Rb.y + L")";
		
		CooaF.draw(CooaS, 52, WindowHeight-40, Palette::Red);
		CooaF.draw(CoobS, 52, WindowHeight-20, Palette::Blue);
	};

	void ThrowGridPoint(Point *A, Point *B){
		*A = a;
		*B = b;
	};

	bool PointRight(){
		if(a.x < 0 || a.y < 0 || b.x > WindowWidth || b.y > WindowHeight) return false;
		else return true;
	};
};

#endif