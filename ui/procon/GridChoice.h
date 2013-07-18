#ifndef GRIDCHOICE_H
#define GRIDCHOICE_H

#include<Siv3D.hpp>

class GridChoice
{
public:
	Rect backrect;
	Point a,b;

	GridChoice(){
		backrect = Rect(50,0,700,500);
	};

	void Position(){
		if(backrect.leftPressed)
			a = Mouse::Pos();
		if(backrect.rightPressed)
			b = Mouse::Pos();
	};

	void DrawBack(){
		backrect.draw(Palette::Black);
	}

	void DrawGrid(){
		if(a.x != NULL) Circle(a, 10);
		if(b.x != NULL) Circle(b, 10);
		if(a.x != NULL && b.x != NULL)
			Rect(a.x, a.y, b.x, b.y).drawFrame(1,0,Palette::Red);
	};
	
};

#endif