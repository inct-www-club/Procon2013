# include <Siv3D.hpp>

void Main()
{
	const Font font(10);
	const Rect rect(0,0,50,50);
	Texture texture;
	bool tex = false;

	Window::SetTitle(L"TRIDE HC++");
	Window::Resize(700,500);

	Graphics::SetBackGround(Slategray);

	while(System::Update())
	{
		if(rect.leftPressed){
			texture = Dialog::OpenTexture();
			tex = true;
		}


		Rect (50,0,700,500).draw(Palette::Black);
		
		rect.draw(Palette::Brown);
		font.draw(L"‰æ‘œ‚ð\nŠJ‚­", 2,2, Palette::Azure);
		
		if(tex) texture.draw(50,0);
	}
}
