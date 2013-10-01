## preview.hs

```haskell
{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, LambdaCase, MultiWayIf #-}
{-
Requires GHC >= 7.6
cabal update && cabal install free-game lens
cat /path/to/text | encoder encode | runhaskell preview.hs
-}
import Graphics.UI.FreeGame
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Monad.State
import System.Random
import Control.Monad.Trans.Maybe
import Control.Lens
import Control.Concurrent.MVar
import System.Environment

loadBitmaps "images"

theFont = unsafePerformIO $ loadFont "VL-PGothic-Regular.ttf"

die :: Picture2D m => Char -> m ()
die 'R' = fromBitmap _die_1_png
die 'W' = fromBitmap _die_2_png
die 'B' = fromBitmap _die_5_png

dieMedium :: Picture2D m => Char -> m ()
dieMedium = scale (10/16) . die

sizeLarge = 96

sizeMedium = 96 * 10 / 16

data Packet = Packet {
    _meta :: String
    , _dice :: [Char]
    }
makeLenses ''Packet

data World = World {
    _packetIndex :: Int
    , _packets :: [Packet]
    , _keyL :: Bool
    , _keyR :: Bool
    }
makeLenses ''World

pickHead :: (MonadState [a] m, MonadIO m, MonadPlus m) => m a
pickHead = get >>= \r -> case r of
    (x:xs) -> put xs >> return x
    [] -> mzero

renderPacket :: (Picture2D m, MonadIO m) => Packet -> m ()
renderPacket pkt = scale 0.7 $ translate (V2 48 48) $ flip evalStateT (view dice pkt) $ do
    runMaybeT $ do
        forM_ [0..4] $ \r -> do
            forM_ [0..8] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeLarge) (die x)
        forM_ [0..1] $ \r -> do
            forM_ [0..13] $ \c -> do
                x <- pickHead
                translate (V2 c r ^* sizeMedium - V2 18 18 + V2 0 480) $ dieMedium x
    return ()

renderWorld :: (MonadState World m, Picture2D m, MonadIO m) => m ()
renderWorld = do
    i <- use packetIndex
    preuse (packets . ix i) >>= maybe (return ()) renderPacket

toPackets :: String -> [Packet]
toPackets "" = []
toPackets xs = Packet { _meta = "", _dice = take 90 xs } : toPackets (drop 90 xs)

stateToMVar :: MonadIO m => MVar s -> StateT s m a -> m a
stateToMVar mv m = do
    s <- liftIO $ takeMVar mv
    (a, s') <- runStateT m s
    liftIO $ putMVar mv s'
    return a

previewMain window = do
    pkts <- toPackets <$> getLine
    font <- loadFont "VL-PGothic-Regular.ttf"
    runGame def { _windowed = window } $ flip execStateT (World { _packetIndex = 0, _packets = pkts, _keyL = False, _keyR = False }) $ foreverTick $ do
        whenM (notM (use keyL) <&=> keySpecial KeyLeft) $ packetIndex -= 1
        whenM (notM (use keyR) <&=> keySpecial KeyRight) $ packetIndex += 1
        keyL <~ keySpecial KeyLeft
        keyR <~ keySpecial KeyRight
        renderWorld

        i <- use packetIndex
        translate (V2 100 240) $ colored (blue & _Alpha .~ 0.5) $ text font 240 $ "#" ++ show i

        whenM (keySpecial KeyEsc) quit `asTypeOf` return ()

main = do
    args <- getArgs
    if  | "-h" `elem` args -> putStrLn "\
            \Usage: preview [-f] [-h]\n\
            \\tstdin: a sequence of dice ([RWS]*)\n\
            \\t-f Run in a full-screen mode\n\
            \\t-h Show this message\n\n\
            \\tPrevious packet: Left\n\
            \\tNext packet: Right\n\
            \\tQuit: Esc"
        | otherwise -> previewMain ("-f" `notElem` args) >> return ()
```

## encoder.hs

```haskell
import Control.Applicative
import Data.List
import Control.Lens
import Control.Monad.State
import System.Environment

letters :: [Char]
letters = concat $ f <$> [2..7] <*> [0..15] where
    f x y = [toEnum v | let v = x * 16 + y, v `notElem` [0x20, 0x5B, 0x5C, 0x5D, 0x5E, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F]]

_Letter :: Iso' Char Int
_Letter = iso (\x -> elemIndex x letters ^?! _Just) (letters !!)

data Bit = Black | Red | White deriving (Show, Eq, Ord, Enum)

_Stream :: Iso' [Char] [Bit]
_Stream = encodeString . encodeBlocks

data Block = Triad Int Int Int | Pair Int Int | Unit Int deriving (Show, Eq, Ord)

encodeString :: Iso' [Char] [Block]
encodeString = iso toBlocks fromBlocks where
    toBlocks :: [Char] -> [Block]
    toBlocks (x:y:z:zs) = Triad (view _Letter x) (view _Letter y) (view _Letter z) : toBlocks zs
    toBlocks (x:y:ys) = Pair (view _Letter x) (view _Letter y) : toBlocks ys
    toBlocks (x:xs) = Unit (view _Letter x) : toBlocks xs
    toBlocks [] = []

    fromBlocks :: [Block] -> [Char]
    fromBlocks (Unit x : bs) = view (from _Letter) x : fromBlocks bs
    fromBlocks (Pair x y : bs) = view (from _Letter) x : view (from _Letter) y : fromBlocks bs
    fromBlocks (Triad x y z : bs) = view (from _Letter) x : view (from _Letter) y : view (from _Letter) z : fromBlocks bs
    fromBlocks [] = []

encodeBlocks :: Iso' [Block] [Bit]
encodeBlocks = iso encode decode where
    encode (b : bs) = case b ^? encodeBlock of
        Just r -> r ++ encode bs
        Nothing -> encode (recons $ b : bs)
    encode [] = []
    decode bs
        | length bs >= 9 = (review encodeBlock $ take 9 bs) : decode (drop 9 bs)
        | length bs == 0 = []
        | otherwise = error "extra bits"

consBlock :: Int -> [Block] -> [Block]
consBlock x [] = [Unit x]
consBlock x (Unit y:ts) = Pair x y : ts
consBlock x (Pair y z:zs) = Triad x y z : zs
consBlock x (Triad y z w : ws) = Triad x y z : consBlock w ws

recons (Triad x y z : xs) = Pair x y : consBlock z xs
recons xs = xs

encodeBlock :: Prism' Block [Bit]
encodeBlock = prism' decode encode where
    encode (Triad x y z) = (x * 86 ^ 2 + y * 86 + z + 86 ^ 2 - 1) ^? _Int_Bit
    encode (Pair x y) = ((x + 1) * 86 + y) ^? _Int_Bit
    encode (Unit x) = x ^? _Int_Bit
    decode bs
        | n < 86 = Unit n
        | otherwise = Pair (div n 86 - 1) (mod n 86)
        where
            n = review _Int_Bit bs

_Int_Bit :: Prism' Int [Bit]
_Int_Bit = prism' (foldr (\x r -> r * 3 + fromEnum x) 0) (enc 9) where
    enc m n
        | m < 0 = Nothing
        | n == 0 = Just (replicate m Black)
        | otherwise = (toEnum (mod n 3):) <$> enc (m - 1) (div n 3)

_ReprBits :: Iso' [Bit] String
_ReprBits = iso encode decode where
    encode (Red : xs) = 'R' : encode xs
    encode (Black : xs) = 'B' : encode xs
    encode (White : xs) = 'W' : encode xs
    encode [] = []
    decode ('R' : xs) = Red : decode xs
    decode ('W' : xs) = White : decode xs
    decode ('B' : xs) = Black : decode xs
    decode (_ : xs) = error "Illegal character: expecting R, W, B"
    decode [] = []

_Lines :: Iso' String [String]
_Lines = iso lines unlines

main = do
    args <- getArgs
    let f = interact . over _Lines . view . mapping
    case args of
         ("encode":_) -> f (_Stream . _ReprBits)
         ("decode":_) -> f (from (_Stream . _ReprBits))
         _ -> putStrLn "Usage: encoder [encode|decode]"
```

## Result.h

```cpp
﻿#ifndef RESULT_H
#define RESULT_H

#include<Siv3D.hpp>
#include"AnalyzePacket.h"

class Die{
public:
	int Roll;
	int ChangedRoll;
	Point LeftTop, RightBottom;
	Font font;

	Die(){
		Roll = 0;
		ChangedRoll = 0;
		LeftTop = Point(0,0);			;
		RightBottom = Point(0,0);
	}

	Die(int roll, Point lefttop, Point rightbottom){
		Roll = roll;
		ChangedRoll = Roll;
		LeftTop = lefttop;
		RightBottom = rightbottom;
	};

	void DrawRoll(){
		font.draw(ChangedRoll, LeftTop.x ,LeftTop.y, Palette::Green);
	};
};

class Result{
public:
	Die BigDice[5][9], SmallDice[2][14];

	void Result_Set(int *AnalyzedPacket){
		int *Packet = AnalyzedPacket;
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				Die(Packet[i*9 + j], Point(50+j*50,i*50), Point(0,0));
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				Die(Packet[i*14 + j - 45], Point(50+j*50, (i+5)*50), Point(0,0));
			}
		}
	}

	void DrawPacket(){
		for(int i = 0; i < 5; i++){
			for(int j = 0; j < 9; j++){
				BigDice[i][j].DrawRoll();
			}
		}
		for(int i = 0; i < 2; i++){
			for(int j = 0; j < 14; j++){
				SmallDice[i][j].DrawRoll();
			}
		}
	};

};

#endif
```

## OpenImage.h

```cpp
﻿#ifndef OPENIMAGE_H
#define OPENIMAGE_H

#include<Siv3D.hpp>

class OpenImage
{
public:
	Image image;
	Texture texture;
	Font font;
	Rect rect, AnalyzeButton;
	bool tex;

	OpenImage(){
		font = Font(10);
		rect = Rect(0,0,50,50);
		AnalyzeButton = Rect(0, 50, 50, 50);
		tex = false;
	};

	bool ButtonClicke(){
		if(rect.leftClicked){
			image = Dialog::OpenImage();
			texture = Texture(image);
			tex = true;
			return false;
		}
		else if(AnalyzeButton.leftClicked){
			return true;
		}
		return false;
	};

	void Draw(){
		rect.draw(Palette::Brown);
		font.draw(L"画像を\n開く", 2,2, Palette::Azure);
		AnalyzeButton.draw(Palette::Red);
		font.draw(L"画像を\n解析", 2, 52, Palette::Azure);
		
		if(tex && texture != NULL)
			texture.scale(0.5).draw(50,0);
	};
};

#endif
```

## OpenImage.cpp

```cpp
﻿
```

## Main.cpp

```cpp
﻿#include <Siv3D.hpp>
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
            std::vector<int> result = packet.analyzePacket(appGridChoice.Ra.x, appGridChoice.Ra.y, appGridChoice.Rb.x, appGridChoice.Rb.y);
            
            int raw[90];
            for (int i = 0; i < 90; i++) raw[i] = result[i];
            appResult.Result_Set(raw);
		
        }

		appGridChoice.Position2();

		appGridChoice.DrawBack();
		appOpenImage.Draw();
		appGridChoice.DrawGridCoordinate();
		appResult.DrawPacket();
		appGridChoice.DrawGrid();
	}
}
```

## GridChoice.h

```cpp
﻿#ifndef GRIDCHOICE_H
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
```

## GridChoice.cpp

```cpp
﻿
```

## BaseInfo.h

```cpp
#define WindowWidth (960+50)

#define WindowHeight 540
```

## AnalyzePacket.h

```cpp
#ifndef AnalyzePacket_H
#define AnalyzePacket_H

#include<Siv3D.hpp>

class RGB{
	public:
		char r, g, b;
		RGB();
        RGB(char r, char g, char b);
        float Brightness();
        float Dot(RGB x);
        float Distance(RGB x);
};

/gパケット画像関連の情報はここに詰め込む。変数追加の余地あり。
class PacketImage{
	public:
		Image image;
		PacketImage(void);
		PacketImage(Image src);
        
        RGB criterion1;
        RGB criterion2;
        RGB criterion5;

        void calculateCriteria();
		RGB colorAverage(int xCoordinate, int yCoordinate, int diceSize);
		int decideRoll(RGB average);
		std::vector<int> analyzePacket(const int left, const int top, const int right, const int bottom);
};

#endif
```

## AnalyzePacket.cpp

```cpp
/*
encoding: UTF-8

各グリッドの中央部の平均値が算出可能。
*/

#include "AnalyzePacket.h"
#include <stdio.h>
#include <iostream>

RGB::RGB(){
	r=0;
	g=0;
	b=0;
}

RGB::RGB(char _r, char _g, char _b)
{
    r = _r;
    g = _g;
    b = _b;
}

float RGB::Brightness()
{
    return ((float)r + (float)g + (float)b) / 256 / 3;
}

float RGB::Dot(RGB x)
{
    return ((float)r * (float)x.r + (float)g * (float)x.g + (float)b * (float)x.b) / 256;
}
float RGB::Distance(RGB x)
{
    float dr = (float)(r - x.r), dg = (float)(g - x.g), db = (float)(b - x.b);
    return dr * dr + dg * dg + db * db;
}

PacketImage::PacketImage(void){
	image = Dialog::OpenImage();
    calculateCriteria();
}

PacketImage::PacketImage(Image src){
	image = src;
    calculateCriteria();
}

void PacketImage::calculateCriteria()
{
    // TODO: Actually calculate
    criterion1 = RGB(255, 0, 0);
    criterion2 = RGB(255, 255, 255);
    criterion5 = RGB(0, 0, 0);
}

RGB PacketImage::colorAverage(int tx, int ty, int radius){

	unsigned long int accumR = 0, accumG = 0, accumB = 0;

    int n = 0;

	for(int x= -radius; x < radius; x++){
		for(int y = -radius; y < radius; y++){
            if (x * x + y * y < radius * radius)
            {
                Color p = image.getPixel(ty + y, tx + x); // Note that getPixel takes (y, x)
                accumR += p.r;
                accumG += p.g;
                accumB += p.b;
                n++;
            }
		}

	}
	return RGB((char)(accumR / n), (char)(accumG / n), (char)(accumB / n));
}


int PacketImage::decideRoll(RGB average){

    float d1 = average.Distance(criterion1), d2 = average.Distance(criterion2), d5 = average.Distance(criterion5);
	//result = Format() + result + "\nave.r = " + average.r + "\nave.g = " + average.g + "\nave.b = " + average.b;
    if(d1 < d2){
        return (d5 < d1) ? 5 : 1;
	}
	else {
        return (d5 < d2) ? 5 : 2;
	}
}

std::vector<int> PacketImage::analyzePacket(const int left, const int top, int right, int bottom){
	const int packetWidth = right - left;
	const int packetHeight = bottom - top;

	double mediumSize = (double)packetHeight / 10.0;
	double largeSize  = mediumSize * 1.6; 

	//result = Format() + result + L"\nlefttopX = " + lefttopX + " mSize = " + mediumSize;

	double DiceSize;
	int DiceColumns;
	const int DiceRows = 7;
	double y = (double)top;
    std::vector<int> result(90);

	for(int i=0; i<DiceRows; i++, y+=DiceSize){
		if(i < 5){
			DiceSize = largeSize;
			DiceColumns = 9;
		}
		else if(i >= 5){
			DiceSize = mediumSize;
			DiceColumns = 14;
		}
		double x = (double)top;
		for(int j=0; j<DiceColumns; j++, x+=DiceSize){
			result.push_back(decideRoll( colorAverage(x + (int)(DiceSize / 2), (int)(y + DiceSize / 2), (int)(DiceSize/3.0))));
		}
	}
    return result;

}



//デバッグ用メイン (out of date?)
/*
void Main()
{
	writer.open(L"debug.txt");
	const Font font(7);
	PacketImage packet;
	packet.analyzePacket(371, 129, 1563, 945);
	printf("GO!\n");
	while(System::Update())
	{

		font.draw(result);
	}
}
*/
```

