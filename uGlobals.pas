unit uGlobals;

interface

uses uFonts, GLBitmap;

const
  GUI_WIDTH = 165;

var
  Fonts: record
    GUIText,
      LargeText: TtsFont;
  end;

  Textures: record
    BFactories,
    BFactory,
    BHouse,
    BSmallIndustry,
    BUnknown: TglBitmap2D;
  end;

implementation

end.

