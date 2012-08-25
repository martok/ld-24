unit uGlobals;

interface

uses uFonts, GLBitmap;

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

