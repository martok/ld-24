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
    BElementarySchool,
    BLibrary,
    BHighschool,
    BCollege,

    BHouse,
    BAppartement,
    BAppartement1stClass,

    BSmallIndustry,
    BFactory,
    BFactories,

    BPark,
    BCinema,
    BPool,
    BShopping,
    BTheater,
    BCasino,

    BResearchCenter,
    BWellnessCenter,
    BBusinessApartmentComplex,
    BWaterFront,

    BUnknown: TglBitmap2D;
  end;

implementation

end.

