unit uGlobals;

interface

uses uFonts, GLBitmap, uSound;

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

    BUnknown: TglBitmap2D;
  end;

  Sounds: record
    BackgroundMusic,
    EffectClick
    : TsndSound;
  end;

implementation

end.

