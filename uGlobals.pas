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

    BSpecial,
    BResearchCenter,
    BWellnessCenter,
    BBusinessApartmentComplex,
    BWaterFront,

    BUnknown: TglBitmap2D;
  end;

  Sounds: record
    BackgroundMusic,
    EffectClick
    : TsndSound;
  end;

procedure FreeTextures;
procedure FreeFonts;
procedure FreeSounds;

implementation

uses
  SysUtils;

procedure FreeTextures;
begin
  with Textures do begin
    FreeAndNil(BElementarySchool);
    FreeAndNil(BLibrary);
    FreeAndNil(BHighschool);
    FreeAndNil(BCollege);
    FreeAndNil(BHouse);
    FreeAndNil(BAppartement);
    FreeAndNil(BAppartement1stClass);
    FreeAndNil(BSmallIndustry);
    FreeAndNil(BFactory);
    FreeAndNil(BFactories);
    FreeAndNil(BPark);
    FreeAndNil(BCinema);
    FreeAndNil(BPool);
    FreeAndNil(BShopping);
    FreeAndNil(BTheater);
    FreeAndNil(BCasino);
    FreeAndNil(BSpecial);
    FreeAndNil(BResearchCenter);
    FreeAndNil(BWellnessCenter);
    FreeAndNil(BBusinessApartmentComplex);
    FreeAndNil(BWaterFront);
    FreeAndNil(BUnknown);
  end
end;

procedure FreeFonts;
begin
  with Fonts do begin
    FreeAndNil(GUIText);
    FreeAndNil(LargeText);
  end
end;

procedure FreeSounds;
begin
  with Sounds do begin
    FreeAndNil(BackgroundMusic);
    FreeAndNil(EffectClick);
  end;
end;

end.
