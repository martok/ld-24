unit uBldSpecial;

interface

uses
  uCityBlock, glBitmap, GLHelper;

type
  TBSpecial = class(TBuilding)
  private
  public
    class function NeededBlockType: TBlockType; virtual;
    class function MinEducation: Integer; override;
  end;

  TBResearchCenter = class(TBSpecial)
  private
  public
    function SEducation: integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    class function NeededBlockType: TBlockType; override;
  end;

  TBWellnessCenter = class(TBSpecial)
  private
  public
    function SLuxury: integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;  
    class function NeededBlockType: TBlockType; override;
  end;

  TBBusinessApartmentComplex = class(TBSpecial)
  private
  public
    function SLivingSpace: integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;  
    class function NeededBlockType: TBlockType; override;
  end;

  TBWaterFront = class(TBSpecial)
  private
  public
    function SIndustryValue: integer; override;
    function SPollution: integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;  
    class function NeededBlockType: TBlockType; override;
  end;


implementation

uses
  uGlobals;

{ TBSpecial }

class function TBSpecial.MinEducation: Integer;
begin
  Result:= 50;
end;

class function TBSpecial.NeededBlockType: TBlockType;
begin
  result := btNormal;
end;

{ TBResearchCenter }

constructor TBResearchCenter.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.0, 1.0, 0.6);
end;

class function TBResearchCenter.DisplayName: String;
begin
  result := 'Research Center';
end;

class function TBResearchCenter.NeededBlockType: TBlockType;
begin
  result := btResearch;
end;

class function TBResearchCenter.Price: Integer;
begin
  result := 10000;
end;

function TBResearchCenter.SEducation: integer;
begin
  result := 10;
end;

class function TBResearchCenter.Texture: TglBitmap2D;
begin
  result := Textures.BResearchCenter;
end;

{ TBWellnessCenter }

constructor TBWellnessCenter.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 1.0, 0.0);
end;

class function TBWellnessCenter.DisplayName: String;
begin
  result := 'Wellness Center';
end;

class function TBWellnessCenter.NeededBlockType: TBlockType;
begin
  result := btLuxury;
end;

class function TBWellnessCenter.Price: Integer;
begin
  result := 10000;
end;

function TBWellnessCenter.SLuxury: integer;
begin
  result := 10;
end;

class function TBWellnessCenter.Texture: TglBitmap2D;
begin
  result := Textures.BWellnessCenter;
end;

{ TBBusinessApartmentComplex }

constructor TBBusinessApartmentComplex.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 0.5, 1);
end;

class function TBBusinessApartmentComplex.DisplayName: String;
begin
  result := 'Business Apartment Complex';
end;

class function TBBusinessApartmentComplex.NeededBlockType: TBlockType;
begin
  result := btHouse;
end;

class function TBBusinessApartmentComplex.Price: Integer;
begin
  result := 10000;
end;

function TBBusinessApartmentComplex.SLivingSpace: integer;
begin
  result := 100;
end;

class function TBBusinessApartmentComplex.Texture: TglBitmap2D;
begin
  result := Textures.BBusinessApartmentComplex;
end;

{ TBWaterFront }

constructor TBWaterFront.Create;
begin
  inherited;
  FColor:= ColorToRGBA(1.0, 0.3, 0.0); 
end;

class function TBWaterFront.DisplayName: String;
begin
  result := 'Water Front';
end;

class function TBWaterFront.NeededBlockType: TBlockType;
begin
  result := btIndustry;
end;

class function TBWaterFront.Price: Integer;
begin
  result := 10000;
end;

function TBWaterFront.SIndustryValue: integer;
begin
  result := 10;
end;

function TBWaterFront.SPollution: integer;
begin
  result := 5; 
end;

class function TBWaterFront.Texture: TglBitmap2D;
begin
  result := Textures.BWaterFront;
end;

end.
