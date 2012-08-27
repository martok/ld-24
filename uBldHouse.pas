unit uBldHouse;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBHouse = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLivingSpace: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBAppartement = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLivingSpace: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBAppartement1stClass = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLivingSpace: Integer; override;  
    function SLuxury: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    class function MinEducation: Integer; override;
  end;

implementation

{ TBHouse }

constructor TBHouse.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 0.5, 1);
end;

procedure TBHouse.RenderShape(Height: Single);
begin
  inherited RenderShape((0.5 + FRndHeight) * Height);
end;

class function TBHouse.DisplayName: String;
begin
  Result:= 'House';
end;

class function TBHouse.Price: Integer;
begin
  Result:= 100;
end;

function TBHouse.SLivingSpace: Integer;
begin
  Result:= 10;
end;

class function TBHouse.Texture: TglBitmap2D;
begin
  Result:= Textures.BHouse;
end;

{ TBAppartement }

constructor TBAppartement.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 0.5, 1);
end;

procedure TBAppartement.RenderShape(Height: Single);
begin
  inherited RenderShape((0.8 + FRndHeight) * Height);
end;

class function TBAppartement.DisplayName: String;
begin
  Result:= 'Appartement Building';
end;

class function TBAppartement.Price: Integer;
begin
  Result:= 800;
end;

function TBAppartement.SLivingSpace: Integer;
begin
  Result:= 50;
end;

class function TBAppartement.Texture: TglBitmap2D;
begin
  Result:= Textures.BAppartement;
end;

{ TBAppartement1stClass }

constructor TBAppartement1stClass.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 0.5, 1);
end;

procedure TBAppartement1stClass.RenderShape(Height: Single);
begin
  inherited RenderShape((0.8 + FRndHeight) * Height);
end;

class function TBAppartement1stClass.DisplayName: String;
begin
  Result:= 'First Class Appartement Building';
end;

class function TBAppartement1stClass.Price: Integer;
begin
  Result:= 1000;
end;

function TBAppartement1stClass.SLivingSpace: Integer;
begin
  Result:= 60;
end;

class function TBAppartement1stClass.Texture: TglBitmap2D;
begin
  Result:= Textures.BAppartement1stClass;
end;

function TBAppartement1stClass.SLuxury: Integer;
begin
  Result:= 1;
end;

class function TBAppartement1stClass.MinEducation: Integer;
begin
  Result:= 4;
end;

end.
