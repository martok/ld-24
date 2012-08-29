unit uBldEducation;

{$MODE Delphi}

interface

uses
  dglOpenGL, uCityBlock, GLHelper, uGlobals, glBitmap;

type
  TBElementarySchool = class(TBuilding)
  protected
    procedure RenderShape(Height: Single; Outline: Boolean = false); override;
  public
    constructor Create; override;
    function SEducation: Integer; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBLibrary = class(TBuilding)
  protected
    procedure RenderShape(Height: Single; Outline: Boolean = false); override;
  public
    constructor Create; override;
    function SEducation: Integer; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    class function MinEducation: Integer; override;
  end;

  TBHighschool = class(TBuilding)
  protected
    procedure RenderShape(Height: Single; Outline: Boolean = false); override;
  public
    constructor Create; override;
    function SEducation: Integer; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    class function MinEducation: Integer; override;
  end;

  TBCollege = class(TBuilding)
  protected
    procedure RenderShape(Height: Single; Outline: Boolean = false); override;
  public
    constructor Create; override;
    function SEducation: Integer; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    class function MinEducation: Integer; override;
  end;

implementation

{ TBElementarySchool }

constructor TBElementarySchool.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0,1,0.6);
end;

procedure TBElementarySchool.RenderShape(Height: Single; Outline: Boolean = false);
begin
  inherited RenderShape(2.0, Outline);
end;

class function TBElementarySchool.DisplayName: String;
begin
  Result:= 'Elementary School';
end;

class function TBElementarySchool.Price: Integer;
begin
  Result:= 1000;
end;

function TBElementarySchool.SEducation: Integer;
begin
  Result:= 1;
end;

class function TBElementarySchool.Texture: TglBitmap2D;
begin
  Result:= Textures.BElementarySchool;
end;

{ TBLibrary }

constructor TBLibrary.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0,1,0.6);
end;

procedure TBLibrary.RenderShape(Height: Single; Outline: Boolean = false);
begin
  inherited RenderShape(2.5, Outline);
end;

class function TBLibrary.DisplayName: String;
begin
  Result:= 'Public Library';
end;

class function TBLibrary.Price: Integer;
begin
  Result:= 1500;
end;

function TBLibrary.SEducation: Integer;
begin
  Result:= 2;
end;

class function TBLibrary.Texture: TglBitmap2D;
begin
  Result:= Textures.BLibrary;
end;

class function TBLibrary.MinEducation: Integer;
begin
  MinEducation:= 1;
end;

{ TBHighschool }

constructor TBHighschool.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0,1,0.6);
end;

procedure TBHighschool.RenderShape(Height: Single; Outline: Boolean = false);
begin
  inherited RenderShape(2.5, Outline);
end;

class function TBHighschool.DisplayName: String;
begin
  Result:= 'High School';
end;

class function TBHighschool.Price: Integer;
begin
  Result:= 2000;
end;

function TBHighschool.SEducation: Integer;
begin
  Result:= 3;
end;

class function TBHighschool.Texture: TglBitmap2D;
begin
  Result:= Textures.BHighschool;
end;

class function TBHighschool.MinEducation: Integer;
begin
  Result:= 2;
end;

{ TBCollege }

constructor TBCollege.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0,1,0.6);
end;

procedure TBCollege.RenderShape(Height: Single; Outline: Boolean = false);
begin
  inherited RenderShape(2.5, Outline);
end;

class function TBCollege.DisplayName: String;
begin
  Result:= 'College';
end;

class function TBCollege.Price: Integer;
begin
  Result:= 3000;
end;

function TBCollege.SEducation: Integer;
begin
  Result:= 4;
end;

class function TBCollege.Texture: TglBitmap2D;
begin
  Result:= Textures.BCollege;
end;

class function TBCollege.MinEducation: Integer;
begin
  Result:= 2;
end;

end.
