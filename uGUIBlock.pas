unit uGUIBlock;

interface

uses
  dglOpenGL, Graphics, GLHelper, uCity, uCityBlock, uViewFrame;

type
  TGUIBlock = class(TGUILayer)
  private
    FCity: TCity;
    FBlock: TCityBlock;
  public                       
    constructor Create(ACity:TCity; X,Y: integer);
    procedure Render; override;
  end;

implementation

uses Classes;

{ TGUIBlock }

constructor TGUIBlock.Create(ACity: TCity; X, Y: integer);
begin
  inherited Create;
  FCity:= ACity;
  FBlock:= FCity.Block[X,Y];
  ClientRect:= Rect(20,100,780,500);
end;

procedure TGUIBlock.Render;
begin
  inherited;
end;

end.
