unit uGUIBlock;

interface

uses
  dglOpenGL, Graphics, GLHelper, uCity, uCityBlock, uViewFrame, TextSuite;

type
  TGUIBlock = class(TGUILayer)
  private
    FCity: TCity;
    FBlock: TCityBlock;
    FCurrentList: array of String;
  protected
    procedure BuildClick(Sender: TObject);
  public
    constructor Create(ACity: TCity; X, Y: integer);
    procedure Render; override;
  end;

implementation

uses Classes, uGlobals, uBldHouse, uBldIndustry;

{ TGUIBlock }

constructor TGUIBlock.Create(ACity: TCity; X, Y: integer);
var
  i,k: integer;
  procedure Buildable(cls: TBuildingClass);
  var ck: TGUIClickable;
  begin
    ck:= TGUIClickable.Create(Bounds(300 + (i div 5)*110, 130 + (i mod 5)*40, 100,30));
    ck.Text:= cls.ClassName;
    ck.Tag:= Integer(cls);
    ck.OnClick:= BuildClick;
    Clickables.Add(ck);
    inc(i);
  end;
begin
  inherited Create;
  FCity:= ACity;
  FBlock:= FCity.Block[X, Y];
  ClientRect:= Rect(20, 100, 780, 500);

  k:= 0;
  for i:= 0 to 8 do
    if Assigned(FBlock.Building[i]) then begin
      SetLength(FCurrentList,k+1);
      FCurrentList[k]:= FBlock.Building[i].ClassName;
      inc(k);
    end;
  if k < 9 then begin
    i:= 0;
    Buildable(TBHouse);
    Buildable(TBAppartement);
    Buildable(TBSmallIndustry);
    Buildable(TBFactory);
  end;
end;

procedure TGUIBlock.BuildClick(Sender: TObject);
var
  cls: TBuildingClass;
begin
  cls:= TBuildingClass(TGUIClickable(Sender).Tag);
  FCity.CreateBuilding(cls, FBlock);
  Close;
end;

procedure TGUIBlock.Render;
var
  i: integer;
begin
  inherited;
  for i:= 0 to high(FCurrentList) do begin
    tsTextColor3f(0, 0, 0);
    Fonts.GUIText.TextOut(ClientRect.Left + 20, ClientRect.Top + 20 + i * 20, FCurrentList[i]);
  end;
end;

end.

