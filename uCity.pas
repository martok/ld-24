unit uCity;

{$MODE Delphi}

interface

uses
  uCityBlock;

type
  TCityBlocks = array of array of TCityBlock;

  { TCity }

  TCity = class
  private
    FCityBlocks: TCityBlocks;
    function GetBlock(X, Y: integer): TCityBlock;
  public
    property Block[X, Y: integer]: TCityBlock read GetBlock;
  end;

implementation

{ TCity }

function TCity.GetBlock(X, Y: integer): TCityBlock;
begin
  Result:= FCityBlocks[X,Y];
end;

end.
