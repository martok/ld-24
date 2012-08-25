unit uCityBlock;

interface

type
  TBuilding = class;

  { TCityBlock }

  TCityBlock = class
  private
    FFields: array[0..8] of TBuilding;
    FPeople: integer;
    FLuxury: integer;
    FSatisfaction: integer;
    FPollution: integer;
    FIndustry: integer;
    FEducation: integer;
    function GetBuilding(Index: integer): TBuilding;
  public
    property Industry: integer read FIndustry;
    property Pollution: integer read FPollution;
    property Education: integer read FEducation;
    property Luxury: integer read FLuxury;
    property People: integer read FPeople;
    property Satisfaction: integer read FSatisfaction;
    property Building[Index: integer]: TBuilding read GetBuilding;
  end;

  TBuilding = class
  private
  public
  
  end;

implementation

{ TCityBlock }

function TCityBlock.GetBuilding(Index: integer): TBuilding;
begin
  Result:= FFields[Index];
end;

end.
