unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    PaintBox: TPaintBox;
    WidthEd: TEdit;
    HeightEd: TEdit;
    ChangeSizeBt: TButton;
    SaveBt: TButton;
    LoadBt: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure ChangeSizeBtClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveBtClick(Sender: TObject);
  private
    fSize: TPoint;
    fBlocks: array of array of Byte;
    fStreets: array of packed record
      width: Byte;
      startPos, endPos: packed record
        x, y: Single;
      end;
    end;
    fMousePos, fMouseDown: TPoint;
    fMouseDownTime: Cardinal;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  BLOCK_SIZE   = 40;
  BLOCK_BORDER = 5;

implementation

uses
  uConfigFile; 

{$R *.dfm}

procedure TForm1.ChangeSizeBtClick(Sender: TObject);
begin
  fSize := Point(StrToIntDef(WidthEd.Text, 5), StrToIntDef(HeightEd.Text, 5));
  SetLength(fBlocks, fSize.X, fSize.Y);  
  PaintBox.Width  := BLOCK_SIZE*fSize.X;
  PaintBox.Height := BLOCK_SIZE*fSize.Y;
  PaintBox.Repaint;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
var
  x, y, i: Integer;
begin
  with PaintBox.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Rectangle(-10, -10, PaintBox.Width+10, PaintBox.Height+10);
    for x := 0 to High(fBlocks) do
      for y := 0 to High(fBlocks[x]) do begin
        case fBlocks[x, y] of
          0: Brush.Color := clWhite;
          1: Brush.Color := clBlack;
        end;
        Rectangle(
          x*BLOCK_SIZE+BLOCK_BORDER,
          y*BLOCK_SIZE+BLOCK_BORDER,
          (x+1)*BLOCK_SIZE-BLOCK_BORDER,
          (y+1)*BLOCK_SIZE-BLOCK_BORDER);
      end;

    Pen.Color := clGray;
    Pen.Width := 3;
    for i := 0 to High(fStreets) do with fStreets[i] do begin
      MoveTo(Round(startPos.x * BLOCK_SIZE), Round(startPos.y * BLOCK_SIZE));
      LineTo(Round(endPos.x * BLOCK_SIZE), Round(endPos.y * BLOCK_SIZE));
    end;

    if fMouseDownTime > 0 then begin
      Pen.Width := 2;
      MoveTo(fMouseDown.X, fMouseDown.Y);
      LineTo(fMousePos.X,  fMousePos.Y);
    end;
  end;
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  fMousePos := Point(X, Y);
  if fMouseDownTime > 0 then
    PaintBox.Repaint;
end;

procedure TForm1.PaintBoxDblClick(Sender: TObject);
var
  x, y: Integer;
begin
  x := fMousePos.x div BLOCK_SIZE;
  y := fMousePos.y div BLOCK_SIZE;
  if (x >= 0) and (x < fSize.X) and (y >= 0) and (y < fSize.Y) then begin
    inc(fBlocks[x, y]);
    if (fBlocks[x, y] > 1) then
      fBlocks[x, y] := 0;
    PaintBox.Repaint;
  end;
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := Point(((X + BLOCK_SIZE div 2) div BLOCK_SIZE)*BLOCK_SIZE, ((Y + BLOCK_SIZE div 2) div BLOCK_SIZE)*BLOCK_SIZE);
  fMouseDownTime := GetTickCount;
  PaintBox.Repaint;
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  up: TPoint;
begin
  if (GetTickCount - fMouseDownTime) > 500 then begin
    up := Point(((X + BLOCK_SIZE div 2) div BLOCK_SIZE)*BLOCK_SIZE, ((Y + BLOCK_SIZE div 2) div BLOCK_SIZE)*BLOCK_SIZE);
    SetLength(fStreets, Length(fStreets)+1);
    with fStreets[High(fStreets)] do begin
      startPos.x := fMouseDown.x div BLOCK_SIZE;
      startPos.y := fMouseDown.y div BLOCK_SIZE;
      endPos.x := up.x div BLOCK_SIZE;
      endPos.y := up.y div BLOCK_SIZE;
      width := 1;
    end;
  end;
  PaintBox.Repaint;  
  fMouseDownTime := 0;
end;

procedure TForm1.SaveBtClick(Sender: TObject);
var
  kcf: TkcfConfigFile;
  stream: TFileStream;
  c, x, y, i: Integer;
begin
  if SaveDialog.Execute then begin
    stream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      kcf := TkcfConfigFile.Create(stream);
      try
        with kcf.Section('Map') do begin
          SetValue('Width', fSize.X);
          SetValue('Height', fSize.Y);
          c := 0;
          with Section('Blocks') do begin
            for x := 0 to High(fBlocks) do
              for y := 0 to High(fBlocks[x]) do
                if fBlocks[x, y] <> 0 then begin
                  with Section(IntToStr(c)) do begin
                    SetValue('x', x);
                    SetValue('y', y);
                    SetValue('type', fBlocks[x, y]);
                  end;
                  inc(c);
                end;
          end;
          SetValue('BlockCount', c);

          SetValue('StreetCount', Length(fStreets));
          with Section('Streets') do begin
            for i := 0  to High(fStreets) do with Section(IntToStr(i)) do begin
              SetValue('StartX', fStreets[i].startPos.x);
              SetValue('StartY', fStreets[i].startPos.y);
              SetValue('EndX', fStreets[i].endPos.x);
              SetValue('EndY', fStreets[i].endPos.y);
              SetValue('Width', fStreets[i].width);
            end;
          end;
        end;
        kcf.SaveToStream(stream);
      finally
        kcf.free;
      end;
    finally
      stream.free;
    end;
  end;
end;

end.
