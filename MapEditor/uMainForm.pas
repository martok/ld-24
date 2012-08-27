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
    LoadTextureBt: TButton;
    StreetsLB: TListBox;
    procedure ChangeSizeBtClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveBtClick(Sender: TObject);
    procedure LoadTextureBtClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadBtClick(Sender: TObject);
    procedure StreetsLBClick(Sender: TObject);
    procedure StreetsLBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fTex: TBitmap;
    fSize: TPoint;
    fBlocks: array of array of Shortint;
    fStreets: array of packed record
      width: Byte;
      startPos, endPos: packed record
        x, y: Single;
      end;
    end;
    fMousePos, fMouseDown: TPoint;
    fMouseDownTime: Cardinal;
    procedure UpdateStreetList;
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
var
  x, y: Integer;
begin
  fSize := Point(StrToIntDef(WidthEd.Text, 5), StrToIntDef(HeightEd.Text, 5));
  SetLength(fBlocks, fSize.X, fSize.Y);
  for x := 0 to High(fBlocks) do
    for y := 0 to High(fBlocks[x]) do
      fBlocks[x, y] := -1;
  SetLength(fStreets, 0);  
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
    //Rectangle(-10, -10, PaintBox.Width+10, PaintBox.Height+10);
    StretchDraw(Rect(0, 0, PaintBox.Width, PaintBox.Height), fTex);
    for x := 0 to High(fBlocks) do
      for y := 0 to High(fBlocks[x]) do begin
        case fBlocks[x, y] of
          -1: Brush.Color := clWhite;
           0: Brush.Color := clBlack;
           1: Brush.Color := clRed;
           2: Brush.Color := clBlue;
           3: Brush.Color := clYellow;
           4: Brush.Color := clGreen;
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
      if i = StreetsLB.ItemIndex then
        Pen.Color := clRed;
      MoveTo(Round(startPos.x * BLOCK_SIZE), Round(startPos.y * BLOCK_SIZE));
      LineTo(Round(endPos.x * BLOCK_SIZE), Round(endPos.y * BLOCK_SIZE));
      if i = StreetsLB.ItemIndex then
        Pen.Color := clGray;
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
    if (fBlocks[x, y] > 4) then
      fBlocks[x, y] := -1;
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
    ShowMessage(IntToStr(Length(fStreets)));
    SetLength(fStreets, Length(fStreets)+1);
    with fStreets[High(fStreets)] do begin
      startPos.x := fMouseDown.x div BLOCK_SIZE;
      startPos.y := fMouseDown.y div BLOCK_SIZE;
      endPos.x := up.x div BLOCK_SIZE;
      endPos.y := up.y div BLOCK_SIZE;
      width := 1;
    end;
    UpdateStreetList;
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
                if fBlocks[x, y] >= 0 then begin
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

procedure TForm1.LoadTextureBtClick(Sender: TObject);
var
  x, y: Integer;
  r: Byte;
begin
  if OpenDialog.Execute then begin
    fTex.LoadFromFile(OpenDialog.FileName);
    for x := 0 to fTex.Width-1 do
      for y := 0 to fTex.Height-1 do begin
        r := fTex.Canvas.Pixels[x, y] or $FF000000;
        if r = 128 then
          fTex.Canvas.Pixels[x, y] := clGreen
        else if r < 128 then
          fTex.Canvas.Pixels[x, y] := clBlack
        else
          fTex.Canvas.Pixels[x, y] := clWhite;
      end;
    PaintBox.Repaint;
  end;  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fTex := TBitmap.Create;
  fTex.Width  := 128;
  fTex.Height := 128;
  fTex.Canvas.Brush.Color := clWhite;
  fTex.Canvas.Brush.Style := bsSolid;
  fTex.Canvas.Rectangle(-100, -100, 20, 200);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fTex);
end;

procedure TForm1.LoadBtClick(Sender: TObject);
var
  stream: TFileStream;
  kcf: TkcfConfigFile;
  i, c, x, y, t: Integer;
begin
  if OpenDialog.Execute then begin
    stream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      kcf := TkcfConfigFile.Create(stream);
      try
        with kcf.Section('Map') do begin
          fSize.X := GetValue('Width', 10);
          fSize.Y := GetValue('Height', 10);
          WidthEd.Text := IntToStr(fSize.X);
          HeightEd.Text := IntToStr(fSize.Y);

          SetLength(fBlocks, fSize.X, fSize.Y);
          for x := 0 to High(fBlocks) do
            for y := 0 to High(fBlocks[x]) do
              fBlocks[x, y] := -1;
              
          c := GetValue('BlockCount', 0);
          with Section('Blocks') do begin
            for i := 0 to c-1 do begin
              with Section(IntToStr(i)) do begin
                x := GetValue('x', -1);
                y := GetValue('y', -1);
                t := GetValue('type', 0);
                fBlocks[x, y] := t;
              end;
            end;
          end;

          c := GetValue('StreetCount', 0);
          SetLength(fStreets, c);
          with Section('Streets') do begin
            for i := 0 to c-1 do begin
              with Section(IntToStr(i)) do begin
                fStreets[i].startPos.x := GetValue('StartX', 0);
                fStreets[i].startPos.y := GetValue('StartY', 0);
                fStreets[i].endPos.x := GetValue('EndX', 0);
                fStreets[i].endPos.y := GetValue('EndY', 0);
                fStreets[i].width := GetValue('Width', 1);
              end;
            end;
          end;
        end;
        UpdateStreetList;
        PaintBox.Repaint;
      finally
        kcf.Free;
      end;
    finally
      stream.Free;
    end;

    fSize := Point(StrToIntDef(WidthEd.Text, 5), StrToIntDef(HeightEd.Text, 5));
    SetLength(fBlocks, fSize.X, fSize.Y);
    PaintBox.Width  := BLOCK_SIZE*fSize.X;
    PaintBox.Height := BLOCK_SIZE*fSize.Y;
    PaintBox.Repaint;
  end;
end;

procedure TForm1.UpdateStreetList;
var
  i: Integer;
begin
  with StreetsLB.Items do begin
    BeginUpdate;
    Clear;
    for i := 0 to High(fStreets) do with (fStreets[i]) do begin
      Add(format('%f,%f - %f,%f - %d',
      [startPos.x, startPos.y, endPos.x, endPos.y, width]));
    end;
    EndUpdate;
  end;
end;

procedure TForm1.StreetsLBClick(Sender: TObject);
begin
  PaintBox.Repaint;
end;

procedure TForm1.StreetsLBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if (Key = VK_DELETE) and (StreetsLB.ItemIndex >= 0) then begin
    for i := StreetsLB.ItemIndex to High(fStreets)-1 do
      fStreets[i] := fStreets[i+1];
    SetLength(fStreets, High(fStreets));
    UpdateStreetList;
    PaintBox.Repaint;
  end;
end;

end.
