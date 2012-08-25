unit Camera;

interface

uses Geometry, dglOpenGL, Math;

type
  TCamera = class
  public
    X,Y,Z,                    // Position
    Alpha,Beta: double;       // Alpha=Hoch; Beta=Rechts in °
    procedure Apply;
    procedure DrehAlpha(Delta: double);
    procedure DrehBeta(Delta: double);

    procedure BewegReinRaus(Delta: double);
    procedure BewegVorZuruck(Delta: double);
    procedure BewegLinksRechts(Delta: double);
  end;

implementation

const
  BewegFaktor = 0.3;

{ TCamera }

procedure TCamera.Apply;
begin
  glRotatef(Alpha,-1,0,0);     // um Alpha nach oben und Beta nach Rechts drehen
  glRotatef(90+Beta,0,1,0);

  glTranslatef(-x,-y,-z);      // an die gewuenschte Position setzen
end;

procedure TCamera.DrehAlpha(Delta: double);
begin
  Alpha:= Alpha+Delta;
end;

procedure TCamera.DrehBeta(Delta: double);
begin
  Beta:= Beta-Delta;
end;

procedure TCamera.BewegReinRaus(Delta: double);
begin
  X:=X+2*BewegFaktor*Delta*cos(DegToRad(Alpha))*cos(DegToRad(Beta));
  Z:=Z+2*BewegFaktor*Delta*cos(DegToRad(Alpha))*sin(DegToRad(Beta));
  Y:=Y+2*BewegFaktor*Delta*sin(DegToRad(Alpha));
end;

procedure TCamera.BewegLinksRechts(Delta: double);
begin
 Z:=Z+BewegFaktor*Delta*cos(DegToRad(Beta));
 X:=X-BewegFaktor*Delta*sin(DegToRad(Beta));
end;

procedure TCamera.BewegVorZuruck(Delta: double);
begin
  X:=X+2*BewegFaktor*Delta*cos(DegToRad(Beta));
  Z:=Z+2*BewegFaktor*Delta*sin(DegToRad(Beta));
end;


end.
