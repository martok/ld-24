{
 ***************************************************************
 *
 * Unit Name: FastGL
 * Purpose  : Simplification of common tasks in OpenGL.
 * Author   : Sebastian H.
 * History  :
 *
 ****************************************************************
}

unit FastGL;

interface

uses dglOpenGL,Windows,GLHelper,Graphics, Geometry;


var
  glFOV          : single =   45.0;
  glNearClipping : single =    1.0;
  glFarClipping  : single = 1000.0;

procedure glOpenRender(DC:HDC;W,H:integer;var RC:HGLRC);      overload;
procedure glOpenRender(Wnd:HWND;W,H:integer;var DC:HDC; var RC:HGLRC);  overload;
procedure glInit;
procedure glResizeWnd(Width, Height : Integer);
function glInitFont(Name:String;DC:HDC; Height:integer;Style:TFontStyles=[]):TGLuint;
function glInit3DFont(Name:String;DC:HDC; Height:integer;Extrusion:TGLfloat;Style:TFontStyles=[]; Mode:integer=WGL_FONT_POLYGONS):TGLuint;

procedure glPreDraw(R:TGLFloat=0.1;G:TGLFloat=0.1;B:TGLFloat=0.3;A:TGLFloat=1);
procedure glPosition(X,Y,Z,Theta,Phi:single);
procedure glLight(Pos:TVector; ambient, diffuse, Specular:TRGBA; Number:integer; SourceSize:GLFloat);
procedure glTextOut(Font:TGLuint; Text:String);
procedure glRotater(R,X,Y,Z:GLFloat);
procedure glAfterDraw(DC:HDC);

procedure glCloseRender(RC:HGLRC);

function Normalize(X:Double; isDegrees:boolean=false):double;

const
  FNT_CHAR_COUNT    =   256;

implementation

uses Math;

function Normalize(X:Double; isDegrees:boolean=false):double;
begin
  if isDegrees then begin
    while X<0   do X:= X+360;
    while X>360 do X:= X-360;
  end else begin
    while X<0    do X:= X+2*pi;
    while X>2*pi do X:= X-2*pi;
  end;
  Result:= X;
end;


{------------------------------------------------------------------}
{  Initialisierung von OpenGL                                      }
{------------------------------------------------------------------}
procedure glInit();
begin
  glEnable(GL_TEXTURE_2D);	       // Aktiviert Texture Mapping
  glShadeModel(GL_SMOOTH);	       // Aktiviert weiches Shading
  glClearColor(0.0, 0.0, 0.0, 0.5);    // Bildschirm löschen (schwarz)
  glClearDepth(1.0);		       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);	       // Aktiviert Depth Testing
  glDepthFunc(GL_LEQUAL);	       // Bestimmt den Typ des Depth Testing
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_LIGHTING);
end;


{------------------------------------------------------------------}
{  Behandelt Größenveränderung des Fensters                        }
{------------------------------------------------------------------}
procedure glResizeWnd(Width, Height : Integer);
begin
  if (Height = 0) then Height := 1;
  glViewport(0, 0, Width, Height);    // Setzt den Viewport für das OpenGL Fenster
  glMatrixMode(GL_PROJECTION);        // Matrix Mode auf Projection setzen
  glLoadIdentity();                   // Reset View
  gluPerspective(glFOV, Width/Height, glNearClipping, glFarClipping);  // Perspektive den neuen Maßen anpassen.

  glMatrixMode(GL_MODELVIEW);         // Zurück zur Modelview Matrix
  glLoadIdentity();                   // Reset View
end;


{------------------------------------------------------------------}
{  Font vorbereiten                                                }
{------------------------------------------------------------------}
function glInitFont(Name:String;DC:HDC; Height:integer;Style:TFontStyles=[]):TGLuint;
var
  CustomFont:HFont;
  w:integer;
begin
  Result := glGenLists(FNT_CHAR_COUNT);
  w:= IfThen(fsBold in Style,700,0);
  (*CustomFont := GetStockObject (SYSTEM_FONT);*)//Eine Alternative zu CreateFont
  CustomFont := CreateFont(Height,              // Höhe
                           0,                   // Breite 0=Keine Vorgabe
                           0,
                           0,
                           w,                         // Fett?
                           Byte(fsItalic    in Style),// Kursiv?
                           Byte(fsUnderline in Style),// Unterstrichen?
                           Byte(fsStrikeout in Style),// Durchgestrichen?
                           ANSI_CHARSET,
                           OUT_TT_PRECIS,
                           CLIP_DEFAULT_PRECIS,
                           NONANTIALIASED_QUALITY,
                           FF_DONTCARE or DEFAULT_PITCH,
                           PChar(Name));        // Name der Schrift
  SelectObject(DC, CustomFont);
  wglUseFontBitmaps (DC, 0, FNT_CHAR_COUNT-1, Result);
  DeleteObject(CustomFont);
end;

function glInit3DFont(Name:String;DC:HDC; Height:integer;Extrusion:TGLfloat;Style:TFontStyles=[]; Mode:integer=WGL_FONT_POLYGONS):TGLuint;
var
  CustomFont:HFont;
  w:integer;
  agmf:array [0..255] of GLYPHMETRICSFLOAT ;
begin
  Result := glGenLists(FNT_CHAR_COUNT);
  w:= IfThen(fsBold in Style,700,0);
  (*CustomFont := GetStockObject (SYSTEM_FONT);*)//Eine Alternative zu CreateFont
  CustomFont := CreateFont(Height,              // Höhe
                           0,                   // Breite 0=Keine Vorgabe
                           0,
                           0,
                           w,                         // Fett?
                           Byte(fsItalic    in Style),// Kursiv?
                           Byte(fsUnderline in Style),// Unterstrichen?
                           Byte(fsStrikeout in Style),// Durchgestrichen?
                           ANSI_CHARSET,
                           OUT_TT_PRECIS,
                           CLIP_DEFAULT_PRECIS,
                           NONANTIALIASED_QUALITY,
                           FF_DONTCARE or DEFAULT_PITCH,
                           PChar(Name));        // Name der Schrift
  SelectObject(DC, CustomFont);
  wglUseFontOutlines(DC,
                     0,   //Von Zeichen #0
                     FNT_CHAR_COUNT-1, //Bis Zeichen #255
                     Result,
                     0, //So genau wie möglich
                     Extrusion, //0.2 LE tiefe Buchstaben
                     Mode,//Linen keine Polygone
                     @agmf);//Speichere dort die Informationen
  DeleteObject(CustomFont);
end;

{------------------------------------------------------------------}
{  Zeichnen der Szene                                              }
{------------------------------------------------------------------}
procedure glPreDraw(R:TGLFloat;G:TGLFloat;B:TGLFloat;A:TGLFloat);
begin
  glClearColor(R,G,B,A);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

{------------------------------------------------------------------}
{  "Kamera" o.ä. positionieren                                     }
{------------------------------------------------------------------}
procedure glPosition(X,Y,Z,Theta,Phi:single);
var RotMatrix: THomogeneousMatrix;
begin
  Theta:= Normalize(Theta);
  Rotmatrix:= IdentityHmgMatrix;
  RotMatrix:= Turn(RotMatrix,Theta);
  RotMatrix:= Pitch(RotMatrix,Phi);
{
  RotMatrix:= MatrixMultiply(CreateRotationMatrixY(Sin(DegToRad(90-Theta)),
                                                   Cos(DegToRad(90-Theta))),RotMatrix);
  RotMatrix:= MatrixMultiply(CreateRotationMatrixX(Sin(DegToRad(Phi)),
                                                   Cos(DegToRad(Phi))),RotMatrix); }
  glTranslatef(X,Y, Z);
  glRotater(Theta,0,1,0);
  glRotater(Phi,1,0,0);
  //glMultMatrixf(@RotMatrix[0,0]);
end;

{------------------------------------------------------------------}
{  "Light" positionieren                                          }
{------------------------------------------------------------------}
procedure glLight(Pos:TVector;
       ambient,
       diffuse,
       Specular:TRGBA;
       Number:integer;
       SourceSize:GLFloat);
var quadObj:PGLUquadric;
begin
  glLightfv(GL_LIGHT0+Number, GL_AMBIENT,  @ambient.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_DIFFUSE,  @diffuse.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_SPECULAR, @Specular.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_POSITION, @Pos[0]);

//  glLightf(GL_LIGHT0+Number, GL_CONSTANT_ATTENUATION, 1.0);
//  glLightf(GL_LIGHT0+Number, GL_LINEAR_ATTENUATION, 0.001);
//  glLightf(GL_LIGHT0+Number, GL_QUADRATIC_ATTENUATION, 0.004);

  glEnable(GL_LIGHT0+Number);
  
  if SourceSize>0 then begin
    quadObj := gluNewQuadric;
    glPushMatrix;
      glTranslatef(Pos[0],Pos[1],Pos[2]);
      gluQuadricDrawStyle(quadObj, GLU_FILL);
      gluQuadricNormals(quadObj, GLU_SMOOTH);
      gluSphere(quadObj, SourceSize, 36, 36);
    glPopMatrix;
    gluDeleteQuadric(quadObj);
  end;
end;


{------------------------------------------------------------------}
{  Text ausgeben                                                   }
{------------------------------------------------------------------}
procedure glTextOut(Font:TGLuint; Text:String);
begin
  glPushMatrix;
    glListBase(Font); //Liste auswählen
    glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));//Entsprechende Listen aufrufen
  glPopMatrix;
end;


{------------------------------------------------------------------}
{  Rotate mt Rad                                                   }
{------------------------------------------------------------------}
procedure glRotater(R,X,Y,Z:GLFloat);
begin
  glRotatef(R*180/pi,X,Y,Z);
end;


{------------------------------------------------------------------}
{  Zeichnen der Szene II                                           }
{------------------------------------------------------------------}
procedure glAfterDraw(DC:HDC);
begin
  SwapBuffers(DC);
end;

{------------------------------------------------------------------}
{  Anlegen von Context...                                          }
{------------------------------------------------------------------}
procedure glOpenRender(DC:HDC;W,H:integer;var RC:HGLRC);
begin
  InitOpenGL;
  RC:= CreateRenderingContext( DC,
                               [opDoubleBuffered],
                               32,
                               24,
                               0,0,0,
                               0);
  ActivateRenderingContext(DC, RC);
  ReadExtensions;
  glResizeWnd(W,H);
  glInit;
end;

procedure glOpenRender(Wnd:HWND;W,H:integer;var DC:HDC; var RC:HGLRC);
begin
  DC:= GetDC(Wnd);
  glOpenRender(DC,W,H,RC);
end;



{------------------------------------------------------------------}
{  Freigeben von Context...                                        }
{------------------------------------------------------------------}
procedure glCloseRender(RC:HGLRC);
begin
  DeactivateRenderingContext;
  DestroyRenderingContext(RC);
end;


end.
