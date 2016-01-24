unit Example02;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Gala;

const
  WIDTH_02   = 45;
  HEIGHT_02  = 29;
  GM_DRAW_02 = GM_USER;

type
  TX_02 = 0..WIDTH_02  - 1;
  TY_02 = 0..HEIGHT_02 - 1;

  // Направление движения - приращения по X и Y
  TDirection02 = record
    X, Y: -1..1;
  end;

  TForm02 = class(TForm)
    Panel:               TPanel;
    PaintBox:            TPaintBox;
    LabelCMaxProcess:    TLabel;
    LabelMaxProcess:     TLabel;
    ScrollBarMaxProcess: TScrollBar;
    LabelCProcessCount:  TLabel;
    LabelProcessCount:   TLabel;
    LabelListCount:      TLabel;
    ButtonStart:         TButton;
    ButtonFinish:        TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonFinishClick(Sender: TObject);
    procedure ScrollBarMaxProcessChange(Sender: TObject);

  private
    CellWidth,
    CellHeight: Integer;
    ScaleX,
    ScaleY:     Double;
    Group:      Integer;
    Count,
    MaxCount:   Integer;

    procedure OnDraw(var Mes: TMessage);        message GM_DRAW_02;
    procedure OnStart(var Mes: TMessage);       message GM_PROCESS_START;
    procedure OnTermination(var Mes: TMessage); message GM_PROCESS_TERMINATE;
    procedure ShowListCount;

  public
    function CanCreate: Boolean;
  end;

  TProcess02 = class(TGalaProcess)
  private
    DeathTick,
    DivideTick: Cardinal;
    Direction:  TDirection02;
    Speed:      Integer;

  protected
    procedure Execute; override;
    procedure OnNormalTermination; override;
    procedure OnPrematureTermination; override;

  public
    PrevPoint,
    NextPoint:      TPoint;
    Color:          TColor;
    CanChildCreate: Boolean;

    constructor Create(aGroup: Integer; aParentForm: TForm; aPoint: TPoint;
                aDirection: TDirection02);
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption  = 'Размножающиеся процессы';
  SMaxCaption     = 'Макс. процессов';
  SCountCaption   = 'Число процессов';
  SStartCaption   = 'Старт';
  SFinishCaption  = 'Финиш';
{$ELSE}
  SWindowCaption  = 'Process propagation';
  SMaxCaption     = 'Max. processes';
  SCountCaption   = 'Process count';
  SStartCaption   = 'Start';
  SFinishCaption  = 'Finish';
{$ENDIF}

var
  FormX, FormY: Integer;

{ TForm02 }

procedure TForm02.FormCreate(Sender: TObject);
begin
  Left     := FormX mod (Screen.DesktopWidth - Width);
  Top      := FormY mod (Screen.DesktopHeight - Height);
  FormX    := FormX + 32;
  FormY    := FormY + 32;
  Group    := 0;
  Count    := 0;
  MaxCount := 5;
  with PaintBox do begin
    CellWidth  := Width  div WIDTH_02;
    CellHeight := Height div HEIGHT_02;
    ScaleX     := Width   /  WIDTH_02;
    ScaleY     := Height  /  HEIGHT_02;
  end;
  LabelMaxProcess.Caption      := IntToStr(MaxCount);
  ScrollBarMaxProcess.Position := MaxCount;
  Caption                      := SWindowCaption;
  LabelCMaxProcess.Caption     := SMaxCaption;
  LabelCProcessCount.Caption   := SCountCaption;
  ButtonStart.Caption          := SStartCaption;
  ButtonFinish.Caption         := SFinishCaption;
  ShowListCount;
end;

procedure TForm02.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Group <> 0 then begin
    GalaTheater.DestroyGroup(Group);
    Group := 0;
  end;
  Action := caFree;
end;

procedure TForm02.ButtonStartClick(Sender: TObject);
var
  p: TPoint;
  d: TDirection02;
begin
  try
    Group := GalaTheater.GetNewGroup;
    p.x   := (WIDTH_02 div 4)  + Random(WIDTH_02 div 2);
    p.y   := (HEIGHT_02 div 4) + Random(HEIGHT_02 div 2);
    d.x   := 1;
    d.y   := -1;
    ButtonStart.Enabled := False;
    TProcess02.Create(Group, Self, p, d);
    ButtonFinish.Enabled := True;
  except
    on E: EGalaObjectCreationFail do begin
      Application.MessageBox(PChar(E.Message), 'GalaExample', MB_OK);
      Group := 0;
    end;
  end;
end;

procedure TForm02.ButtonFinishClick(Sender: TObject);
begin
  if GalaTheater.TryToDestroyGroup(Group) then begin
    ButtonFinish.Enabled := False;
    ButtonStart.Enabled  := True;
  end;
end;

procedure TForm02.ScrollBarMaxProcessChange(Sender: TObject);
begin
  MaxCount                := ScrollBarMaxProcess.Position;
  LabelMaxProcess.Caption := IntToStr(MaxCount);
end;

procedure TForm02.OnDraw(var Mes: TMessage);
var
  rp, rn: TRect;
begin
  with TProcess02(Mes.LParam) do begin
    with rp, PrevPoint do begin
      Left   := Round(ScaleX * x);
      Right  := Left + CellWidth;
      Top    := Round(ScaleY * y);
      Bottom := Top + CellHeight;
    end;
    with rn, NextPoint do begin
      Left   := Round(ScaleX * x);
      Right  := Left + CellWidth;
      Top    := Round(ScaleY * y);
      Bottom := Top + CellHeight;
    end;
    with PaintBox.Canvas do begin
      Brush.Color := clWhite;
      FillRect(rp);
      Brush.Color := Color;
      FillRect(rn);
    end;
  end;
end;

procedure TForm02.OnStart(var Mes: TMessage);
begin
  ShowListCount;
  Inc(Count);
  LabelProcessCount.Caption := IntToStr(Count);
end;

procedure TForm02.OnTermination(var Mes: TMessage);
begin
  ShowListCount;
  Dec(Count);
  LabelProcessCount.Caption := IntToStr(Count);
end;

procedure TForm02.ShowListCount;
begin
  LabelListCount.Caption := Format('A:%d, T:%d',
    [GalaTheater.AllActiveCount, GalaTheater.AllTerminatedCount]);
end;

function TForm02.CanCreate: Boolean;
begin
  result := Count < MaxCount;
end;

{ TProcess02 }

procedure TProcess02.Execute;
var
  p: TPoint;
  d: TDirection02;
const
  Border = 3;
begin
  FFreeOnTerminate := True;
  Priority   := -1;
  DivideTick := GetTickCount + 1000 + Cardinal(Random(2000));
  Send(GM_PROCESS_START);
  while (not Terminated) and (GetTickCount < DeathTick) do begin
    with NextPoint do begin
      x := PrevPoint.x + Direction.x;
      y := PrevPoint.y + Direction.y;
      if (x <= Low(TX_02)) or (x >= High(TX_02)) then
        Direction.x := -Direction.x;
      if (y <= Low(TY_02)) or (y >= High(TY_02)) then
        Direction.y := -Direction.y;
    end;
    if (GetTickCount > DivideTick) then begin
      DivideTick := GetTickCount + 1000 + Cardinal(Random(2000));
      if  (PrevPoint.x >= (Low(TX_02)  + Border)) and
          (PrevPoint.y >= (Low(TY_02)  + Border)) and
          (PrevPoint.x <= (High(TX_02) - Border)) and
          (PrevPoint.y <= (High(TY_02) - Border)) and
          ((ParentForm as TForm02).CanCreate)     then
      begin
        p   := PrevPoint;
        d   := Direction;
        d.x := -d.x;
        try
          TProcess02.Create(Group, ParentForm, p, d);
        except
          on EGalaObjectCreationFail do
            Beep;
        end;
      end;
    end;
    try
      Send(GM_DRAW_02, Self, ParentForm, 200);
      PrevPoint := NextPoint;
      Pause(Speed);
    except
      on EGalaTimeout do
        Terminate;
    end;
  end;
end;

procedure TProcess02.OnPrematureTermination;
begin
  OnNormalTermination;
end;

procedure TProcess02.OnNormalTermination;
begin
  Color := clWhite;
  Send(GM_DRAW_02);
  Send(GM_PROCESS_TERMINATE);
end;

constructor TProcess02.Create(aGroup: Integer; aParentForm: TForm;
  aPoint: TPoint; aDirection: TDirection02);
begin
  inherited Create(aGroup, aParentForm);
  PrevPoint := aPoint;
  NextPoint := PrevPoint;
  Direction := aDirection;
  Color     := TColor(Random(Integer(clYellow)));
  DeathTick := GetTickCount + 5000 + Cardinal(Random(10000));
  Speed     := 40 + Random(200);
end;

initialization
  FormX := 0;
  FormY := 120;

end.
