unit Example01;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Gala;

const
  GM_DRAW_01 = GM_USER;

type
  TForm01 = class(TForm)
    Label1:       TLabel;
    LabelCounter: TLabel;
    ButtonStart:  TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);

  private
    Group: Integer;

    procedure OnDraw(var Mes: TMessage); message GM_DRAW_01;
  end;

  TProcess01 = class(TGalaProcess)
  protected
    procedure Execute; override;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Самый простой - проще не бывает';
  SLabelCaption  = 'Счет:';
  SStartCaption  = 'Старт';
  SFinishCaption = 'Финиш';
{$ELSE}
  SWindowCaption = 'Most simple';
  SLabelCaption  = 'Count:';
  SStartCaption  = 'Start';
  SFinishCaption = 'Finish';
{$ENDIF}

var
  FormX, FormY: Integer;

procedure TForm01.FormCreate(Sender: TObject);
begin
  Group := 0;
  Left  := FormX mod (Screen.DesktopWidth - Width);
  Top   := FormY mod (Screen.DesktopHeight - Height);
  FormX := FormX + 32;
  FormY := FormY + Height;
  Caption             := SWindowCaption;
  Label1.Caption      := SLabelCaption;
  ButtonStart.Caption := SStartCaption;
end;

procedure TForm01.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Group <> 0) then begin
    GalaTheater.DestroyGroup(Group);
    Group := 0;
  end;
  Action := caFree;
end;

procedure TForm01.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm01.ButtonStartClick(Sender: TObject);
begin
  if Group = 0 then begin
    Group := GalaTheater.GetNewGroup;
    TProcess01.Create(Group, Self);
    ButtonStart.Caption := SFinishCaption;
  end
  else begin
    if Group <> 0 then begin
      GalaTheater.DestroyGroup(Group);
      Group := 0;
    end;
    ButtonStart.Caption := SStartCaption;
    LabelCounter.Caption := '0';
  end;
end;

procedure TForm01.OnDraw(var Mes: TMessage);
begin
  LabelCounter.Caption := IntToStr(PInteger(Mes.LParam)^);
end;

{ TProcess01 }

procedure TProcess01.Execute;
var
  Counter: Integer;
begin
  Counter := 0;
  while not Terminated do begin
    Inc(Counter);
    Send(GM_DRAW_01, @Counter);
    Pause(100);
  end;
end;

initialization
  FormX := 0;
  FormY := 120;

end.
