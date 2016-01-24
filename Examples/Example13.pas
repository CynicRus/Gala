unit Example13;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Gala;

type
  TProcess13 = class(TGalaProcess)
  public
    Counter: Integer;

  protected
    procedure Execute; override;
    procedure OnNormalTermination; override;
    procedure OnPrematureTermination; override;
  end;

  TForm13 = class(TForm)
    GroupBox1:        TGroupBox;
    StaticText1:      TStaticText;
    LabelTimer:       TLabel;
    GroupBox2:        TGroupBox;
    StaticText2:      TStaticText;
    TrackBarPriority: TTrackBar;
    LabelPriority:    TLabel;
    ButtonStart:      TButton;
    ButtonResume:     TButton;
    ButtonClear:      TButton;
    TimerDraw:        TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerDrawTimer(Sender: TObject);
    procedure TrackBarPriorityChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonResumeClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);

  private
    Group:    Integer;
    Process1: TProcess13;
    Process2: TProcess13;

    procedure DisplayPriority;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption    = 'Изменение состояния процесса';
  SGroupBox1Caption = 'Процесс с нормальным приоритетом';
  SGroupBox2Caption = 'Процесс с изменяемым состоянием';
  SPriorityCaption  = 'Приоритет';
  SStartCaption     = 'Создать';
  SStopCaption      = 'Завершить';
  SResumeCaption    = 'Возобновить';
  SSuspendCaption   = 'Остановить';
  SClearCaption     = 'Очистить оба счетчика';
{$ELSE}
  SWindowCaption    = 'Changing of the process state';
  SGroupBox1Caption = 'The process with normal priority';
  SGroupBox2Caption = 'The process with a changed state';
  SPriorityCaption  = 'Priority';
  SStartCaption     = 'Create';
  SStopCaption      = 'Terninate';
  SResumeCaption    = 'Resume';
  SSuspendCaption   = 'Suspend';
  SClearCaption     = 'To clear both counters';
{$ENDIF}

var
  FormX, FormY: Integer;

{ TProcess13 }

procedure TProcess13.Execute;
var
  i: Integer;
  t: Cardinal;
begin
  t := GetTickCount + 1000;
  while not Terminated do begin
    for i := 0 to 100000 do begin
      if GetTickCount > t then begin
        t := GetTickCount + 1000;
        Pause(10);
      end;
    end;
    Inc(Counter);
  end;
end;

procedure TProcess13.OnNormalTermination;
begin
  Counter := 0;
end;

procedure TProcess13.OnPrematureTermination;
begin
  Counter := 0;
end;


{ TForm13 }

procedure TForm13.FormCreate(Sender: TObject);
begin
  Group := GalaTheater.GetNewGroup;
  Left  := FormX mod (Screen.DesktopWidth - Width);
  Top   := FormY mod (Screen.DesktopHeight - Height);
  FormX := FormX + 32;
  FormY := FormY + 32;
  Caption              := SWindowCaption;
  GroupBox1.Caption    := SGroupBox1Caption;
  GroupBox2.Caption    := SGroupBox2Caption;
  ButtonStart.Caption  := SStopCaption;
  ButtonResume.Caption := SSuspendCaption;
  ButtonClear.Caption  := SClearCaption;
  Process1 := TProcess13.Create(Group, Self);
  Process2 := TProcess13.Create(Group, Self);
  DisplayPriority;
end;

procedure TForm13.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Process1 := nil;
  Process2 := nil;
  GalaTheater.DestroyGroup(Group);
  Action := caFree;
end;

procedure TForm13.TimerDrawTimer(Sender: TObject);
begin
  if LabelTimer.Font.Color = clWhite then
    LabelTimer.Font.Color := clBlack
  else
    LabelTimer.Font.Color := clWhite;
  if Assigned(Process1) then
    StaticText1.Caption := IntToStr(Process1.Counter)
  else
    StaticText1.Caption := '';
  if Assigned(Process2) then
    StaticText2.Caption := IntToStr(Process2.Counter)
  else
    StaticText2.Caption := '';
end;

procedure TForm13.TrackBarPriorityChange(Sender: TObject);
begin
  if Assigned(Process2) then
    Process2.Priority := TrackBarPriority.Position;
  DisplayPriority;
end;

procedure TForm13.ButtonStartClick(Sender: TObject);
begin
  if Assigned(Process2) then begin
    Process2.Terminate;
    Process2 := nil;
    ButtonStart.Caption      := SStartCaption;
    ButtonResume.Caption     := SSuspendCaption;
    ButtonResume.Enabled     := False;
    TrackBarPriority.Enabled := False;
  end
  else begin
    Process2 := TProcess13.Create(Group, Self);
    ButtonStart.Caption       := SStopCaption;
    ButtonResume.Enabled      := True;
    ButtonResume.Caption      := SSuspendCaption;
    TrackBarPriority.Enabled  := True;
    TrackBarPriority.Position := 0;
  end;
  DisplayPriority;
end;

procedure TForm13.ButtonResumeClick(Sender: TObject);
begin
  if Assigned(Process2) then begin
    if Process2.Suspended then begin
      Process2.Resume;
      ButtonResume.Caption := SSuspendCaption;
    end
    else begin
      Process2.Suspend;
      ButtonResume.Caption := SResumeCaption;
    end;
  end;
end;

procedure TForm13.ButtonClearClick(Sender: TObject);
begin
  if Assigned(Process1) then
    Process1.Counter := 0;
  if Assigned(Process2) then
    Process2.Counter := 0;
end;

procedure TForm13.DisplayPriority;
begin
  if Assigned(Process2) then
    LabelPriority.Caption := SPriorityCaption + ': ' +
      IntToStr(TrackBarPriority.Position)
  else
    LabelPriority.Caption := '';
end;

initialization
  FormX := 0;
  FormY := 120;

end.
