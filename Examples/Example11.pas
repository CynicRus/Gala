unit Example11;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Gala, GalaUtils;

const
  GM_DRAW_11              = GM_USER;
  GM_SIGNAL_MONITOR_11    = GM_USER + 1;
  GM_PROCESSES_MONITOR_11 = GM_USER + 2;
  PROCESSES_AMOUNT_11     = 9;

type
  TForm11 = class(TForm)
    StringGrid:   TStringGrid;
    ButtonShow:   TButton;
    LabelComment: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonShowClick(Sender: TObject);

  private
    Group: Integer;
    Time0: Cardinal;

    procedure OnDraw(var Mes: TMessage); message GM_DRAW_11;
    procedure OnSignalMonitor(var Mes: TMessage); message GM_SIGNAL_MONITOR_11;
    procedure OnProcessesMonitor(var Mes: TMessage); message GM_PROCESSES_MONITOR_11;

  public
    Processes: array[0..PROCESSES_AMOUNT_11 - 1] of TGalaProcess;
  end;

  TProcess11 = class(TGalaProcess)
  protected
    procedure Execute; override;

  public
    Counter: Integer;
    Index:   Integer;

    constructor Create(aGroup: Integer; aParentForm: TForm; aIndex: Integer);
  end;

  TProcessSignalMonitor11 = class(TGalaProcess)
  private
    FSignal: TGalaSignal;

  protected
    procedure Execute; override;

  public
    constructor Create(aGroup: Integer; aParentForm: TForm; aHandle: THandle);
    destructor  Destroy; override;
  end;

  TProcessProcessesMonitor11 = class(TGalaProcess)
  protected
    procedure Execute; override;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption  = 'Отладочный протокол';
  SShowCaption    = 'Показать';
  SComment        = 'Данные выводятся в файл протокола...';
  SErrorLog       = 'Ошибка открытия файла протокола';
  SWaitSignal     = 'Жду закрытия Блокнота';
  SWaitCompletion = 'Жду завершения процессов';
{$ELSE}
  SWindowCaption  = 'Debug Log';
  SShowCaption    = 'Show Log';
  SComment        = 'Data puts to the log...';
  SErrorLog       = 'The open error of the log';
  SWaitSignal     = 'I wait Notepad closing';
  SWaitCompletion = 'I wait processes completion';
{$ENDIF}

var
  FormX, FormY: Integer;

procedure TForm11.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Group := GalaTheater.GetNewGroup;
  Time0 := GetTickCount();
  Left  := FormX mod (Screen.DesktopWidth - Width);
  Top   := FormY mod (Screen.DesktopHeight - Height);
  FormX := FormX + 32;
  FormY := FormY + 32;
  Caption              := SWindowCaption;
  ButtonShow.Caption   := SShowCaption;
  LabelComment.Caption := SComment;
  StringGrid.RowCount  := PROCESSES_AMOUNT_11;
  for i := 0 to PROCESSES_AMOUNT_11 - 1 do begin
    StringGrid.Cells[0, i] := IntToStr(i + 1);
    Processes[i] := TProcess11.Create(Group, Self, i);
  end;
  TProcessProcessesMonitor11.Create(Group, Self);
end;

procedure TForm11.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GalaTheater.DestroyGroup(Group);
  Action := caFree;
end;

procedure TForm11.ButtonShowClick(Sender: TObject);
var
  s: String;
  r: TRect;
  h: THandle;
begin
  ButtonShow.Enabled := False;
  s := 'notepad.exe ' + GalaTheater.LogFileName;
  r.Left   := Left + Width;
  r.Right  := r.Left + 300;
  r.Top    := Top;
  r.Bottom := Top + Height;
  h        := GalaExec(s, nil, SW_SHOW, @r);
  if h <> 0 then
    TProcessSignalMonitor11.Create(Group, Self, h)
  else
    Beep;
end;

procedure TForm11.OnDraw(var Mes: TMessage);
var
  p: TProcess11;
begin
  p := TProcess11(Mes.LParam);
  StringGrid.Cells[1, p.Index] := IntToStr(p.Counter);
end;

procedure TForm11.OnSignalMonitor(var Mes: TMessage);
begin
  ButtonShow.Enabled := True;
end;

procedure TForm11.OnProcessesMonitor(var Mes: TMessage);
begin
  LabelComment.Caption := '';
end;

{ TProcess11 }

procedure TProcess11.Execute;
var
  t: Double;
begin
  Log('         '#9 + ProcessName + ': '#9 + 'started');
  while (not Terminated) and (Counter < 50) do begin
    Inc(Counter);
    Send(GM_DRAW_11);
    t := Int(GetTickCount() - (ParentForm as TForm11).Time0) / 1000.0;
    Log(Format('%8.2f '#9'%s: '#9'%d', [t, ProcessName, Counter]));
    Trace(IntToStr(Counter));
    Pause(100 + Random(200));
  end;
  Log('         '#9 + ProcessName + ': '#9 + 'finished');
end;

constructor TProcess11.Create(aGroup: Integer; aParentForm: TForm;
  aIndex: Integer);
begin
  inherited Create(aGroup, aParentForm);
  Index       := aIndex;
  ProcessName := 'Process11.' + IntToStr(aGroup) + '.' + IntToStr(aIndex + 1);
end;

{ TProcessSignalMonitor11 }

constructor TProcessSignalMonitor11.Create(aGroup: Integer; aParentForm: TForm;
  aHandle: THandle);
begin
  inherited Create(aGroup, aParentForm);
  FSignal := TGalaSignal.Create(aHandle);
end;

destructor TProcessSignalMonitor11.Destroy;
begin
  FSignal.Free;
  inherited Destroy;
end;

procedure TProcessSignalMonitor11.Execute;
begin
  Trace(SWaitSignal);
  Wait(FSignal);
  Send(GM_SIGNAL_MONITOR_11);
end;

{ TProcessProcessesMonitor11 }

procedure TProcessProcessesMonitor11.Execute;
begin
  Trace(SWaitCompletion);
  WaitCompletion((ParentForm as TForm11).Processes, 30000);
  Send(GM_PROCESSES_MONITOR_11);
end;

initialization
  FormX := 0;
  FormY := 120;

end.
