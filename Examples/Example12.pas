unit Example12;

{$MODE Delphi}

interface

uses
 Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Gala, GalaSignals, GalaUtils, ComCtrls;

const
  GM_NOTIFICATION_12 = GM_USER;

type
  TForm12 = class(TForm)
    EditDir:         TEdit;
    ButtonDir:       TButton;
    StringGrid:      TStringGrid;
    ButtonExplorer:  TButton;
    ButtonAttention: TButton;
    ButtonDefault:   TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonExplorerClick(Sender: TObject);
    procedure ButtonAttentionClick(Sender: TObject);
    procedure ButtonDirClick(Sender: TObject);

  private
    Group: Integer;
    Count: array[0..5] of Integer;

    procedure OnNotification(var Mes: TMessage); message GM_NOTIFICATION_12;

  public
    Event: array[0..5] of TGalaSignal;
  end;

  TProcessNotification12 = class(TGalaProcess)
  protected
    procedure Execute; override;
  end;

implementation

{$R *.lfm}

uses
  FileCtrl;

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption    = 'Файловые уведомления';
  SExplorerCaption  = 'Старт Проводника';
  SAttentionCaption = 'Внимание';
  SEvents: array[0..5] of String =
  ( 'Создание, удаление и переименование файла',
    'Создание и удаление каталога',
    'Изменение атрибутов',
    'Изменение размера файлов',
    'Была запись в файл',
    'Событие по кнопке <Внимание>');
{$ELSE}
  SWindowCaption    = 'File notifications';
  SExplorerCaption  = 'Start of Explorer';
  SAttentionCaption = 'Attention';
  SEvents: array[0..5] of String =
  ( 'Create, delete and rename of file',
    'Create and delete of directory',
    'Change file attributes',
    'Change file size',
    'Change last write',
    'Event of <Attention> button');
{$ENDIF}

var
  FormX, FormY: Integer;

procedure TForm12.FormCreate(Sender: TObject);
const
  flag: array[0..4] of Cardinal =
        ( FILE_NOTIFY_CHANGE_FILE_NAME,
          FILE_NOTIFY_CHANGE_DIR_NAME,
          FILE_NOTIFY_CHANGE_ATTRIBUTES,
          FILE_NOTIFY_CHANGE_SIZE,
          FILE_NOTIFY_CHANGE_LAST_WRITE);
var
  i:   Integer;
  Dir: String;
begin
  Group := GalaTheater.GetNewGroup;
  Left  := FormX mod (Screen.DesktopWidth - Width);
  Top   := FormY mod (Screen.DesktopHeight - Height);
  FormX := FormX + 32;
  FormY := FormY + 32;
  Caption                 := SWindowCaption;
  ButtonExplorer.Caption  := SExplorerCaption;
  ButtonAttention.Caption := SAttentionCaption;
  StringGrid.RowCount     := 6;
  for i := 0 to 5 do
    StringGrid.Cells[0, i] := SEvents[i];
  Dir := GalaTheater.ExePath;
  AnsiLower(PChar(Dir));
  EditDir.Text := Dir;
  for i := 0 to 4 do
    Event[i] := TGalaChangeNotification.Create(Dir, True, flag[i]);
  Event[5] := TGalaEvent.Create(False);
  TProcessNotification12.Create(Group, Self);
end;

procedure TForm12.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  GalaTheater.DestroyGroup(Group);
  for i := 0 to 5 do
    Event[i].Free;
  Action := caFree;
end;

procedure TForm12.ButtonDirClick(Sender: TObject);
var
  i:   Integer;
  Dir: String;
begin
  Dir := EditDir.Text;
  if SelectDirectory(Dir, [], 0) then begin
    AnsiLower(PChar(Dir));
    EditDir.Text := Dir;
    for i := 0 to 4 do
      (Event[i] as TGalaChangeNotification).NewDir(Dir);
  end;
end;

procedure TForm12.ButtonExplorerClick(Sender: TObject);
begin
  GalaExec('explorer.exe /n,/e,' + EditDir.Text);
end;

procedure TForm12.ButtonAttentionClick(Sender: TObject);
begin
  (Event[5] as TGalaEvent).SetState(True);
end;

procedure TForm12.OnNotification(var Mes: TMessage);
var
  i: Integer;
begin
  i := PInteger(Mes.LParam)^;
  if (i >= 0) and (i <= 5) then begin
    Count[i] := Count[i] + 1;
    StringGrid.Cells[1, i] := IntToStr(Count[i]);
  end
  else
    Beep;
end;

{ TProcessNotification12 }

procedure TProcessNotification12.Execute;
var
  i:  Integer;
  ok: Boolean;
begin
  while not Terminated do begin
    Ok := True;
    for i := 0 to 4 do
      if (ParentForm as TForm12).Event[i].Handle = 0 then
        Ok := False;
    try
      if Ok then
        i := AlternativeWait((ParentForm as TForm12).Event, 1000)
      else begin
        i := 5;
        Wait((ParentForm as TForm12).Event[5], 1000);
      end;
      Send(GM_NOTIFICATION_12, @i);
      Trace(IntToStr(i));
    except
      on EGalaTimeout do
        ;
      else
        raise;
    end;
  end;
end;

initialization
  FormX := 0;
  FormY := 120;

end.
