unit ExamplesForm;

{$MODE Delphi}

interface

uses
  Messages,SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, Gala;

type
  TFormMain = class(TForm)
    PanelControl:    TPanel;
    ComboBoxExample: TComboBox;
    ButtonStart:     TButton;
    ButtonHelp:      TButton;
    ListView:        TListView;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);

  private
    procedure OnProcessStart(var Mes: TMessage);
              message GM_PROCESS_START;
    procedure OnProcessTrace(var Mes: TMessage);
              message GM_PROCESS_TRACE;
    procedure OnProcessTermination(var Mes: TMessage);
              message GM_PROCESS_TERMINATE;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Example01,
  Example02,
  Example03,
  Example04,
  Example05,
  Example06,
  Example07,
  Example08,
  Example09,
  Example10,
  Example11,
  Example12,
  Example13;

const
  EXAMPLES_AMOUNT = 13;
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Примеры использования Гала-библиотеки';
  SButtonCaption = 'старт';
  SButtonHint    = 'Выполнение примера';
  SExampleCaption: array[1..EXAMPLES_AMOUNT] of String =
  ( 'Example01: Самый простой - проще не бывает',
    'Example02: Размножающиеся процессы',
    'Example03: Буфер между процессами',
    'Example04: Взаимодействие процессов по одному каналу',
    'Example05: Взаимодействие процессов с альтернативой',
    'Example06: Взаимодействие процессов с условной альтернативой',
    'Example07: Пять обедающих философов (возможность дедлока)',
    'Example08: Пять обедающих философов (решение - таймаут)',
    'Example09: Пять обедающих философов (решение - слуга)',
    'Example10: Пять обедающих философов (альтернативные сигналы)',
    'Example11: Отладочный протокол и Завершение процессов',
    'Example12: Файловые уведомления и События',
    'Example13: Изменение состояния процесса'
  );
  SColumnCaption: array[0..2] of String = ('Группа', 'Процесс', 'Состояние');
{$ELSE}
  SWindowCaption = 'Examples of Use of the GalaLibrary';
  SButtonCaption = 'start';
  SButtonHint    = 'Example execution';
  SExampleCaption: array[1..EXAMPLES_AMOUNT] of String =
  ( 'Example01: Most simple',
    'Example02: Process propagation',
    'Example03: The buffer between processes',
    'Example04: Interaction of processes by one channel',
    'Example05: Interaction of processes with alternative',
    'Example06: Interaction of processes with conditional alternative',
    'Example07: Five philosophers having dinner (deadlock possibility)',
    'Example08: Five philosophers having dinner (solution - timeout)',
    'Example09: Five philosophers having dinner (solution - servant)',
    'Example10: Five philosophers having dinner (alternate signals)',
    'Example11: Debug Log & Processes Completion',
    'Example12: File notifications & Events',
    'Example13: Changing of the process state'
  );
  SColumnCaption: array[0..2] of String = ('Group', 'Process', 'State');
{$ENDIF}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Left := 0;
  Top  := 0;
  Randomize;
  TGalaTheater.Create;
  GalaTheater.NotificationWindow := Handle;
  Caption             := SWindowCaption;
  ButtonStart.Caption := SButtonCaption;
  ButtonStart.Hint    := SButtonHint;
  for i := 1 to EXAMPLES_AMOUNT do
    ComboBoxExample.Items.Add(SExampleCaption[i]);
  ComboBoxExample.ItemIndex := 0;
  for i := 0 to 2 do
    ListView.Columns[i].Caption := SColumnCaption[i];
  Application.HelpFile := ChangeFileExt(Application.ExeName, '.hlp');
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GalaTheater.Log('Close');
  GalaTheater.DestroyAllProcesses;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  GalaTheater.Free;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  case (ComboBoxExample.ItemIndex + 1) of
    01: TForm01.Create(Self).Show;
    02: TForm02.Create(Self).Show;
    03: TForm03.Create(Self).Show;
    04: TForm04.Create(Self).Show;
    05: TForm05.Create(Self).Show;
    06: TForm06.Create(Self).Show;
    07: TForm07.Create(Self).Show;
    08: TForm08.Create(Self).Show;
    09: TForm09.Create(Self).Show;
    10: TForm10.Create(Self).Show;
    11: TForm11.Create(Self).Show;
    12: TForm12.Create(Self).Show;
    13: TForm13.Create(Self).Show;
  end;
end;

procedure TFormMain.ButtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(ComboBoxExample.ItemIndex + 2);
end;

procedure TFormMain.OnProcessStart(var Mes: TMessage);
var
  item: TListItem;
  p:    TGalaProcess;
begin
  item := ListView.Items.Add;
  p    := TGalaProcess(Mes.LParam);
  if Assigned(item) then begin
    with item, p do begin
      Data    := p;
      Caption := IntToStr(Group);
      SubItems.Add(ProcessName);
      SubItems.Add(TraceString);
    end;
  end;
end;

procedure TFormMain.OnProcessTrace(var Mes: TMessage);
var
  item: TListItem;
  p:    TGalaProcess;
begin
  p    := TGalaProcess(Mes.LParam);
  item := ListView.FindData(-1, p, False, False);
  if Assigned(item) then
    item.SubItems.Strings[1] := p.TraceString;
end;

procedure TFormMain.OnProcessTermination(var Mes: TMessage);
var
  item: TListItem;
  p:    TGalaProcess;
begin
  p    := TGalaProcess(Mes.LParam);
  item := ListView.FindData(-1, p, False, False);
  if Assigned(item) then
    ListView.Items.Delete(item.Index);
end;

end.



