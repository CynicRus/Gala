unit Phil5;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ExtCtrls, ComCtrls, StdCtrls, Gala, GalaSignals;

const
  PHIL_COUNT = 5;

type
  TPhilState = (stToMeditate, stWantToEat, stToSit,
                stToGetFork, stToGetRightFork, stToGetLeftFork,
                stToEat,
                stToPutFork, stToPutForks, stToPutRightFork, stToPutLeftFork,
                stOnTimeout, stInPerplexity);
  TPhilIndex = 1..PHIL_COUNT;
  TFormPhil5 = class;
  TPhilosopher = class;

  TFork = class(TGalaMutex)
  protected
    procedure AfterWaiting(p: TGalaProcess); override;
  public
    Owner: TGalaProcess;
    procedure Release; override;
  end;

  TPhilosopher = class(TGalaProcess)
  protected
    Index:     TPhilIndex;
    LeftFork,
    RightFork: TFork;
    State:     TPhilState;
    procedure  NewState(aState: TPhilState; aTime: Cardinal = 0);
    procedure  Meditate;
    procedure  WantToEat;
    procedure  Sit;
    procedure  Eat;
    procedure  BePerplexed;
    procedure  GetLeftFork(aTimeout: Cardinal = INFINITE);
    procedure  GetRightFork(aTimeout: Cardinal = INFINITE);
    procedure  PutLeftFork;
    procedure  PutRightFork;
  public
    constructor Create(aGroup: Integer; aParentForm: TForm; aIndex: TPhilIndex);
  end;

  TFormPhil5 = class(TForm)
    PanelHome:     TPanel;
    PaintBoxHome:  TPaintBox;
    PanelRoom:     TPanel;
    PaintBoxRoom:  TPaintBox;
    ImageListPhil: TImageList;
    ImageListFork: TImageList;
    LabelState:    TLabel;
    TreeViewState: TTreeView;
    DrawTimer:     TTimer;
    ButtonDefault: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DrawTimerTimer(Sender: TObject);
  protected
    function CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher;
               virtual; abstract;
  public
    Group:       Integer;
    Philosopher: array[TPhilIndex] of TPhilosopher;
    Fork:        array[TPhilIndex] of TFork;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SPhilState: array[TPhilState] of String =
  ( 'думаю',
    'хочу есть',
    'сажусь',
    'беру вилку',
    'беру правую вилку',
    'беру левую вилку',
    'ем',
    'кладу вилку',
    'кладу обе вилки',
    'кладу правую вилку',
    'кладу левую вилку',
    'вилки нет!!!',
    'недоумеваю'
  );
  SPhilName   = 'Философ ';
  SLabelState = 'Мнемоники состояний';
{$ELSE}
  SPhilState: array[TPhilState] of String =
  ( 'to meditate',
    'to want to eat',
    'to sit',
    'to get fork',
    'to get right fork',
    'to get left fork',
    'to eat',
    'to put fork',
    'to put forks',
    'to put right fork',
    'to put left fork',
    'fork is absent!!!',
    'in perplexity'
  );
  SPhilName   = 'Philosopher ';
  SLabelState = 'State mnemonics';
{$ENDIF}

var
  FormX, FormY: Integer;

{ TFork }

procedure TFork.AfterWaiting(p: TGalaProcess);
begin
  Owner := p;
end;

procedure TFork.Release;
begin
  Owner := nil;
  inherited Release;
end;

{ TPhilosopher }

constructor TPhilosopher.Create(aGroup: Integer;
  aParentForm: TForm; aIndex: TPhilIndex);
begin
  inherited Create(aGroup, aParentForm);
  Index := aIndex;
  RightFork := (ParentForm as TFormPhil5).Fork[Index];
  if Index = High(TPhilIndex) then
    LeftFork := (ParentForm as TFormPhil5).Fork[Low(TPhilIndex)]
  else
    LeftFork := (ParentForm as TFormPhil5).Fork[Succ(Index)];
  State             := stToMeditate;
  ProcessName       := SPhilName + IntToStr(Index);
  FStackSize        := 8000;
  FSuspendedOnStart := True;
end;

procedure TPhilosopher.NewState(aState: TPhilState; aTime: Cardinal);
begin
  State := aState;
  Trace(SPhilState[State]);
  if aTime <> 0 then
    Pause(aTime);
end;

procedure TPhilosopher.Meditate;
begin
  NewState(stToMeditate, 4000 + Random(8000));
end;

procedure TPhilosopher.WantToEat;
begin
  NewState(stWantToEat, 1000);
end;

procedure TPhilosopher.Sit;
begin
  NewState(stToSit, 2000);
end;

procedure TPhilosopher.Eat;
begin
  NewState(stToEat, 2000 + Random(7000));
end;

procedure TPhilosopher.BePerplexed;
begin
  NewState(stOnTimeout, 2000);
  NewState(stInPerplexity, 1000 + Random(3000));
end;

procedure TPhilosopher.GetLeftFork(aTimeout: Cardinal);
begin
  NewState(stToGetLeftFork);
  Wait(LeftFork, aTimeout);
  Pause(1000);
end;

procedure TPhilosopher.GetRightFork(aTimeout: Cardinal);
begin
  NewState(stToGetRightFork);
  Wait(RightFork, aTimeout);
  Pause(1000);
end;

procedure TPhilosopher.PutLeftFork;
begin
  NewState(stToPutLeftFork);
  LeftFork.Release;
  Pause(1000);
end;

procedure TPhilosopher.PutRightFork;
begin
  NewState(stToPutRightFork);
  RightFork.Release;
  Pause(1000);
end;

{ TFormPhil5 }

procedure TFormPhil5.FormCreate(Sender: TObject);
var
  i: TPhilIndex;

  procedure ToMnemo(aState: TPhilState);
  var
    Node: TTreeNode;
  begin
    with TreeViewState.Items do
    begin
      Node := AddChild(nil, SPhilState[aState]);
      Node.ImageIndex    := Ord(aState);
      Node.SelectedIndex := Ord(aState);
    end;
  end;

begin
  Left  := FormX mod (Screen.DesktopWidth - Width);
  Top   := FormY mod (Screen.DesktopHeight - Height);
  FormX := FormX + 32;
  FormY := FormY + 32;
  Group := GalaTheater.GetNewGroup;
  for i := Low(TPhilIndex) to High(TPhilIndex) do
    Fork[i] := TFork.Create(False);
  for i := Low(TPhilIndex) to High(TPhilIndex) do
    Philosopher[i] := CreatePhilosopher(i);
  GalaTheater.ResumeGroup(Group);
  ToMnemo(stToMeditate);
  ToMnemo(stWantToEat);
  ToMnemo(stToSit);
  ToMnemo(stToGetFork);
  ToMnemo(stToEat);
  ToMnemo(stOnTimeout);
  ToMnemo(stInPerplexity);
  LabelState.Caption := SLabelState;
end;

procedure TFormPhil5.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: TPhilIndex;
begin
  GalaTheater.DestroyGroup(Group);
  for i := Low(TPhilIndex) to High(TPhilIndex) do
    Fork[i].Free;
  Action := caFree;
end;

procedure TFormPhil5.DrawTimerTimer(Sender: TObject);
const
  RFORK  = 42;
  RTABLE = RFORK + 18;
  RPHIL  = RTABLE + 18;
var
  i, j:   TPhilIndex;
  st:     TPhilState;
  f, F0:  Double;
  X0, Y0,
  x,  y:  Integer;
  bmp:    TBitmap;
begin
  X0 := PaintBoxRoom.ClientWidth  div 2;
  Y0 := PaintBoxRoom.ClientHeight div 2;
  F0 := 2 * Pi / PHIL_COUNT;

  bmp := TBitmap.Create;
  try
    bmp.Width  := PaintBoxHome.ClientWidth;
    bmp.Height := PaintBoxHome.ClientHeight;
    bmp.Canvas.Font := Font;
    for i := Low(TPhilIndex) to High(TPhilIndex) do
    begin
      st := Philosopher[i].State;
      if st <= stWantToEat then
      begin
        x := (PaintBoxHome.ClientWidth div PHIL_COUNT) * Ord(i) - 33;
        y := (PaintBoxHome.ClientHeight div 2) - 16;
        ImageListPhil.Draw(bmp.Canvas, x, y, Ord(st));
        bmp.Canvas.TextOut(x, y, IntToStr(Ord(i)));
      end;
    end;
    PaintBoxHome.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;

  bmp := TBitmap.Create;
  try
    bmp.Width  := PaintBoxRoom.ClientWidth;
    bmp.Height := PaintBoxRoom.ClientHeight;
    bmp.Canvas.Font := Font;
    for i := Low(TPhilIndex) to High(TPhilIndex) do
    begin
      st := Philosopher[i].State;
      if st > stWantToEat then
      begin
        f := F0 * (Ord(i) - 1);
        x := X0 - Round(RPHIL * Sin(f)) - 16;
        y := Y0 - Round(RPHIL * Cos(f)) - 16;
        ImageListPhil.Draw(bmp.Canvas, x, y, Ord(st));
        bmp.Canvas.TextOut(x, y, IntToStr(Ord(i)));
      end;
      if Assigned(Fork[i].Owner) then
      begin
        j := (Fork[i].Owner as TPhilosopher).Index;
        f := F0 * (Ord(j) - 1);
        if (j = i) then
          f := f - F0 / 3
        else
          f := f + F0 / 3;
        x := X0 - Round(RPHIL * Sin(f)) - 16;
        y := Y0 - Round(RPHIL * Cos(f)) - 16;
      end
      else
      begin
        f := F0 * (Ord(i) - 1) - F0 / 2;
        x := X0 - Round(RFORK * Sin(f)) - 16;
        y := Y0 - Round(RFORK * Cos(f)) - 16;
      end;
      ImageListFork.Draw(bmp.Canvas, x, y, 0);
      bmp.Canvas.TextOut(x, y, IntToStr(Ord(i)));
    end;
    bmp.Canvas.Arc(X0 - RTABLE, Y0 - RTABLE,
      X0 + RTABLE + 1, Y0 + RTABLE + 1,
      X0, Y0 - RTABLE, X0 + 1, Y0 - RTABLE);
    PaintBoxRoom.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

initialization
  FormX := 0;
  FormY := 120;

end.
