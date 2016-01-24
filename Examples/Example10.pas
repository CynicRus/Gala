unit Example10;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, ComCtrls, StdCtrls, Gala, Phil5;

type
  TPhilosopher10 = class(TPhilosopher)
  protected
    procedure Execute; override;
  end;

  TForm10 = class(TFormPhil5)
    procedure FormCreate(Sender: TObject);

  protected
    function CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher; override;
  end;

var
  Form10: TForm10;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Пять обедающих философов (сигналы)';
{$ELSE}
  SWindowCaption = 'Five dinning philosophers (signals)';
{$ENDIF}

{ TPhilosopher10 }

procedure TPhilosopher10.Execute;
begin
  while True do begin
    Meditate;
    WantToEat;
    Sit;
    NewState(stToGetFork);
    case AlternativeWait([RightFork, LeftFork]) of
      0: GetLeftFork;
      1: GetRightFork;
    end;
    Eat;
    PutLeftFork;
    PutRightFork;
  end;
end;

{ TForm10 }

function TForm10.CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher;
begin
  result := TPhilosopher10.Create(Group, Self, aIndex);
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SWindowCaption;
end;

end.
