unit Example07;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, ComCtrls, StdCtrls, Gala, Phil5;

type
  TPhilosopher07 = class(TPhilosopher)
  protected
    procedure Execute; override;
  end;

  TForm07 = class(TFormPhil5)
    procedure FormCreate(Sender: TObject);

  protected
    function CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher; override;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Пять обедающих философов (дедлок)';
{$ELSE}
  SWindowCaption = 'Five dinning philosophers (deadlock)';
{$ENDIF}

{ TPhilosopher07 }

procedure TPhilosopher07.Execute;
begin
  while True do begin
    Meditate;
    WantToEat;
    Sit;
    GetRightFork;
    GetLeftFork;
    Eat;
    PutLeftFork;
    PutRightFork;
  end;
end;

{ TForm07 }

function TForm07.CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher;
begin
  result := TPhilosopher07.Create(Group, Self, aIndex);
end;

procedure TForm07.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SWindowCaption;
end;

end.
