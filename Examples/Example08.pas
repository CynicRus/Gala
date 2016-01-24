unit Example08;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, ComCtrls, StdCtrls, Gala, Phil5;

type
  TPhilosopher08 = class(TPhilosopher)
  protected
    procedure Execute; override;
  end;

  TForm08 = class(TFormPhil5)
    procedure FormCreate(Sender: TObject);

  protected
    function CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher; override;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Пять обедающих философов (таймаут)';
{$ELSE}
  SWindowCaption = 'Five dinning philosophers (timeout)';
{$ENDIF}

{ TPhilosopher08 }

procedure TPhilosopher08.Execute;
begin
  while True do begin
    Meditate;
    WantToEat;
    Sit;
    repeat
      repeat
        try
          GetRightFork(7000);
        except
          on E: EGalaTimeout do
            BePerplexed
          else
            raise;
        end;
      until RightFork.Owner = Self;
      try
        GetLeftFork(7000);
      except
        on E: EGalaTimeout do begin
          PutRightFork;
          BePerplexed;
        end
        else
          raise;
      end;
    until LeftFork.Owner = Self;
    Eat;
    PutLeftFork;
    PutRightFork;
  end;
end;

{ TForm08 }

function TForm08.CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher;
begin
  result := TPhilosopher08.Create(Group, Self, aIndex);
end;

procedure TForm08.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SWindowCaption;
end;

end.
