unit Example09;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, ComCtrls, StdCtrls, Gala, GalaSignals, Phil5;

type
  TPhilosopher09 = class(TPhilosopher)
  protected
    procedure Execute; override;
  end;

  TForm09 = class(TFormPhil5)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  protected
    function CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher; override;

  public
    Servant: TGalaSemaphore;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Пять обедающих философов (слуга)';
{$ELSE}
  SWindowCaption = 'Five dinning philosophers (servant)';
{$ENDIF}

{ TPhilosopher09 }

procedure TPhilosopher09.Execute;
begin
  while True do begin
    Meditate;
    WantToEat;
    Wait((ParentForm as TForm09).Servant);
    Sit;
    GetRightFork;
    GetLeftFork;
    Eat;
    PutLeftFork;
    PutRightFork;
    (ParentForm as TForm09).Servant.Release;
  end;
end;

{ TForm09 }

procedure TForm09.FormCreate(Sender: TObject);
begin
  Servant := TGalaSemaphore.Create(PHIL_COUNT - 1);
  inherited;
  Caption := SWindowCaption;
end;

procedure TForm09.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Servant.Free;
end;

function TForm09.CreatePhilosopher(aIndex: TPhilIndex): TPhilosopher;
begin
  result := TPhilosopher09.Create(Group, Self, aIndex);
end;

end.
