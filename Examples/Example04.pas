unit Example04;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gala, PMU;

type
  TProcessUser04 = class(TGalaProcess)
  protected
    procedure Execute; override;
    procedure DoPut(aData: Pointer);

  public
    Put: TGalaProcessChannel;

    constructor Create(aGroup: Integer; aParentForm: TForm);
  end;

  TProcessPlus04 = class(TProcessPlus)
  protected
    procedure Action; override;
  end;

  TProcessMinus04 = class(TProcessMinus)
  protected
    procedure Action; override;
  end;

  TForm04 = class(TFormPMU)
    procedure FormCreate(Sender: TObject);

  protected
    procedure CreatePlus; override;
    procedure CreateMinus; override;
    procedure CreateUser; override;
    procedure Enabler; override;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Взаимодействие по одному каналу';
  SConsume       = 'Потребляю';
  SWait          = 'Жду';
{$ELSE}
  SWindowCaption = 'Interaction by one channel';
  SConsume       = 'Consume';
  SWait          = 'Wait';
{$ENDIF}

{ TProcessUser04 }

procedure TProcessUser04.Execute;
begin
  while not Terminated do begin
    Trace(SWait);
    Accept(Put);
  end;
end;

procedure TProcessUser04.DoPut(aData: Pointer);
begin
  Send(GM_USER_PM, aData);
  Trace(SConsume);
  Pause(500);
end;

constructor TProcessUser04.Create(aGroup: Integer; aParentForm: TForm);
begin
  inherited Create(aGroup, aParentForm);
  Put := CreateChannel(DoPut);
end;

{ TProcessPlus04 }

procedure TProcessPlus04.Action;
var
  p:    TGalaProcess;
  User: TProcessUser04;
begin
  p := (ParentForm as TForm04).User;
  if Assigned(p) then begin
    User := p as TProcessUser04;
    try
      User.Put.Send(Self, @Counter);
    except
      on E: EGalaProcessWasTerminated do
        Terminate;
    end;
  end;
end;

{ TProcessMinus04 }

procedure TProcessMinus04.Action;
var
  p:    TGalaProcess;
  User: TProcessUser04;
begin
  p := (ParentForm as TForm04).User;
  if Assigned(p) then begin
    User := p as TProcessUser04;
    try
      User.Put.Send(Self, @Counter);
    except
      on E: EGalaProcessWasTerminated do
        Terminate;
    end;
  end;
end;

{ TForm04 }

procedure TForm04.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SWindowCaption;
end;

procedure TForm04.CreatePlus;
begin
  Plus := TProcessPlus04.Create(Group, Self);
end;

procedure TForm04.CreateMinus;
begin
  Minus := TProcessMinus04.Create(Group, Self);
end;

procedure TForm04.CreateUser;
begin
  User := TProcessUser04.Create(Group, Self);
end;

procedure TForm04.Enabler;
begin
  EnablerUser;
end;

end.
