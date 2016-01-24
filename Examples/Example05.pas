unit Example05;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gala, PMU;

type
  TProcessUser05 = class(TGalaProcess)
  protected
    procedure Execute; override;
    procedure DoPutPlus(aData: Pointer);
    procedure DoPutMinus(aData: Pointer);

  public
    PutPlus:  TGalaProcessChannel;
    PutMinus: TGalaProcessChannel;

    constructor Create(aGroup: Integer; aParentForm: TForm);
  end;

  TProcessPlus05 = class(TProcessPlus)
  protected
    procedure Action; override;
  end;

  TProcessMinus05 = class(TProcessMinus)
  protected
    procedure Action; override;
  end;

  TForm05 = class(TFormPMU)
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
  SWindowCaption = 'Взаимодействие с альтернативой';
  SConsume       = 'Потребляю ';
  SWait          = 'Жду';
{$ELSE}
  SWindowCaption = 'Interaction with alternative';
  SConsume       = 'Consume ';
  SWait          = 'Wait';
{$ENDIF}

{ TProcessUser05 }

procedure TProcessUser05.Execute;
begin
  while not Terminated do begin
    Trace(SWait);
    AlternativeAccept([PutPlus, PutMinus]);
  end;
end;

procedure TProcessUser05.DoPutPlus(aData: Pointer);
begin
  Send(GM_USER_PM, aData);
  Trace(SConsume + '+');
  Pause(500);
end;

procedure TProcessUser05.DoPutMinus(aData: Pointer);
begin
  Send(GM_USER_PM, aData);
  Trace(SConsume + '-');
  Pause(500);
end;

constructor TProcessUser05.Create(aGroup: Integer; aParentForm: TForm);
begin
  inherited Create(aGroup, aParentForm);
  PutPlus  := CreateChannel(DoPutPlus);
  PutMinus := CreateChannel(DoPutMinus);
end;

{ TProcessPlus05 }

procedure TProcessPlus05.Action;
var
  p:    TGalaProcess;
  User: TProcessUser05;
begin
  p := (ParentForm as TForm05).User;
  if Assigned(p) then begin
    User := p as TProcessUser05;
    try
      User.PutPlus.Send(Self, @Counter);
    except
      on E: EGalaProcessWasTerminated do
        Terminate;
    end;
  end;
end;

{ TProcessMinus05 }

procedure TProcessMinus05.Action;
var
  p:    TGalaProcess;
  User: TProcessUser05;
begin
  p := (ParentForm as TForm05).User;
  if Assigned(p) then begin
    User := p as TProcessUser05;
    try
      User.PutMinus.Send(Self, @Counter);
    except
      on E: EGalaProcessWasTerminated do
        Terminate;
    end;
  end;
end;

{ TForm05 }

procedure TForm05.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SWindowCaption;
end;

procedure TForm05.CreatePlus;
begin
  Plus := TProcessPlus05.Create(Group, Self);
end;

procedure TForm05.CreateMinus;
begin
  Minus := TProcessMinus05.Create(Group, Self);
end;

procedure TForm05.CreateUser;
begin
  User := TProcessUser05.Create(Group, Self);
end;

procedure TForm05.Enabler;
begin
  EnablerUser;
end;

end.
