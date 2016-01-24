unit Example06;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gala, PMU, Example05;

type
  TProcessUser06 = class(TProcessUser05)
  protected
    procedure Execute; override;
    function  CanPutPlus: Boolean;
    function  CanPutMinus: Boolean;

  public
    constructor Create(aGroup: Integer; aParentForm: TForm);
  end;

  TForm06 = class(TForm05)
    CheckBoxPlus:  TCheckBox;
    CheckBoxMinus: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure CheckBoxPlusClick(Sender: TObject);
    procedure CheckBoxMinusClick(Sender: TObject);

  protected
    procedure CreateUser; override;

  public
    EnablePlus:  Boolean;
    EnableMinus: Boolean;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Взаимодействие с условной альтернативой';
  SWait          = 'Жду';
  STimeout       = 'Таймаут';
{$ELSE}
  SWindowCaption = 'Interaction with conditional alternative';
  SWait          = 'Wait';
  STimeout       = 'Timeout';
{$ENDIF}

{ TProcessUser06 }

constructor TProcessUser06.Create(aGroup: Integer; aParentForm: TForm);
begin
  inherited Create(aGroup, aParentForm);
  PutPlus.Guard  := CanPutPlus;
  PutMinus.Guard := CanPutMinus;
end;

procedure TProcessUser06.Execute;
begin
  while not Terminated do begin
    Trace(SWait);
    try
      AlternativeAccept([PutPlus, PutMinus], 2000);
    except
      on E: EGalaTimeout do begin
        Trace(STimeout);
        Pause(500);
      end
      else
        raise;
    end;
  end;
end;

function TProcessUser06.CanPutPlus: Boolean;
begin
  result := (ParentForm as TForm06).EnablePlus;
end;

function TProcessUser06.CanPutMinus: Boolean;
begin
  result := (ParentForm as TForm06).EnableMinus;
end;

{ TForm06 }

procedure TForm06.FormCreate(Sender: TObject);
begin
  inherited;
  EnablePlus  := True;
  EnableMinus := True;
  CheckBoxPlus.Checked  := EnablePlus;
  CheckBoxMinus.Checked := EnableMinus;
  Caption := SWindowCaption;
end;

procedure TForm06.CheckBoxPlusClick(Sender: TObject);
begin
  EnablePlus := CheckBoxPlus.Checked;
end;

procedure TForm06.CheckBoxMinusClick(Sender: TObject);
begin
  EnableMinus := CheckBoxMinus.Checked;
end;

procedure TForm06.CreateUser;
begin
  User := TProcessUser06.Create(Group, Self);
end;

end.
