unit PMU;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gala;

const
  GM_PLUS    = GM_USER;
  GM_MINUS   = GM_USER + 1;
  GM_USER_PM = GM_USER + 2;

type
  TProcessPM = class(TGalaProcess)
  protected
    Counter: Integer;
    procedure Action; virtual; abstract;
  public
    constructor Create(aGroup: Integer; aParentForm: TForm);
  end;

  TProcessPlus = class(TProcessPM)
  protected
    procedure Execute; override;
  end;

  TProcessMinus = class(TProcessPM)
  protected
    procedure Execute; override;
  end;

  TFormPMU = class(TForm)
    GroupBoxPlus:  TGroupBox;
    ButtonPlus:    TButton;
    LabelPlus:     TLabel;
    GroupBoxMinus: TGroupBox;
    ButtonMinus:   TButton;
    LabelMinus:    TLabel;
    GroupBoxUser:  TGroupBox;
    ButtonUser:    TButton;
    LabelUser:     TLabel;
    ListBoxUser:   TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
    procedure ButtonUserClick(Sender: TObject);
  private
    procedure OnPlus(var Mes: TMessage); message GM_PLUS;
    procedure OnMinus(var Mes: TMessage); message GM_MINUS;
    procedure OnUser(var Mes: TMessage); message GM_USER_PM;
  protected
    Group:  Integer;
    procedure CreatePlus; virtual; abstract;
    procedure CreateMinus; virtual; abstract;
    procedure CreateUser; virtual; abstract;
    procedure Enabler; virtual;
    procedure EnablerUser; virtual;
  public
    Plus:  TProcessPM;
    Minus: TProcessPM;
    User:  TGalaProcess;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SPlusCaption   = 'Генератор положительных чисел';
  SMinusCaption  = 'Генератор отрицательных чисел';
  SUserCaption   = 'Потребитель';
  SStartCaption  = 'Старт';
  SFinishCaption = 'Финиш';
  SGenerate      = 'Генерирую';
  SWait          = 'Жду';
{$ELSE}
  SPlusCaption   = 'Producer of Plus numbers';
  SMinusCaption  = 'Producer of Minus numbers';
  SUserCaption   = 'Consumer';
  SStartCaption  = 'Start';
  SFinishCaption = 'Finish';
  SGenerate      = 'Produce';
  SWait          = 'Wait';
{$ENDIF}

var
  FormX, FormY: Integer;

{ TProcessPM }

constructor TProcessPM.Create(aGroup: Integer; aParentForm: TForm);
begin
  inherited Create(aGroup, aParentForm);
  Counter := 0;
end;

{ TProcessPlus }

procedure TProcessPlus.Execute;
begin
  while not Terminated do
  begin
    Trace(SGenerate);
    Pause(Random(1000));
    Inc(Counter);
    Trace(SWait);
    Action;
    Send(GM_PLUS, @Counter);
  end;
end;

{ TProcessMinus }

procedure TProcessMinus.Execute;
begin
  while not Terminated do
  begin
    Trace(SGenerate);
    Pause(Random(1000));
    Dec(Counter);
    Trace(SWait);
    Action;
    Send(GM_MINUS, @Counter);
  end;
end;

{ TFormPMU }

procedure TFormPMU.FormCreate(Sender: TObject);
begin
  Left   := FormX mod (Screen.DesktopWidth - Width);
  Top    := FormY mod (Screen.DesktopHeight - Height);
  FormX  := FormX + 32;
  FormY  := FormY + 32;
  Group  := GalaTheater.GetNewGroup;
  User   := nil;
  Plus   := nil;
  Minus  := nil;
  GroupBoxPlus.Caption  := SPlusCaption;
  GroupBoxMinus.Caption := SMinusCaption;
  GroupBoxUser.Caption  := SUserCaption;
  ButtonPlus.Caption    := SStartCaption;
  ButtonMinus.Caption   := SStartCaption;
  ButtonUser.Caption    := SStartCaption;
  Enabler;
end;

procedure TFormPMU.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Group <> 0 then
  begin
    GalaTheater.DestroyGroup(Group);
    Group := 0;
  end;
  Action := caFree;
end;

procedure TFormPMU.ButtonPlusClick(Sender: TObject);
begin
  if Assigned(Plus) then
  begin
    Plus.Terminate;
    Plus := nil;
    ButtonPlus.Caption := SStartCaption;
  end
  else
  begin
    CreatePlus;
    ButtonPlus.Caption := SFinishCaption;
  end;
  Enabler;
end;

procedure TFormPMU.ButtonMinusClick(Sender: TObject);
begin
  if Assigned(Minus) then
  begin
    Minus.Terminate;
    Minus := nil;
    ButtonMinus.Caption := SStartCaption;
  end
  else
  begin
    CreateMinus;
    ButtonMinus.Caption := SFinishCaption;
  end;
  Enabler;
end;

procedure TFormPMU.ButtonUserClick(Sender: TObject);
begin
  if Assigned(User) then
  begin
    User.Terminate;
    User := nil;
    ButtonUser.Caption := SStartCaption;
    ListBoxUser.Items.Clear;
  end
  else
  begin
    CreateUser;
    ButtonUser.Caption := SFinishCaption;
  end;
  Enabler;
end;

procedure TFormPMU.OnPlus(var Mes: TMessage);
begin
  LabelPlus.Caption := IntToStr(PInteger(Mes.LParam)^)
end;

procedure TFormPMU.OnMinus(var Mes: TMessage);
begin
  LabelMinus.Caption := IntToStr(PInteger(Mes.LParam)^)
end;

procedure TFormPMU.OnUser(var Mes: TMessage);
var
  s: String;
begin
  s := IntToStr(PInteger(Mes.LParam)^);
  LabelUser.Caption := s;
  if ListBoxUser.Items.Count >= 11 then
    ListBoxUser.Items.Delete(0);
  ListBoxUser.Items.Add(s);
end;

procedure TFormPMU.Enabler;
begin

end;

procedure TFormPMU.EnablerUser;
begin
  if not Assigned(User) then
  begin
    if Assigned(Plus) then
    begin
      Plus.Terminate;
      Plus := nil;
    end;
    if Assigned(Minus) then
    begin
      Minus.Terminate;
      Minus := nil;
    end;
    ButtonPlus.Caption  := SStartCaption;
    ButtonMinus.Caption := SStartCaption;
    ButtonPlus.Enabled  := False;
    ButtonMinus.Enabled := False;
  end
  else
  begin
    ButtonPlus.Enabled  := True;
    ButtonMinus.Enabled := True;
  end;
end;

initialization
  FormX := 0;
  FormY := 120;

end.
