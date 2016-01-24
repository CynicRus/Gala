unit Example03;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gala, GalaContainer, PMU;

const
  MAX_BUFFER_COUNT_03 = 8;

type
  TBufferSize03 = 0..MAX_BUFFER_COUNT_03 - 1;

  TBuffer03 = class(TGalaContainer)
  protected
    Buffer: array[TBufferSize03] of Integer;
    Count:  Integer;
    PPut:   TBufferSize03;
    PGet:   TBufferSize03;

    function  CanPut: Boolean;
    procedure DoPut(aData: Pointer);
    function  CanGet: Boolean;
    procedure DoGet(aData: Pointer);

  public
    Put: TGalaContainerChannel;
    Get: TGalaContainerChannel;

    constructor Create;
  end;

  TProcessPlus03 = class(TProcessPLus)
  protected
    procedure Action; override;
  end;

  TProcessMinus03 = class(TProcessMinus)
  protected
    procedure Action; override;
  end;

  TProcessUser03 = class(TGalaProcess)
  protected
    procedure Execute; override;
  end;

  TForm03 = class(TFormPMU)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  protected
    procedure CreatePlus; override;
    procedure CreateMinus; override;
    procedure CreateUser; override;

  public
    Buffer: TBuffer03;
  end;

implementation

{$R *.lfm}

const
{$IFDEF RUSSIAN_VERSION}
  SWindowCaption = 'Буфер между процессами';
  SConsume       = 'Потребляю';
  SWait          = 'Жду';
{$ELSE}
  SWindowCaption = 'The buffer between processes';
  SConsume       = 'Consume';
  SWait          = 'Wait';
{$ENDIF}

{ TBuffer03 }

function TBuffer03.CanPut: Boolean;
begin
  result := Count < MAX_BUFFER_COUNT_03;
end;

procedure TBuffer03.DoPut(aData: Pointer);
begin
  Buffer[PPut] := PInteger(aData)^;
  Inc(PPut);
  if PPut > High(TBufferSize03) then
    PPut := Low(TBufferSize03);
  Inc(Count);
end;

function TBuffer03.CanGet: Boolean;
begin
  result := Count > 0;
end;

procedure TBuffer03.DoGet(aData: Pointer);
begin
  PInteger(aData)^ := Buffer[PGet];
  Inc(PGet);
  if PGet > High(TBufferSize03) then
    PGet := Low(TBufferSize03);
  Dec(Count);
end;

constructor TBuffer03.Create;
begin
  inherited Create(False);
  Count := 0;
  PPut  := Low(TBufferSize03);
  PGet  := Low(TBufferSize03);
  Put   := CreateChannel(DoPut, CanPut);
  Get   := CreateChannel(DoGet, CanGet);
end;

{ TProcessPlus03 }

procedure TProcessPlus03.Action;
begin
  (ParentForm as TForm03).Buffer.Put.Send(Self, @Counter);
end;

{ TProcessMinus03 }

procedure TProcessMinus03.Action;
begin
  (ParentForm as TForm03).Buffer.Put.Send(Self, @Counter);
end;

{ TProcessUser03 }

procedure TProcessUser03.Execute;
var
  i: Integer;
begin
  while not Terminated do begin
    Trace(SWait);
    (ParentForm as TForm03).Buffer.Get.Send(Self, @i);
    Send(GM_USER_PM, @i);
    Trace(SConsume);
    Pause(500);
  end;
end;

{ TForm03 }

procedure TForm03.FormCreate(Sender: TObject);
begin
  inherited;
  Buffer  := TBuffer03.Create;
  Caption := SWindowCaption;
end;

procedure TForm03.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Buffer.Free;
end;

procedure TForm03.CreatePlus;
begin
  Plus := TProcessPlus03.Create(Group, Self);
end;

procedure TForm03.CreateMinus;
begin
  Minus := TProcessMinus03.Create(Group, Self);
end;

procedure TForm03.CreateUser;
begin
  User := TProcessUser03.Create(Group, Self);
end;

end.
