unit GalaContainer;

{$MODE Delphi}

{ Библиотека параллельного программирования Gala.
  Классы GalaContainer & GalaContainerChannel.
}

interface

uses
  Windows, SysUtils, Gala;

type
  TGalaContainer = class;

  TGalaContainerChannel = class
  private
    FOwner: TGalaContainer;
    FEntry: TGalaEntry;
    FGuard: TGalaGuard;
    FPrev:  TGalaContainerChannel;

    function Enabled: Boolean;

  public
    procedure Send(aSender: TGalaProcess; aData: Pointer = nil;
              aTimeout: Cardinal = INFINITE);
    property  Entry: TGalaEntry read FEntry write FEntry;
    property  Guard: TGalaGuard read FGuard write FGuard;
  end;

  TGalaContainer = class
  protected
    FMutex:            THandle;
    FLatch:            TGalaLatch;
    FPossibleToOccupy: THandle;
    FChannelsList:     TGalaContainerChannel;

    function  CreateChannel(aEntry: TGalaEntry; aGuard: TGalaGuard = nil):
              TGalaContainerChannel;
    procedure Log(const S: String);

  public
    constructor Create(AsMutex: Boolean = True);
    destructor  Destroy; override;
  end;

implementation

{ TGalaContainerChannel }

procedure TGalaContainerChannel.Send(aSender: TGalaProcess;
  aData: Pointer; aTimeout: Cardinal);
var
  Objs:      array[0..1] of THandle;
  ResWait:   DWORD;
  Ok:        Boolean;
  StartTime: Cardinal;

  function Timeout: Cardinal;
  var
    dt: Cardinal;
  begin
    if aTimeOut = INFINITE then
      result := INFINITE
    else begin
      dt := GetTickCount - StartTime;
      if aTimeOut > dt then
        result := aTimeout - dt
      else
        result := 1;
    end;
  end;

  procedure Action;
  begin
    { контейнер захвачен. Повторная проверка, так как за время ожидания могут
      произойти изменения, влияющие на результат охраняющей функции }
    if Enabled then begin
      // выполнение действий в контейнере в режиме взаимного исключения
      Entry(aData);
      // посылаем сигнал всем процессам, ждущим в каналах контейнера
      SetEvent(FOwner.FPossibleToOccupy);
      Ok := True;
    end;
  end;

begin
  if not Assigned(aSender) then
    raise EGalaInvalidArgument.Create('aSender', ClassName, 'Send');
  if not Assigned(aData) then
    aData := aSender;
  StartTime := GetTickCount;
  Objs[1]   := aSender.TerminationEvent;
  Ok        := False;
  while not Ok do begin
    if Enabled then begin
      if FOwner.FMutex <> 0 then begin
        Objs[0] := FOwner.FMutex;
        { ожидание мьютекса:
          a) готовности (незанятости) контейнера
          б) или внешнего завершения процесса
          в) или таймаута
        }
        ResWait := WaitForMultipleObjects(2, @Objs, False, Timeout);
        if ResWait = WAIT_FAILED then
          raise EGalaWaitFailed.Create;
        if ResWait = WAIT_TIMEOUT then
          raise EGalaTimeout.Create;
        if ResWait = (WAIT_OBJECT_0 + 1) then
          raise EGalaPrematureTermination.Create;
        try
          Action;
        finally
          ReleaseMutex(FOwner.FMutex);
        end;
      end
      else begin // критическая секция
        // ожидание готовности (незанятости) контейнера
        FOwner.FLatch.Lock;
        try
          Action;
        finally
          FOwner.FLatch.Unlock;
        end;
      end;
    end;
    if not Ok then begin
      { ожидание:
        a) возможности захвата контейнера
        б) или внешнего завершения процесса
        в) или таймаута
      }
      Objs[0] := FOwner.FPossibleToOccupy;
      ResWait := WaitForMultipleObjects(2, @Objs, False, Timeout);
      if ResWait = WAIT_FAILED then
        raise EGalaWaitFailed.Create;
      if ResWait = WAIT_TIMEOUT then
        raise EGalaTimeout.Create;
      if ResWait = (WAIT_OBJECT_0 + 1) then
        raise EGalaPrematureTermination.Create;
      // ручной сброс события
      ResetEvent(FOwner.FPossibleToOccupy);
    end;
  end;
end;

// Канал разрешен, если охраняющая функция есть и она возвращает True
function TGalaContainerChannel.Enabled: Boolean;
begin
  if Assigned(Guard) then
    result := Guard
  else
    result := True;
end;


{ TGalaContainer }

constructor TGalaContainer.Create(AsMutex: Boolean);
begin
  inherited Create;
  if AsMutex then begin
    FMutex := CreateMutex(
      nil,
      False, // изначально свободен
      nil
    );
    if FMutex = 0 then
      raise EGalaObjectCreationFail.Create;
  end
  else
    FLatch := TGalaLatch.Create;
  FPossibleToOccupy := CreateEvent(
    nil,
    True, { ручная переустановка. Используется чтобы активизировать
            одновременно все ожидающие процессы (а не один)
          }
    True, // начальное состояние - сигнализирующее
    nil
  );
  if FPossibleToOccupy = 0 then
    raise EGalaObjectCreationFail.Create;
end;

destructor TGalaContainer.Destroy;
var
  chan, prev: TGalaContainerChannel;
begin
  // уничтожение собственных каналов
  chan := FChannelsList;
  while Assigned(chan) do begin
    prev := chan.FPrev;
    chan.Free;
    chan := prev;
  end;
  if FPossibleToOccupy <> 0 then
    FileClose(FPossibleToOccupy); { *Преобразовано из CloseHandle* }
  if FMutex <> 0 then
    FileClose(FMutex); { *Преобразовано из CloseHandle* }
  FLatch.Free;
  inherited Destroy;
end;

function TGalaContainer.CreateChannel(aEntry: TGalaEntry; aGuard: TGalaGuard):
  TGalaContainerChannel;
begin
  if not Assigned(aEntry) then
    raise EGalaInvalidArgument.Create('aEntry', ClassName, 'CreateChannel');
  result        := TGalaContainerChannel.Create;
  result.FOwner := self;
  result.FEntry := aEntry;
  result.FGuard := aGuard;
  // добавление канала в список собственных каналов
  result.FPrev  := FChannelsList;
  FChannelsList := result;
end;

procedure TGalaContainer.Log(const S: String);
begin
  GalaTheater.Log(S);
end;

end.
