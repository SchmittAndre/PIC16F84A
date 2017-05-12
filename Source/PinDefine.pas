unit PinDefine;

interface

uses
  SysUtils, Lists;

type

  TPin = class;
  TPinArray = class;

  TPinSet = TObjectSet<TPin>;

  { TPin }

  TPin = class
  public
    type

      TDirection = (pdWrite, pdRead);

  private
    FWritePower: Boolean;

    FIndex: Cardinal;
    FDirection: TDirection;
    FState: Cardinal;
    FConnections: TObjectArray<TPin>;

    FPinArray: TPinArray;

    procedure SendSignalHelp(ACount: Integer; AIgnoreList: TPinSet);
    procedure SendSignal(ACount: Integer);

    procedure ReevaluateSystem;

    function GetState: Boolean;
    procedure SetDirection(AValue: TDirection);
    procedure SetState(AValue: Boolean);

  public
    constructor Create(APinArray: TPinArray; AIndex: Cardinal = 0);
    destructor Destroy; override;

    procedure Connect(APin: TPin);
    procedure Disconnect(APin: TPin);

    function ConnectedTo(APin: TPin): Boolean;
    function ConnectedToRecursive(APin: TPin): Boolean;

    property PinArray: TPinArray read FPinArray;
    property Index: Cardinal read FIndex;

    property Direction: TDirection read FDirection write SetDirection;

    function GetEnumerator: TObjectArray<TPin>.TIterator;

    property WritePower: Boolean read FWritePower;
    property State: Boolean read GetState write SetState;
    property PowerSources: Cardinal read FState;

  end;

  { TPinArray }

  TPinArray = class
  public
    type

      TPinChangeEvent = procedure (APin: TPin) of object;
      TPinConnectionEvent = procedure (ASender, AOther: TPin) of object;
      TPinRedirectEvent = procedure (APin: TPin) of object;
      TNameChangeEvent = procedure (Sender: TPinArray; AOldName, ANewName: String; var AReject: Boolean) of object;
      TVisibilityChangeEvent = procedure (Sender: TPinArray) of object;
      TCreateEvent = procedure (Sender: TPinArray) of object;
      TDestroyEvent = procedure (Sender: TPinArray) of object;

  private
    FPins: TObjectArray<TPin>;
    FVisiblePinArrays: TObjectArray<TPinArray>;
    FName: String;

    function GetCount: Integer;
    function GetPin(AIndex: Integer): TPin;
    function GetVisiblePinArray(AIndex: Integer): TPinArray;
    function GetVisiblePinArrayCount: Integer;
    procedure SetCount(AValue: Integer);
    procedure SetName(AValue: String);

  public

    OnPinChange: TDelegate<TPinChangeEvent>;
    OnPinConnect: TDelegate<TPinConnectionEvent>;
    OnPinDisconnect: TDelegate<TPinConnectionEvent>;
    OnPinRedirect: TDelegate<TPinRedirectEvent>;

    OnNameChange: TDelegate<TNameChangeEvent>;
    OnVisibilityChange: TDelegate<TVisibilityChangeEvent>;

    class var
      OnCreate: TDelegate<TCreateEvent>;
      OnDestroy: TDelegate<TDestroyEvent>;

    constructor Create(AName: String; ACount: Integer = 1);
    destructor Destroy; override;

    property Name: String read FName write SetName;

    property Pins[AIndex: Integer]: TPin read GetPin; default;
    property Count: Integer read GetCount write SetCount;

    property VisiblePinArrays[AIndex: Integer]: TPinArray read GetVisiblePinArray;
    property VisiblePinArrayCount: Integer read GetVisiblePinArrayCount;

    procedure AddVisiblePinArray(APinArray: TPinArray);
    procedure DelVisiblePinArray(APinArray: TPinArray);
    procedure DelAllVisiblePinArrays;

  end;

  { EPinNotWriteable }

  EPinNotWriteable = class (Exception)
    constructor Create(APin: TPin);
  end;

  { EPinNoChangeHandler }

  EPinNoChangeHandler = class (Exception)
    constructor Create(APin: TPin);
  end;


implementation

{ EPinNoChangeHandler }

constructor EPinNoChangeHandler.Create(APin: TPin);
begin
  inherited CreateFmt('Pin %d in PinArray %s cannot be set to "read" without an OnChange-handler',
                      [APin.Index, APin.PinArray.Name]);
end;

{ EPinNotWriteable }

constructor EPinNotWriteable.Create(APin: TPin);
begin
  inherited CreateFmt('Pin %d in PinArray %s is not writeable',
                      [APin.Index, APin.PinArray.Name]);
end;

{ TPin }

procedure TPin.SetState(AValue: Boolean);
begin
  if Direction <> pdWrite then
    raise EPinNotWriteable.Create(Self);
  FWritePower := AValue;
  if AValue = State then
    Exit;
  if AValue then
    SendSignal(+1)
  else
    SendSignal(-1);
end;

procedure TPin.SendSignalHelp(ACount: Integer; AIgnoreList: TPinSet);
var
  Pin: TPin;
  Before: Boolean;
begin
  Before := State;
  Inc(FState, ACount);
  if Before <> State then
    PinArray.OnPinChange.Call(Self);

  AIgnoreList.Add(Self);

  for Pin in FConnections do
    if not AIgnoreList[Pin] then
      Pin.SendSignalHelp(ACount, AIgnoreList);
end;

procedure TPin.SendSignal(ACount: Integer);
var
  IgnoreList: TPinSet;
begin
  IgnoreList := TPinSet.Create(True);
  SendSignalHelp(ACount, IgnoreList);
  IgnoreList.Free;
end;

procedure TPin.ReevaluateSystem;

  function FindPoweredPins(APin: TPin; APinList: TPinSet): Cardinal;
  var
    Pin: TPin;
  begin
    APinList.Add(APin);
    if APin.WritePower then
      Result := 1
    else
      Result := 0;
    for Pin in APin.FConnections do
      if not APinList[Pin] then
        Result := Result + FindPoweredPins(Pin, APinList);
  end;

var
  PinList: TPinSet;
  Pin: TPin;
  PoweredPins: Cardinal;
  Before: Boolean;
begin
  PinList := TPinSet.Create(True);
  PoweredPins := FindPoweredPins(Self, PinList);

  for Pin in PinList do
  begin
    Before := Pin.State;
    Pin.FState := PoweredPins;
    if Before <> Pin.State then
      Pin.PinArray.OnPinChange.Call(Pin);
  end;

  PinList.Free;
end;

function TPin.GetState: Boolean;
begin
  Result := FState > 0;
end;

procedure TPin.SetDirection(AValue: TDirection);
begin
  if FDirection = AValue then
    Exit;
  FDirection := AValue;
  PinArray.OnPinRedirect.Call(Self);
  if FDirection = pdRead then
  begin
    FWritePower := False;
    if State then
      SendSignal(-1);
  end;
end;

constructor TPin.Create(APinArray: TPinArray; AIndex: Cardinal);
begin
  FPinArray := APinArray;
  FIndex := AIndex;
  FDirection := pdRead;
  FConnections := TObjectArray<TPin>.Create(True);
end;

destructor TPin.Destroy;
var
  Pin: TPin;
begin
  for Pin in FConnections.IterReversed do
    Disconnect(Pin);
  FConnections.Free;
  inherited Destroy;
end;

procedure TPin.Connect(APin: TPin);
var
  Pin: TPin;
  Tmp: Cardinal;
begin
  if Self = APin then
    Exit;
  for Pin in FConnections do
    if Pin = APin then
      Exit;
  if not ConnectedToRecursive(APin) then
  begin
    Tmp := FState;
    SendSignal(APin.FState);
    APin.SendSignal(Tmp);
  end;
  FConnections.Add(APin);
  APin.FConnections.Add(Self);
  PinArray.OnPinConnect.Call(Self, APin);
  APin.PinArray.OnPinConnect.Call(APin, Self);
end;

procedure TPin.Disconnect(APin: TPin);
begin
  FConnections.DelObject(APin);
  APin.FConnections.DelObject(Self);
  PinArray.OnPinDisconnect.Call(Self, APin);
  APin.PinArray.OnPinDisconnect.Call(APin, Self);
  ReevaluateSystem;
  APin.ReevaluateSystem;
end;

function TPin.ConnectedTo(APin: TPin): Boolean;
begin
  Result := FConnections.FindObject(APin) <> -1;
end;

function TPin.ConnectedToRecursive(APin: TPin): Boolean;

  function Find(APos, APin: TPin; APinList: TPinSet): Boolean;
  var
    Pin: TPin;
  begin
    if APos = APin then
      Exit(True);
    APinList.Add(APos);
    for Pin in APos.FConnections do
      if not APinList[Pin] and Find(Pin, APin, APinList) then
        Exit(True);
    Result := False;
  end;

var
  PinList: TPinSet;
begin
  PinList := TPinSet.Create(False);
  Result := Find(Self, APin, PinList);
  PinList.Free;
end;

function TPin.GetEnumerator: TObjectArray<TPin>.TIterator;
begin
  Result := FConnections.GetEnumerator;
end;

{ TPinArray }

function TPinArray.GetCount: Integer;
begin
  Result := FPins.Count;
end;

function TPinArray.GetPin(AIndex: Integer): TPin;
begin
  Result := FPins[AIndex];
end;

function TPinArray.GetVisiblePinArray(AIndex: Integer): TPinArray;
begin
  Result := FVisiblePinArrays[AIndex];
end;

function TPinArray.GetVisiblePinArrayCount: Integer;
begin
  Result := FVisiblePinArrays.Count;
end;

procedure TPinArray.SetCount(AValue: Integer);
begin
  while Count > AValue do
    FPins.DelLast;
  while Count < AValue do
    FPins.Add(TPin.Create(Self, FPins.Count));
end;

procedure TPinArray.SetName(AValue: String);
var
  Rejected: Boolean;
begin
  if FName = AValue then
    Exit;
  OnNameChange.Call(Self, FName, AValue, Rejected);
  if not Rejected then
    FName := AValue;
end;

constructor TPinArray.Create(AName: String; ACount: Integer);
begin
  FName := AName;
  FPins := TObjectArray<TPin>.Create;
  Count := ACount;

  FVisiblePinArrays := TObjectArray<TPinArray>.Create(True);

  OnCreate.Call(Self);
end;

destructor TPinArray.Destroy;
begin
  OnDestroy.Call(Self);

  DelAllVisiblePinArrays;

  FVisiblePinArrays.Free;
  FPins.Free;
  inherited Destroy;
end;

procedure TPinArray.AddVisiblePinArray(APinArray: TPinArray);
begin
  FVisiblePinArrays.Add(APinArray);
  OnVisibilityChange.Call(Self);
  if Self <> APinArray then
  begin
    APinArray.FVisiblePinArrays.Add(Self);
    APinArray.OnVisibilityChange.Call(APinArray);
  end;
end;

procedure TPinArray.DelVisiblePinArray(APinArray: TPinArray);
var
  I: Integer;
  Pin: TPin;
  DisconnectPins: TObjectArray<TPin>;
begin
  DisconnectPins := TObjectArray<TPin>.Create(True);
  for I := 0 to Count - 1 do
  begin
    DisconnectPins.DelAll;
    for Pin in Pins[I] do
      if Pin.PinArray = APinArray then
        DisconnectPins.Add(Pin);
    for Pin in DisconnectPins do
      Pins[I].Disconnect(Pin);
  end;
  DisconnectPins.Free;
  FVisiblePinArrays.DelObject(APinArray);
  OnVisibilityChange.Call(Self);
  if Self <> APinArray then
  begin
    APinArray.FVisiblePinArrays.DelObject(Self);
    APinArray.OnVisibilityChange.Call(Self);
  end;
end;

procedure TPinArray.DelAllVisiblePinArrays;
begin
  while not FVisiblePinArrays.Empty do
    DelVisiblePinArray(FVisiblePinArrays.Last);
end;

end.

