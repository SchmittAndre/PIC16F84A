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

      TChangeEvent = procedure (APin: TPin) of object;
      TConnectionEvent = procedure (ASender, AOther: TPin) of object;

      TPinDirection = (pdWrite, pdRead);

  private
    FOnChange: TChangeEvent;
    FOnConnect: TConnectionEvent;
    FOnDisconnect: TConnectionEvent;

    FIndex: Cardinal;
    FPinDirection: TPinDirection;
    FState: Cardinal;
    FConnections: TObjectArray<TPin>;

    FPinArray: TPinArray;

    procedure SendSignalHelp(ACount: Integer; AIgnoreList: TPinSet);
    procedure SendSignal(ACount: Integer);

    procedure ReevaluateSystem;

    function GetState: Boolean;
    procedure SetPinDirection(AValue: TPinDirection);
    procedure SetState(AValue: Boolean);

  public
    constructor Create(APinArray: TPinArray; AIndex: Cardinal = 0; AOnChange: TChangeEvent = nil);
    destructor Destroy; override;

    procedure Connect(APin: TPin);
    procedure Disconnect(APin: TPin);

    function ConnectedTo(APin: TPin): Boolean;

    property OnChange: TChangeEvent read FOnChange write FOnChange;
    property OnConnect: TConnectionEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TConnectionEvent read FOnDisconnect write FOnDisconnect;

    property PinArray: TPinArray read FPinArray;
    property Index: Cardinal read FIndex;

    property PinDirection: TPinDirection read FPinDirection write SetPinDirection;

    function GetEnumerator: TObjectArray<TPin>.TIterator;

    property State: Boolean read GetState write SetState;

  end;

  { TPinArray }

  TPinArray = class
  public
    type

      TNameChangeEvent = procedure (Sender: TPinArray; AOldName, ANewName: String; var AReject: Boolean) of object;
      TVisibilityChangeEvent = procedure (Sender: TPinArray) of object;

      TCreateEvent = procedure (Sender: TPinArray) of object;
      TDestroyEvent = procedure (Sender: TPinArray) of object;

  private

    FPins: TObjectArray<TPin>;
    FVisiblePinArrays: TObjectArray<TPinArray>;
    FName: String;

    FOnPinChange: TPin.TChangeEvent;
    FOnPinConnect: TPin.TConnectionEvent;
    FOnPinDisconnect: TPin.TConnectionEvent;
    FOnVisibilityChange: TVisibilityChangeEvent;
    FOnNameChange: TNameChangeEvent;

    class var
      FOnCreate: TCreateEvent;
      FOnDestroy: TDestroyEvent;

    function GetCount: Integer;
    function GetPin(AIndex: Integer): TPin;
    function GetVisiblePinArray(AIndex: Integer): TPinArray;
    function GetVisiblePinArrayCount: Integer;
    procedure SetCount(AValue: Integer);
    procedure SetOnPinChange(AValue: TPin.TChangeEvent);
    procedure SetOnPinConnect(AValue: TPin.TConnectionEvent);
    procedure SetOnPinDisconnect(AValue: TPin.TConnectionEvent);

    procedure VisibilityChanged;

  public
    constructor Create(AName: String; AOnPinChange: TPin.TChangeEvent = nil; ACount: Integer = 1);
    destructor Destroy; override;

    property OnNameChange: TNameChangeEvent read FOnNameChange write FOnNameChange;
    property OnPinChange: TPin.TChangeEvent read FOnPinChange write SetOnPinChange;
    property OnPinConnect: TPin.TConnectionEvent read FOnPinConnect write SetOnPinConnect;
    property OnPinDisconnect: TPin.TConnectionEvent read FOnPinDisconnect write SetOnPinDisconnect;
    property OnVisibilityChange: TVisibilityChangeEvent read FOnVisibilityChange write FOnVisibilityChange;

    property Name: String read FName;

    property Pins[AIndex: Integer]: TPin read GetPin; default;
    property Count: Integer read GetCount write SetCount;

    property VisiblePinArrays[AIndex: Integer]: TPinArray read GetVisiblePinArray;
    property VisiblePinArrayCount: Integer read GetVisiblePinArrayCount;

    procedure AddVisiblePinArray(APinArray: TPinArray);
    procedure DelVisiblePinArray(APinArray: TPinArray);
    procedure DelAllVisiblePinArrays;

    class property OnCreate: TCreateEvent read FOnCreate write FOnCreate;
    class property OnDestroy: TDestroyEvent read FOnDestroy write FOnDestroy;

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
  if PinDirection <> pdWrite then
    raise EPinNotWriteable.Create(Self);
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
  if (Before <> State) and Assigned(OnChange) then
    OnChange(Self);

  AIgnoreList.Add(Self);

  for Pin in FConnections do
  begin
    if not AIgnoreList[Pin] and (Pin.PinDirection = pdRead) then
    begin
      Pin.SendSignalHelp(ACount, AIgnoreList);
    end;
  end;
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
    if (APin.PinDirection = pdWrite) and APin.State then
      Result := 1
    else
      Result := 0;
    for Pin in FConnections do
      if not APinList[Pin] then
        Result := Result + FindPoweredPins(APin, APinList);
  end;

var
  PinList: TPinSet;
  Pin: TPin;
  PoweredPins: Cardinal;
  Before: Boolean;
begin
  // Find all pins
  // Count powered write pins
  // Set all pins read pins to count of powered wite pins
  // Send OnChange events if something changed

  PinList := TPinSet.Create(True);
  PoweredPins := FindPoweredPins(Self, PinList);

  for Pin in PinList do
    if Pin.PinDirection = pdRead then
    begin
      Before := Pin.State;
      Pin.FState := PoweredPins;
      if (Before <> Pin.State) and Assigned(OnChange) then
        Pin.OnChange(Pin);
    end;

  PinList.Free;
end;

function TPin.GetState: Boolean;
begin
  Result := FState > 0;
end;

procedure TPin.SetPinDirection(AValue: TPinDirection);
begin
  if FPinDirection = AValue then
    Exit;
  if (AValue = pdRead) and not Assigned(FOnChange) then
    raise EPinNoChangeHandler.Create(Self);
  FPinDirection := AValue;
  if (FPinDirection = pdRead) and State then
    SendSignal(-1);
end;

constructor TPin.Create(APinArray: TPinArray; AIndex: Cardinal; AOnChange: TChangeEvent);
begin
  FPinArray := APinArray;
  FIndex := AIndex;
  FOnChange := AOnChange;
  if Assigned(AOnChange) then
    FPinDirection := pdRead
  else
    FPinDirection := pdWrite;
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
begin
  if Self = APin then
    Exit;
  for Pin in FConnections do
    if Pin = APin then
      Exit;
  SendSignal(APin.FState);
  APin.SendSignal(FState);
  FConnections.Add(APin);
  APin.FConnections.Add(Self);
  if Assigned(OnConnect) then
    OnConnect(Self, APin);
  if Assigned(APin.OnConnect) then
    APin.OnConnect(APin, Self);
end;

procedure TPin.Disconnect(APin: TPin);
begin
  FConnections.DelObject(APin);
  APin.FConnections.DelObject(Self);
  if Assigned(OnDisconnect) then
    OnDisconnect(Self, APin);
  if Assigned(APin.OnDisconnect) then
    APin.OnDisconnect(APin, Self);
  ReevaluateSystem;
  APin.ReevaluateSystem;
end;

function TPin.ConnectedTo(APin: TPin): Boolean;
begin
  Result := FConnections.FindObject(APin) <> -1;
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
var
  Pin: TPin;
begin
  while Count > AValue do
    FPins.DelLast;
  while Count < AValue do
  begin
    Pin := FPins.Add(TPin.Create(Self, FPins.Count, FOnPinChange));
    Pin.OnConnect := OnPinConnect;
    Pin.OnDisconnect := OnPinDisconnect;
  end;
end;

procedure TPinArray.SetOnPinChange(AValue: TPin.TChangeEvent);
var
  Pin: TPin;
begin
  FOnPinChange := AValue;
  for Pin in FPins do
    Pin.OnChange := AValue;
end;

procedure TPinArray.SetOnPinConnect(AValue: TPin.TConnectionEvent);
var
  Pin: TPin;
begin
  FOnPinConnect := AValue;
  for Pin in FPins do
    Pin.OnConnect := AValue;
end;

procedure TPinArray.SetOnPinDisconnect(AValue: TPin.TConnectionEvent);
var
  Pin: TPin;
begin
  FOnPinDisconnect := AValue;
  for Pin in FPins do
    Pin.OnDisconnect := AValue;
end;

procedure TPinArray.VisibilityChanged;
begin
  if Assigned(OnVisibilityChange) then
    OnVisibilityChange(Self);
end;

constructor TPinArray.Create(AName: String; AOnPinChange: TPin.TChangeEvent; ACount: Integer);
begin
  FName := AName;
  FOnPinChange := AOnPinChange;
  FPins := TObjectArray<TPin>.Create;
  FVisiblePinArrays := TObjectArray<TPinArray>.Create(True);
  Count := ACount;
  if Assigned(OnCreate) then
    OnCreate(Self);
end;

destructor TPinArray.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);
  DelAllVisiblePinArrays;
  FVisiblePinArrays.Free;
  FPins.Free;
  inherited Destroy;
end;

procedure TPinArray.AddVisiblePinArray(APinArray: TPinArray);
begin
  FVisiblePinArrays.Add(APinArray);
  VisibilityChanged;
  if Self <> APinArray then
  begin
    APinArray.FVisiblePinArrays.Add(Self);
    APinArray.VisibilityChanged;
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
  VisibilityChanged;
  if Self <> APinArray then
  begin
    APinArray.FVisiblePinArrays.DelObject(Self);
    APinArray.VisibilityChanged;
  end;
end;

procedure TPinArray.DelAllVisiblePinArrays;
begin
  while not FVisiblePinArrays.Empty do
    DelVisiblePinArray(FVisiblePinArrays.Last);
end;

end.

