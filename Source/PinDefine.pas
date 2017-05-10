unit PinDefine;

interface

uses
  SysUtils, Lists;

type

  { EPinNotWriteable }

  EPinNotWriteable = class (Exception)
    constructor Create;
  end;

  { EPinNoChangeHandler }

  EPinNoChangeHandler = class (Exception)
    constructor Create;
  end;

  { TPin }

  TPinArray = class;
  TPin = class
  public
    type

      TChangeEvent = procedure (AIndex: Cardinal) of object;
      TConnectionEvent = procedure (AIndex: Cardinal; AOther: TPin) of object;

      TPinDirection = (pdWrite, pdRead);

  private
    FOnChange: TChangeEvent;
    FOnConnect: TConnectionEvent;
    FOnDisconnect: TConnectionEvent;

    FIndex: Cardinal;
    FPinDirection: TPinDirection;
    FState: Boolean;
    FConnections: TObjectArray<TPin>;

    FPinArray: TPinArray;

    function GetState: Boolean;
    procedure SetPinDirection(AValue: TPinDirection);
    procedure SetState(AValue: Boolean);

    procedure TestForChanges;

  public
    constructor Create(APinArray: TPinArray; AIndex: Cardinal = 0; AOnChange: TChangeEvent = nil);
    destructor Destroy; override;

    procedure Connect(APin: TPin);
    procedure Disconnect(APin: TPin);

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
    procedure SetOnPinConnect(AValue: TPin.TConnectionEvent);
    procedure SetOnPinDisconnect(AValue: TPin.TConnectionEvent);

    procedure VisibilityChanged;

  public
    constructor Create(AName: String; AOnPinChange: TPin.TChangeEvent = nil; ACount: Integer = 1);
    destructor Destroy; override;

    property OnNameChange: TNameChangeEvent read FOnNameChange write FOnNameChange;
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

implementation

{ EPinNoChangeHandler }

constructor EPinNoChangeHandler.Create;
begin
  inherited Create('Pin cannot be set to "read" without an OnChange-handler');
end;

{ EPinNotWriteable }

constructor EPinNotWriteable.Create;
begin
  inherited Create('Pin is not writeable');
end;

{ TPin }

procedure TPin.SetState(AValue: Boolean);
var
  Pin: TPin;
begin
  if FState = AValue then
    Exit;
  if PinDirection <> pdWrite then
    raise EPinNotWriteable.Create;
  FState := AValue;
  for Pin in FConnections do
    if Pin.PinDirection = pdRead then
      Pin.TestForChanges;
end;

procedure TPin.TestForChanges;
var
  FNewState: Boolean;
begin
  FNewState := State;
  if FState <> FNewState then
  begin
    FState := FNewState;
    FOnChange(FIndex);
  end;
end;

function TPin.GetState: Boolean;
var
  Pin: TPin;
begin
  case PinDirection of
    pdRead:
    begin
      for Pin in FConnections do
        if (Pin.PinDirection = pdWrite) and Pin.FState then
          Exit(True);
      Result := False;
    end;
    pdWrite:
      Result := FState;
  end;
end;

procedure TPin.SetPinDirection(AValue: TPinDirection);
begin
  if FPinDirection = AValue then
    Exit;
  if not Assigned(FOnChange) and (AValue = pdRead) then
    raise EPinNoChangeHandler.Create;
  FPinDirection := AValue;
  FState := False;
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
begin
  FConnections.Add(APin);
  APin.FConnections.Add(Self);
  if Assigned(OnConnect) then
    OnConnect(FIndex, APin);
  if Assigned(APin.OnConnect) then
    APin.OnConnect(APin.FIndex, Self);
  TestForChanges;
  APin.TestForChanges;
end;

procedure TPin.Disconnect(APin: TPin);
begin
  FConnections.DelObject(APin);
  APin.FConnections.DelObject(Self);
  if Assigned(OnDisconnect) then
    OnDisconnect(FIndex, APin);
  if Assigned(APin.OnDisconnect) then
    APin.OnDisconnect(APin.FIndex, Self);
  TestForChanges;
  APin.TestForChanges;
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
var
  PinArray: TPinArray;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);
  FPins.Free;
  DelAllVisiblePinArrays;
  FVisiblePinArrays.Free;
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
begin
  FVisiblePinArrays.DelObject(APinArray);
  VisibilityChanged;
  if Self <> APinArray then
  begin
    APinArray.FVisiblePinArrays.DelObject(Self);
    APinArray.VisibilityChanged;
  end;
end;

procedure TPinArray.DelAllVisiblePinArrays;
var
  PinArray: TPinArray;
begin
  for PinArray in FVisiblePinArrays do
    if PinArray <> Self then
    begin
      PinArray.FVisiblePinArrays.DelObject(Self);
      PinArray.VisibilityChanged;
    end;
  FVisiblePinArrays.DelAll;
  VisibilityChanged;
end;

end.

