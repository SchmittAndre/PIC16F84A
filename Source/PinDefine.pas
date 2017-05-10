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
    FConnections: TObjectSet<TPin>;

    function GetConnection(AIndex: Integer): TPin;
    function GetConnectionCount: Integer;
    function GetState: Boolean;
    procedure SetPinDirection(AValue: TPinDirection);
    procedure SetState(AValue: Boolean);

    procedure TestForChanges;

  public
    constructor Create(AIndex: Cardinal = 0; AOnChange: TChangeEvent = nil);
    destructor Destroy; override;

    procedure Connect(APin: TPin);
    procedure Disconnect(APin: TPin);

    property OnConnect: TConnectionEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TConnectionEvent read FOnDisconnect write FOnDisconnect;

    property PinDirection: TPinDirection read FPinDirection write SetPinDirection;

    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[AIndex: Integer]: TPin read GetConnection;

    property State: Boolean read GetState write SetState;

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

function TPin.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TPin.GetConnection(AIndex: Integer): TPin;
begin
  Result
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

constructor TPin.Create(AIndex: Cardinal; AOnChange: TChangeEvent);
begin
  FIndex := AIndex;
  FOnChange := AOnChange;
  if Assigned(AOnChange) then
    FPinDirection := pdRead
  else
    FPinDirection := pdWrite;
  FConnections := TObjectSet<TPin>.Create(True);
end;

destructor TPin.Destroy;
var
  Pin: TPin;
begin
  for Pin in FConnections do
  begin
    Pin.FConnections.Del(Self);
    if Pin.PinDirection = pdRead then
      Pin.TestForChanges;
  end;
  FConnections.Free;
  inherited Destroy;
end;

procedure TPin.Connect(APin: TPin);
begin
  if Assigned(OnConnect) then
    OnConnect(FIndex, APin);
  if Assigned(APin.OnConnect) then
    APin.OnConnect(FIndex, Self);
  FConnections.Add(APin);
  APin.FConnections.Add(Self);
  APin.TestForChanges;
end;

procedure TPin.Disconnect(APin: TPin);
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(FIndex, APin);
  if Assigned(APin.OnDisconnect) then
    APin.OnDisconnect(FIndex, Self);
  FConnections.Del(APin);
  APin.FConnections.Del(Self);
  APin.TestForChanges;
end;

function TPin.GetEnumerator: TObjectSet<TPin>.TIterator;
begin
  Result := FConnections.GetEnumerator;
end;

end.

