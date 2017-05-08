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
  public type

    TOnChange = procedure (AIndex: Cardinal) of object;

    type
      TPinDirection = (pdWrite, pdRead);

  private
    FOnChange: TOnChange;
    FIndex: Cardinal;
    FPinDirection: TPinDirection;
    FState: Boolean;
    FConnections: TObjectSet<TPin>;

    function GetState: Boolean;
    procedure SetPinDirection(AValue: TPinDirection);
    procedure SetState(AValue: Boolean);

    procedure TestForChanges;

  public
    constructor Create(AIndex: Cardinal = 0; AOnChange: TOnChange = nil);
    destructor Destroy; override;

    procedure Connect(APin: TPin);
    procedure Disconnect(APin: TPin);

    property PinDirection: TPinDirection read FPinDirection write SetPinDirection;

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

procedure TPin.SetPinDirection(AValue: TPinDirection);
begin
  if FPinDirection = AValue then
    Exit;
  if not Assigned(FOnChange) and (AValue = pdRead) then
    raise EPinNoChangeHandler.Create;
  FPinDirection := AValue;
  FState := False;
end;

constructor TPin.Create(AIndex: Cardinal; AOnChange: TOnChange);
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
begin
  FConnections.Free;
  inherited Destroy;
end;

procedure TPin.Connect(APin: TPin);
begin
  FConnections.Add(APin);
  APin.FConnections.Add(Self);
  APin.TestForChanges;
end;

procedure TPin.Disconnect(APin: TPin);
begin
  FConnections.Del(APin);
  APin.FConnections.Del(Self);
  APin.TestForChanges;
end;

end.

