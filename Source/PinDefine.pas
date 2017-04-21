unit PinDefine;

interface

type

  { TPin }

  TPin = class
  public type
    TReadEvent = function(AIndex: Cardinal): Boolean of object;

  private
    FDefaultState: Boolean;
    FOnRead: TReadEvent;
    FIndex: Cardinal;

    function GetConnected: Boolean;
    function GetState: Boolean;

  public
    constructor Create(AIndex: Cardinal = 0; ADefaultState: Boolean = False);

    property State: Boolean read GetState;
    property DefaultState: Boolean read FDefaultState write FDefaultState;
    property OnRead: TReadEvent read FOnRead write FOnRead;
    property Connected: Boolean read GetConnected;
  end;

implementation

{ TPin }

function TPin.GetState: Boolean;
begin
  Result := Connected and OnRead(FIndex) or not Connected and FDefaultState;
end;

function TPin.GetConnected: Boolean;
begin
  Result := Assigned(FOnRead);
end;

constructor TPin.Create(AIndex: Cardinal; ADefaultState: Boolean);
begin
  FIndex := AIndex;
  FDefaultState := ADefaultState;
end;

end.

