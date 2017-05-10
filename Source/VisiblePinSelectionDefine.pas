unit VisiblePinSelectionDefine;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls, ExtCtrls, ActnList, PinDefine,
  Lists;

type

  { TPinArray }

  TPinArray = class
  public
    type

      TNameChangeEvent = procedure (Sender: TPinArray; AOldName, ANewName: String; var AReject: Boolean) of object;
      TVisibilityChangeEvent = procedure (Sender: TPinArray) of object;

  private
    FPins: TObjectArray<TPin>;
    FVisiblePinArrays: TObjectArray<TPinArray>;
    FName: String;

    FOnPinChange: TPin.TChangeEvent;
    FOnPinConnect: TPin.TConnectionEvent;
    FOnPinDisconnect: TPin.TConnectionEvent;
    FOnVisibilityChange: TVisibilityChangeEvent;
    FOnNameChange: TNameChangeEvent;

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

  end;

  { TfrmVisiblePinSelection }

  TfrmVisiblePinSelection = class(TForm)
    actMoveEntryDown: TAction;
    actMoveEntryUp: TAction;
    alVisiblePins: TActionList;
    btnCancel: TButton;
    btnOK: TButton;
    btnUp: TButton;
    btnDown: TButton;
    clbVisiblePinArrays: TCheckListBox;
    lbHeader: TLabel;
    pnlControl: TPanel;
  private
    FPinArrays: TObjectArray<TPinArray>;

    function GetPinArray(AIndex: Integer): TPinArray;
    function GetPinArrayCount: Integer;
    procedure LoadPinDevice(APinArray: TPinArray);
    procedure SavePinDevice(APinArray: TPinArray);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute(APinDevice: TPinArray);

    procedure RegisterArray(APinArray: TPinArray);
    procedure UnregisterArray(APinArray: TPinArray);

    property PinArrayCount: Integer read GetPinArrayCount;
    property PinArrays[AIndex: Integer]: TPinArray read GetPinArray;
  end;

var
  frmVisiblePinSelection: TfrmVisiblePinSelection;

implementation

{$R *.lfm}

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
    Pin := FPins.Add(TPin.Create(FPins.Count, FOnPinChange));
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
  frmVisiblePinSelection.RegisterArray(Self);
end;

destructor TPinArray.Destroy;
var
  PinArray: TPinArray;
begin
  frmVisiblePinSelection.UnregisterArray(Self);
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

{ TfrmVisiblePinSelection }

procedure TfrmVisiblePinSelection.LoadPinDevice(APinArray: TPinArray);
var
  I: Integer;
begin
  lbHeader.Caption := 'I want to connect the ' + APinArray.Name + ' to:';
  clbVisiblePinArrays.Clear;
  for I := 0 to APinArray.VisiblePinArrayCount - 1 do
    clbVisiblePinArrays.AddItem(APinArray.VisiblePinArrays[I].Name, APinArray.VisiblePinArrays[I]);
  clbVisiblePinArrays.CheckAll(cbChecked);
  for I := 0 to FPinArrays.Count - 1 do
    if clbVisiblePinArrays.Items.IndexOfObject(FPinArrays[I]) = -1 then
      clbVisiblePinArrays.AddItem(FPinArrays[I].Name, FPinArrays[I]);
end;

function TfrmVisiblePinSelection.GetPinArrayCount: Integer;
begin
  Result := FPinArrays.Count;
end;

function TfrmVisiblePinSelection.GetPinArray(AIndex: Integer): TPinArray;
begin
  Result := FPinArrays[AIndex];
end;

procedure TfrmVisiblePinSelection.SavePinDevice(APinArray: TPinArray);
var
  I: Integer;
begin
  APinArray.DelAllVisiblePinArrays;
  for I := 0 to clbVisiblePinArrays.Count - 1 do
  begin
    if clbVisiblePinArrays.Checked[I] then
      APinArray.AddVisiblePinArray(TPinArray(clbVisiblePinArrays.Items.Objects[I]));
  end;
end;

constructor TfrmVisiblePinSelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPinArrays := TObjectArray<TPinArray>.Create(True);
end;

destructor TfrmVisiblePinSelection.Destroy;
begin
  FPinArrays.Free;
  inherited Destroy;
end;

procedure TfrmVisiblePinSelection.Execute(APinDevice: TPinArray);
var
  PinArray: TPinArray;
begin
  LoadPinDevice(APinDevice);
  if ShowModal = mrOK then
    SavePinDevice(APinDevice);
end;

procedure TfrmVisiblePinSelection.RegisterArray(APinArray: TPinArray);
begin
  FPinArrays.Add(APinArray);
end;

procedure TfrmVisiblePinSelection.UnregisterArray(APinArray: TPinArray);
begin
  FPinArrays.DelObject(APinArray);
end;

end.

