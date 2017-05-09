unit VisiblePinSelectionDefine;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls, ExtCtrls, PinDefine, Lists;

type

  { TPinArray }

  TPinArray = class
  private
    FPins: TObjectArray<TPin>;
    FVisiblePinArrays: TObjectArray<TPinArray>;
    FName: String;

    FOnChange: TPin.TOnChange;

    function GetCount: Integer;
    function GetPin(AIndex: Integer): TPin;
    function GetVisiblePinArray(AIndex: Integer): TPinArray;
    procedure SetCount(AValue: Integer);

  public
    constructor Create(AName: String; AOnChange: TPin.TOnChange = nil; ACount: Integer = 1);
    destructor Destroy; override;

    property Name: String read FName;

    property Pins[AIndex: Integer]: TPin read GetPin;
    property Count: Integer read GetCount write SetCount;

    property VisiblePinArrays[AIndex: Integer]: TPinArray read GetVisiblePinArray;
    property VisiblePinArrayCount: Integer read GetCount;

    procedure AddVisiblePinArray(APinArray: TPinArray);
    procedure DelVisiblePinArray(APinArray: TPinArray);
    procedure DelAllVisiblePinArrays;
  end;

  { TfrmVisiblePinSelection }

  TfrmVisiblePinSelection = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnUp: TButton;
    btnDown: TButton;
    clbVisiblePinArrays: TCheckListBox;
    lbHeader: TLabel;
    Panel1: TPanel;
  private
    FPinArrays: TObjectArray<TPinArray>;

    procedure LoadPinDevice(APinArray: TPinArray);
    procedure SavePinDevice(APinArray: TPinArray);
  public
    procedure Execute(APinDevice: TPinArray);

    procedure RegisterArray(APinArray: TPinArray);
    procedure UnregisterArray(APinArray: TPinArray);
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

procedure TPinArray.SetCount(AValue: Integer);
begin
  while Count > AValue do
    FPins.DelLast;
  while Count < AValue do
    FPins.Add(TPin.Create(FPins.Count, FOnChange));
end;

constructor TPinArray.Create(AName: String; AOnChange: TPin.TOnChange; ACount: Integer);
begin
  FName := AName;
  FOnChange := AOnChange;
  FPins := TObjectArray<TPin>.Create;
  FVisiblePinArrays := TObjectArray<TPinArray>.Create(True);
  Count := ACount;
  frmVisiblePinSelection.RegisterArray(Self);
end;

destructor TPinArray.Destroy;
begin
  frmVisiblePinSelection.UnregisterArray(Self);
  FPins.Free;
  FVisiblePinArrays.Free;
  inherited Destroy;
end;

procedure TPinArray.AddVisiblePinArray(APinArray: TPinArray);
begin
  FVisiblePinArrays.Add(APinArray);
end;

procedure TPinArray.DelVisiblePinArray(APinArray: TPinArray);
begin
  FVisiblePinArrays.DelObject(APinArray);
end;

procedure TPinArray.DelAllVisiblePinArrays;
begin
  FVisiblePinArrays.DelAll;
end;

{ TfrmVisiblePinSelection }

procedure TfrmVisiblePinSelection.LoadPinDevice(APinArray: TPinArray);
var
  PinArray: TPinArray;
begin
  lbHeader.Caption := 'I want to connect the ' + APinArray.Name + ' to:';
  clbVisiblePinArrays.Clear;
  for PinArray in FPinArrays do
    clbVisiblePinArrays.AddItem(PinArray.Name, PinArray);
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

procedure TfrmVisiblePinSelection.Execute(APinDevice: TPinArray);
begin
  LoadPinDevice(APinDevice);
  if ShowModal = mrOK then
    SavePinDevice(APinDevice);
end;

procedure TfrmVisiblePinSelection.RegisterArray(APinArray: TPinArray);
begin

end;

procedure TfrmVisiblePinSelection.UnregisterArray(APinArray: TPinArray);
begin

end;

end.

