unit VisiblePinSelectionDefine;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls, ExtCtrls, ActnList, Lists,
  PinDefine;

type

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
  TPinArray.OnCreate := RegisterArray;
  TPinArray.OnDestroy := UnregisterArray;
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

