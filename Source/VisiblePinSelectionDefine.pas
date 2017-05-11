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

    procedure Execute(APinArray: TPinArray);

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
  lbHeader.Caption := 'I want to connect "' + APinArray.Name + '" to:';
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

  type
    TConnection = record
      First, Second: TPin;
    end;

var
  I: Integer;
  OldConnections: TArrayList<TConnection>;
  Connection: TConnection;
  Pin: TPin;
  PinArray: TPinArray;
begin
  // save old connections
  OldConnections := TArrayList<TConnection>.Create;
  for I := 0 to APinArray.Count - 1 do
  begin
    Connection.First := APinArray[I];
    for Pin in APinArray[I] do
    begin
      Connection.Second := Pin;
      OldConnections.Add(Connection);
    end;
  end;

  // regenerate
  APinArray.DelAllVisiblePinArrays;
  for I := 0 to clbVisiblePinArrays.Count - 1 do
  begin
    if clbVisiblePinArrays.Checked[I] then
    begin
      PinArray := TPinArray(clbVisiblePinArrays.Items.Objects[I]);
      APinArray.AddVisiblePinArray(PinArray);
      // load back connections to this PinArray
      for Connection in OldConnections do
      begin
        if Connection.Second.PinArray = PinArray then
          Connection.First.Connect(Connection.Second);
      end;
    end;
  end;
  OldConnections.Free;
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

procedure TfrmVisiblePinSelection.Execute(APinArray: TPinArray);
begin
  LoadPinDevice(APinArray);
  if ShowModal = mrOK then
    SavePinDevice(APinArray);
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

