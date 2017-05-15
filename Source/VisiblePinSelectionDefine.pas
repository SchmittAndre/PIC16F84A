unit VisiblePinSelectionDefine;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls, ExtCtrls, ActnList, Lists,
  PinDefine, Windows;

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
    clbPinArrays: TCheckListBox;
    lbHeader: TLabel;
    pnlControl: TPanel;
    procedure actMoveEntryDownExecute(Sender: TObject);
    procedure actMoveEntryDownUpdate(Sender: TObject);
    procedure actMoveEntryUpExecute(Sender: TObject);
    procedure actMoveEntryUpUpdate(Sender: TObject);
    procedure clbPinArraysKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FPinArrays: TObjectArray<TPinArray>;

    function GetPinArray(AIndex: Integer): TPinArray;
    function GetPinArrayCount: Integer;
    procedure LoadPinArray(APinArray: TPinArray);
    procedure SavePinArray(APinArray: TPinArray);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(APinArray: TPinArray): Boolean;

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

procedure TfrmVisiblePinSelection.LoadPinArray(APinArray: TPinArray);
var
  I: Integer;
begin
  lbHeader.Caption := 'I want to connect "' + APinArray.Name + '" to:';
  clbPinArrays.Clear;
  for I := 0 to APinArray.VisiblePinArrayCount - 1 do
    clbPinArrays.AddItem(APinArray.VisiblePinArrays[I].Name, APinArray.VisiblePinArrays[I]);
  clbPinArrays.CheckAll(cbChecked);
  for I := 0 to FPinArrays.Count - 1 do
    if clbPinArrays.Items.IndexOfObject(FPinArrays[I]) = -1 then
      clbPinArrays.AddItem(FPinArrays[I].Name, FPinArrays[I]);
end;

function TfrmVisiblePinSelection.GetPinArrayCount: Integer;
begin
  Result := FPinArrays.Count;
end;

procedure TfrmVisiblePinSelection.actMoveEntryUpExecute(Sender: TObject);
var
  Tmp: Boolean;
begin
  clbPinArrays.Items.Exchange(clbPinArrays.ItemIndex, clbPinArrays.ItemIndex - 1);
  Tmp := clbPinArrays.Checked[clbPinArrays.ItemIndex];
  clbPinArrays.Checked[clbPinArrays.ItemIndex] := clbPinArrays.Checked[clbPinArrays.ItemIndex - 1];
  clbPinArrays.Checked[clbPinArrays.ItemIndex - 1] := Tmp;
  clbPinArrays.ItemIndex := clbPinArrays.ItemIndex - 1;
end;

procedure TfrmVisiblePinSelection.actMoveEntryDownExecute(Sender: TObject);
var
  Tmp: Boolean;
begin
  clbPinArrays.Items.Exchange(clbPinArrays.ItemIndex, clbPinArrays.ItemIndex + 1);
  Tmp := clbPinArrays.Checked[clbPinArrays.ItemIndex];
  clbPinArrays.Checked[clbPinArrays.ItemIndex] := clbPinArrays.Checked[clbPinArrays.ItemIndex + 1];
  clbPinArrays.Checked[clbPinArrays.ItemIndex + 1] := Tmp;
  clbPinArrays.ItemIndex := clbPinArrays.ItemIndex + 1;
end;

procedure TfrmVisiblePinSelection.actMoveEntryDownUpdate(Sender: TObject);
begin
  actMoveEntryDown.Enabled := (clbPinArrays.ItemIndex <> -1) and
                              (clbPinArrays.ItemIndex < clbPinArrays.Count - 1);
end;

procedure TfrmVisiblePinSelection.actMoveEntryUpUpdate(Sender: TObject);
begin
  actMoveEntryUp.Enabled := clbPinArrays.ItemIndex > 0;
end;

procedure TfrmVisiblePinSelection.clbPinArraysKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
      VK_UP:
      begin
        actMoveEntryUp.Execute;
        Key := 0;
      end;
      VK_DOWN:
      begin
        actMoveEntryDown.Execute;
        Key := 0;
      end;
    end;
  end;
end;

function TfrmVisiblePinSelection.GetPinArray(AIndex: Integer): TPinArray;
begin
  Result := FPinArrays[AIndex];
end;

procedure TfrmVisiblePinSelection.SavePinArray(APinArray: TPinArray);

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
  for I := 0 to clbPinArrays.Count - 1 do
  begin
    if clbPinArrays.Checked[I] then
    begin
      PinArray := TPinArray(clbPinArrays.Items.Objects[I]);
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
  TPinArray.OnCreate.Add(RegisterArray);
  TPinArray.OnDestroy.Add(UnregisterArray);
end;

destructor TfrmVisiblePinSelection.Destroy;
begin
  FPinArrays.Free;
  inherited Destroy;
end;

function TfrmVisiblePinSelection.Execute(APinArray: TPinArray): Boolean;
begin
  LoadPinArray(APinArray);
  Result := ShowModal = mrOK;
  if Result then
    SavePinArray(APinArray);
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

