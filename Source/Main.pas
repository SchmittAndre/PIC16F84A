unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, Grids,
  ComCtrls, ValEdit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actExit: TAction;
    actRefreshMemory: TAction;
    actToggleMemoryVisible: TAction;
    actOpenFile: TAction;
    alActionList: TActionList;
    btnRefreshMemory: TButton;
    cbMemorySelection: TComboBox;
    cbAutoRefreshMemory: TCheckBox;
    gbMemory: TGroupBox;
    gbPorts: TGroupBox;
    gbPeripherals: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miView: TMenuItem;
    miFileSplit1: TMenuItem;
    miPlaceholder: TMenuItem;
    miFile: TMenuItem;
    mmMainMenu: TMainMenu;
    pnlMemorySelection: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    edtEditor: TSynEdit;
    sgMemView: TStringGrid;
    procedure actExitExecute(Sender: TObject);
    procedure actToggleMemoryVisibleExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure vlMemoryValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
  private
    function GetAutoRefreshMemory: Boolean;
    procedure ParseStringlist(List: TStringList);

    function GetMemoryVisible: Boolean;
    procedure SetAutoRefreshMemory(AValue: Boolean);
    procedure SetMemoryVisible(AValue: Boolean);

  published
    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible default True;
    property AutoRefreshMemory: Boolean read GetAutoRefreshMemory write SetAutoRefreshMemory default False;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
 { vlMemory.BeginUpdate;
  for I := 0 to $3FF do
  begin
    vlMemory.InsertRow(Format('0x%.4x', [I * 8]), 'DE AD BE EF 00 11 22 33', True);
  end;
  vlMemory.EndUpdate;  }
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actToggleMemoryVisibleExecute(Sender: TObject);
begin
  MemoryVisible := not MemoryVisible;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // TODO: Do you want to save? dialog
  CanClose := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  // TODO: Load dropped file
end;

procedure TfrmMain.vlMemoryValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
begin
  NewValue := OldValue;
end;

function TfrmMain.GetAutoRefreshMemory: Boolean;
begin
  Result := cbAutoRefreshMemory.Checked;
end;

function TfrmMain.GetMemoryVisible: Boolean;
begin
  Result := gbMemory.Visible;
end;

procedure TfrmMain.SetAutoRefreshMemory(AValue: Boolean);
begin
  if AutoRefreshMemory = AValue then
    Exit;
  cbAutoRefreshMemory.Checked := AValue;
end;

procedure TfrmMain.SetMemoryVisible(AValue: Boolean);
begin
  if MemoryVisible = AValue then
    Exit;
  gbMemory.Visible := AValue;
  actToggleMemoryVisible.Checked := MemoryVisible;
  if MemoryVisible then
    frmMain.Width := frmMain.Width + gbMemory.BorderSpacing.ControlWidth
  else
    frmMain.Width := frmMain.Width - gbMemory.BorderSpacing.ControlWidth;
end;

procedure TfrmMain.ParseStringlist(List: TStringList);
var
  Counter: Integer;
begin
  for Counter := List.Count - 1 downto 0 do
  begin
    if List[Counter].StartsWith(' ') then
    begin
      List.Delete(Counter);
    end
    else
    begin
      List[Counter] := Copy(List[Counter], 1 , 9);
    end;
  end;
end;

end.


