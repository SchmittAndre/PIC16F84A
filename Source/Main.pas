unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, Grids,
  ComCtrls, ValEdit, ProcessorDefine;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actExit: TAction;
    actFlashMem: TAction;
    actCompile: TAction;
    actStartStop: TAction;
    actRefreshMemory: TAction;
    actToggleMemoryVisible: TAction;
    actOpenFile: TAction;
    alActionList: TActionList;
    btnCompile: TButton;
    btnOpen: TButton;
    btnStartStop: TButton;
    btnRefreshMemory: TButton;
    btnFlash: TButton;
    cbMemorySelection: TComboBox;
    cbAutoRefreshMemory: TCheckBox;
    gbMemory: TGroupBox;
    gbPorts: TGroupBox;
    gbPeripherals: TGroupBox;
    lbRuntime: TLabel;
    lbCycles: TLabel;
    lbRuntimeTitle: TLabel;
    lbCyclesTitle: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miView: TMenuItem;
    miFileSplit1: TMenuItem;
    miPlaceholder: TMenuItem;
    miFile: TMenuItem;
    mmMainMenu: TMainMenu;
    Panel1: TPanel;
    pnlInfo: TPanel;
    pnlRuntime: TPanel;
    pnlMemorySelection: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    pnlCycles: TPanel;
    spltLeft: TSplitter;
    spltRight: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    edtEditor: TSynEdit;
    sgMemView: TStringGrid;
    procedure actExitExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRefreshMemoryExecute(Sender: TObject);
    procedure actToggleMemoryVisibleExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    FStartTime: Int64;
    FCycles: Cardinal;
    FProcessor: TProcessor;
    FFileData: TStringList;

    function GetAutoRefreshMemory: Boolean;
    procedure ParseStringlist(List: TStringList);

    function GetRuntime: Int64;
    procedure SetCycles(AValue: Cardinal);
    procedure SetStartTime(AValue: Int64);


    function GetMemoryVisible: Boolean;
    procedure SetAutoRefreshMemory(AValue: Boolean);
    procedure SetMemoryVisible(AValue: Boolean);

    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible default True;
    property AutoRefreshMemory: Boolean read GetAutoRefreshMemory write SetAutoRefreshMemory default False;

    procedure UpdateRuntime;
    property StartTime: Int64 read FStartTime write SetStartTime;
    property Runtime: Int64 read GetRuntime;
    property Cycles: Cardinal read FCycles write SetCycles;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FProcessor := TProcessor.Create;
  FFileData := TStringList.Create;

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

procedure TfrmMain.actOpenFileExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := 'Compiled Program|*.lst|Binary Program|*.hex';
    if Execute then
    begin
      FFileData.LoadFromFile(FileName);
    end;
    Free;
  end;
end;

procedure TfrmMain.actRefreshMemoryExecute(Sender: TObject);
begin

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
  FProcessor.Free;
  FFileData.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  // TODO: Load dropped file
end;

function TfrmMain.GetRuntime: Int64;
begin
  Result := GetTickCount64 - StartTime;
end;

procedure TfrmMain.SetCycles(AValue: Cardinal);
begin
  if FCycles = AValue then
    Exit;
  FCycles := AValue;
  lbCycles.Caption := Format('%d', [AValue]);
end;

procedure TfrmMain.SetStartTime(AValue: Int64);
begin
  if FStartTime = AValue then Exit;
  FStartTime := AValue;
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
var
  W: Integer;
begin
  if MemoryVisible = AValue then
    Exit;
  gbMemory.Visible := AValue;
  W := gbMemory.BorderSpacing.ControlWidth + spltRight.BorderSpacing.ControlWidth;
  if MemoryVisible then
    frmMain.Width := frmMain.Width + W
  else
    frmMain.Width := frmMain.Width - W;
  spltRight.Visible := MemoryVisible;
  actToggleMemoryVisible.Checked := MemoryVisible;
end;

procedure TfrmMain.UpdateRuntime;
begin
  lbRuntime.Caption := Format('%d ms', [Runtime]);
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


