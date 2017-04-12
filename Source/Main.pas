unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList,
  StdCtrls, Grids, ComCtrls, ProcessorDefine, SynCompletion, SynHighlighterAny,
  Types, LCLType, SynEditMiscClasses, Math, LazUTF8;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actExit: TAction;
    actCompile: TAction;
    actStepOut: TAction;
    actReset: TAction;
    actStepOver: TAction;
    actStepIn: TAction;
    actSaveFile: TAction;
    actShowAll: TAction;
    actHideAll: TAction;
    actTogglePeripheralsVisible: TAction;
    actTogglePortsVisible: TAction;
    actStartStop: TAction;
    actRefreshMemory: TAction;
    actToggleMemoryVisible: TAction;
    actOpenFile: TAction;
    alActionList: TActionList;
    btnCompile: TButton;
    btnOpen: TButton;
    btnRefreshMemory: TButton;
    btnReset: TButton;
    btnSave: TButton;
    btnStartStop: TButton;
    btnStep: TButton;
    btnStepOver: TButton;
    btnStepOut: TButton;
    cbMemorySelection: TComboBox;
    cbAutoRefreshMemory: TCheckBox;
    gbControl: TGroupBox;
    gbMemory: TGroupBox;
    gbPorts: TGroupBox;
    gbPeripherals: TGroupBox;
    gbFile: TGroupBox;
    gbStateInfo: TGroupBox;
    ilMarker: TImageList;
    lbWRegister: TLabel;
    lbFlags: TLabel;
    lbWRegisterTitle: TLabel;
    lbCycles: TLabel;
    lbCyclesTitle: TLabel;
    lbFlagsTitle: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miShowAll: TMenuItem;
    miHideAll: TMenuItem;
    miViewSplit1: TMenuItem;
    miPeripherals: TMenuItem;
    miMemory: TMenuItem;
    miPorts: TMenuItem;
    miView: TMenuItem;
    miFileSplit1: TMenuItem;
    miPlaceholder: TMenuItem;
    miFile: TMenuItem;
    mmMainMenu: TMainMenu;
    pnlControl: TPanel;
    pnlWRegister: TPanel;
    pnlInfo: TPanel;
    pnlMemorySelection: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    pnlCycles: TPanel;
    pnlFlags: TPanel;
    spltPorts: TSplitter;
    spltMemory: TSplitter;
    spltPeripherals: TSplitter;
    StatusBar1: TStatusBar;
    sgSpecialFunction: TStringGrid;
    synEditor: TSynEdit;
    sgMemView: TStringGrid;
    synHighlighter: TSynAnySyn;
    synCompletion: TSynCompletion;
    procedure actExitExecute(Sender: TObject);
    procedure actHideAllExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRefreshMemoryExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actShowAllExecute(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure actStepInExecute(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actToggleMemoryVisibleExecute(Sender: TObject);
    procedure actTogglePeripheralsVisibleExecute(Sender: TObject);
    procedure actTogglePortsVisibleExecute(Sender: TObject);
    procedure cbMemorySelectionChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function synCompletionMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;
    function synCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean;
      Index: integer): boolean;
    procedure synCompletionSearchPosition(var APosition: integer);
    procedure synEditorChange(Sender: TObject);
    procedure synEditorClick(Sender: TObject);
    procedure synEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean;
      Markup: TSynSelectedColor);
  private
    FFlags: TProcessor.TCalcFlags;
    procedure SetFlags(AValue: TProcessor.TCalcFlags);
  private
    FMemViewColumns: Cardinal;
    FCycles: Cardinal;
    FWRegister: Byte;
    FProcessor: TProcessor;
    FFileData: TStringList;

    procedure InitSynEdit;
    procedure InitMemView;

    function GetAutoRefreshMemory: Boolean;
    procedure ParseStringListFromLST(List: TStringList);

    procedure SetMemViewColumns(AValue: Cardinal);
    procedure SetCycles(AValue: Cardinal);
    procedure SetWRegister(AValue: Byte);

    function GetMemoryVisible: Boolean;
    procedure SetAutoRefreshMemory(AValue: Boolean);
    procedure SetMemoryVisible(AValue: Boolean);
    function GetMemViewType: TProcessor.TMemoryType;
    procedure SetMemViewType(AValue: TProcessor.TMemoryType);

    function GetPeripheralsVisible: Boolean;
    function GetPortsVisible: Boolean;
    procedure SetPeripheralsVisible(AValue: Boolean);
    procedure SetPortsVisible(AValue: Boolean);

    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible;
    property PortsVisible: Boolean read GetPortsVisible write SetPortsVisible;
    property PeripheralsVisible: Boolean read GetPeripheralsVisible write SetPeripheralsVisible;
    property AutoRefreshMemory: Boolean read GetAutoRefreshMemory write SetAutoRefreshMemory;

    property Cycles: Cardinal read FCycles write SetCycles;
    property Flags: TProcessor.TCalcFlags read FFlags write SetFlags;
    property WRegister: Byte read FWRegister write SetWRegister;
    property MemViewColumns: Cardinal read FMemViewColumns write SetMemViewColumns;
    property MemViewType: TProcessor.TMemoryType read GetMemViewType write SetMemViewType;

    procedure RefreshMemView;
    procedure RefreshSynEditMarkup;

    procedure UpdateActions;

    procedure IdleHandler(Sender: TObject; var ADone: Boolean);

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
  InitSynEdit;
  InitMemView;
  Application.OnIdle := IdleHandler;
end;

procedure TfrmMain.SetMemViewColumns(AValue: Cardinal);
var
  I: Integer;
begin
  AValue := EnsureRange(AValue, 1, 256);
  if FMemViewColumns = AValue then
    Exit;
  FMemViewColumns := AValue;

  while sgMemView.Columns.Count > 1 do
    sgMemView.Columns.Delete(sgMemView.Columns.Count - 1);
  for I := 0 to MemViewColumns - 1 do
    sgMemView.Columns.Add.Title.Caption := Format('%.2x', [I]);

  RefreshMemView;

  sgMemView.AutoAdjustColumns;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actHideAllExecute(Sender: TObject);
begin
  MemoryVisible := False;
  PeripheralsVisible := False;
  PortsVisible := False;
end;

procedure TfrmMain.actOpenFileExecute(Sender: TObject);

type
  TLoadType = (ltAssembler = 1, ltCompiled, ltBinary);

const
  TypeName: array [TLoadType] of String = (
    'Assembler Program',
    'Compiled Program',
    'Binary Program'
  );

  Extension: array [TLoadType] of String = (
    '*.asm',
    '*.lst',
    '*.hex'
  );

var
  T: TLoadType;
  FileData: RawByteString;
  X: TSystemCodePage;
begin
  with TOpenDialog.Create(Self) do
  begin
     try
      for T := Low(TLoadType) to High(TLoadType) do
        Filter := Filter + TypeName[T] + '|' + Extension[T] + '|';
      FilterIndex := Integer(ltCompiled);
      if Execute then
      begin
        case TLoadType(FilterIndex) of
          ltAssembler:
          begin
            raise ENotImplemented.Create('Can''t load! Loading not implemented!');
          end;
          ltCompiled:
          begin
            FFileData.LoadFromFile(FileName);
            FileData := FFileData.Text;
            SetCodePage(FileData, 1252, False);
            SetCodePage(FileData, DefaultSystemCodePage);
            FFileData.Text := FileData;
            FProcessor.LoadProgram(FFileData);
            ParseStringListFromLST(FFileData);
          end;
          ltBinary:
          begin
            raise ENotImplemented.Create('Can''t load! Loading not implemented!');
          end;
        end;
      end;

    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actRefreshMemoryExecute(Sender: TObject);
begin
  RefreshMemView;
end;

procedure TfrmMain.actSaveFileExecute(Sender: TObject);
type
  TSaveType = (stAssembler = 1, stCompiledCommented, stCompiledUncommented, stBinary);

const
  TypeName: array [TSaveType] of String = (
    'Assembler Program',
    'Compiled Program Commented',
    'Compiled Program Uncommented',
    'Binary Program'
  );

  Extension: array [TSaveType] of String = (
    '*.asm',
    '*.lst',
    '*.lst',
    '*.hex'
  );

var
  T: TSaveType;
begin
  with TSaveDialog.Create(Self) do
  begin
    try
      for T := Low(TSaveType) to High(TSaveType) do
        Filter := Filter + TypeName[T] + '|' + Extension[T] + '|';
      FilterIndex := Integer(stAssembler);
      if Execute then
      begin
        raise ENotImplemented.Create('Can''t save! Saving not implemented!');
        T := TSaveType(FilterIndex);
        if T = stAssembler then
        begin

        end
        else
        begin
          //if not Compiled then
          //  MessageDlg('Please compile, before saving to a compiled file!', mtError, [mbOk], 0);

          case T of
            stCompiledCommented:
            begin

            end;
            stCompiledUncommented:
            begin

            end;
            stBinary:
            begin

            end;
          end;
        end;
      end;

    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actShowAllExecute(Sender: TObject);
begin
  MemoryVisible := True;
  PeripheralsVisible := True;
  PortsVisible := True;
end;

procedure TfrmMain.actStartStopExecute(Sender: TObject);
begin
  if FProcessor.Running then
  begin
    FProcessor.Stop;
    actStartStop.Caption := 'Start';
  end
  else
  begin
    FProcessor.Start;
    actStartStop.Caption := 'Stop';
  end;
  RefreshSynEditMarkup;
end;

procedure TfrmMain.actStepInExecute(Sender: TObject);
begin
  FProcessor.StepIn;
  Cycles := FProcessor.Cycles;
  Flags := FProcessor.CalcFlags;
  WRegister := FProcessor.WRegister;
  RefreshMemView;
  RefreshSynEditMarkup;
end;

procedure TfrmMain.actStepOutExecute(Sender: TObject);
begin
  FProcessor.StepOut;
  Cycles := FProcessor.Cycles;
  Flags := FProcessor.CalcFlags;
  WRegister := FProcessor.WRegister;
  RefreshMemView;
  RefreshSynEditMarkup;
end;

procedure TfrmMain.actStepOverExecute(Sender: TObject);
begin
  FProcessor.StepOver;
  Cycles := FProcessor.Cycles;
  Flags := FProcessor.CalcFlags;
  WRegister := FProcessor.WRegister;
  RefreshMemView;
  RefreshSynEditMarkup;
end;

procedure TfrmMain.actToggleMemoryVisibleExecute(Sender: TObject);
begin
  MemoryVisible := not MemoryVisible;
end;

procedure TfrmMain.actTogglePeripheralsVisibleExecute(Sender: TObject);
begin
  PeripheralsVisible := not PeripheralsVisible;
end;

procedure TfrmMain.actTogglePortsVisibleExecute(Sender: TObject);
begin
  PortsVisible := not PortsVisible;
end;

procedure TfrmMain.cbMemorySelectionChange(Sender: TObject);
begin
  RefreshMemView;
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

function TfrmMain.synCompletionMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean;
  Index: integer): TPoint;
begin
  ACanvas.Font := synCompletion.TheForm.Font;
  Result := Point(ACanvas.Font.GetTextWidth(AKey), synCompletion.FontHeight);
end;

function TfrmMain.synCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean;
  Index: integer): boolean;
begin
  ACanvas.TextOut(X, Y, AKey);
  Result := True;
end;

procedure TfrmMain.synCompletionSearchPosition(var APosition: integer);
const
  {$REGION Highlight Strings}
  P: array [0 .. 38] of String = (
    'org',
    'device',
    'equ',
    'list',
    'addwf',
    'andwf',
    'clrf',
    'clrw',
    'comf',
    'decf',
    'decfsz',
    'incf',
    'incfsz',
    'iorwf',
    'movf',
    'movwf',
    'nop',
    'rlf',
    'rrf',
    'subwf',
    'swapf',
    'xorwf',
    'bcf',
    'bsf',
    'btfsc',
    'btfss',
    'addlw',
    'andlw',
    'call',
    'clrwdt',
    'goto',
    'iorlw',
    'movlw',
    'retfie',
    'retlw',
    'return',
    'sleep',
    'sublw',
    'xorlw'
  );
  {$ENDREGION}
var
  C: String;
  W: String;
begin
  synCompletion.ItemList.Clear;
  W := LowerCase(synCompletion.CurrentString);
  for C in P do
  begin
    if W.IsEmpty or C.StartsWith(W) then
      synCompletion.ItemList.Add(C);
  end;
  APosition := 0;
end;

procedure TfrmMain.synEditorChange(Sender: TObject);
begin
  // TODO: not compiled anymore;
end;

procedure TfrmMain.synEditorClick(Sender: TObject);
var
  P: TPoint;
begin
  P := synEditor.PixelsToRowColumn(synEditor.ScreenToClient(Mouse.CursorPos));
  if P.X = 0 then
  begin
    FProcessor.Breakpoint[P.Y] := not FProcessor.Breakpoint[P.Y];
    RefreshSynEditMarkup;
  end;
end;

procedure TfrmMain.synEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean;
  Markup: TSynSelectedColor);
const
  ColorPC = $7799FF;
  ColorBreakpoint = $2233FF;
  ColorPCBreakpoint = $4466FF;
begin
  if not FProcessor.Running then
  begin
    if Line = FProcessor.CurrentInstruction.Line then
    begin
      Special := True;
      if FProcessor.Breakpoint[Line] then
        Markup.Background := ColorPCBreakpoint
      else
        Markup.Background := ColorPC;
      Exit;
    end;
  end;

  if FProcessor.Breakpoint[Line] then
  begin
    Special := True;
    Markup.Background := ColorBreakpoint;
    Exit;
  end;
end;

procedure TfrmMain.SetFlags(AValue: TProcessor.TCalcFlags);
var
  F: TProcessor.TCalcFlag;
begin
  if FFlags = AValue then
    Exit;
  FFlags := AValue;
  lbFlags.Caption := '';
  for F in Flags do
  begin
    lbFlags.Caption := lbFlags.Caption + TProcessor.CalcFlagName[F] + ', ';
  end;
  if lbFlags.Caption = '' then
    lbFlags.Caption := 'none'
  else
    lbFlags.Caption := Copy(lbFlags.Caption, 0, Length(lbFlags.Caption) - 2);
end;

procedure TfrmMain.SetWRegister(AValue: Byte);
begin
  if FWRegister = AValue then
    Exit;
  FWRegister := AValue;
  lbWRegister.Caption := Format('%.2x [%s]', [WRegister, BinStr(WRegister, 8)]);
end;

function TfrmMain.GetMemViewType: TProcessor.TMemoryType;
begin
  Result := TProcessor.TMemoryType(cbMemorySelection.ItemIndex);
end;

procedure TfrmMain.RefreshSynEditMarkup;
begin
  synEditor.Invalidate;
end;

procedure TfrmMain.UpdateActions;
begin
  {
  actSaveFile.Enabled := ;
  actOpenFile.Enabled := ;
  actStartStop.Enabled := ;
  actStartStop.Caption := FProcessor.Running;
  actStepIn.Enabled := ;
  actStepOver.Enabled := ;
  actCompile.Enabled := ;
  }
end;

procedure TfrmMain.SetMemViewType(AValue: TProcessor.TMemoryType);
begin
  if MemViewType = AValue then
    Exit;
  cbMemorySelection.ItemIndex := Ord(AValue);
  RefreshMemView;
end;

procedure TfrmMain.InitSynEdit;
begin
  synCompletion.TheForm.Font := synEditor.Font;
  synCompletion.TheForm.Font.Bold := True;
end;

procedure TfrmMain.InitMemView;
var
  I: Integer;
  T: TProcessor.TMemoryType;
begin
  FMemViewColumns := 4;
  sgMemView.TitleFont := Font;
  sgMemView.Columns.Add.Title.Caption := 'Adress';
  for I := 0 to MemViewColumns - 1 do
    with sgMemView.Columns.Add do
    begin
      Title.Caption := Format('%.2x', [I]);
    end;

  cbMemorySelection.Clear;
  for T := Low(T) to High(T) do
    cbMemorySelection.Items.Add(TProcessor.MemoryName[T]);
  MemViewType := mtRAM;

  RefreshMemView;
  sgMemView.AutoSizeColumns;
end;

function TfrmMain.GetPeripheralsVisible: Boolean;
begin
  Result := gbPeripherals.Visible;
end;

function TfrmMain.GetPortsVisible: Boolean;
begin
  Result := gbPorts.Visible;
end;

procedure TfrmMain.SetPeripheralsVisible(AValue: Boolean);
begin
  if PeripheralsVisible = AValue then
    Exit;
  DisableAlign;
  gbPeripherals.Visible := AValue;
  actTogglePeripheralsVisible.Checked := AValue;
  spltPeripherals.Visible := AValue;
  EnableAlign;
end;

procedure TfrmMain.SetPortsVisible(AValue: Boolean);
begin
  if PortsVisible = AValue then
    Exit;
  DisableAlign;
  gbPorts.Visible := AValue;
  actTogglePortsVisible.Checked := AValue;
  spltPorts.Visible := AValue;
  EnableAlign;
end;

procedure TfrmMain.SetCycles(AValue: Cardinal);
begin
  if FCycles = AValue then
    Exit;
  FCycles := AValue;
  lbCycles.Caption := Format('%d', [AValue]);
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
  DisableAlign;
  gbMemory.Visible := AValue;
  spltMemory.Visible := AValue;
  actToggleMemoryVisible.Checked := AValue;
  EnableAlign;
end;

procedure TfrmMain.RefreshMemView;
var
  R, C, P: Integer;
begin
  sgMemView.BeginUpdate;
  sgMemView.RowCount := Ceil(TProcessor.MemorySize[MemViewType] / MemViewColumns) + 1;
  for R := 0 to sgMemView.RowCount - 2 do
  begin
    sgMemView.Rows[R + 1][0] := Format('0x%.4x', [R * MemViewColumns]);
    for C := 0 to MemViewColumns - 1 do
    begin
      P := C + R * MemViewColumns;
      if FProcessor.ReadAsZero[MemViewType, P] then
        sgMemView.Rows[R + 1][C + 1] := 'XX'
      else
        sgMemView.Rows[R + 1][C + 1] := Format('%.2x', [FProcessor.Memory[MemViewType, P]]);
    end;
  end;
  sgMemView.EndUpdate;
end;

procedure TfrmMain.IdleHandler(Sender: TObject; var ADone: Boolean);
begin
  if FProcessor.Running then
  begin
    if FProcessor.CatchUp then
    begin
      WRegister := FProcessor.WRegister;
      RefreshMemView;
      RefreshSynEditMarkup;
    end;
    Cycles := FProcessor.Cycles;
    Flags := FProcessor.CalcFlags;
    ADone := False;
  end
  else
    ADone := True;
end;

procedure TfrmMain.ParseStringListFromLST(List: TStringList);
const
  AssemblerOffset: Integer = 7;
var
  Counter: Integer;
  CharCounter: Integer;
  MinWhitespacecount: Integer;
begin
  MinWhitespacecount := Integer.MaxValue;
  synEditor.Lines := List;
  for Counter := 0 to synEditor.lines.Count - 1 do
  begin
    for CharCounter := 10 to synEditor.Lines.Strings[Counter].Length do
    begin
      if synEditor.Lines[Counter][CharCounter] <> ' ' then
      begin
        MinWhitespacecount := Min(MinWhitespacecount, CharCounter);
        Break;
      end;
    end;
  end;
  for Counter := 0 to List.Count - 1 do
  begin
    synEditor.Lines[Counter] := synEditor.Lines[Counter].Remove(0, MinWhitespacecount - 1 + AssemblerOffset );
  end;

end;

end.


