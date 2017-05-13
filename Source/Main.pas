unit Main;

interface

uses

  // Standard Units
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList, StdCtrls, Grids,
  ComCtrls, SynCompletion, SynHighlighterAny, Types, LCLType, SynEditMiscClasses, Math, LazUTF8,
  // Our Units
  ProcessorDefine,
  VisiblePinSelectionDefine,
  PeripheralFormDefine,
  PinDefine,
  // Peripherals
  ProcessorFormDefine,
  PeripheralLEDArray;

type

  TLineFollowMode = (
    lfOff,
    lfBorder,
    lfCenter
  );

  TLoadType = (ltAssembler = 1, ltCompiled, ltBinary);
  TSaveType = (stAssembler = 1, stCompiledCommented, stCompiledUncommented, stBinary);

  { TfrmMain }

  TfrmMain = class (TForm, IHasProcessor)
    actExit: TAction;
    actCompile: TAction;
    actHelp: TAction;
    actCloseAllPeripherals: TAction;
    actProcessorMCLR: TAction;
    actProcessorPortB: TAction;
    actProcessorPortA: TAction;
    actNew: TAction;
    actSaveFileAs: TAction;
    actStepOut: TAction;
    actReset: TAction;
    actStepOver: TAction;
    actStepIn: TAction;
    actSaveFile: TAction;
    actShowAll: TAction;
    actHideAll: TAction;
    actTogglePortsVisible: TAction;
    actStartStop: TAction;
    actToggleMemoryVisible: TAction;
    actOpenFile: TAction;
    alActionList: TActionList;
    btnCompile: TButton;
    btnOpen: TButton;
    btnReset: TButton;
    btnSave: TButton;
    btnSaveAs: TButton;
    btnStartStop: TButton;
    btnStep: TButton;
    btnStepOver: TButton;
    btnStepOut: TButton;
    cbMemorySelection: TComboBox;
    gbControl: TGroupBox;
    gbMemory: TGroupBox;
    gbSpecialFunction: TGroupBox;
    gbFile: TGroupBox;
    gbStateInfo: TGroupBox;
    gbPeripherals: TGroupBox;
    ilMarker: TImageList;
    lbCycles: TLabel;
    lbCyclesTitle: TLabel;
    lbPreScaler: TLabel;
    lbPreScalerTitle: TLabel;
    lbWRegister: TLabel;
    lbFlags: TLabel;
    lbWRegisterTitle: TLabel;
    lbFlagsTitle: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miShowMCLR: TMenuItem;
    miShowPortA: TMenuItem;
    miShowPortB: TMenuItem;
    miPeripheralSplitter1: TMenuItem;
    miCloseAllPeripherals: TMenuItem;
    miPeripheralSplitter2: TMenuItem;
    miPeripherals: TMenuItem;
    miHelp: TMenuItem;
    miHelpHeader: TMenuItem;
    miNew: TMenuItem;
    miFileSplitter1: TMenuItem;
    miCompile: TMenuItem;
    miControlSplitter1: TMenuItem;
    MenuItem5: TMenuItem;
    miStepIn: TMenuItem;
    miStepOver: TMenuItem;
    miStepOut: TMenuItem;
    miReset: TMenuItem;
    miControl: TMenuItem;
    miShowAll: TMenuItem;
    miHideAll: TMenuItem;
    miViewSplit1: TMenuItem;
    miMemory: TMenuItem;
    miPorts: TMenuItem;
    miView: TMenuItem;
    miFileSplitter2: TMenuItem;
    miPlaceholder: TMenuItem;
    miFile: TMenuItem;
    mmMainMenu: TMainMenu;
    pnlLeft: TPanel;
    pnlControl: TPanel;
    pnlCycles: TPanel;
    pnlPreScaler: TPanel;
    pnlWRegister: TPanel;
    pnlInfo: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    pnlFlags: TPanel;
    sbStatus: TStatusBar;
    sgSpecialFunction: TStringGrid;
    synEditor: TSynEdit;
    sgMemView: TStringGrid;
    synHighlighter: TSynAnySyn;
    synCompletion: TSynCompletion;
    procedure actCloseAllPeripheralsExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actHideAllExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actOpenFileUpdate(Sender: TObject);
    procedure actProcessorMCLRExecute(Sender: TObject);
    procedure actProcessorPortAExecute(Sender: TObject);
    procedure actProcessorPortBExecute(Sender: TObject);
    procedure actResetExecute(Sender: TObject);
    procedure actResetUpdate(Sender: TObject);
    procedure actSaveFileAsExecute(Sender: TObject);
    procedure actSaveFileAsUpdate(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure actShowAllExecute(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure actStartStopUpdate(Sender: TObject);
    procedure actStepInExecute(Sender: TObject);
    procedure actStepInUpdate(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actStepOutUpdate(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStepOverUpdate(Sender: TObject);
    procedure actToggleMemoryVisibleExecute(Sender: TObject);
    procedure actTogglePortsVisibleExecute(Sender: TObject);
    procedure cbMemorySelectionChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure sgMemViewGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure sgMemViewPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    function synCompletionMeasureItem(const AKey: string; ACanvas: TCanvas; {%H-}Selected: boolean;
      {%H-}Index: integer): TPoint;
    function synCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; {%H-}Selected: boolean;
      {%H-}Index: integer): boolean;
    procedure synCompletionSearchPosition(var APosition: integer);
    procedure synEditorChange(Sender: TObject);
    procedure synEditorClick(Sender: TObject);
    procedure synEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean;
      Markup: TSynSelectedColor);
  private
    FMemViewColumns: Cardinal;
    FPreScaler: Byte;
    FCycles: Cardinal;
    FWRegister: Byte;
    FProcessor: TProcessor;
    FFileData: TStringList;
    FCompiled: Boolean;
    FFlags: TProcessor.TCalcFlags;
    FLineFollowMode: TLineFollowMode;
    FLineFollowRange: Cardinal;

    FProcessorPortAForm: TProcessorPortAForm;
    FProcessorPortBForm: TProcessorPortBForm;
    FProcessorMasterClearForm: TProcessorMasterClearForm;

    procedure InitSynEdit;
    procedure InitSpecialFunction;
    procedure InitMemView;

    procedure ParseStringListFromLST(List: TStringList);

    procedure SetMemViewColumns(AValue: Cardinal);
    procedure SetCycles(AValue: Cardinal);
    procedure SetWRegister(AValue: Byte);
    procedure SetPreScaler(AValue: Byte);

    procedure SetLineFollowMode(AValue: TLineFollowMode);
    procedure SetLineFollowRange(AValue: Cardinal);

    function GetMemoryVisible: Boolean;
    procedure SetMemoryVisible(AValue: Boolean);
    function GetMemViewType: TProcessor.TMemoryType;
    procedure SetMemViewType(AValue: TProcessor.TMemoryType);

    function GetMemViewWidth: Integer;
    function GetSpecialFunctionWidth: Integer;
    function GetSpecialFunctionHeight: Integer;
    procedure SetMemViewWidth(AValue: Integer);
    procedure SetSpecialFunctionWidth(AValue: Integer);
    procedure SetSpecialFunctionHeight(AValue: Integer);

    function GetMemViewCellIndex(ACol, ARow: Integer): Integer;

    procedure SetCompiled(AValue: Boolean);
    procedure SetFlags(AValue: TProcessor.TCalcFlags);

    function GetPortsVisible: Boolean;
    procedure SetPortsVisible(AValue: Boolean);

    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible;
    property PortsVisible: Boolean read GetPortsVisible write SetPortsVisible;

    property Compiled: Boolean read FCompiled write SetCompiled;
    property Cycles: Cardinal read FCycles write SetCycles;
    property Flags: TProcessor.TCalcFlags read FFlags write SetFlags;
    property PreScaler: Byte read FPreScaler write SetPreScaler;
    property WRegister: Byte read FWRegister write SetWRegister;
    property MemViewColumns: Cardinal read FMemViewColumns write SetMemViewColumns;
    property MemViewType: TProcessor.TMemoryType read GetMemViewType write SetMemViewType;
    property MemViewCellIndex[ACol, ARow: Integer]: Integer read GetMemViewCellIndex;
    property LineFollowMode: TLineFollowMode read FLineFollowMode write SetLineFollowMode;
    property LineFollowRange: Cardinal read FLineFollowRange write SetLineFollowRange;

    property SpecialFunctionWidth: Integer read GetSpecialFunctionWidth write SetSpecialFunctionWidth;
    property SpecialFunctionHeight: Integer read GetSpecialFunctionHeight write SetSpecialFunctionHeight;
    property MemViewWidth: Integer read GetMemViewWidth write SetMemViewWidth;

    procedure SpecialFunctionAutoWidth;
    procedure SpecialFunctionAutoHeight;
    procedure MemViewAutoWidth;

    procedure UpdateMemView;
    procedure UpdateSpecialFunction;
    procedure UpdateSynEditMarkup;
    procedure UpdateCycles;
    procedure UpdateALUInfo;
    procedure UpdateSynEditScroll;

    procedure IdleHandler(Sender: TObject; var ADone: Boolean);

    procedure LoadFile(AFileName: String);
    function ExtractFileType(AFileName: String): TLoadType;

    procedure ProcessParams;

    procedure OnPeripheralAdd(Sender: TObject);
    procedure GeneratePeripheralLists;

    procedure OnAsyncProcessorChange(AProcessor: TProcessor);

  protected
    procedure UpdateActions; override;

  public

    // IHasProcessor
    function GetProcessor: TProcessor;

  end;

  { EUnsupportedException }

  EUnsupportedException = class (Exception)
    constructor Create(AExtension: String);
  end;

const
  LoadTypeNames: array [TLoadType] of String = (
    'Assembler Program',
    'Compiled Program',
    'Binary Program'
  );

  LoadExtensions: array [TLoadType] of String = (
    '.asm',
    '.lst',
    '.hex'
  );

  SaveTypeNames: array [TSaveType] of String = (
    'Assembler Program',
    'Compiled Program Commented',
    'Compiled Program Uncommented',
    'Binary Program'
  );

  SaveExtensions: array [TSaveType] of String = (
    '.asm',
    '.lst',
    '.lst',
    '.hex'
  );

  PeripheralClasses: array [0 .. 0] of TPeripheralFormClass = (
    TPeripheralLEDArray
  );

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ EUnsupportedException }

constructor EUnsupportedException.Create(AExtension: String);
begin
  inherited Create('Extension "' + AExtension + '" is not supported!');
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmVisiblePinSelection := TfrmVisiblePinSelection.Create(Self);

  FProcessor := TProcessor.Create;
  FProcessor.OnAsyncMemoryChange.Add(OnAsyncProcessorChange);

  FFileData := TStringList.Create;
  LineFollowRange := 3;
  LineFollowMode := lfBorder;
  InitSynEdit;
  InitMemView;
  InitSpecialFunction;
  Application.OnIdle := IdleHandler;

  FProcessorPortAForm := TProcessorPortAForm.Create(Self);
  FProcessorPortBForm := TProcessorPortBForm.Create(Self);
  FProcessorMasterClearForm := TProcessorMasterClearForm.Create(Self);

  GeneratePeripheralLists;
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

  UpdateMemView;

  sgMemView.AutoAdjustColumns;
  MemViewAutoWidth;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actHelpExecute(Sender: TObject);
begin
   raise ENotImplemented.Create('Help is not Implemented');
end;

procedure TfrmMain.actCompileExecute(Sender: TObject);
begin
  // TODO: Compile
  raise ENotImplemented.Create('Compiling not implemented');
end;

procedure TfrmMain.actCloseAllPeripheralsExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := ComponentCount - 1 downto 0 do
    if Components[I] is TPeripheralForm then
      Components[I].Free;
end;

procedure TfrmMain.actCompileUpdate(Sender: TObject);
begin
  actCompile.Enabled := not Compiled;
  if Compiled then
    actCompile.Caption := 'Compiled'
  else
    actCompile.Caption := 'Compile';
end;

procedure TfrmMain.actHideAllExecute(Sender: TObject);
begin
  MemoryVisible := False;
  PortsVisible := False;
end;

procedure TfrmMain.actOpenFileExecute(Sender: TObject);
var
  T: TLoadType;
begin
  with TOpenDialog.Create(Self) do
  begin
     try
      for T := Low(TLoadType) to High(TLoadType) do
        Filter := Filter + LoadTypeNames[T] + '|*' + LoadExtensions[T] + '|';
      FilterIndex := Integer(ltCompiled);
      if Execute then
        LoadFile(FileName);
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actOpenFileUpdate(Sender: TObject);
begin
  actOpenFile.Enabled := not FProcessor.Running;
end;

procedure TfrmMain.actProcessorMCLRExecute(Sender: TObject);
begin
  with FProcessorMasterClearForm do
    Visible := not Visible;
end;

procedure TfrmMain.actProcessorPortAExecute(Sender: TObject);
begin
  with FProcessorPortAForm do
    Visible := not Visible;
end;

procedure TfrmMain.actProcessorPortBExecute(Sender: TObject);
begin
  with FProcessorPortBForm do
    Visible := not Visible;
end;

procedure TfrmMain.actResetExecute(Sender: TObject);
begin
  FProcessor.ResetPowerON;
  UpdateMemView;
  UpdateSpecialFunction;
  UpdateSynEditMarkup;
  UpdateALUInfo;
  UpdateCycles;
  UpdateSynEditScroll;
end;

procedure TfrmMain.actResetUpdate(Sender: TObject);
begin
  actReset.Enabled := Compiled and not FProcessor.Running;
end;

procedure TfrmMain.actSaveFileAsExecute(Sender: TObject);
begin

end;

procedure TfrmMain.actSaveFileAsUpdate(Sender: TObject);
begin
  actSaveFileAs.Enabled := not FProcessor.Running;
end;

procedure TfrmMain.actSaveFileExecute(Sender: TObject);
var
  T: TSaveType;
begin
  with TSaveDialog.Create(Self) do
  begin
    try
      for T := Low(TSaveType) to High(TSaveType) do
        Filter := Filter + SaveTypeNames[T] + '|*' + SaveExtensions[T] + '|';
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

procedure TfrmMain.actSaveFileUpdate(Sender: TObject);
begin
  actSaveFile.Enabled := not FProcessor.Running;
end;

procedure TfrmMain.actShowAllExecute(Sender: TObject);
begin
  MemoryVisible := True;
  PortsVisible := True;
end;

procedure TfrmMain.actStartStopExecute(Sender: TObject);
begin
  if FProcessor.Running then
  begin
    FProcessor.Stop;
    UpdateALUInfo;
    UpdateSynEditMarkup;
    UpdateSynEditScroll;
  end
  else
    FProcessor.Start;
  UpdateSynEditMarkup;
  UpdateMemView;
  UpdateSpecialFunction;
  UpdateActions;
end;

procedure TfrmMain.actStartStopUpdate(Sender: TObject);
begin
  actStartStop.Enabled := Compiled;
  if FProcessor.Running then
    actStartStop.Caption := 'Stop'
  else
    actStartStop.Caption := 'Start';
end;

procedure TfrmMain.actStepInExecute(Sender: TObject);
begin
  FProcessor.StepIn;
  UpdateCycles;
  UpdateALUInfo;
  UpdateMemView;
  UpdateSpecialFunction;
  UpdateSynEditMarkup;
  UpdateSynEditScroll;
end;

procedure TfrmMain.actStepInUpdate(Sender: TObject);
begin
  actStepIn.Enabled := Compiled and not FProcessor.Running;
end;

procedure TfrmMain.actStepOutExecute(Sender: TObject);
begin
  if FProcessor.StepOut = siSingle then
  begin
    UpdateCycles;
    UpdateALUInfo;
    UpdateMemView;
    UpdateSpecialFunction;
    UpdateSynEditMarkup;
    UpdateSynEditScroll;
  end
  else
    UpdateActions;
end;

procedure TfrmMain.actStepOutUpdate(Sender: TObject);
begin
  actStepOut.Enabled := Compiled and not FProcessor.Running and (FProcessor.PCStackPos <> 0);
end;

procedure TfrmMain.actStepOverExecute(Sender: TObject);
begin
  if FProcessor.StepOver = siSingle then
  begin
    UpdateCycles;
    UpdateALUInfo;
    UpdateMemView;
    UpdateSpecialFunction;
    UpdateSynEditMarkup;
    UpdateSynEditScroll;
  end
  else
    UpdateActions;
end;

procedure TfrmMain.actStepOverUpdate(Sender: TObject);
begin
  actStepOver.Enabled := Compiled and not FProcessor.Running;
end;

procedure TfrmMain.actToggleMemoryVisibleExecute(Sender: TObject);
begin
  MemoryVisible := not MemoryVisible;
end;

procedure TfrmMain.actTogglePortsVisibleExecute(Sender: TObject);
begin
  PortsVisible := not PortsVisible;
end;

procedure TfrmMain.cbMemorySelectionChange(Sender: TObject);
var
  Old: TMouseWheelOption;
begin
  UpdateMemView;
  // scroll back up to the first row
  Old := sgMemView.MouseWheelOption;
  sgMemView.MouseWheelOption := mwCursor;
  sgMemView.Col := 1;
  sgMemView.Row := 1;
  sgMemView.Selection := Rect(1, 1, 1, 1);
  sgMemView.MouseWheelOption := Old;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // TODO: Do you want to save? dialog
  CanClose := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // must free those forms first, as the desturction tries to do stuff with the processor
  FProcessorPortAForm.Free;
  FProcessorPortBForm.Free;
  FProcessorMasterClearForm.Free;
  FProcessor.Free;
  FFileData.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if Length(FileNames) <> 1 then
    MessageDlg('Drop only one file please.', mtInformation, [mbOK], 0)
  else
    LoadFile(FileNames[0]);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SpecialFunctionAutoWidth;
  SpecialFunctionAutoHeight;
  MemViewAutoWidth;
  sgSpecialFunction.Invalidate;
  ProcessParams;
end;

{
procedure TfrmMain.pnlPeripheralsPaint(Sender: TObject);

type
  TRing = (rnBlue, rnYellow, rnBlack, rnGreen, rnRed);

const
  RingColor: array [TRing] of TColor = ($C88500, $00C3F3, $000000, $3D9E00, $2400E0);
  RingPos: array [TRing] of TPoint = (
    (X: 10; Y: 10),
    (X: 40; Y: 40),
    (X: 70; Y: 10),
    (X: 100; Y: 40),
    (X: 130; Y: 10)
  );
var
  R: TRing;
begin
  with pnlPeripherals.Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := 5;
    Pen.Style := psSolid;
    for R := Low(R) to High(R) do
    begin
      Pen.Color := RingColor[R];
      Ellipse(RingPos[R].X, RingPos[R].Y, RingPos[R].X + 50, RingPos[R].Y + 50);
    end;
  end;
end;
}

procedure TfrmMain.sgMemViewGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
var
  I: Integer;
begin
  I := MemViewCellIndex[ACol, ARow];
  if I <> -1 then
  begin
    case MemViewType of
      mtRAM, mtROM:
      begin
        HintText := Format('[0x%.4x]', [I]) + sLineBreak;
        if FProcessor.ReadAsZero[MemViewType, I] then
          HintText := HintText + 'unimplemented, read as zero'
        else
        begin
          HintText := HintText + Format('Hex: 0x%.2x', [FProcessor.Memory[MemViewType, I]]) + sLineBreak;
          HintText := HintText + Format('Dec: %d', [FProcessor.Memory[MemViewType, I]]) + sLineBreak;
          HintText := HintText + Format('Oct: %s', [OctStr(FProcessor.Memory[MemViewType, I], 3)]) + sLineBreak;
          HintText := HintText + Format('Bin: %s', [BinStr(FProcessor.Memory[MemViewType, I], 8)]);
        end;
      end;
      mtProgram:
      begin
        HintText := TProcessor.FormatInstruction(FProcessor.Code[I div 2].Instruction);
      end;
      mtProgramCounterStack:
      begin

      end;
    end;
  end;
end;

procedure TfrmMain.sgMemViewPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  I: Integer;
begin
  if (sgMemView.Selection.Size = Size(0, 0)) or not (gdSelected in aState) then
  begin
    // Custom coloring here
    I := MemViewCellIndex[aCol, aRow];
    if (I <> -1) and FProcessor.ReadAsZero[MemViewType, I] then
    begin
      sgMemView.Canvas.Brush.Color := $DDDDDD;
      sgMemView.Canvas.Font.Color := clGrayText;
    end;

    case MemViewType of
      mtRAM:
      begin
        if not FProcessor.Running and (I <> -1) and (I = FProcessor.RAM[b0FSR]) then
          sgMemView.Canvas.Brush.Color := $55DD55
        else if I = 0 then
          sgMemView.Canvas.Brush.Color := $44AA44
        else if I = FProcessor.NormalizeRAMPointer(Ord(b0FSR)) then
          sgMemView.Canvas.Brush.Color := $44DDDD;
      end;
      mtProgram:
      begin
        if not FProcessor.Running and (I <> -1) and (I div 2 = FProcessor.CurrentProgramPos) then
          sgMemView.Canvas.Brush.Color := $3333FF;
      end;
      mtROM:
      begin

      end;
      mtProgramCounterStack:
      begin
        if not FProcessor.Running and (I <> -1) and (I div 2 = FProcessor.PCStackPos - 1) then
          sgMemView.Canvas.Brush.Color := $3333FF;
      end;
    end;
  end
  else
  begin
    // highlight selection, but only if selection is not singular
    sgMemView.Canvas.Brush.Color := sgMemView.SelectedColor;
    sgMemView.Canvas.Font.Color := clHighlightText;
  end;
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
  Compiled := False;
end;

procedure TfrmMain.synEditorClick(Sender: TObject);
var
  P: TPoint;
begin
  P := synEditor.PixelsToRowColumn(synEditor.ScreenToClient(Mouse.CursorPos));
  if P.X = 0 then
  begin
    FProcessor.Breakpoint[P.Y] := not FProcessor.Breakpoint[P.Y];
    UpdateSynEditMarkup;
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

function TfrmMain.GetSpecialFunctionHeight: Integer;
begin
  Result := sgSpecialFunction.Height;
end;

procedure TfrmMain.SetSpecialFunctionHeight(AValue: Integer);
var
  Diff: Integer;
begin
  Diff := AValue - SpecialFunctionHeight;
  gbSpecialFunction.Height := gbSpecialFunction.Height + Diff;
end;

function TfrmMain.GetMemViewWidth: Integer;
begin
  Result := sgMemView.ClientWidth;
end;

function TfrmMain.GetSpecialFunctionWidth: Integer;
begin
  Result := sgSpecialFunction.Width;
end;

procedure TfrmMain.SetMemViewWidth(AValue: Integer);
var
  Diff: Integer;
begin
  Diff := AValue - MemViewWidth;
  gbMemory.Width := gbMemory.Width + Diff;
end;

procedure TfrmMain.SetSpecialFunctionWidth(AValue: Integer);
var
  Diff: Integer;
begin
  Diff := AValue - SpecialFunctionWidth;
  pnlLeft.Width := pnlLeft.Width + Diff;
end;

procedure TfrmMain.SetPreScaler(AValue: Byte);
begin
  if FPreScaler = AValue then
    Exit;
  FPreScaler := AValue;
  lbPreScaler.Caption := Format('%.2x/%d', [PreScaler, PreScaler]);
end;

procedure TfrmMain.SetLineFollowMode(AValue: TLineFollowMode);
begin
  if FLineFollowMode = AValue then
    Exit;
  FLineFollowMode := AValue;
  UpdateSynEditScroll;
end;

procedure TfrmMain.SetLineFollowRange(AValue: Cardinal);
begin
  if FLineFollowRange = AValue then
    Exit;
  FLineFollowRange := AValue;
  UpdateSynEditScroll;
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

procedure TfrmMain.SetCompiled(AValue: Boolean);
begin
  if FCompiled = AValue then
    Exit;
  FCompiled := AValue;
  UpdateActions;
  UpdateSynEditMarkup;
end;

procedure TfrmMain.SetWRegister(AValue: Byte);
begin
  if FWRegister = AValue then
    Exit;
  FWRegister := AValue;
  lbWRegister.Caption := Format('%.2xh/%s', [WRegister, BinStr(WRegister, 8)]);
end;

function TfrmMain.GetMemViewType: TProcessor.TMemoryType;
begin
  Result := TProcessor.TMemoryType(cbMemorySelection.ItemIndex);
end;

procedure TfrmMain.UpdateSynEditMarkup;
begin
  synEditor.Invalidate;
end;

procedure TfrmMain.UpdateCycles;
begin
  Cycles := FProcessor.Cycles;
end;

procedure TfrmMain.UpdateALUInfo;
begin
  WRegister := FProcessor.WRegister;
  Flags := FProcessor.CalcFlags;
  PreScaler := FProcessor.PreScaler;
end;

procedure TfrmMain.UpdateSynEditScroll;
begin
  case LineFollowMode of
    lfBorder:
      synEditor.TopLine := Min(Max(
        synEditor.TopLine, FProcessor.CurrentInstruction.Line - synEditor.LinesInWindow + Integer(LineFollowRange)),
        FProcessor.CurrentInstruction.Line - LineFollowRange);
    lfCenter:
      synEditor.TopLine := FProcessor.CurrentInstruction.Line - synEditor.LinesInWindow div 2;
  end;
end;

procedure TfrmMain.SetMemViewType(AValue: TProcessor.TMemoryType);
begin
  if MemViewType = AValue then
    Exit;
  cbMemorySelection.ItemIndex := Ord(AValue);
  UpdateMemView;
end;

function TfrmMain.GetMemViewCellIndex(ACol, ARow: Integer): Integer;
begin
  if (ACol < 1) or (ARow < 1) then
    Result := -1
  else
    Result := (aRow - 1) * MemViewColumns + (aCol - 1);
end;

procedure TfrmMain.InitSynEdit;
begin
  synCompletion.TheForm.Font := synEditor.Font;
  synCompletion.TheForm.Font.Bold := True;
end;

procedure TfrmMain.InitSpecialFunction;
var
  I: Integer;
begin
  sgSpecialFunction.Columns.Add.Title.Caption := 'Register';
  for I := 7 downto 0 do
    sgSpecialFunction.Columns.Add.Title.Caption := Format('%d', [I]);

  UpdateSpecialFunction;
  sgSpecialFunction.AutoSizeColumns;
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
    sgMemView.Columns.Add.Title.Caption := Format('%.2x', [I]);

  cbMemorySelection.Clear;
  for T := Low(T) to High(T) do
    cbMemorySelection.Items.Add(TProcessor.MemoryName[T]);
  MemViewType := mtRAM;

  UpdateMemView;
  sgMemView.AutoSizeColumns;
end;

procedure TfrmMain.OnPeripheralAdd(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  PeripheralClasses[MenuItem.Tag].Create(Self);
end;

procedure TfrmMain.LoadFile(AFileName: String);
var
  FileData: RawByteString;
  LoadType: TLoadType;
begin
  try
    LoadType := ExtractFileType(AFileName);
    case LoadType of
      ltAssembler:
      begin
        raise ENotImplemented.Create('Can''t load! Loading not implemented!');
      end;
      ltCompiled:
      begin
        FFileData.LoadFromFile(AFileName);
        FileData := FFileData.Text;
        SetCodePage(FileData, 1252, False);
        SetCodePage(FileData, DefaultSystemCodePage);
        FFileData.Text := FileData;
        ParseStringListFromLST(FFileData);
        FProcessor.LoadProgram(FFileData);
        Compiled := True;
        UpdateActions;
        UpdateMemView;
        UpdateCycles;
        UpdateALUInfo;
        UpdateSpecialFunction;
        UpdateSynEditScroll;
        UpdateSynEditMarkup;
      end;
      ltBinary:
      begin
        raise ENotImplemented.Create('Can''t load! Loading not implemented!');
      end;
    end;
  except
    on E: EUnsupportedException do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

function TfrmMain.ExtractFileType(AFileName: String): TLoadType;
var
  LoadType: TLoadType;
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  for LoadType := Low(LoadType) to High(LoadType) do
    if LoadExtensions[LoadType] = Ext then
      Exit(LoadType);
  raise EUnsupportedException.Create(Ext);
end;

function TfrmMain.GetPortsVisible: Boolean;
begin
  Result := gbSpecialFunction.Visible;
end;

procedure TfrmMain.SetPortsVisible(AValue: Boolean);
begin
  if PortsVisible = AValue then
    Exit;
  DisableAlign;
  gbSpecialFunction.Visible := AValue;
  actTogglePortsVisible.Checked := AValue;
  EnableAlign;
end;

procedure TfrmMain.SpecialFunctionAutoWidth;
begin
  SpecialFunctionWidth := sgSpecialFunction.GridWidth + 4;
end;

procedure TfrmMain.SpecialFunctionAutoHeight;
begin
  SpecialFunctionHeight := sgSpecialFunction.GridHeight + 4;
end;

procedure TfrmMain.MemViewAutoWidth;
begin
  MemViewWidth := sgMemView.GridWidth;
end;

procedure TfrmMain.SetCycles(AValue: Cardinal);
begin
  if FCycles = AValue then
    Exit;
  FCycles := AValue;
  lbCycles.Caption := Format('%d', [AValue]);
end;

function TfrmMain.GetMemoryVisible: Boolean;
begin
  Result := gbMemory.Visible;
end;

procedure TfrmMain.SetMemoryVisible(AValue: Boolean);
begin
  if MemoryVisible = AValue then
    Exit;
  DisableAlign;
  gbMemory.Visible := AValue;
  actToggleMemoryVisible.Checked := AValue;
  EnableAlign;
end;

procedure TfrmMain.UpdateMemView;
var
  R, C, P: Cardinal;
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
        sgMemView.Rows[R + 1][C + 1] := '00'
      else
        sgMemView.Rows[R + 1][C + 1] := Format('%.2x', [FProcessor.Memory[MemViewType, P]]);
    end;
  end;
  sgMemView.EndUpdate;
end;

procedure TfrmMain.UpdateSpecialFunction;
var
  B: Cardinal;
  R0: TProcessor.TRegisterBank0;
  R1: TProcessor.TRegisterBank1;
begin
  sgSpecialFunction.BeginUpdate;
  sgSpecialFunction.RowCount := 1;
  for R0 := Low(R0) to High(R0) do
  begin
    if R0 = b0Unused then
      Continue;
    sgSpecialFunction.RowCount := sgSpecialFunction.RowCount + 1;
    sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][0] := TProcessor.RegisterBank0Name[R0];
    for B := 0 to 7 do
    begin
      if FProcessor.RAMBit[R0, B] then
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][(7 - B) + 1] := '1'
      else
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][(7 - B) + 1] := '0';
    end;
  end;
  for R1 := Low(R1) to High(R1) do
  begin
    if (R1 = b1Unused) or (R1 in TProcessor.RegisterBank1Mapped) then
      Continue;
    sgSpecialFunction.RowCount := sgSpecialFunction.RowCount + 1;
    sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][0] :=
      FProcessor.RegisterBank1Name[R1];
    for B := 0 to 7 do
    begin
      if FProcessor.RAMBit[R1, B] then
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][(7 - B) + 1] := '1'
      else
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][(7 - B) + 1] := '0';
    end;
  end;
  sgSpecialFunction.EndUpdate;
end;

procedure TfrmMain.IdleHandler(Sender: TObject; var ADone: Boolean);
begin
  if FProcessor.Running then
  begin
    FProcessor.CatchUp;

    if not FProcessor.Running then
    begin
      UpdateALUInfo;
      UpdateMemView;
      UpdateSynEditMarkup;
      UpdateSynEditScroll;
    end;

    UpdateCycles;
    Sleep(5);
    ADone := False;
  end
  else
    ADone := True;
end;

procedure TfrmMain.UpdateActions;
begin
  inherited UpdateActions;
  cbMemorySelection.Enabled := not FProcessor.Running;
end;

function TfrmMain.GetProcessor: TProcessor;
begin
  Result := FProcessor;
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

procedure TfrmMain.ProcessParams;
begin
  if ParamCount >= 1 then
    LoadFile(ParamStr(1));
end;

procedure TfrmMain.GeneratePeripheralLists;
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to Length(PeripheralClasses) - 1 do
  begin
    MenuItem := TMenuItem.Create(miPeripherals);
    MenuItem.Caption := PeripheralClasses[I].GetDefaultPeripheralName;
    MenuItem.OnClick := OnPeripheralAdd;
    MenuItem.Tag := I;
    miPeripherals.Add(MenuItem);
  end;
end;

procedure TfrmMain.OnAsyncProcessorChange(AProcessor: TProcessor);
begin
  UpdateMemView;
  UpdateSpecialFunction;
end;

end.


