unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList,
  StdCtrls, Grids, ComCtrls, ProcessorDefine, SynCompletion, SynHighlighterAny,
  Types, LCLType, SynEditMiscClasses, Math, LazUTF8;

type

  TLineFollowMode = (
    lfOff,
    lfBorder,
    lfCenter
  );

  { TfrmMain }

  TfrmMain = class(TForm)
    actExit: TAction;
    actCompile: TAction;
    actNew: TAction;
    actSaveFileAs: TAction;
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
    gbPorts: TGroupBox;
    gbPeripherals: TGroupBox;
    gbFile: TGroupBox;
    gbStateInfo: TGroupBox;
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
    miPeripherals: TMenuItem;
    miMemory: TMenuItem;
    miPorts: TMenuItem;
    miView: TMenuItem;
    miFileSplitter2: TMenuItem;
    miPlaceholder: TMenuItem;
    miFile: TMenuItem;
    mmMainMenu: TMainMenu;
    pnlControl: TPanel;
    pnlCycles: TPanel;
    pnlPreScaler: TPanel;
    pnlPeripherals: TPanel;
    pnlWRegister: TPanel;
    pnlInfo: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    pnlFlags: TPanel;
    spltMemory: TSplitter;
    spltPeripherals: TSplitter;
    sbStatus: TStatusBar;
    sgSpecialFunction: TStringGrid;
    synEditor: TSynEdit;
    sgMemView: TStringGrid;
    synHighlighter: TSynAnySyn;
    synCompletion: TSynCompletion;
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actHideAllExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actOpenFileUpdate(Sender: TObject);
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
    procedure actTogglePeripheralsVisibleExecute(Sender: TObject);
    procedure actTogglePortsVisibleExecute(Sender: TObject);
    procedure cbMemorySelectionChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure pnlPeripheralsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlPeripheralsPaint(Sender: TObject);
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
    FPreScaler: Byte;
    procedure SetPreScaler(AValue: Byte);
  private
    FMemViewColumns: Cardinal;
    FCycles: Cardinal;
    FWRegister: Byte;
    FProcessor: TProcessor;
    FFileData: TStringList;
    FCompiled: Boolean;
    FFlags: TProcessor.TCalcFlags;
    FLineFollowMode: TLineFollowMode;
    FLineFollowRange: Cardinal;

    procedure InitSynEdit;
    procedure InitSpecialFunction;
    procedure InitMemView;

    procedure ParseStringListFromLST(List: TStringList);

    procedure SetMemViewColumns(AValue: Cardinal);
    procedure SetCycles(AValue: Cardinal);
    procedure SetWRegister(AValue: Byte);

    procedure SetLineFollowMode(AValue: TLineFollowMode);
    procedure SetLineFollowRange(AValue: Cardinal);

    function GetMemoryVisible: Boolean;
    procedure SetMemoryVisible(AValue: Boolean);
    function GetMemViewType: TProcessor.TMemoryType;
    procedure SetMemViewType(AValue: TProcessor.TMemoryType);

    function GetMemViewCellIndex(ACol, ARow: Integer): Integer;

    procedure SetCompiled(AValue: Boolean);
    procedure SetFlags(AValue: TProcessor.TCalcFlags);

    function GetPeripheralsVisible: Boolean;
    function GetPortsVisible: Boolean;
    procedure SetPeripheralsVisible(AValue: Boolean);
    procedure SetPortsVisible(AValue: Boolean);

    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible;
    property PortsVisible: Boolean read GetPortsVisible write SetPortsVisible;
    property PeripheralsVisible: Boolean read GetPeripheralsVisible write SetPeripheralsVisible;

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

    procedure UpdateMemView;
    procedure UpdateSpecialFunction;
    procedure UpdateSynEditMarkup;
    procedure UpdateCycles;
    procedure UpdateALUInfo;
    procedure UpdateSynEditScroll;

    procedure IdleHandler(Sender: TObject; var ADone: Boolean);

  protected
    procedure UpdateActions; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);

  procedure RecursiveDoubleBuffered(AComponent: TComponent);
  var
    C: TComponent;
  begin
    if AComponent is TWinControl then
      TWinControl(AComponent).DoubleBuffered := True;
    for C in AComponent do
      RecursiveDoubleBuffered(C);
  end;

begin
  RecursiveDoubleBuffered(Self);
  FProcessor := TProcessor.Create;
  FFileData := TStringList.Create;
  LineFollowRange := 3;
  LineFollowMode := lfBorder;
  InitSynEdit;
  InitMemView;
  InitSpecialFunction;
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

  UpdateMemView;

  sgMemView.AutoAdjustColumns;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actCompileExecute(Sender: TObject);
begin
  // TODO: Compile
  raise ENotImplemented.Create('Compiling not implemented');
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
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actOpenFileUpdate(Sender: TObject);
begin
  actOpenFile.Enabled := not FProcessor.Running;
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

procedure TfrmMain.actSaveFileUpdate(Sender: TObject);
begin
  actSaveFile.Enabled := not FProcessor.Running;
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

procedure TfrmMain.actTogglePeripheralsVisibleExecute(Sender: TObject);
begin
  PeripheralsVisible := not PeripheralsVisible;
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
  FProcessor.Free;
  FFileData.Free;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  // TODO: Load dropped file
end;

procedure TfrmMain.pnlPeripheralsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

end;

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
  RingSliceA: array [TRing] of array [0 .. 1] of Integer = (
    (-1, ),
    (),
    (),
    (),
    ()
  );
  RingSliceB: array [TRing] of array [0 .. 1] of Integer = (
    (),
    (),
    (),
    (),
    ()
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
      Arc(RingPos[R].X, RingPos[R].Y, RingPos[R].X + 50, RingPos[R].Y + 50, RingSliceA[R] * 720, RingSliceA[R] * 720);
    end;
    for R := Low(R) to High(R) do
    begin
      Pen.Color := RingColor[R];
      Arc(RingPos[R].X, RingPos[R].Y, RingPos[R].X + 50, RingPos[R].Y + 50, RingSliceB[R] * 720, RingSliceB[R] * 720);
    end;
  end;
end;

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
  for I := 0 to 7 do
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
  EnableAlign;
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
  spltMemory.Visible := AValue;
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
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][B + 1] := '1'
      else
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][B + 1] := '0';
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
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][B + 1] := '1'
      else
        sgSpecialFunction.Rows[sgSpecialFunction.RowCount - 1][B + 1] := '0';
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


