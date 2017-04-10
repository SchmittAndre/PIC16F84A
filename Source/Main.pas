unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ActnList,
  StdCtrls, Grids, ComCtrls, ProcessorDefine, SynCompletion, SynHighlighterAny,
  Types, LCLType, SynEditMiscClasses, Math;

type

  { TfrmMain }

  TfrmMain = class(TForm)
  published
    actExit: TAction;
    actFlashMem: TAction;
    actCompile: TAction;
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
    btnSave: TButton;
    btnStartStop: TButton;
    btnRefreshMemory: TButton;
    btnFlash: TButton;
    cbMemorySelection: TComboBox;
    cbAutoRefreshMemory: TCheckBox;
    gbMemory: TGroupBox;
    gbPorts: TGroupBox;
    gbPeripherals: TGroupBox;
    ilMarker: TImageList;
    lbRuntime: TLabel;
    lbCycles: TLabel;
    lbRuntimeTitle: TLabel;
    lbCyclesTitle: TLabel;
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
    pnlInfo: TPanel;
    pnlRuntime: TPanel;
    pnlMemorySelection: TPanel;
    pnlCenter: TPanel;
    pmMain: TPopupMenu;
    pnlCycles: TPanel;
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
    procedure Action1Execute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRefreshMemoryExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actShowAllExecute(Sender: TObject);
    procedure actToggleMemoryVisibleExecute(Sender: TObject);
    procedure actTogglePeripheralsVisibleExecute(Sender: TObject);
    procedure actTogglePortsVisibleExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function synCompletionMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;
    function synCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean;
      Index: integer): boolean;
    procedure synCompletionSearchPosition(var APosition: integer);
    procedure synEditorChange(Sender: TObject);
    procedure synEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean;
      Markup: TSynSelectedColor);
  private
    FStartTime: Int64;
    FCycles: Cardinal;
    FProcessor: TProcessor;
    FFileData: TStringList;

    procedure InitSynEdit;
    procedure InitMemView;

    function GetAutoRefreshMemory: Boolean;
    procedure ParseStringListFromLST(List: TStringList);


    function GetRuntime: Int64;
    procedure SetCycles(AValue: Cardinal);
    procedure SetStartTime(AValue: Int64);

    function GetMemoryVisible: Boolean;
    procedure SetAutoRefreshMemory(AValue: Boolean);
    procedure SetMemoryVisible(AValue: Boolean);

    function GetPeripheralsVisible: Boolean;
    function GetPortsVisible: Boolean;
    procedure SetPeripheralsVisible(AValue: Boolean);
    procedure SetPortsVisible(AValue: Boolean);

    property MemoryVisible: Boolean read GetMemoryVisible write SetMemoryVisible;
    property PortsVisible: Boolean read GetPortsVisible write SetPortsVisible;
    property PeripheralsVisible: Boolean read GetPeripheralsVisible write SetPeripheralsVisible;
    property AutoRefreshMemory: Boolean read GetAutoRefreshMemory write SetAutoRefreshMemory;

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
  InitSynEdit;
  InitMemView;
   sgSpecialFunction.Rows[3][1] := '0';
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

procedure TfrmMain.Action1Execute(Sender: TObject);
begin

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

procedure TfrmMain.synEditorSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean;
  Markup: TSynSelectedColor);
begin

end;

procedure TfrmMain.InitSynEdit;
begin
  synCompletion.TheForm.Font := synEditor.Font;
  synCompletion.TheForm.Font.Bold := True;
end;

procedure TfrmMain.InitMemView;
var
  I, J: Integer;
begin
  sgMemView.TitleFont := Font;
  sgMemView.Columns.Add.Title.Caption := 'Adress';
  for I := 0 to 7  do
    with sgMemView.Columns.Add do
    begin
      Title.Caption := Format('%.2x', [I]);
    end;

  sgMemView.RowCount := 9;
  for I := 0 to 7 do
  begin
    sgMemView.Rows[I + 1][0] := Format('0x%.4x', [I * 8]);
    for J := 1 to 8 do
      sgMemView.Rows[I + 1][J] := Format('%.2x', [Random(256)]);
  end;

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
begin
  if MemoryVisible = AValue then
    Exit;
  DisableAlign;
  gbMemory.Visible := AValue;
  spltMemory.Visible := AValue;
  actToggleMemoryVisible.Checked := AValue;
  EnableAlign;
end;

procedure TfrmMain.UpdateRuntime;
begin
  lbRuntime.Caption := Format('%d ms', [Runtime]);
end;

procedure TfrmMain.ParseStringListFromLST(List: TStringList);
const
  AssamblerOffset: Integer = 7;
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
    synEditor.Lines[Counter] := synEditor.Lines[Counter].Remove(0, MinWhitespacecount - 1 + AssamblerOffset );
  end;

end;

end.


