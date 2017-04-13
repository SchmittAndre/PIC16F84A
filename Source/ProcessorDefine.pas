unit ProcessorDefine;

{$M+}

interface

uses
  Classes, windows, SysUtils, Dialogs, Lists;

type

  { TProcessor }

  TProcessor = class
  public const
    {$REGION Simple Numerical Constants for Size and such}
    // Stack
    StackSize = 8;
    // Program Memory
    ProgramMemorySize = $400;
    // Ram
    FileMapOffset = $80;
    RAMStart = $0C;
    RAMEnd = $4F;
    RAMSize = RAMEnd - RAMStart + 1;
    RAMMappedStart = RAMStart + FileMapOffset;
    RAMMappedEnd = RAMEnd + FileMapOffset;
    RAMFullSize = $100;
    // EEPRom
    ROMSize = $40;
    // Timing
    OperationTime = 1e-6;
    OperationFrequeny = 1 / OperationTime;
    {$ENDREGION}

  public type
    {$REGION TRegisterBank}
    TRegisterBank0 = (
      b0IndirectAddr,
      b0TMR0,
      b0PCL,
      b0STATUS,
      b0FSR,
      b0PORTA,
      b0PORTB,
      b0Unused,
      b0EEDATA,
      b0EEADR,
      b0PCLATH,
      b0INTCON
    );

    TRegisterBank1 = (
      b1IndirectAddr = $80,
      b1OPTION_REG,
      b1PCL,
      b1STATUS,
      b1FSR,
      b1TRISA,
      b1TRISB,
      b1Unused,
      b1EECON1,
      b1EECON2,
      b1PCLATH,
      b1INTCON
    );
    {$ENDREGION}

    {$REGION TMemoryType}
    TMemoryType = (
      mtRAM,
      mtProgram,
      mtROM,
      mtProgramCounterStack
    );
    {$ENDREGION}

    {$REGION Custom Rangers}
    TProgramCounter = $0 .. $1FFF;
    TProgramCounterStackPos = 0 .. StackSize - 1;
    TProgramCounterStackPointer = 0 .. StackSize * 2 - 1;
    TInstruction = $0 .. $3FFF;
    TProgramMemPos = 0 .. ProgramMemorySize - 1;
    TProgramMemPointer = 0 .. ProgramMemorySize * 2 - 1;
    TFileAdress = $0 .. $7F;
    TRAMPointer = $0 .. $FF;
    TROMPointer = 0 .. ROMSize - 1;
    TBitIndex = 0 .. 7;
    {$ENDREGION}

    {$REGION TInstructionType}
    TInstructionType = (
      itADDWF = 1,
      itANDWF,
      itCLRF,
      itCLRW,
      itCOMF,
      itDECF,
      itDECFSZ,
      itINCF,
      itINCFSZ,
      itIORWF,
      itMOVF,
      itMOVWF,
      itNOP1,
      itNOP2,
      itNOP3,
      itNOP4,
      itRLF,
      itRRF,
      itSUBWF,
      itSWAPF,
      itXORWF,
      itBCF,
      itBSF,
      itBTFSC,
      itBTFSS,
      itADDLW,
      itANDLW,
      itCALL,
      itCLRWDT,
      itGOTO,
      itIORLW,
      itMOVLW,
      itRETFIE,
      itRETLW,
      itRETURN,
      itSLEEP,
      itSUBLW,
      itXORLW
    );
    {$ENDREGION}

    {$REGION TCalcFlag}
    TCalcFlag = (
      cfZero,
      cfDigitCarry,
      cfCarry
    );
    {$ENDREGION}

    TCalcFlags = set of TCalcFlag;

    TInstructionInfo = record
      Name: String;
      SignificantBits: Byte;
      Instruction: TInstruction;
    end;

    TInstructionMethod = procedure (AInstruction: TInstruction) of object;

    TStepInfo = (
      siSingle,
      siMultiple
    );

  public const
    {$REGION RegisterBank Mapped Parts}
    RegisterBank0Mapped: set of TRegisterBank0 = [
      b0IndirectAddr,
      b0PCL,
      b0STATUS,
      b0FSR,
      b0PCLATH,
      b0INTCON
    ];

    RegisterBank1Mapped: set of TRegisterBank1 = [
      b1IndirectAddr,
      b1PCL,
      b1STATUS,
      b1FSR,
      b1PCLATH,
      b1INTCON
    ];
    {$ENDREGION}

    {$REGION Memory Info}
    MemorySize: array [TMemoryType] of Word = (
      RAMFullSize,
      ProgramMemorySize,
      ROMSize,
      StackSize * 2
    );

    MemoryName: array [TMemoryType] of String = (
      'RAM',
      'Program',
      'EEPROM',
      'PC-Stack'
    );
    {$ENDREGION}

    {$REGION CalcFlag Names}
    CalcFlagName: array [TCalcFlag] of String = (
      'Z',
      'DC',
      'C'
    );
    {$ENDREGION}

    {$REGION Instructions}
    InstructionInfo: array [TInstructionType] of TInstructionInfo = (
      (Name: 'ADDWF';  SignificantBits: 6;  Instruction: $0700),
      (Name: 'ANDWF';  SignificantBits: 6;  Instruction: $0500),
      (Name: 'CLRF';   SignificantBits: 7;  Instruction: $0180),
      (Name: 'CLRW';   SignificantBits: 7;  Instruction: $0100),
      (Name: 'COMF';   SignificantBits: 6;  Instruction: $0900),
      (Name: 'DECF';   SignificantBits: 6;  Instruction: $0300),
      (Name: 'DECFSZ'; SignificantBits: 6;  Instruction: $0B00),
      (Name: 'INCF';   SignificantBits: 6;  Instruction: $0A00),
      (Name: 'INCFSZ'; SignificantBits: 6;  Instruction: $0F00),
      (Name: 'IORWF';  SignificantBits: 6;  Instruction: $0400),
      (Name: 'MOVF';   SignificantBits: 6;  Instruction: $0800),
      (Name: 'MOVWF';  SignificantBits: 7;  Instruction: $0080),
      (Name: 'NOP';    SignificantBits: 14; Instruction: $0000),
      (Name: 'NOP';    SignificantBits: 14; Instruction: $0020),
      (Name: 'NOP';    SignificantBits: 14; Instruction: $0040),
      (Name: 'NOP';    SignificantBits: 14; Instruction: $0060),
      (Name: 'RLF';    SignificantBits: 6;  Instruction: $0D00),
      (Name: 'RRF';    SignificantBits: 6;  Instruction: $0C00),
      (Name: 'SUBWF';  SignificantBits: 6;  Instruction: $0200),
      (Name: 'SWAPF';  SignificantBits: 6;  Instruction: $0E00),
      (Name: 'XORWF';  SignificantBits: 6;  Instruction: $0600),
      (Name: 'BCF';    SignificantBits: 4;  Instruction: $1000),
      (Name: 'BSF';    SignificantBits: 4;  Instruction: $1400),
      (Name: 'BTFSC';  SignificantBits: 4;  Instruction: $1800),
      (Name: 'BTFSS';  SignificantBits: 4;  Instruction: $1C00),
      (Name: 'ADDLW';  SignificantBits: 5;  Instruction: $3E00),
      (Name: 'ANDLW';  SignificantBits: 6;  Instruction: $3900),
      (Name: 'CALL';   SignificantBits: 3;  Instruction: $2000),
      (Name: 'CLRWDT'; SignificantBits: 14; Instruction: $0064),
      (Name: 'GOTO';   SignificantBits: 3;  Instruction: $2800),
      (Name: 'IORLW';  SignificantBits: 6;  Instruction: $3800),
      (Name: 'MOVLW';  SignificantBits: 4;  Instruction: $3000),
      (Name: 'RETFIE'; SignificantBits: 14; Instruction: $0009),
      (Name: 'RETLW';  SignificantBits: 4;  Instruction: $3400),
      (Name: 'RETURN'; SignificantBits: 14; Instruction: $0008),
      (Name: 'SLEEP';  SignificantBits: 14; Instruction: $0063),
      (Name: 'SUBLW';  SignificantBits: 5;  Instruction: $3C00),
      (Name: 'XORLW';  SignificantBits: 6;  Instruction: $3A00)
    );
    {$ENDREGION}

  private type
    TLineInstruction = record
      Instruction: TInstruction;
      Line: Integer;
    end;

  private class var
    FInstructionArray: array [TInstruction] of TInstructionType;

    class constructor Create;

  private
    // Program
    FProgramCounter: TProgramCounter;
    FProgramMem: array [TProgramMemPos] of TLineInstruction;

    // Program Stack
    FProgramCounterStackPos: TProgramCounterStackPos;
    FProgramCounterStack: array [TProgramCounterStackPos] of TProgramCounter;

    // Banks + RAM
    FRAM: array [TRAMPointer] of Byte;
    FWRegister: Byte;

    // EEPROM
    FROM: array [TROMPointer] of Byte;

    // Function Pointer
    FMethods: array [TInstructionType] of TInstructionMethod;
    FKeepProgramCounter: Boolean;

    // State
    FRunning: Boolean;
    FCycles: Int64;           // All Cycles that happened after a call of Start
    FCyclesFromStart: Int64;  // Cycles from StartTime
    FFrequency: Int64;
    FStartTime: Int64;        // Changes for ResetSyncTime
    FSpeedFactor: Single;
    FBreakpoints: TCardinalSet;

    FHelpBreakpointDepth: TProgramCounterStackPos;
    FHelpBreakpointEnabled: Boolean;

    function GetBreakpoint(ALine: Cardinal): Boolean;
    function GetCalcFlags: TCalcFlags;
    function GetCurrentInstruction: TLineInstruction;
    function GetDataMem(P: TROMPointer): Byte;
    function GetMemory(AType: TMemoryType; APos: Cardinal): Byte;
    function GetPCStack(P: TProgramCounterStackPos): TProgramCounter;
    function GetCode(P: TProgramCounter): TLineInstruction;
    function GetPCStackMem(P: TProgramCounterStackPointer): Byte;
    function GetProgramMem(P: TProgramMemPointer): Byte;
    function GetRAM(P: TRAMPointer): Byte;
    function GetRegisterBank0(S: TRegisterBank0): Byte;
    function GetRegisterBank1(S: TRegisterBank1): Byte;
    function GetTimeBehind: Single;
    function GetReadAsZero(AType: TMemoryType; APos: Cardinal): Boolean;
    function GetCarryFlag: Boolean;
    function GetDigitCarryFlag: Boolean;
    function GetFileMap(P: TRAMPointer): Byte;
    function GetFlag(P: TRAMPointer; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank0; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank1; ABit: TBitIndex): Boolean; overload;
    function GetZeroFlag: Boolean;
    procedure SetBreakpoint(ALine: Cardinal; AValue: Boolean);
    procedure SetCarryFlag(AValue: Boolean);
    procedure SetDigitCarryFlag(AValue: Boolean);
    procedure SetFileMap(P: TRAMPointer; AValue: Byte);
    procedure SetFlag(P: TRAMPointer; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank0; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank1; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetZeroFlag(AValue: Boolean);

    function NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;

    procedure SetSpeedFactor(AValue: Single);

    property FileMap[P: TRAMPointer]: Byte read GetFileMap write SetFileMap;
    property Flag[P: TRAMPointer; ABit: TBitIndex]: Boolean read GetFlag write SetFlag;
    property CarryFlag: Boolean read GetCarryFlag write SetCarryFlag;
    property DigitCarryFlag: Boolean read GetDigitCarryFlag write SetDigitCarryFlag;
    property ZeroFlag: Boolean read GetZeroFlag write SetZeroFlag;

    // help-functions
    procedure KeepProgramCounter;

    class function ExtractByteLiteral(AInstruction: TInstruction): Byte; static;
    class function ExtractFileAdress(AInstruction: TInstruction): TFileAdress; static;
    class function ExtractProgramCounter(AInstruction: TInstruction): TProgramCounter; static;
    class function ExtractDestIsFile(AInstruction: TInstruction): Boolean; static;

    function DoAdd(A, B: Byte): Byte;
    function DoSub(A, B: Byte): Byte;

    function NextProgramCounter(ACount: Integer = 1): TProgramCounter;
    procedure AdvanceProgramCounter(ACount: Integer = 1);
    procedure AdvanceCycles(ACount: Integer = 1);

    procedure PushStack(AProgramCounter: TProgramCounter);
    function PopStack: TProgramCounter;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadProgram(AFileData: TStrings);
    procedure Initialize;
    procedure ResetROM;

    procedure Start;
    procedure Stop;

    procedure ProcessInstruction(AInstruction: TInstruction; ALine: Integer = -1); overload;
    procedure ProcessInstruction(AInstruction: TLineInstruction); overload;

    procedure ResetSyncTime;

    procedure StepIn;
    function StepOver: TStepInfo;
    function StepOut: TStepInfo;
    function CatchUp: Boolean;
    property TimeBehind: Single read GetTimeBehind;

    property ProgramMem[P: TProgramMemPointer]: Byte read GetProgramMem;
    property Code[P: TProgramCounter]: TLineInstruction read GetCode;
    property CurrentInstruction: TLineInstruction read GetCurrentInstruction;
    property WRegister: Byte read FWRegister;
    property RegisterBank0[S: TRegisterBank0]: Byte read GetRegisterBank0;
    property RegisterBank1[S: TRegisterBank1]: Byte read GetRegisterBank1;
    property RAM[P: TRAMPointer]: Byte read GetRAM;
    property ROM[P: TROMPointer]: Byte read GetDataMem;
    property PCStack[P: TProgramCounterStackPos]: TProgramCounter read GetPCStack;
    property PCStackMem[P: TProgramCounterStackPointer]: Byte read GetPCStackMem;
    property CalcFlags: TCalcFlags read GetCalcFlags;

    property ReadAsZero[AType: TMemoryType; APos: Cardinal]: Boolean read GetReadAsZero;
    property Memory[AType: TMemoryType; APos: Cardinal]: Byte read GetMemory;

    property Running: Boolean read FRunning;
    property Cycles: Int64 read FCycles;
    property SpeedFactor: Single read FSpeedFactor write SetSpeedFactor;

    property Breakpoint[ALine: Cardinal]: Boolean read GetBreakpoint write SetBreakpoint;

  published
    {$REGION --- Byte-Oriented File Register Operations --- }
    procedure InstructionADDWF(AInstruction: TInstruction);
    procedure InstructionANDWF(AInstruction: TInstruction);
    procedure InstructionCLRF(AInstruction: TInstruction);
    procedure InstructionCLRW({%H-}AInstruction: TInstruction);
    procedure InstructionCOMF(AInstruction: TInstruction);
    procedure InstructionDECF(AInstruction: TInstruction);
    procedure InstructionDECFSZ(AInstruction: TInstruction);
    procedure InstructionINCF(AInstruction: TInstruction);
    procedure InstructionINCFSZ(AInstruction: TInstruction);
    procedure InstructionIORWF(AInstruction: TInstruction);
    procedure InstructionMOVF(AInstruction: TInstruction);
    procedure InstructionMOVWF(AInstruction: TInstruction);
    procedure InstructionNOP({%H-}AInstruction: TInstruction);
    procedure InstructionRLF(AInstruction: TInstruction);
    procedure InstructionRRF(AInstruction: TInstruction);
    procedure InstructionSUBWF(AInstruction: TInstruction);
    procedure InstructionSWAPF(AInstruction: TInstruction);
    procedure InstructionXORWF(AInstruction: TInstruction);
    {$ENDREGION}

    {$REGION --- Bit-Oriented File Register Operations --- }
    // TODO: procedure InstructionBCF(AInstruction: TInstruction);
    // TODO: procedure InstructionBSF(AInstruction: TInstruction);
    // TODO: procedure InstructionBTFSC(AInstruction: TInstruction);
    // TODO: procedure InstructionBTFSS(AInstruction: TInstruction);
    {$ENDREGION}

    {$REGION --- Literal and Control Operations --- }
    procedure InstructionADDLW(AInstruction: TInstruction);
    procedure InstructionANDLW(AInstruction: TInstruction);
    procedure InstructionCALL(AInstruction: TInstruction);
    // TODO: procedure InstructionCLRWDT(AInstruction: TInstruction);
    procedure InstructionGOTO(AInstruction: TInstruction);
    procedure InstructionIORLW(AInstruction: TInstruction);
    procedure InstructionMOVLW(AInstruction: TInstruction);
    // TODO: procedure InstructionRETFIE(AInstruction: TInstruction);
    procedure InstructionRETLW(AInstruction: TInstruction);
    procedure InstructionRETURN({%H-}AInstruction: TInstruction);
    // TODO: procedure InstructionSLEEP(AInstruction: TInstruction);
    procedure InstructionSUBLW(AInstruction: TInstruction);
    procedure InstructionXORLW(AInstruction: TInstruction);
    {$ENDREGION}
  end;

implementation

{ TProcessor }

procedure TProcessor.AdvanceProgramCounter(ACount: Integer);
begin
  FProgramCounter := NextProgramCounter(ACount);
end;

procedure TProcessor.AdvanceCycles(ACount: Integer);
begin
  Inc(FCycles, ACount);
  Inc(FCyclesFromStart, ACount);
end;

class constructor TProcessor.Create;
var
  T: TInstructionType;
  I: TInstruction;
begin
  FillByte(FInstructionArray, SizeOf(FInstructionArray), 0);
  for T := Low(T) to High(T) do
  begin
    for I := InstructionInfo[T].Instruction to
      InstructionInfo[T].Instruction + (1 shl (14 - InstructionInfo[T].SignificantBits)) - 1 do
    begin
      FInstructionArray[InstructionInfo[T].Instruction or I] := T;
    end;
  end;
end;

function TProcessor.GetCarryFlag: Boolean;
begin
  Result := Flag[b0STATUS, 0];
end;

function TProcessor.GetDigitCarryFlag: Boolean;
begin
  Result := Flag[b0STATUS, 1];
end;

function TProcessor.GetFileMap(P: TRAMPointer): Byte;
begin
  Result := FRAM[NormalizeRAMPointer(P)];
end;

function TProcessor.GetFlag(P: TRAMPointer; ABit: TBitIndex): Boolean;
begin
  Result := ((FRAM[NormalizeRAMPointer(P)] shr ABit) and 1) = 1;
end;

function TProcessor.GetFlag(P: TRegisterBank0; ABit: TBitIndex): Boolean;
begin
  Result := Flag[Ord(P), ABit];
end;

function TProcessor.GetFlag(P: TRegisterBank1; ABit: TBitIndex): Boolean;
begin
  Result := Flag[Ord(P), ABit];
end;

function TProcessor.GetZeroFlag: Boolean;
begin
  Result := Flag[b0STATUS, 2];
end;

procedure TProcessor.SetBreakpoint(ALine: Cardinal; AValue: Boolean);
begin
  FBreakpoints[ALine] := AValue;
end;

procedure TProcessor.SetCarryFlag(AValue: Boolean);
begin
  Flag[b0STATUS, 0] := AValue;
end;

procedure TProcessor.SetDigitCarryFlag(AValue: Boolean);
begin
  Flag[b0STATUS, 1] := AValue;
end;

procedure TProcessor.SetFileMap(P: TRAMPointer; AValue: Byte);
begin
  FRAM[NormalizeRAMPointer(P)] := AValue;
end;

procedure TProcessor.SetFlag(P: TRAMPointer; ABit: TBitIndex; AValue: Boolean);
begin
  if AValue then // set
    FRAM[P] := FRAM[P] or (1 shl ABit)
  else           // clear
    FRAM[P] := FRAM[P] and not (1 shl ABit);
end;

procedure TProcessor.SetFlag(P: TRegisterBank0; ABit: TBitIndex; AValue: Boolean);
begin
  Flag[Ord(P), ABit] := AValue;
end;

procedure TProcessor.SetFlag(P: TRegisterBank1; ABit: TBitIndex; AValue: Boolean);
begin
  Flag[Ord(P), ABit] := AValue;
end;

procedure TProcessor.SetZeroFlag(AValue: Boolean);
begin
  Flag[b0STATUS, 2] := AValue;
end;

function TProcessor.GetDataMem(P: TROMPointer): Byte;
begin
  Result := FROM[P];
end;

function TProcessor.GetCurrentInstruction: TLineInstruction;
begin
  Result := Code[FProgramCounter];
end;

function TProcessor.GetBreakpoint(ALine: Cardinal): Boolean;
begin
  Result := FBreakpoints[ALine];
end;

function TProcessor.GetCalcFlags: TCalcFlags;
begin
  Result := [];
  if ZeroFlag then
    Include(Result, cfZero);
  if DigitCarryFlag then
    Include(Result, cfDigitCarry);
  if CarryFlag then
    Include(Result, cfCarry);
end;

function TProcessor.GetMemory(AType: TMemoryType; APos: Cardinal): Byte;
begin
  try
  case AType of
    mtRAM:
      Result := RAM[APos];
    mtProgram:
      Result := ProgramMem[APos];
    mtROM:
      Result := ROM[APos];
    mtProgramCounterStack:
      Result := PCStackMem[APos];
  end;
  except
    on ERangeError do
      Result := 0;
  end;
end;

function TProcessor.GetPCStack(P: TProgramCounterStackPos): TProgramCounter;
begin
  Result := FProgramCounterStack[P];
end;

function TProcessor.GetCode(P: TProgramCounter): TLineInstruction;
begin
  Result := FProgramMem[P mod ProgramMemorySize];
end;

function TProcessor.GetPCStackMem(P: TProgramCounterStackPointer): Byte;
begin
  if P mod 2 = 0 then
    Result := (FProgramCounterStack[P div 2] shr 8) and $FF
  else
    Result := FProgramCounterStack[P div 2] and $FF;
end;

function TProcessor.GetProgramMem(P: TProgramMemPointer): Byte;
begin
  if P mod 2 = 0 then
    Result := (FProgramMem[P div 2].Instruction shr 8) and $FF
  else
    Result := FProgramMem[P div 2].Instruction and $FF;
end;

function TProcessor.GetRAM(P: TRAMPointer): Byte;
begin
  Result := FRAM[NormalizeRAMPointer(P)];
end;

function TProcessor.GetRegisterBank0(S: TRegisterBank0): Byte;
begin
  Result := FRAM[TRAMPointer(S)];
end;

function TProcessor.GetRegisterBank1(S: TRegisterBank1): Byte;
begin
  Result := FRAM[NormalizeRAMPointer(TRAMPointer(S))];
end;

function TProcessor.GetTimeBehind: Single;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result := (T - FStartTime) / FFrequency * FSpeedFactor - FCyclesFromStart * TProcessor.OperationTime;
end;

function TProcessor.GetReadAsZero(AType: TMemoryType; APos: Cardinal): Boolean;
begin
  case AType of
    mtRAM:
    begin
      if APos >= FileMapOffset then
        APos := APos - FileMapOffset;
      Result := (APos > RAMEnd) or (APos = Ord(b0Unused));
    end;
    mtProgram:
    begin
      Result := False;
    end;
    mtROM:
    begin
      Result := False;
    end;
    mtProgramCounterStack:
    begin
      Result := False;
    end;
  end;
end;

function TProcessor.NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;
begin
  try
    if (ARAMPointer >= FileMapOffset) and (
       (ARAMPointer >= RAMMappedStart) and (ARAMPointer <= RAMMappedEnd) or
       (TRegisterBank1(ARAMPointer) in (RegisterBank1Mapped))) then
      Exit(ARamPointer - FileMapOffset);
  except
  end;
  Result := ARAMPointer;
end;

procedure TProcessor.KeepProgramCounter;
begin
  FKeepProgramCounter := True;
end;

procedure TProcessor.SetSpeedFactor(AValue: Single);
begin
  if FSpeedFactor = AValue then
    Exit;
  FSpeedFactor := AValue;
  ResetSyncTime;
end;

class function TProcessor.ExtractByteLiteral(AInstruction: TInstruction): Byte;
begin
  Result := AInstruction and High(Byte);
end;

class function TProcessor.ExtractFileAdress(AInstruction: TInstruction): TFileAdress;
begin
  Result := AInstruction and High(TFileAdress);
end;

class function TProcessor.ExtractProgramCounter(AInstruction: TInstruction): TProgramCounter;
begin
  Result := AInstruction and $7FF;
end;

class function TProcessor.ExtractDestIsFile(AInstruction: TInstruction): Boolean;
begin
  Result := ((AInstruction shr 7) and 1) = 1;
end;

function TProcessor.DoAdd(A, B: Byte): Byte;
var
  R: Word;
begin
  R := A + B;
  CarryFlag := R > $FF;
  DigitCarryFlag := (A and $F) + (B and $F) > $F;
  Result := R and $FF;
  ZeroFlag := Result = 0;
end;

function TProcessor.DoSub(A, B: Byte): Byte;
begin
  Result := DoAdd(A, (not B + 1) and $FF);
end;

function TProcessor.NextProgramCounter(ACount: Integer): TProgramCounter;
begin
  Result := (FProgramCounter + ACount) and High(FProgramCounter);
end;

procedure TProcessor.PushStack(AProgramCounter: TProgramCounter);
begin
  FProgramCounterStack[FProgramCounterStackPos] := AProgramCounter;
  FProgramCounterStackPos := (FProgramCounterStackPos + 1) and High(TProgramCounterStackPos);
end;

function TProcessor.PopStack: TProgramCounter;
begin
  FProgramCounterStackPos := (FProgramCounterStackPos - 1) and High(TProgramCounterStackPos);
  Result := FProgramCounterStack[FProgramCounterStackPos];
end;

constructor TProcessor.Create;
var
  T: TInstructionType;
  M: TMethod;
begin
  FBreakpoints := TCardinalSet.Create;
  M.Data := Self;
  for T := Low(T) to High(T) do
  begin
    M.Code := MethodAddress('Instruction' + InstructionInfo[T].Name);
    if M.Code <> nil then
      FMethods[T] := TInstructionMethod(M);
  end;
  QueryPerformanceFrequency(FFrequency);
  FSpeedFactor := 1;
end;

destructor TProcessor.Destroy;
begin
  FBreakpoints.Free;
  inherited;
end;

procedure TProcessor.LoadProgram(AFileData: TStrings);
var
  Counter: Integer;
  CodePos: Cardinal;
  Instruction: DWORD;
begin
  FillByte(FProgramMem, SizeOf(FProgramMem), 0);
  FBreakpoints.Clear;
  Initialize;
  for Counter := 0 to AFileData.Count - 1 do
  begin
    if not AFileData[Counter].StartsWith(' ') then
    begin
      CodePos := StrToDWord('$' + AFileData[Counter].Substring(0, 4));
      Instruction := StrToDWord('$' + AFileData[Counter].Substring(5, 4));
      try
        FProgramMem[CodePos].Instruction := TInstruction(Instruction);
      except
        on ERangeError do
          raise Exception.CreateFmt('Impossible/Unknown Instruction 0x%.2h in line %d', [Instruction, Counter + 1]);
      end;
      FProgramMem[CodePos].Line := Counter + 1;
    end;
  end;
end;

procedure TProcessor.Initialize;
begin
  FProgramCounter := 0;
  FProgramCounterStackPos := 0;
  FillByte(FProgramCounterStack, SizeOf(FProgramCounterStack), 0);
  FillByte(FRAM, SizeOf(FRAM), 0);
  FWRegister := 0;
  FCycles := 0;
end;

procedure TProcessor.ResetROM;
begin
  FillByte(FROM, SizeOf(FROM), 0);
end;

procedure TProcessor.Start;
begin
  ResetSyncTime;
  FRunning := True;
end;

procedure TProcessor.Stop;
begin
  FHelpBreakpointEnabled := False;
  FRunning := False;
end;

procedure TProcessor.ProcessInstruction(AInstruction: TInstruction; ALine: Integer);
var
  T: TInstructionType;
begin
  T := FInstructionArray[AInstruction];

  try
    if Assigned(FMethods[T]) then
      FMethods[T](AInstruction)
    else if ALine = -1 then
      raise ENotImplemented.CreateFmt('Instruction "%s" not implemented',
                                      [InstructionInfo[T].Name])
    else
      raise ENotImplemented.CreateFmt('Instruction "%s" (line %u) not implemented',
                                      [InstructionInfo[T].Name, ALine]);
  finally
    if FKeepProgramCounter then
    begin
      // program counter changed, this needs 2 cycles
      FKeepProgramCounter := False;
      AdvanceCycles(2);
    end
    else
    begin
      AdvanceProgramCounter;
      AdvanceCycles;
    end;
  end;
end;

procedure TProcessor.ProcessInstruction(AInstruction: TLineInstruction);
begin
  ProcessInstruction(AInstruction.Instruction, AInstruction.Line);
end;

procedure TProcessor.ResetSyncTime;
begin
  QueryPerformanceCounter(FStartTime);
  FCyclesFromStart := 0;
end;

procedure TProcessor.StepIn;
begin
  ProcessInstruction(CurrentInstruction);
end;

function TProcessor.StepOver: TStepInfo;
begin
  if FInstructionArray[CurrentInstruction.Instruction] = itCALL then
  begin
    FHelpBreakpointDepth := FProgramCounterStackPos;
    FHelpBreakpointEnabled := True;
    Start;
    Result := siMultiple;
  end
  else
  begin
    StepIn;
    Result := siSingle;
  end;
end;

function TProcessor.StepOut: TStepInfo;
begin
  if FProgramCounterStackPos > 0 then
  begin
    FHelpBreakpointDepth := FProgramCounterStackPos - 1;
    FHelpBreakpointEnabled := True;
    Start;
    Result := siMultiple;
  end
  else
    Result := StepOver;
end;

function TProcessor.CatchUp: Boolean;
begin
  while TimeBehind > 0 do
  begin
    StepIn;
    if Breakpoint[CurrentInstruction.Line] or
       FHelpBreakpointEnabled and (FProgramCounterStackPos = FHelpBreakpointDepth) then
    begin
      Stop;
      Exit(True);
    end;
  end;
  Result := False;
end;

{$REGION METHODS}

procedure TProcessor.InstructionADDWF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
    FileMap[A] := DoAdd(FileMap[A], FWRegister)
  else
    FWRegister := DoAdd(FileMap[A], FWRegister);
end;

procedure TProcessor.InstructionANDWF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := FileMap[A] and FWRegister;
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := FileMap[A] and FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionCLRF(AInstruction: TInstruction);
begin
  FileMap[ExtractFileAdress(AInstruction)] := 0;
  ZeroFlag := True;
end;

procedure TProcessor.InstructionCLRW(AInstruction: TInstruction);
begin
  FWRegister := 0;
  ZeroFlag := True;
end;

procedure TProcessor.InstructionCOMF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := not FileMap[A];
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := not FileMap[A];
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionDECF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := (FileMap[A] - 1) and $FF;
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := (FileMap[A] - 1) and $FF;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionDECFSZ(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := (FileMap[A] - 1) and $FF;
    if FileMap[A] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end
  else
  begin
    FWRegister := (FileMap[A] - 1) and $FF;
    if FileMap[A] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end;
end;

procedure TProcessor.InstructionINCF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := (FileMap[A] + 1) and $FF;
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := (FileMap[A] + 1) and $FF;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionINCFSZ(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := (FileMap[A] + 1) and $FF;
    if FileMap[A] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end
  else
  begin
    FWRegister := (FileMap[A] + 1) and $FF;
    if FileMap[A] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end;
end;

procedure TProcessor.InstructionIORWF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := FileMap[A] or FWRegister;
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := FileMap[A] or FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionMOVF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := FileMap[A];
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionMOVWF(AInstruction: TInstruction);
begin
  FileMap[ExtractFileAdress(AInstruction)] := FWRegister;
end;

procedure TProcessor.InstructionNOP(AInstruction: TInstruction);
begin
  // no operation
end;

procedure TProcessor.InstructionRLF(AInstruction: TInstruction);
var
  A: TFileAdress;
  C: Boolean;
begin
  A := ExtractFileAdress(AInstruction);
  C := CarryFlag;
  if ExtractDestIsFile(AInstruction) then
  begin
    CarryFlag := Flag[A, 7];
    FileMap[A] := (FileMap[A] shl 1) and $FF;
    Flag[A, 0] := C;
  end
  else
  begin
    CarryFlag := Flag[A, 7];
    FWRegister := (FileMap[A] shl 1) and $FF;
    if C then
      FWRegister := FWRegister or $01;
  end;
end;

procedure TProcessor.InstructionRRF(AInstruction: TInstruction);
var
  A: TFileAdress;
  C: Boolean;
begin
  A := ExtractFileAdress(AInstruction);
  C := CarryFlag;
  if ExtractDestIsFile(AInstruction) then
  begin
    CarryFlag := Flag[A, 0];
    FileMap[A] := FileMap[A] shr 1;
    Flag[A, 7] := C;
  end
  else
  begin
    CarryFlag := Flag[A, 0];
    FWRegister := FileMap[A] shr 1;
    if C then
      FWRegister := FWRegister or $80;
  end;
end;

procedure TProcessor.InstructionSUBWF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := DoSub(FileMap[A], FWRegister);
  end
  else
  begin
    FWRegister := DoSub(FileMap[A], FWRegister);
  end;
end;

procedure TProcessor.InstructionSWAPF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
    FileMap[A] := (FileMap[A] shr 4) or ((FileMap[A] shl 4) and $FF)
  else
    FWRegister := (FileMap[A] shr 4) or ((FileMap[A] shl 4) and $FF);
end;

procedure TProcessor.InstructionXORWF(AInstruction: TInstruction);
var
  A: TFileAdress;
begin
  A := ExtractFileAdress(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[A] := FileMap[A] xor FWRegister;
    ZeroFlag := FileMap[A] = 0;
  end
  else
  begin
    FWRegister := FileMap[A] xor FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionADDLW(AInstruction: TInstruction);
begin
  FWRegister := DoAdd(FWRegister, ExtractByteLiteral(AInstruction));
end;

procedure TProcessor.InstructionANDLW(AInstruction: TInstruction);
begin
  FWRegister := FWRegister and ExtractByteLiteral(AInstruction);
  ZeroFlag := FWRegister = 0;
end;

procedure TProcessor.InstructionCALL(AInstruction: TInstruction);
begin
  AdvanceProgramCounter;
  PushStack(FProgramCounter);
  FProgramCounter := ExtractProgramCounter(AInstruction);
  KeepProgramCounter;
end;

procedure TProcessor.InstructionGOTO(AInstruction: TInstruction);
begin
  FProgramCounter := ExtractProgramCounter(AInstruction);
  KeepProgramCounter;
end;

procedure TProcessor.InstructionIORLW(AInstruction: TInstruction);
begin
  FWRegister := FWRegister or ExtractByteLiteral(AInstruction);
  ZeroFlag := FWRegister = 0;
end;

procedure TProcessor.InstructionMOVLW(AInstruction: TInstruction);
begin
  FWRegister := ExtractByteLiteral(AInstruction);
  ZeroFlag := FWRegister = 0;
end;

procedure TProcessor.InstructionRETLW(AInstruction: TInstruction);
begin
  FWRegister := ExtractByteLiteral(AInstruction);
  FProgramCounter := PopStack;
  KeepProgramCounter;
end;

procedure TProcessor.InstructionRETURN(AInstruction: TInstruction);
begin
  FProgramCounter := PopStack;
  KeepProgramCounter;
end;

procedure TProcessor.InstructionSUBLW(AInstruction: TInstruction);
begin
  FWRegister := DoSub(ExtractByteLiteral(AInstruction), FWRegister);
end;

procedure TProcessor.InstructionXORLW(AInstruction: TInstruction);
begin
  FWRegister := FWRegister xor ExtractByteLiteral(AInstruction);
  ZeroFlag := FWRegister = 0;
end;

{$ENDREGION}

end.

