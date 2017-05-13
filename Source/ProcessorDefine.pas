unit ProcessorDefine;

{$M+}

interface

uses
  Classes, windows, SysUtils, Dialogs, Lists, PinDefine;

type

  { TProcessor }

  TProcessor = class
  public const
    {$REGION Simple Numerical Constants for Size and such}
    // Stack
    StackSize = 8;
    // Program Memory
    ProgramMemorySize = $400;
    //Intersrupt entry ardress
    InterruptEntryAdress = $004;
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
    // Ports
    PortACount = 5;
    PortBCount = 8;
    {$ENDREGION}

  public type
    {$REGION TRegisterBank}
    TRegisterBank0 = (
      b0INDF,
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
      b1OPTION,
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

    {$REGION Custom Ranges}
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

    TInstructionParameter = (
      ipDT,  // Destination
      ipFA,  // FileAdress
      ipBI,  // BitIndex
      ipBL,  // ByteLiteral
      ipPC   // ProgramCounter
    );

    TInstructionParameters = set of TInstructionParameter;

    TCalcFlags = set of TCalcFlag;

    TInstructionInfo = record
      Name: String;
      SignificantBits: Byte;
      Instruction: TInstruction;
      Params: TInstructionParameters;
    end;

    TInstructionMethod = procedure (AInstruction: TInstruction) of object;

    TStepInfo = (
      siSingle,
      siMultiple
    );

    TPreScalerAssignment = (
      paTimer0,
      paWatchdog
    );

    TAsyncProcessorChangeEvent = procedure (AProcessor: TProcessor) of object;

  public const
    {$REGION RegisterBank Mapped Parts}
    RegisterBank0Mapped: set of TRegisterBank0 = [
      b0INDF,
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

    {$REGION Special Functions}
      RegisterBank0Name: array [TRegisterBank0] of String = (
        'INDF',
        'TMR0',
        'PCL',
        'STATUS',
        'FSR',
        'PORTA',
        'PORTB',
        '-',
        'EEDATA',
        'EEADR',
        'PCLATH',
        'INTCOUN'
      );
      RegisterBank1Name: array [TRegisterBank1] of String = (
        'INDF',
        'OPTION',
        'PCL',
        'STATUS',
        'FSR',
        'TRISA',
        'TRISB',
        '-',
        'EECON1',
        'EECON2',
        'PCLATH',
        'INTCON'
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
      (Name: 'addwf';  SignificantBits: 6;  Instruction: $0700; Params: [ipDT, ipFA]),
      (Name: 'andwf';  SignificantBits: 6;  Instruction: $0500; Params: [ipDT, ipFA]),
      (Name: 'clrf';   SignificantBits: 7;  Instruction: $0180; Params: [ipFA]),
      (Name: 'clrw';   SignificantBits: 7;  Instruction: $0100; Params: []),
      (Name: 'comf';   SignificantBits: 6;  Instruction: $0900; Params: [ipDT, ipFA]),
      (Name: 'decf';   SignificantBits: 6;  Instruction: $0300; Params: [ipDT, ipFA]),
      (Name: 'decfsz'; SignificantBits: 6;  Instruction: $0B00; Params: [ipDT, ipFA]),
      (Name: 'incf';   SignificantBits: 6;  Instruction: $0A00; Params: [ipDT, ipFA]),
      (Name: 'incfsz'; SignificantBits: 6;  Instruction: $0F00; Params: [ipDT, ipFA]),
      (Name: 'iorwf';  SignificantBits: 6;  Instruction: $0400; Params: [ipDT, ipFA]),
      (Name: 'movf';   SignificantBits: 6;  Instruction: $0800; Params: [ipDT, ipFA]),
      (Name: 'movwf';  SignificantBits: 7;  Instruction: $0080; Params: [ipFA]),
      (Name: 'nop';    SignificantBits: 14; Instruction: $0000; Params: []),
      (Name: 'nop';    SignificantBits: 14; Instruction: $0020; Params: []),
      (Name: 'nop';    SignificantBits: 14; Instruction: $0040; Params: []),
      (Name: 'nop';    SignificantBits: 14; Instruction: $0060; Params: []),
      (Name: 'rlf';    SignificantBits: 6;  Instruction: $0D00; Params: [ipDT, ipFA]),
      (Name: 'rrf';    SignificantBits: 6;  Instruction: $0C00; Params: [ipDT, ipFA]),
      (Name: 'subwf';  SignificantBits: 6;  Instruction: $0200; Params: [ipDT, ipFA]),
      (Name: 'swapf';  SignificantBits: 6;  Instruction: $0E00; Params: [ipDT, ipFA]),
      (Name: 'xorwf';  SignificantBits: 6;  Instruction: $0600; Params: [ipDT, ipFA]),
      (Name: 'bcf';    SignificantBits: 4;  Instruction: $1000; Params: [ipBI, ipFA]),
      (Name: 'bsf';    SignificantBits: 4;  Instruction: $1400; Params: [ipBI, ipFA]),
      (Name: 'btfsc';  SignificantBits: 4;  Instruction: $1800; Params: [ipBI, ipFA]),
      (Name: 'btfss';  SignificantBits: 4;  Instruction: $1C00; Params: [ipBI, ipFA]),
      (Name: 'addlw';  SignificantBits: 5;  Instruction: $3E00; Params: [ipBL]),
      (Name: 'andlw';  SignificantBits: 6;  Instruction: $3900; Params: [ipBL]),
      (Name: 'call';   SignificantBits: 3;  Instruction: $2000; Params: [ipPC]),
      (Name: 'clrwdt'; SignificantBits: 14; Instruction: $0064; Params: [ipBL]),
      (Name: 'goto';   SignificantBits: 3;  Instruction: $2800; Params: [ipPC]),
      (Name: 'iorlw';  SignificantBits: 6;  Instruction: $3800; Params: [ipBL]),
      (Name: 'movlw';  SignificantBits: 4;  Instruction: $3000; Params: [ipBL]),
      (Name: 'retfie'; SignificantBits: 14; Instruction: $0009; Params: [ipBL]),
      (Name: 'retlw';  SignificantBits: 4;  Instruction: $3400; Params: [ipBL]),
      (Name: 'return'; SignificantBits: 14; Instruction: $0008; Params: [ipBL]),
      (Name: 'sleep';  SignificantBits: 14; Instruction: $0063; Params: [ipBL]),
      (Name: 'sublw';  SignificantBits: 5;  Instruction: $3C00; Params: [ipBL]),
      (Name: 'xorlw';  SignificantBits: 6;  Instruction: $3A00; Params: [ipBL])
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
    FPreScaler: Byte;
    FInhibitTimer0: Cardinal;

    // EEPROM
    FROM: array [TROMPointer] of Byte;

    // Pins
    FPortAPins: TPinArray;
    FPortBPins: TPinArray;
    FMasterClearPin: TPinArray;
    FSkipPortWrite: Boolean;

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
    function GetCurrentProgramPos: TProgramMemPos;
    function GetDataMem(P: TROMPointer): Byte;
    function GetMemory(AType: TMemoryType; APos: Cardinal): Byte;
    function GetPCStack(P: TProgramCounterStackPos): TProgramCounter;
    function GetCode(P: TProgramCounter): TLineInstruction;
    function GetPCStackMem(P: TProgramCounterStackPointer): Byte;
    function GetProgramMem(P: TProgramMemPointer): Byte;
    function GetTimeBehind: Single;
    function GetReadAsZero(AType: TMemoryType; APos: Cardinal): Boolean;
    function GetCarryFlag: Boolean;
    function GetDigitCarryFlag: Boolean;
    function GetFileMap(P: TRAMPointer): Byte; overload;
    function GetFileMap(P: TRegisterBank0): Byte; overload;
    function GetFileMap(P: TRegisterBank1): Byte; overload;
    function GetFlag(P: TRAMPointer; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank0; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank1; ABit: TBitIndex): Boolean; overload;
    function GetZeroFlag: Boolean;
    function GetGlobalInterruptEnable: Boolean;
    procedure SetGlobalInterruptEnable(AValue: Boolean);
    function GetEEWriteDoneInterruptEnable: Boolean;
    function GetExternalInterruptEnable: Boolean;
    function GetExternalInterruptFlag: Boolean;
    function GetPortBInterruptChangeEnable: Boolean;
    function GetPortBInterruptChangeFlag: Boolean;
    function GetTimer0InterruptEnable: Boolean;
    function GetTimer0InterruptFlag: Boolean;
    procedure SetEEWriteDoneInterruptEnable(AValue: Boolean);
    procedure SetExternalInterruptEnable(AValue: Boolean);
    procedure SetExternalInterruptFlag(AValue: Boolean);
    procedure SetPortBInterruptChangeEnable(AValue: Boolean);
    procedure SetPortBInterruptChangeFlag(AValue: Boolean);
    procedure SetTimer0InterruptEnable(AValue: Boolean);
    procedure SetTimer0InterruptFlag(AValue: Boolean);
    procedure SetBreakpoint(ALine: Cardinal; AValue: Boolean);
    procedure SetCarryFlag(AValue: Boolean);
    procedure SetDigitCarryFlag(AValue: Boolean);
    procedure SetFileMap(P: TRAMPointer; AValue: Byte); overload;
    procedure SetFileMap(P: TRegisterBank0; AValue: Byte); overload;
    procedure SetFileMap(P: TRegisterBank1; AValue: Byte); overload;
    procedure SetFlag(P: TRAMPointer; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank0; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank1; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetZeroFlag(AValue: Boolean);
    function GetBank1Selected: Boolean;
    procedure SetBank1Selected(AValue: Boolean);
    function GetExtClockSrc: Boolean;
    procedure SetExtClockSrc(AValue: Boolean);
    function GetPreScalerAssignment: TPreScalerAssignment;
    function GetPreScalerMax: Byte;
    procedure SetPreScalerAssignment(AValue: TPreScalerAssignment);
    function GetEEWriteDoneInterruptFlag: Boolean;
    procedure SetEEWriteDoneInterruptFlag(AValue: Boolean);

    procedure SetSpeedFactor(AValue: Single);

    property FileMap[P: TRAMPointer]: Byte read GetFileMap write SetFileMap;
    property Flag[P: TRAMPointer; ABit: TBitIndex]: Boolean read GetFlag write SetFlag;

    property CarryFlag: Boolean read GetCarryFlag write SetCarryFlag;
    property DigitCarryFlag: Boolean read GetDigitCarryFlag write SetDigitCarryFlag;
    property ZeroFlag: Boolean read GetZeroFlag write SetZeroFlag;
    property ExtClockSrc: Boolean read GetExtClockSrc write SetExtClockSrc;
    property Bank1Selected: Boolean read GetBank1Selected write SetBank1Selected;
    property PreScalerAssignment: TPreScalerAssignment read GetPreScalerAssignment write SetPreScalerAssignment;
    property GlobalInterruptEnable: Boolean read GetGlobalInterruptEnable write SetGlobalInterruptEnable;
    property EEWriteDoneInterruptEnable: Boolean read GetEEWriteDoneInterruptEnable write SetEEWriteDoneInterruptEnable;
    property Timer0InterruptEnable: Boolean read GetTimer0InterruptEnable write SetTimer0InterruptEnable;
    property ExternalInterruptEnable: Boolean read GetExternalInterruptEnable write SetExternalInterruptEnable;
    property PortBInterruptChangeEnable: Boolean read GetPortBInterruptChangeEnable write SetPortBInterruptChangeEnable;
    property Timer0InterruptFlag: Boolean read GetTimer0InterruptFlag write SetTimer0InterruptFlag;
    property ExternalInterruptFlag: Boolean read GetExternalInterruptFlag write SetExternalInterruptFlag;
    property PortBInterruptChangeFlag: Boolean read GetPortBInterruptChangeFlag write SetPortBInterruptChangeFlag;
    property EEWriteDoneInterruptFlag: Boolean read GetEEWriteDoneInterruptFlag write SetEEWriteDoneInterruptFlag;

    property PreScalerMax: Byte read GetPreScalerMax; // returns 0 instead of 256, as the byte perfectly overflows

    // help-functions
    procedure KeepProgramCounter;

    class function ExtractFileAdress(AInstruction: TInstruction): TFileAdress; static;
    class function ExtractByteLiteral(AInstruction: TInstruction): Byte; static;
    class function ExtractProgramCounter(AInstruction: TInstruction): TProgramCounter; static;
    class function ExtractDestIsFile(AInstruction: TInstruction): Boolean; static;
    class function ExtractBitIndex(AInstruction: TInstruction): TBitIndex; static;

    function ExtractRAMPointer(AInstruction: TInstruction): TRAMPointer;

    function AdressToRAMPointer(AAdress: TFileAdress): TRAMPointer;

    function DoAdd(A, B: Byte): Byte;
    function DoSub(A, B: Byte): Byte;

    function NextProgramCounter(ACount: Integer = 1): TProgramCounter;
    procedure AdvanceProgramCounter(ACount: Integer = 1);
    procedure AdvanceCycles(ACount: Integer = 1);

    procedure PushStack(AProgramCounter: TProgramCounter);
    function PopStack: TProgramCounter;

    procedure ProcessTimer(ACount: Cardinal = 1);
    procedure CheckTimer0Overflow;

    procedure OnPortAChanged(APin: TPin);
    procedure OnPortBChanged(APin: TPin);
    procedure OnMasterClearChanged(APin: TPin);

  public

    // whenever something changes, not while the processor is processing a command (like a port change)
    OnAsyncMemoryChange: TDelegate<TAsyncProcessorChangeEvent>;

    constructor Create;
    destructor Destroy; override;

    procedure LoadProgram(AFileData: TStrings);
    procedure ResetROM;
    procedure ResetPowerON;

    procedure Start;
    procedure Stop;

    procedure ProcessInstruction(AInstruction: TInstruction; ALine: Integer = -1); overload;
    procedure ProcessInstruction(AInstruction: TLineInstruction); overload;

    procedure ResetSyncTime;

    function NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;

    procedure StepIn;
    function StepOver: TStepInfo;
    function StepOut: TStepInfo;
    procedure CatchUp;
    property TimeBehind: Single read GetTimeBehind;

    property ProgramMem[P: TProgramMemPointer]: Byte read GetProgramMem;
    property Code[P: TProgramCounter]: TLineInstruction read GetCode;
    property CurrentProgramPos: TProgramMemPos read GetCurrentProgramPos;
    property CurrentInstruction: TLineInstruction read GetCurrentInstruction;
    property WRegister: Byte read FWRegister;
    property RAMBit[P: TRAMPointer; ABit: TBitIndex]: Boolean read GetFlag;
    property PreScaler: Byte read FPreScaler;
    property RAM[P: TRAMPointer]: Byte read GetFileMap;
    property ROM[P: TROMPointer]: Byte read GetDataMem;
    property PCStackPos: TProgramCounterStackPos read FProgramCounterStackPos;
    property PCStack[P: TProgramCounterStackPos]: TProgramCounter read GetPCStack;
    property PCStackMem[P: TProgramCounterStackPointer]: Byte read GetPCStackMem;
    property CalcFlags: TCalcFlags read GetCalcFlags;

    property PortAPins: TPinArray read FPortAPins;
    property PortBPins: TPinArray read FPortBPins;
    property MasterClearPin: TPinArray read FMasterClearPin;

    property ReadAsZero[AType: TMemoryType; APos: Cardinal]: Boolean read GetReadAsZero;
    property Memory[AType: TMemoryType; APos: Cardinal]: Byte read GetMemory;

    property Running: Boolean read FRunning;
    property Cycles: Int64 read FCycles;
    property SpeedFactor: Single read FSpeedFactor write SetSpeedFactor;

    property Breakpoint[ALine: Cardinal]: Boolean read GetBreakpoint write SetBreakpoint;

    class function FormatInstruction(AInstruction: TInstruction): String;

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
    procedure InstructionBCF(AInstruction: TInstruction);
    procedure InstructionBSF(AInstruction: TInstruction);
    procedure InstructionBTFSC(AInstruction: TInstruction);
    procedure InstructionBTFSS(AInstruction: TInstruction);
    {$ENDREGION}

    {$REGION --- Literal and Control Operations --- }
    procedure InstructionADDLW(AInstruction: TInstruction);
    procedure InstructionANDLW(AInstruction: TInstruction);
    procedure InstructionCALL(AInstruction: TInstruction);
    // TODO: procedure InstructionCLRWDT(AInstruction: TInstruction);
    procedure InstructionGOTO(AInstruction: TInstruction);
    procedure InstructionIORLW(AInstruction: TInstruction);
    procedure InstructionMOVLW(AInstruction: TInstruction);
    procedure InstructionRETFIE({%H-}AInstruction: TInstruction);
    procedure InstructionRETLW(AInstruction: TInstruction);
    procedure InstructionRETURN({%H-}AInstruction: TInstruction);
    // TODO: procedure InstructionSLEEP(AInstruction: TInstruction);
    procedure InstructionSUBLW(AInstruction: TInstruction);
    procedure InstructionXORLW(AInstruction: TInstruction);
    {$ENDREGION}
  end;

  { IHasProcessor }

  IHasProcessor = interface
  ['{F7D8497B-3262-4B22-BC25-4D8C1E380772}']
    function GetProcessor: TProcessor;
    property Processor: TProcessor read GetProcessor;
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

function TProcessor.GetEEWriteDoneInterruptFlag: Boolean;
begin
  Result := Flag[b1EECON1, 4];
end;

procedure TProcessor.SetEEWriteDoneInterruptFlag(AValue: Boolean);
begin
  Flag[b1EECON1, 4] := AValue;
end;

function TProcessor.GetEEWriteDoneInterruptEnable: Boolean;
begin
  Result := Flag[b0INTCON, 6];
end;

function TProcessor.GetExternalInterruptEnable: Boolean;
begin
  Result := Flag[b0INTCON, 4];
end;

function TProcessor.GetExternalInterruptFlag: Boolean;
begin
  Result := Flag[b0INTCON, 1];
end;

function TProcessor.GetPortBInterruptChangeEnable: Boolean;
begin
  Result := Flag[b0INTCON, 3];
end;

function TProcessor.GetPortBInterruptChangeFlag: Boolean;
begin
  Result := Flag[b0INTCON, 0];
end;

function TProcessor.GetTimer0InterruptEnable: Boolean;
begin
  Result := Flag[b0INTCON, 5];
end;

function TProcessor.GetTimer0InterruptFlag: Boolean;
begin
  Result := Flag[b0INTCON, 2];
end;

procedure TProcessor.SetEEWriteDoneInterruptEnable(AValue: Boolean);
begin
  Flag[b0INTCON, 6] := AValue;
end;

procedure TProcessor.SetExternalInterruptEnable(AValue: Boolean);
begin
  Flag[b0INTCON, 4] := AValue;
end;

procedure TProcessor.SetExternalInterruptFlag(AValue: Boolean);
begin
  Flag[b0INTCON, 1] := AValue;
end;

procedure TProcessor.SetPortBInterruptChangeEnable(AValue: Boolean);
begin
  Flag[b0INTCON, 3] := AValue;
end;

procedure TProcessor.SetPortBInterruptChangeFlag(AValue: Boolean);
begin
  Flag[b0INTCON, 0] := AValue;
end;

procedure TProcessor.SetTimer0InterruptEnable(AValue: Boolean);
begin
  Flag[b0INTCON, 5] := AValue;
end;

procedure TProcessor.SetTimer0InterruptFlag(AValue: Boolean);
begin
  Flag[b0INTCON, 2] := AValue;
end;

function TProcessor.GetGlobalInterruptEnable: Boolean;
begin
  Result := Flag[b0INTCON, 7];
end;

procedure TProcessor.SetGlobalInterruptEnable(AValue: Boolean);
begin
  Flag[b0INTCON, 7] := AValue;
end;

function TProcessor.GetPreScalerAssignment: TPreScalerAssignment;
begin
  Result := TPreScalerAssignment(Flag[b1OPTION, 3]);
end;

function TProcessor.GetPreScalerMax: Byte;
begin
  Result := 1 shl (FileMap[b1OPTION] and $07);
  if PreScalerAssignment = paTimer0 then
    Result := (Result shl 1) and $FF;
end;

procedure TProcessor.SetPreScalerAssignment(AValue: TPreScalerAssignment);
begin
  Flag[b1OPTION, 3] := Boolean(AValue);
end;

function TProcessor.GetExtClockSrc: Boolean;
begin
  Result := Flag[b1OPTION, 5];
end;

procedure TProcessor.SetExtClockSrc(AValue: Boolean);
begin
  Flag[b1OPTION, 5] := AValue;
end;

procedure TProcessor.ResetPowerON;
begin
  FillByte(FProgramCounterStack, SizeOf(FProgramCounterStack), 0);
  FillByte(FRAM, SizeOf(FRAM), 0);
  FProgramCounter := 0;
  FProgramCounterStackPos := 0;
  FCycles := 0;
  FWRegister := 0;
  FileMap[b0STATUS] := $18;
  FileMap[b1OPTION] := $FF;
  FileMap[b1TRISA] := $1F;
  FileMap[b1TRISB] := $FF;
end;

function TProcessor.GetBank1Selected: Boolean;
begin
  Result := Flag[b0STATUS, 5];
end;

procedure TProcessor.SetBank1Selected(AValue: Boolean);
begin
  Flag[b0STATUS, 5] := AValue;
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
  P := NormalizeRAMPointer(P);
  if P = Ord(b0INDF) then
  begin
    // indirect adressing
    Result := FRAM[NormalizeRAMPointer(FileMap[b0FSR])];
  end
  else
    Result := FRAM[P];
end;

function TProcessor.GetFileMap(P: TRegisterBank0): Byte;
begin
  Result := FileMap[Ord(P)];
end;

function TProcessor.GetFileMap(P: TRegisterBank1): Byte;
begin
  Result := FileMap[Ord(P)];
end;

function TProcessor.GetFlag(P: TRAMPointer; ABit: TBitIndex): Boolean;
begin
  Result := ((FileMap[P] shr ABit) and 1) = 1;
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
var
  B: TBitIndex;
  Diff: Byte;
begin
  P := NormalizeRAMPointer(P);

  if P = Ord(b1TRISA) then
  begin
    Diff := FileMap[P] xor AValue;
    for B := Low(B) to High(B) do
      if Diff shr B and $01 = 1 then
        FPortAPins[B].Direction := TPin.TDirection(AValue shr B and $01);
  end
  else if P = Ord(b1TRISB) then
  begin
    Diff := FileMap[P] xor AValue;
    for B := Low(B) to High(B) do
      if Diff shr B and $01 = 1 then
        FPortBPins[B].Direction := TPin.TDirection(AValue shr B and $01);
  end;

  if not FSkipPortWrite then
  begin
    if P = Ord(b0PORTA) then
    begin
      Diff := FileMap[P] xor AValue;
      for B := Low(B) to High(B) do
        if Diff shr B and $01 = 1 then
          FPortAPins[B].State := AValue shr B and $01 <> 0;
    end
    else if P = Ord(b0PORTB) then
    begin
      Diff := FileMap[P] xor AValue;
      for B := Low(B) to High(B) do
        if Diff shr B and $01 = 1 then
          FPortBPins[B].State := AValue shr B and $01 <> 0;
    end;
  end;

  if P = Ord(b0INDF) then
  begin
    // indirect adressing
    P := NormalizeRAMPointer(FileMap[b0FSR]);
    if P <> 0 then
      FRAM[P] := AValue;
    // writing indirect to INDF => "NOP"
  end
  else
    FRAM[P] := AValue;

  if (P = Ord(b0TMR0)) and (PreScalerAssignment = paTimer0) then
  begin
    // all assignments to TMR0 will clear the prescaler
    FPreScaler := 0;
    FInhibitTimer0 := 2;
  end;
end;

procedure TProcessor.SetFileMap(P: TRegisterBank0; AValue: Byte);
begin
  FileMap[Ord(P)] := AValue;
end;

procedure TProcessor.SetFileMap(P: TRegisterBank1; AValue: Byte);
begin
  FileMap[Ord(P)] := AValue;
end;

procedure TProcessor.SetFlag(P: TRAMPointer; ABit: TBitIndex; AValue: Boolean);
begin
  if AValue then // set
    FileMap[P] := FileMap[P] or (1 shl ABit)
  else           // clear
    FileMap[P] := FileMap[P] and not (1 shl ABit);
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

function TProcessor.GetCurrentProgramPos: TProgramMemPos;
begin
  Result := FProgramCounter mod ProgramMemorySize;
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
      else
      begin
        Assert(False, 'Unhandled TMemoryType in GetMemory');
      end;
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
    else
    begin
      Result := False;
      Assert(False, 'Unhandled TMemoryType in GetReadAsZero');
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

class function TProcessor.ExtractFileAdress(AInstruction: TInstruction): TFileAdress;
begin
  Result := AInstruction and High(TFileAdress);
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

function TProcessor.ExtractRAMPointer(AInstruction: TInstruction): TRAMPointer;
begin
  Result := AdressToRAMPointer(ExtractFileAdress(AInstruction));
end;

class function TProcessor.ExtractProgramCounter(AInstruction: TInstruction): TProgramCounter;
begin
  Result := AInstruction and $7FF;
end;

class function TProcessor.ExtractDestIsFile(AInstruction: TInstruction): Boolean;
begin
  Result := ((AInstruction shr 7) and 1) = 1;
end;

class function TProcessor.ExtractBitIndex(AInstruction: TInstruction): TBitIndex;
begin
  Result := (AInstruction shr 7) and $07;
end;

function TProcessor.AdressToRAMPointer(AAdress: TFileAdress): TRAMPointer;
begin
  if Bank1Selected then
    Result := AAdress + FileMapOffset
  else
    Result := AAdress;
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
  if B = 0 then
  begin
    // B = 0000 0000
    // not B = 1111 1111
    // + 1 through carry-in of last full adder
    // AAAA AAAA
    // 1111 1111+1 -> all the 1s will cause the carry to get all through independent of A, and the result will be A
    Result := A;
    CarryFlag := True;
  end
  else
    Result := DoAdd(A, not B + 1); // don't need a 0xFF Mask, as this only happenes for B = 0 and that is spereate
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

procedure TProcessor.ProcessTimer(ACount: Cardinal);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    if PreScalerAssignment = paTimer0 then
    begin
      // use PreScaler for Timer0
      if not ExtClockSrc or ExtClockSrc and False then // TODO: check for rising/falling edge
      begin
        if not ExtClockSrc and (FInhibitTimer0 > 0) then
          Dec(FInhibitTimer0)
        else
        begin
          FPreScaler := (FPreScaler + 1) and High(Byte);
          if FPreScaler = PreScalerMax then
          begin
            FPreScaler := 0;
            // using FileMap here will set the inhibit
              FRAM[Ord(b0TMR0)] := (FRAM[Ord(b0TMR0)] + 1) and High(Byte);
              CheckTimer0Overflow;
          end;
        end;
      end;

      // TODO: inc WDT without PreScaler
    end
    else
    begin
      // process Timer0 without PreScaler
      if not ExtClockSrc or ExtClockSrc and False then // TODO: check for rising/falling edge
      begin
        if not ExtClockSrc and (FInhibitTimer0 > 0) then
          Dec(FInhibitTimer0)
        else
        begin
          // using FileMap here will set the inhibit
          FRAM[Ord(b0TMR0)] := (FRAM[Ord(b0TMR0)] + 1) and High(Byte);
          CheckTimer0Overflow;
        end;

      end;

      // TODO: WDT selected for PreScaler
    end;
  end;
end;

procedure TProcessor.CheckTimer0Overflow;
begin
  if FRAM[Ord(b0TMR0)] = 0 then
    Timer0InterruptFlag := True;
end;

procedure TProcessor.OnPortAChanged(APin: TPin);
begin
  if APin.Direction = pdRead then
  begin
    FSkipPortWrite := True;
    Flag[b0PORTA, APin.Index] := APin.State;
    OnAsyncMemoryChange.Call(Self);
    FSkipPortWrite := False;
  end;
end;

procedure TProcessor.OnPortBChanged(APin: TPin);
begin
  if APin.Direction = pdRead then
  begin
    FSkipPortWrite := True;
    Flag[b0PORTB, APin.Index] := APin.State;
    OnAsyncMemoryChange.Call(Self);
    FSkipPortWrite := False;
  end;
end;

procedure TProcessor.OnMasterClearChanged(APin: TPin);
begin
  FSkipPortWrite := True;
  Flag[b0PORTB, APin.Index] := APin.State;
  OnAsyncMemoryChange.Call(Self);
  FSkipPortWrite := False;
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
  FPortAPins := TPinArray.Create('Port A', PortACount);
  FPortAPins.OnPinChange.Add(OnPortAChanged);
  FPortBPins := TPinArray.Create('Port B', PortBCount);
  FPortBPins.OnPinChange.Add(OnPortBChanged);
  FMasterClearPin := TPinArray.Create('MCLR');
  FMasterClearPin.OnPinChange.Add(OnMasterClearChanged);
end;

destructor TProcessor.Destroy;
begin
  FBreakpoints.Free;
  FPortAPins.Free;
  FPortBPins.Free;
  FMasterClearPin.Free;
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
  ResetPowerON;
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
      raise ENotImplemented.CreateFmt('Instruction "%s" not implemented', [InstructionInfo[T].Name])
    else
      raise ENotImplemented.CreateFmt('Instruction "%s" (line %u) not implemented', [InstructionInfo[T].Name, ALine]);
  finally
    if FKeepProgramCounter then
    begin
      // program counter changed, this needs 2 cycles
      FKeepProgramCounter := False;
      AdvanceCycles(2);
      ProcessTimer(2);
    end
    else
    begin
      AdvanceProgramCounter;
      AdvanceCycles;
      ProcessTimer;
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
  if GlobalInterruptEnable then
  begin
    if (EEWriteDoneInterruptEnable and EEWriteDoneInterruptFlag) or
       (Timer0InterruptEnable and Timer0InterruptFlag) or
       (ExternalInterruptEnable and ExternalInterruptFlag) or
       (PortBInterruptChangeEnable and PortBInterruptChangeFlag)
    then
    begin
      GlobalInterruptEnable := False;
      PushStack(FProgramCounter);
      FProgramCounter := InterruptEntryAdress;
    end;
  end;
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

procedure TProcessor.CatchUp;
begin
  while TimeBehind > 0 do
  begin
    StepIn;
    if Breakpoint[CurrentInstruction.Line] or
       FHelpBreakpointEnabled and (FProgramCounterStackPos = FHelpBreakpointDepth) then
    begin
      Stop;
      Break;
    end;
  end;
end;

class function TProcessor.FormatInstruction(AInstruction: TInstruction): String;
var
  I: TInstructionType;
  Info: TInstructionInfo;
begin
  I := FInstructionArray[AInstruction];
  Info := InstructionInfo[I];
  Result := Info.Name;
  if (ipFA in Info.Params) then
    Result := Result + Format(' %.2xh', [ExtractFileAdress(AInstruction)]);
  if (ipBL in Info.Params) then
    Result := Result + Format(' %.2xh', [ExtractByteLiteral(AInstruction)]);
  if (ipPC in Info.Params) then
    Result := Result + Format(' %.3xh', [ExtractProgramCounter(AInstruction)]);
  if (ipBI in Info.Params) then
    Result := Result + Format(', %d', [ExtractBitIndex(AInstruction)]);
  if (ipDT in Info.Params) and not ExtractDestIsFile(AInstruction) then
    Result := Result + ', w';
end;

{$REGION METHODS}

procedure TProcessor.InstructionADDWF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
    FileMap[P] := DoAdd(FileMap[P], FWRegister)
  else
    FWRegister := DoAdd(FileMap[P], FWRegister);
end;

procedure TProcessor.InstructionANDWF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := FileMap[P] and FWRegister;
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := FileMap[P] and FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionCLRF(AInstruction: TInstruction);
begin
  FileMap[ExtractRAMPointer(AInstruction)] := 0;
  ZeroFlag := True;
end;

procedure TProcessor.InstructionCLRW(AInstruction: TInstruction);
begin
  FWRegister := 0;
  ZeroFlag := True;
end;

procedure TProcessor.InstructionCOMF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := not FileMap[P];
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := not FileMap[P];
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionDECF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := (FileMap[P] - 1) and $FF;
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := (FileMap[P] - 1) and $FF;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionDECFSZ(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := (FileMap[P] - 1) and $FF;
    if FileMap[P] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end
  else
  begin
    FWRegister := (FileMap[P] - 1) and $FF;
    if FileMap[P] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end;
end;

procedure TProcessor.InstructionINCF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := (FileMap[P] + 1) and $FF;
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := (FileMap[P] + 1) and $FF;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionINCFSZ(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := (FileMap[P] + 1) and $FF;
    if FileMap[P] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end
  else
  begin
    FWRegister := (FileMap[P] + 1) and $FF;
    if FileMap[P] = 0 then
    begin
      AdvanceProgramCounter(2);
      KeepProgramCounter;
    end;
  end;
end;

procedure TProcessor.InstructionIORWF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := FileMap[P] or FWRegister;
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := FileMap[P] or FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionMOVF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := FileMap[P];
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionMOVWF(AInstruction: TInstruction);
begin
  FileMap[ExtractRAMPointer(AInstruction)] := FWRegister;
end;

procedure TProcessor.InstructionNOP(AInstruction: TInstruction);
begin
  // no operation
end;

procedure TProcessor.InstructionRLF(AInstruction: TInstruction);
var
  P: TRAMPointer;
  C: Boolean;
begin
  P := ExtractRAMPointer(AInstruction);
  C := CarryFlag;
  if ExtractDestIsFile(AInstruction) then
  begin
    CarryFlag := Flag[P, 7];
    FileMap[P] := (FileMap[P] shl 1) and $FF;
    Flag[P, 0] := C;
  end
  else
  begin
    CarryFlag := Flag[P, 7];
    FWRegister := (FileMap[P] shl 1) and $FF;
    if C then
      FWRegister := FWRegister or $01;
  end;
end;

procedure TProcessor.InstructionRRF(AInstruction: TInstruction);
var
  P: TRAMPointer;
  C: Boolean;
begin
  P := ExtractRAMPointer(AInstruction);
  C := CarryFlag;
  if ExtractDestIsFile(AInstruction) then
  begin
    CarryFlag := Flag[P, 0];
    FileMap[P] := FileMap[P] shr 1;
    Flag[P, 7] := C;
  end
  else
  begin
    CarryFlag := Flag[P, 0];
    FWRegister := FileMap[P] shr 1;
    if C then
      FWRegister := FWRegister or $80;
  end;
end;

procedure TProcessor.InstructionSUBWF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := DoSub(FileMap[P], FWRegister);
  end
  else
  begin
    FWRegister := DoSub(FileMap[P], FWRegister);
  end;
end;

procedure TProcessor.InstructionSWAPF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
    FileMap[P] := (FileMap[P] shr 4) or ((FileMap[P] shl 4) and $FF)
  else
    FWRegister := (FileMap[P] shr 4) or ((FileMap[P] shl 4) and $FF);
end;

procedure TProcessor.InstructionXORWF(AInstruction: TInstruction);
var
  P: TRAMPointer;
begin
  P := ExtractRAMPointer(AInstruction);
  if ExtractDestIsFile(AInstruction) then
  begin
    FileMap[P] := FileMap[P] xor FWRegister;
    ZeroFlag := FileMap[P] = 0;
  end
  else
  begin
    FWRegister := FileMap[P] xor FWRegister;
    ZeroFlag := FWRegister = 0;
  end;
end;

procedure TProcessor.InstructionBCF(AInstruction: TInstruction);
begin
  Flag[ExtractRAMPointer(AInstruction), ExtractBitIndex(AInstruction)] := False;
end;

procedure TProcessor.InstructionBSF(AInstruction: TInstruction);
begin
  Flag[ExtractRAMPointer(AInstruction), ExtractBitIndex(AInstruction)] := True;
end;

procedure TProcessor.InstructionBTFSC(AInstruction: TInstruction);
begin
  if not Flag[ExtractRAMPointer(AInstruction), ExtractBitIndex(AInstruction)] then
  begin
    AdvanceProgramCounter(2);
    KeepProgramCounter;
  end;
end;

procedure TProcessor.InstructionBTFSS(AInstruction: TInstruction);
begin
  if Flag[ExtractRAMPointer(AInstruction), ExtractBitIndex(AInstruction)] then
  begin
    AdvanceProgramCounter(2);
    KeepProgramCounter;
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

procedure TProcessor.InstructionRETFIE(AInstruction: TInstruction);
begin
  FProgramCounter := PopStack;
  GlobalInterruptEnable:= True;
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

