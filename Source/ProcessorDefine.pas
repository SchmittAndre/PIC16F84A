unit ProcessorDefine;

{$M+}

interface

uses
  Classes, Windows, SysUtils, Dialogs, Lists, Delegates, PinDefine, Math;

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
    InterruptEntryAddress = $004;
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
    WatchDogTime = 0.018;
    WatchDogCycles = WatchDogTime / OperationTime;
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

    {$REGION TEEPROMState}
    TEEPROMState = (
      esNotReady,
      esSequenceStarted,
      esReady
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

  public

    type TProcessorNotifyEvent = procedure (Sender: TProcessor) of object; var
      // whenever something changes, not while the processor is processing a command (like a port change)
      OnAsyncMemoryChange: TDelegate1<TProcessorNotifyEvent>;

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
      ProgramMemorySize * 2,
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

    DoubleWordMemories = [mtProgram, mtProgramCounterStack];

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
      (Name: 'retfie'; SignificantBits: 14; Instruction: $0009; Params: []),
      (Name: 'retlw';  SignificantBits: 4;  Instruction: $3400; Params: [ipBL]),
      (Name: 'return'; SignificantBits: 14; Instruction: $0008; Params: []),
      (Name: 'sleep';  SignificantBits: 14; Instruction: $0063; Params: []),
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
    FEEPROMState: TEEPROMState;

    // Pins
    FPortAPins: TPinArray;
    FPortBPins: TPinArray;
    FMasterClearPin: TPinArray;
    FSkipPortWrite: Boolean;

    // Function Pointer
    FMethods: array [TInstructionType] of TInstructionMethod;
    FKeepProgramCounter: Boolean;

    // WatchDog
    FWatchDogTimer: Cardinal;

    // State
    FRunning: Boolean;
    FCycles: Int64;           // All Cycles that happened after a call of Start
    FCyclesFromStart: Int64;  // Cycles from StartTime
    FFrequency: Int64;
    FStartTime: Int64;        // Changes for ResetSyncTime
    FSpeedFactor: Single;
    FOverloadFactor: Single;     // Actual speed in percentage to what the Processor should reach
    FBreakpoints: TCardinalSet;
    FEEPROMDelay: Integer;

    FHelpBreakpointDepth: TProgramCounterStackPos;
    FHelpBreakpointEnabled: Boolean;

    function GetBreakpoint(ALine: Cardinal): Boolean;
    function GetCalcFlags: TCalcFlags;
    function GetCurrentInstruction: TLineInstruction;
    function GetCurrentProgramPos: TProgramMemPos;
    function GetMemory(AType: TMemoryType; APos: Cardinal): Byte;
    function GetOverloaded: Boolean;
    function GetPCStack(P: TProgramCounterStackPos): TProgramCounter;
    function GetCode(P: TProgramCounter): TLineInstruction;
    function GetPCStackMem(P: TProgramCounterStackPointer): Byte;
    function GetProgramMem(P: TProgramMemPointer): Byte;
    function GetTargetCycles: Integer;
    function GetTimeBehind: Single;
    function GetReadAsZero(AType: TMemoryType; APos: Cardinal): Boolean;
    procedure SetBreakpoint(ALine: Cardinal; AValue: Boolean);

    procedure SetSpeedFactor(AValue: Single);

    {$REGION Property-Getter}

    // Writable Memory Access
    function GetFileMap(P: TRAMPointer): Byte; overload;
    function GetFileMap(P: TRegisterBank0): Byte; overload;
    function GetFileMap(P: TRegisterBank1): Byte; overload;
    function GetFlag(P: TRAMPointer; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank0; ABit: TBitIndex): Boolean; overload;
    function GetFlag(P: TRegisterBank1; ABit: TBitIndex): Boolean; overload;
    function GetEEPROM(P: TROMPointer): Byte;

    // STATUS-Register
    function GetCarryFlag: Boolean;
    function GetDigitCarryFlag: Boolean;
    function GetZeroFlag: Boolean;
    function GetPowerDown: Boolean;
    function GetTimeOut: Boolean;
    function GetBank1Selected: Boolean;

    // OPTION-Register
    function GetPreScalerMax: Byte;
    function GetPreScalerAssignment: TPreScalerAssignment;
    function GetExtClockFallingEdge: Boolean;
    function GetExtClockSrc: Boolean;
    function GetInteruptRisingEdge: Boolean;
    function GetPortBPullUpEnabled: Boolean;

    // INTCON-Register
    function GetPortBInterruptChangeFlag: Boolean;
    function GetExternalInterruptFlag: Boolean;
    function GetTimer0InterruptFlag: Boolean;
    function GetPortBInterruptChangeEnable: Boolean;
    function GetExternalInterruptEnable: Boolean;
    function GetTimer0InterruptEnable: Boolean;
    function GetEEWriteDoneInterruptEnable: Boolean;
    function GetGlobalInterruptEnable: Boolean;

    // EECON1-Register
    function GetEEReadControl: Boolean;
    function GetEEWriteControl: Boolean;
    function GetEEWriteEnable: Boolean;
    function GetEEWriteErrorFlag: Boolean;
    function GetEEWriteDoneInterruptFlag: Boolean;

    {$ENDREGION}

    {$REGION Property-Setter}

    // Writable Memory Access
    procedure SetFileMap(P: TRAMPointer; AValue: Byte); overload;
    procedure SetFileMap(P: TRegisterBank0; AValue: Byte); overload;
    procedure SetFileMap(P: TRegisterBank1; AValue: Byte); overload;
    procedure SetFlag(P: TRAMPointer; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank0; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetFlag(P: TRegisterBank1; ABit: TBitIndex; AValue: Boolean); overload;
    procedure SetEEPROM(P: TROMPointer; AValue: Byte);

    // STATUS-Register
    procedure SetCarryFlag(AValue: Boolean);
    procedure SetDigitCarryFlag(AValue: Boolean);
    procedure SetZeroFlag(AValue: Boolean);
    procedure SetPowerDown(AValue: Boolean);
    procedure SetTimeOut(AValue: Boolean);
    procedure SetBank1Selected(AValue: Boolean);

    // OPTION-Register
    procedure SetPreScalerAssignment(AValue: TPreScalerAssignment);
    procedure SetExtClockFallingEdge(AValue: Boolean);
    procedure SetExtClockSrc(AValue: Boolean);
    procedure SetInteruptRisingEdge(AValue: Boolean);
    procedure SetPortBPullUpEnabled(AValue: Boolean);

    // INTCON-Register
    procedure SetPortBInterruptChangeFlag(AValue: Boolean);
    procedure SetExternalInterruptFlag(AValue: Boolean);
    procedure SetTimer0InterruptFlag(AValue: Boolean);
    procedure SetPortBInterruptChangeEnable(AValue: Boolean);
    procedure SetExternalInterruptEnable(AValue: Boolean);
    procedure SetTimer0InterruptEnable(AValue: Boolean);
    procedure SetEEWriteDoneInterruptEnable(AValue: Boolean);
    procedure SetGlobalInterruptEnable(AValue: Boolean);

    // EECON1-Register
    procedure SetEEReadControl(AValue: Boolean);
    procedure SetEEWriteControl(AValue: Boolean);
    procedure SetEEWriteEnable(AValue: Boolean);
    procedure SetEEWriteErrorFlag(AValue: Boolean);
    procedure SetEEWriteDoneInterruptFlag(AValue: Boolean);

    {$ENDREGION}

    // Writable Memory Access
    property FileMap[P: TRAMPointer]: Byte read GetFileMap write SetFileMap;
    property Flag[P: TRAMPointer; ABit: TBitIndex]: Boolean read GetFlag write SetFlag;
    property EEPROM[P: TROMPointer]: Byte read GetEEPROM write SetEEPROM;

    {$REGION Flag-Properties}

    // STATUS-Register
    property CarryFlag: Boolean read GetCarryFlag write SetCarryFlag;
    property DigitCarryFlag: Boolean read GetDigitCarryFlag write SetDigitCarryFlag;
    property ZeroFlag: Boolean read GetZeroFlag write SetZeroFlag;
    property PowerDown: Boolean read GetPowerDown write SetPowerDown;
    property TimeOut: Boolean read GetTimeOut write SetTimeOut;
    property Bank1Selected: Boolean read GetBank1Selected write SetBank1Selected;

    // OPTION-Register
    property PreScalerMax: Byte read GetPreScalerMax; // returns 0 instead of 256, as the byte perfectly overflows
    property PreScalerAssignment: TPreScalerAssignment read GetPreScalerAssignment write SetPreScalerAssignment;
    property ExtClockFallingEdge: Boolean read GetExtClockFallingEdge write SetExtClockFallingEdge;
    property ExtClockSrc: Boolean read GetExtClockSrc write SetExtClockSrc;
    property InteruptRisingEdge: Boolean read GetInteruptRisingEdge write SetInteruptRisingEdge;
    property PortBPullUpEnabled: Boolean read GetPortBPullUpEnabled write SetPortBPullUpEnabled;

    // INTCON-Register
    property PortBInterruptChangeFlag: Boolean read GetPortBInterruptChangeFlag write SetPortBInterruptChangeFlag;
    property ExternalInterruptFlag: Boolean read GetExternalInterruptFlag write SetExternalInterruptFlag;
    property Timer0InterruptFlag: Boolean read GetTimer0InterruptFlag write SetTimer0InterruptFlag;
    property PortBInterruptChangeEnable: Boolean read GetPortBInterruptChangeEnable write SetPortBInterruptChangeEnable;
    property ExternalInterruptEnable: Boolean read GetExternalInterruptEnable write SetExternalInterruptEnable;
    property Timer0InterruptEnable: Boolean read GetTimer0InterruptEnable write SetTimer0InterruptEnable;
    property EEWriteDoneInterruptEnable: Boolean read GetEEWriteDoneInterruptEnable write SetEEWriteDoneInterruptEnable;
    property GlobalInterruptEnable: Boolean read GetGlobalInterruptEnable write SetGlobalInterruptEnable;

    // EECON1-Register
    property EEReadControl: Boolean read GetEEReadControl write SetEEReadControl;
    property EEWriteControl: Boolean read GetEEWriteControl write SetEEWriteControl;
    property EEWriteEnable: Boolean read GetEEWriteEnable write SetEEWriteEnable;
    property EEWriteErrorFlag: Boolean read GetEEWriteErrorFlag write SetEEWriteErrorFlag;
    property EEWriteDoneInterruptFlag: Boolean read GetEEWriteDoneInterruptFlag write SetEEWriteDoneInterruptFlag;

    {$ENDREGION}

    {$REGION Instruction-Helper}
    class function ExtractFileAdress(AInstruction: TInstruction): TFileAdress; static;
    class function ExtractByteLiteral(AInstruction: TInstruction): Byte; static;
    class function ExtractProgramCounter(AInstruction: TInstruction): TProgramCounter; static;
    class function ExtractDestIsFile(AInstruction: TInstruction): Boolean; static;
    class function ExtractBitIndex(AInstruction: TInstruction): TBitIndex; static;

    function AdressToRAMPointer(AAdress: TFileAdress): TRAMPointer;
    function ExtractRAMPointer(AInstruction: TInstruction): TRAMPointer;

    // call this when the instruction changed the ProgramCounter (jump, call, ...)
    procedure KeepProgramCounter;

    // calculates and sets all flags according to the calculation
    function DoAdd(A, B: Byte): Byte;
    function DoSub(A, B: Byte): Byte;

    // push the ProgramCounter onto the Stack/pop it from the stack
    procedure PushStack(AProgramCounter: TProgramCounter);
    function PopStack: TProgramCounter;
    {$ENDREGION}

    function NextProgramCounter(ACount: Integer = 1): TProgramCounter;
    procedure AdvanceProgramCounter(ACount: Integer = 1);
    procedure AdvanceCycles(ACount: Integer = 1);

    procedure ProcessTimer(ACount: Cardinal = 1; AExtClockTick: Boolean = False);
    procedure CheckTimer0Overflow;
    procedure CheckWatchDogDone;

    procedure OnPortAChanged(APin: TPin);
    procedure OnPortBChanged(APin: TPin);
    procedure OnMasterClearChanged(APin: TPin);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromLST(AFileData: TStrings);
    procedure SaveProgram(AFileName: String);
    procedure LoadProgram(AFileName: String; AGeneratedCode: TStrings = nil);

    procedure ClearProgramMemory;
    procedure ClearROM;

    procedure ResetPowerON;
    procedure WatchDogReset;
    procedure WakeUpFromSleep;

    procedure Start;
    procedure Stop;

    procedure StepIn;
    function StepOver: TStepInfo;
    function StepOut: TStepInfo;
    procedure CatchUp;
    property TargetCycles: Integer read GetTargetCycles;

    property TimeBehind: Single read GetTimeBehind;

    procedure ResetSyncTime;

    procedure ProcessInstruction(AInstruction: TInstruction; ALine: Integer = -1); overload;
    procedure ProcessInstruction(AInstruction: TLineInstruction); overload;

    function NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;

    {$REGION ReadOnly-Memory Properties}
    property ProgramMem[P: TProgramMemPointer]: Byte read GetProgramMem;
    property Code[P: TProgramCounter]: TLineInstruction read GetCode;
    property CurrentProgramPos: TProgramMemPos read GetCurrentProgramPos;
    property CurrentInstruction: TLineInstruction read GetCurrentInstruction;
    property WRegister: Byte read FWRegister;
    property RAMBit[P: TRAMPointer; ABit: TBitIndex]: Boolean read GetFlag;
    property PreScaler: Byte read FPreScaler;
    property RAM[P: TRAMPointer]: Byte read GetFileMap;
    property ROM[P: TROMPointer]: Byte read GetEEPROM;
    property PCStackPos: TProgramCounterStackPos read FProgramCounterStackPos;
    property PCStack[P: TProgramCounterStackPos]: TProgramCounter read GetPCStack;
    property PCStackMem[P: TProgramCounterStackPointer]: Byte read GetPCStackMem;
    property CalcFlags: TCalcFlags read GetCalcFlags;
    {$ENDREGION}

    property PortAPins: TPinArray read FPortAPins;
    property PortBPins: TPinArray read FPortBPins;
    property MasterClearPin: TPinArray read FMasterClearPin;

    property ReadAsZero[AType: TMemoryType; APos: Cardinal]: Boolean read GetReadAsZero;
    property Memory[AType: TMemoryType; APos: Cardinal]: Byte read GetMemory;

    property Running: Boolean read FRunning;
    property Cycles: Int64 read FCycles;
    property SpeedFactor: Single read FSpeedFactor write SetSpeedFactor;

    property Breakpoint[ALine: Cardinal]: Boolean read GetBreakpoint write SetBreakpoint;

    property Overloaded: Boolean read GetOverloaded;
    property OverloadFactor: Single read FOverloadFactor;

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
    procedure InstructionCLRWDT(AInstruction: TInstruction);
    procedure InstructionGOTO(AInstruction: TInstruction);
    procedure InstructionIORLW(AInstruction: TInstruction);
    procedure InstructionMOVLW(AInstruction: TInstruction);
    procedure InstructionRETFIE({%H-}AInstruction: TInstruction);
    procedure InstructionRETLW(AInstruction: TInstruction);
    procedure InstructionRETURN({%H-}AInstruction: TInstruction);
    procedure InstructionSLEEP(AInstruction: TInstruction);
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


  // DONE: [ERROR] Memory out of bounds
  // TODO: [HINT]  Unused Macro
  // TODO: [HINT]  Unused Label
  // DONE: [ERROR] ByteLiteral Number out of range (0 .. 255)
  // DONE: [ERROR] FileAdressLiteral out of range  (0 .. 127)
  // DONE: [ERROR] BitIndex out of range (0 .. 7)
  // TODO: [HINT]  Processor not defined
  // TODO: [WARN]  Incompatible Processor defined

  // TODO: TodoLabels

  { TCompiler }

  TCompiler = class
  private
    type

      TCodePosition = (cpHeader, cpCode);

      { TTodoLabelInfo }

      TTodoLabelInfo = record
        ProgramPos: TProcessor.TProgramMemPos;
        LabelName: String;
      end;

      TImpactLevel = (ilVerbose, ilHint, ilWarning, ilError, ilFatal);

      { TLogEntry }

      TLogEntry = record
        Line: Integer;
        ErrorString: String;
        ImpactLevel: TImpactLevel;

        constructor Create(ALine: Integer; AErrorString: String; AImpactLevel: TImpactLevel);
      end;

  public
    const

      CommentStart = ';';

      ListCommand = 'list';
      MacroCommand = 'equ';
      DeviceCommand = 'device';

      OriginCommand = 'org';

      Alpha = ['A' .. 'Z', 'a' .. 'z', '_'];
      Number = ['0' .. '9'];
      AlphaNum = Alpha + Number;

      NumChars = Number + ['a' .. 'f', 'A' .. 'F', 'h', 'H'];

      ImpactStrings: array [TImpactLevel] of String = (
        'Verbose',
        'Hint',
        'Warning',
        'Error',
        'Fatal'
      );

  private
    // "Constant"
    FProcessor: TProcessor;
    FInstructionList: TStringMap<TProcessor.TInstructionType>;
    FMacros: TStringMap<String>;
    FLine: Integer;

    // Used in compilation
    FCodePosition: TCodePosition;
    FProgramPos: Integer;

    FLabelList: TStringMap<TProcessor.TProgramMemPos>;
    FTodoLabels: TArrayList<TTodoLabelInfo>;

    // Error handling
    FLog: TArrayList<TLogEntry>;
    FSuccess: Boolean;

    FDevice: String;

    procedure GenerateInstructionList;

    function GetLogLength: Integer;
    function GetLog(AIndex: Integer): TLogEntry;

    procedure ProcessLine(const ALine: String);
    procedure FinishTodoLabels;

    // Removes comment and trims away any spaces being left
    class function RemoveComment(const ALine: String): String;

    // Replace all found macros
    function ReplaceMacros(ALine: String): String;

    // A letter followed by letters or numbers
    class function ExtractIdentifier(var ALine: String; out AResult: String; ATrim: Boolean = True): Boolean;

    // Combine the given InstructionType with the Parameters and add it into the processor memory
    procedure ParseInstruction(AInstructionType: TProcessor.TInstructionType; var AParamString: String);

    // Adds a label into the label-list if possible
    procedure AddLabel(AIdentifier: String);

    // Parses a number of any accepted format
    class function ExtractNumber(var ALine: String): Integer;

    // Tries to skip a comma and returns true if it found one and skipped it
    class function SkipComma(var ALine: String): Boolean;

    procedure RaiseMemoryOutOfBounds(AMemoryPos: Integer);

  public
    constructor Create(AProcessor: TProcessor; ACode: TStrings);
    destructor Destroy; override;

    class function FormatInstruction(AInstruction: TProcessor.TInstruction): String;

    property Success: Boolean read FSuccess;

    property LogLength: Integer read GetLogLength;
    property Log[AIndex: Integer]: TLogEntry read GetLog;
    property Device: String read FDevice;

  end;

  { ECompileError }

  ECompileError = class (Exception)
  public
    constructor Create(const AMessage: string);
  end;

implementation

{ TCompiler.TLogEntry }

constructor TCompiler.TLogEntry.Create(ALine: Integer; AErrorString: String; AImpactLevel: TImpactLevel);
begin
  Line := ALine;
  ErrorString := AErrorString;
  ImpactLevel := AImpactLevel;
end;

{ ECompileError }

constructor ECompileError.Create(const AMessage: string);
begin
  inherited Create(AMessage);
end;

{ TCompiler }

procedure TCompiler.ProcessLine(const ALine: String);
var
  CodeLine, Identifier, MacroText: String;
  InstructionType: TProcessor.TInstructionType;
begin
  CodeLine := RemoveComment(ALine);

  CodeLine := ReplaceMacros(CodeLine);

  if CodeLine.IsEmpty then
    Exit;

  case FCodePosition of
    cpHeader:
    begin
      if not ExtractIdentifier(CodeLine, Identifier) then
        raise ECompileError.CreateFmt('Expected identifier, got "%s"', [CodeLine]);
      if Identifier = OriginCommand then
      begin
        FProgramPos := ExtractNumber(CodeLine);
        FCodePosition := cpCode;
      end
      else if Identifier = ListCommand then
      begin
        Exit; // whatever... just skip it xD
      end
      else if Identifier = DeviceCommand then
      begin
        FDevice := CodeLine;
        Exit;
      end
      else if ExtractIdentifier(CodeLine, MacroText) and (MacroText = MacroCommand)  then
      begin
        FMacros[Identifier] := CodeLine;
        CodeLine := '';
      end
      else
        raise ECompileError.CreateFmt('Expected EQU, got "%s"', [CodeLine]);
    end;
    cpCode:
    begin
      if not ExtractIdentifier(CodeLine, Identifier) then
        raise ECompileError.CreateFmt('Identifier expected, got "%s"', [CodeLine]);
      if FInstructionList.Get(Identifier, InstructionType) then
        ParseInstruction(InstructionType, CodeLine)
      else if Identifier = OriginCommand then
        FProgramPos := ExtractNumber(CodeLine)
      else
      begin
        AddLabel(Identifier);
        if CodeLine = ':' then
          CodeLine := CodeLine.Substring(1);
        ProcessLine(CodeLine);
        Exit;
      end;
    end;
  end;

  if not CodeLine.IsEmpty then
    raise ECompileError.CreateFmt('Expected comment or line break, got "%s"', [CodeLine]);
end;

procedure TCompiler.FinishTodoLabels;
var
  Entry: TTodoLabelInfo;
  ProgramPos: TProcessor.TProgramMemPos;
begin
  for Entry in FTodoLabels do
  begin
    if FLabelList.Get(Entry.LabelName, ProgramPos) then
    begin
      RaiseMemoryOutOfBounds(Entry.ProgramPos);
      FProcessor.FProgramMem[Entry.ProgramPos].Instruction := FProcessor.FProgramMem[Entry.ProgramPos].Instruction or
                                                              ProgramPos;
    end
    else
    begin
      FLog.Add(TLogEntry.Create(FProcessor.FProgramMem[Entry.ProgramPos].Line,
                                Format('The label "%s" does not exist', [Entry.LabelName]),
                                ilError));
      FSuccess := False;
    end;
  end;
end;

class function TCompiler.RemoveComment(const ALine: String): String;
var
  I: Integer;
begin
  Result := ALine;
  I := Result.IndexOf(CommentStart);
  if I <> -1 then
    Result := Result.Substring(0, I);
  Result := Trim(Result);
end;

function TCompiler.ReplaceMacros(ALine: String): String;
var
  Ident, RepString: String;
begin
  Result := '';
  while not ALine.IsEmpty do
  begin
    if ExtractIdentifier(ALine, Ident, False) then
    begin
      if FMacros.Get(Ident, RepString) then
      begin
        Result := Result + RepString;
        // FLog.Add(TLogEntry.Create(FLine, Format('Replaced %s with %s', [Ident, RepString]), ilVerbose));
      end
      else
        Result := Result + Ident;
    end
    else
    begin
      Result := Result + ALine[1];
      ALine := ALine.Substring(1);
    end;
  end;
end;

class function TCompiler.ExtractIdentifier(var ALine: String; out AResult: String; ATrim: Boolean): Boolean;
var
  I: Integer;
begin
  AResult := '';
  if not ALine.isEmpty and (ALine[1] in Alpha) then
    AResult := AResult + ALine[1]
  else
    Exit(False);
  I := 2;
  while (I <= ALine.Length) and (ALine[I] in AlphaNum) do
  begin
    AResult := AResult + ALine[I];
    Inc(I);
  end;
  ALine := ALine.Substring(I - 1);
  if ATrim then
    Aline := ALine.TrimLeft;
  Result := True;
end;

procedure TCompiler.ParseInstruction(AInstructionType: TProcessor.TInstructionType; var AParamString: String);
var
  Instruction: TProcessor.TInstruction;
  Num: Integer;
  MemPos: TProcessor.TProgramMemPos;
  LabelInfo: TTodoLabelInfo;
begin
  Instruction := TProcessor.InstructionInfo[AInstructionType].Instruction;
  if ipFA in TProcessor.InstructionInfo[AInstructionType].Params then
  begin
    Num := ExtractNumber(AParamString);
    if (Num < Low(TProcessor.TFileAdress)) or (Num > High(TProcessor.TFileAdress)) then
      raise ECompileError.CreateFmt('File adress %.2xh out of range [00h .. 7Fh]', [Num]);
    Instruction := Instruction or Num;
  end;
  if ipDT in TProcessor.InstructionInfo[AInstructionType].Params then
  begin
    if SkipComma(AParamString) and not AParamString.IsEmpty and (LowerCase(AParamString[1]) = 'w') then
      AParamString := AParamString.Substring(1)
    else
      Instruction := Instruction or $80;
  end;
  if ipBI in TProcessor.InstructionInfo[AInstructionType].Params then
  begin
    if not SkipComma(AParamString) then
      raise ECompileError.CreateFmt('Expected bit index, got "%s"', [AParamString]);
    Num := ExtractNumber(AParamString);
    if (Num < Low(TProcessor.TBitIndex)) or (Num > High(TProcessor.TBitIndex)) then
      raise ECompileError.CreateFmt('Bit index %d out of range [0 .. 7]', [Num]);
    Instruction := Instruction or (Num shl 7);
  end;
  if ipBL in TProcessor.InstructionInfo[AInstructionType].Params then
  begin
    Num := ExtractNumber(AParamString);
    if (Num < Low(Byte)) or (Num > High(Byte)) then
      raise ECompileError.CreateFmt('Byte literal %.2xh out of range [00h .. FFh]', [Num]);
    Instruction := Instruction or Num;
  end;
  if ipPC in TProcessor.InstructionInfo[AInstructionType].Params then
  begin
    if not ExtractIdentifier(AParamString, LabelInfo.LabelName) then
      raise ECompileError.CreateFmt('Expected a label, got "%s"', [AParamString]);
    LabelInfo.LabelName := LowerCase(LabelInfo.LabelName);
    if FLabelList.Get(LabelInfo.LabelName, MemPos) then
    begin
      Instruction := Instruction or MemPos;
    end
    else
    begin
      RaiseMemoryOutOfBounds(FProgramPos);
      LabelInfo.ProgramPos := FProgramPos;
      FTodoLabels.Add(LabelInfo);
    end;
  end;
  RaiseMemoryOutOfBounds(FProgramPos);
  FProcessor.FProgramMem[FProgramPos].Instruction := Instruction;
  FProcessor.FProgramMem[FProgramPos].Line := FLine;
  Inc(FProgramPos);
end;

procedure TCompiler.AddLabel(AIdentifier: String);
begin
  AIdentifier := LowerCase(AIdentifier);
  if FLabelList.HasKey(AIdentifier) then
    raise ECompileError.CreateFmt('Duplicate Label "%s"', [AIdentifier]);
  RaiseMemoryOutOfBounds(FProgramPos);
  FLabelList[AIdentifier] := FProgramPos;
end;

class function TCompiler.ExtractNumber(var ALine: String): Integer;
var
  Num: String;
  Base, NumLen, Digit, Significance, I: Integer;
begin

  {
    Valid formats:
    42 / 42d  | decimal
    2Ah       | hex
    00101010b | binary
    '*'       | ascii
  }

  if ALine.IsEmpty then
    raise ECompileError.CreateFmt('Expected number, got "%s"', [ALine]);

  if ALine[1] = '''' then
  begin
    if (ALine.Length < 3) or (ALine[3] <> '''') then
      raise ECompileError.CreateFmt('Invalid char statement: "%s"', [ALine]);
    Result := Ord(ALine[2]);
    ALine := ALine.Substring(3);
    Exit;
  end;

  Num := '';
  I := 1;
  while (I <= ALine.Length) and (ALine[I] in NumChars) do
  begin
    Num := Num + ALine[I];
    Inc(I);
  end;

  if Num.IsEmpty then
    raise ECompileError.CreateFmt('Expected number, got "%s"', [ALine]);

  ALine := ALine.Substring(I - 1);

  if LowerCase(Num[Num.Length]) = 'h' then
  begin
    Base := 16;
    NumLen := Num.Length - 1;
  end
  else if LowerCase(Num[Num.Length]) = 'b' then
  begin
    Base := 2;
    NumLen := Num.Length - 1;
  end
  else if LowerCase(Num[Num.Length]) = 'd' then
  begin
    Base := 10;
    NumLen := Num.Length - 1;
  end
  else if Num[Num.Length] in Number then
  begin
    Base := 10;
    NumLen := Num.Length;
  end
  else
    raise ECompileError.CreateFmt('Cannot parse number: "%s"', [ALine]);

  Result := 0;
  Significance := 1;
  for I := NumLen - 1 downto 0 do
  begin
    if (Base = 16) and not (Num[I + 1] in ['a' .. 'f', 'A' .. 'F', '0' .. '9']) or
       (Base <= 10) and ((Ord(Num[I + 1]) < Ord('0')) or (Ord(Num[I + 1]) >= Ord('0') + Base)) then
      raise ECompileError.CreateFmt('The char "%s" is not allowed for a literal in base %d',
                                    [String(Num[I + 1]), Base]);

    if Num[I + 1] in Number then
      Digit := Ord(Num[I + 1]) - Ord('0')
    else
      Digit := Ord(LowerCase(Num[I + 1])) - Ord('a') + 10;

    Result := Result + Digit * Significance;
    Significance := Significance * Base;
  end;
end;

class function TCompiler.SkipComma(var ALine: String): Boolean;
begin
  if ALine.IsEmpty or (ALine[1] <> ',') then
    Result := False
  else
  begin
    ALine := ALine.Substring(1).TrimLeft;
    Result := True;
  end;
end;

procedure TCompiler.RaiseMemoryOutOfBounds(AMemoryPos: Integer);
begin
  if (AMemoryPos < Low(TProcessor.TProgramMemPos)) or (AMemoryPos > High(TProcessor.TProgramMemPos)) then
    raise ECompileError.CreateFmt('Program memory adress %.3xh out of bounds [0 .. 3FFh]', [AMemoryPos]);
end;

procedure TCompiler.GenerateInstructionList;
var
  InstructionType: TProcessor.TInstructionType;
begin
  FInstructionList := TStringMap<TProcessor.TInstructionType>.Create;
  for InstructionType := Low(InstructionType) to High(InstructionType) do
  begin
    if not (InstructionType in [itNOP2 .. itNOP4]) then
      FInstructionList[TProcessor.InstructionInfo[InstructionType].Name] := InstructionType;
  end;
end;

function TCompiler.GetLogLength: Integer;
begin
  Result := FLog.Count;
end;

function TCompiler.GetLog(AIndex: Integer): TLogEntry;
begin
  Result := FLog[AIndex];
end;

constructor TCompiler.Create(AProcessor: TProcessor; ACode: TStrings);
var
  I: Integer;
begin
  GenerateInstructionList;

  FProcessor := AProcessor;
  FProcessor.ResetPowerON;
  FProcessor.ClearProgramMemory;

  FLabelList := TStringMap<TProcessor.TProgramMemPos>.Create;
  FTodoLabels := TArrayList<TTodoLabelInfo>.Create;
  FMacros := TStringMap<String>.Create;

  FLog := TArrayList<TLogEntry>.Create;

  try
    FSuccess := True;
    FCodePosition := cpHeader;
    for I := 0 to ACode.Count - 1 do
    begin
      FLine := I + 1;
      try
        ProcessLine(ACode[I]);
      except
        on E: ECompileError do
        begin
          FLog.Add(TLogEntry.Create(I + 1, E.Message, ilError));
          FSuccess := False;
        end;
        on E: Exception do
        begin
          FLog.Add(TLogEntry.Create(I + 1, E.Message, ilFatal));
          FSuccess := False;
        end;
      end;
    end;

    FinishTodoLabels;

  finally
    FInstructionList.Free;
    FLabelList.Free;
    FTodoLabels.Free;
    FMacros.Free;
  end;
end;

destructor TCompiler.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

class function TCompiler.FormatInstruction(AInstruction: TProcessor.TInstruction): String;
var
  I: TProcessor.TInstructionType;
  Info: TProcessor.TInstructionInfo;
begin
  I := TProcessor.FInstructionArray[AInstruction];
  Info := TProcessor.InstructionInfo[I];
  Result := Info.Name;
  if (ipFA in Info.Params) then
    Result := Result + Format(' %.2xh', [TProcessor.ExtractFileAdress(AInstruction)]);
  if (ipBL in Info.Params) then
    Result := Result + Format(' %.2xh', [TProcessor.ExtractByteLiteral(AInstruction)]);
  if (ipPC in Info.Params) then
    Result := Result + Format(' %.3xh', [TProcessor.ExtractProgramCounter(AInstruction)]);
  if (ipBI in Info.Params) then
    Result := Result + Format(', %d', [TProcessor.ExtractBitIndex(AInstruction)]);
  if (ipDT in Info.Params) and not TProcessor.ExtractDestIsFile(AInstruction) then
    Result := Result + ', w';
end;

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

function TProcessor.GetPowerDown: Boolean;
begin
  Result := not Flag[b0STATUS, 3];
end;

function TProcessor.GetTimeOut: Boolean;
begin
  Result := not Flag[b0STATUS, 4];
end;

procedure TProcessor.SetPowerDown(AValue: Boolean);
begin
  Flag[b0STATUS, 3] := not AValue;
end;

procedure TProcessor.SetTimeOut(AValue: Boolean);
begin
  Flag[b0STATUS, 4] := not AValue;
end;

function TProcessor.GetInteruptRisingEdge: Boolean;
begin
  Result := Flag[b1OPTION, 6];
end;

function TProcessor.GetPortBPullUpEnabled: Boolean;
begin
  Result := not Flag[b1OPTION, 7];
end;

procedure TProcessor.SetInteruptRisingEdge(AValue: Boolean);
begin
  Flag[b1OPTION, 6] := AValue;
end;

procedure TProcessor.SetPortBPullUpEnabled(AValue: Boolean);
begin
  Flag[b1OPTION, 7] := not AValue;
end;

function TProcessor.GetEEReadControl: Boolean;
begin
  Result := Flag[b1EECON1, 0];
end;

function TProcessor.GetEEWriteControl: Boolean;
begin
  Result := Flag[b1EECON1, 1];
end;

function TProcessor.GetEEWriteEnable: Boolean;
begin
  Result := Flag[b1EECON1, 2];
end;

function TProcessor.GetEEWriteErrorFlag: Boolean;
begin
  Result := Flag[b1EECON1, 3];
end;

procedure TProcessor.SetEEReadControl(AValue: Boolean);
begin
  Flag[b1EECON1, 0] := AValue;
end;

procedure TProcessor.SetEEWriteControl(AValue: Boolean);
begin
  Flag[b1EECON1, 1] := AValue;
end;

procedure TProcessor.SetEEWriteEnable(AValue: Boolean);
begin
  Flag[b1EECON1, 2] := AValue;
end;

procedure TProcessor.SetEEWriteErrorFlag(AValue: Boolean);
begin
  Flag[b1EECON1, 3] := AValue;
end;

function TProcessor.GetExtClockFallingEdge: Boolean;
begin
  Result := Flag[b1OPTION, 4];
end;

procedure TProcessor.SetExtClockFallingEdge(AValue: Boolean);
begin
  Flag[b1OPTION, 4] := AValue;
end;

procedure TProcessor.SetEEPROM(P: TROMPointer; AValue: Byte);
begin
  FROM[P] := AValue;
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
  FEEPROMState := esNotReady;
  FEEPROMDelay := -1;
  TimeOut := False;
  PowerDown := False;
end;

procedure TProcessor.WatchDogReset;
begin
  FProgramCounter := 0;
  FileMap[b0PCLATH] := $00;
  FileMap[b1OPTION] := $FF;
  FileMap[b0INTCON] := FileMap[b0INTCON] and $01;
  FileMap[b1TRISA] := $1F;
  FileMap[b1TRISB] := $FF;
  FileMap[b1EECON1] := FileMap[b1EECON1] and $08;
end;

procedure TProcessor.WakeUpFromSleep;
begin
  PowerDown := False;
  FileMap[b1EECON1] := FileMap[b1EECON1] and $0F;
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
  else if P = Ord(b0PCL) then
  begin
    Result := FProgramCounter and $00FF;
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
  end
  else if P = Ord(b0PCL) then
    FProgramCounter := FProgramCounter and $1F00 or AValue
  else if P = Ord(b1EECON2) then
  begin
    if (AValue = $55) and (FEEPROMState = esNotReady) then
      FEEPROMState := esSequenceStarted
    else if (AValue = $AA) and (FEEPROMState = esSequenceStarted) then
      FEEPROMState := esReady;
    Exit;
  end
  else if (P = Ord(b1EECON1)) then
  begin
    //EEPROM Write
    if FEEPROMState = esReady then
    begin
      Diff := FileMap[P] xor AValue;
      if ((Diff and AValue shr 1 and $01 = 1) and EEWriteEnable) then
        FEEPROMDelay := Random(4) + 2;
    end;
  end;

  if not FSkipPortWrite then
  begin
    if P = Ord(b0PORTA) then
    begin
      AValue := AValue and $1F;
      Diff := FileMap[P] xor AValue;
      for B := Low(B) to High(B) do
        if (Diff shr B and $01 = 1) and (TPin.TDirection(Flag[b1TRISA, B]) = pdWrite) then
          FPortAPins[B].State := AValue shr B and $01 <> 0;
    end
    else if P = Ord(b0PORTB) then
    begin
      Diff := FileMap[P] xor AValue;
      for B := Low(B) to High(B) do
        if (Diff shr B and $01 = 1) and (TPin.TDirection(Flag[b1TRISB, B]) = pdWrite) then
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
  end
  else if ((P = Ord(b1EECON1)) and (EEReadControl)) then
  begin
    //EEPROM Read
    FileMap[b0EEDATA] := EEPROM[FileMap[b0EEADR]];
    EEReadControl := False;
  end
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

function TProcessor.GetEEPROM(P: TROMPointer): Byte;
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

function TProcessor.GetOverloaded: Boolean;
begin
  Result := FOverloadFactor < 1;
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

function TProcessor.GetTargetCycles: Integer;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result := Floor((T - FStartTime) / FFrequency * FSpeedFactor * OperationFrequeny + 0.5);
end;

function TProcessor.GetTimeBehind: Single;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result := (T - FStartTime) / FFrequency * FSpeedFactor - FCyclesFromStart * OperationTime;
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

procedure TProcessor.ProcessTimer(ACount: Cardinal; AExtClockTick: Boolean);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    if PreScalerAssignment = paTimer0 then
    begin
      // use PreScaler for Timer0
      if not PowerDown and (not ExtClockSrc or AExtClockTick) then
      begin
        if FInhibitTimer0 > 0 then
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

      Inc(FWatchDogTimer);
      CheckWatchDogDone;
    end
    else
    begin
      // process Timer0 without PreScaler
      if not PowerDown and (not ExtClockSrc or AExtClockTick) then
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

      FPreScaler := (FPreScaler + 1) and High(Byte);
      if FPreScaler = PreScalerMax then
      begin
        FPreScaler := 0;
        Inc(FWatchDogTimer);
        CheckWatchDogDone;
      end;
    end;
  end;
end;

procedure TProcessor.CheckTimer0Overflow;
begin
  if FRAM[Ord(b0TMR0)] = 0 then
    Timer0InterruptFlag := True;
end;

procedure TProcessor.CheckWatchDogDone;
begin
  if FWatchDogTimer >= WatchDogCycles then
  begin
    FWatchDogTimer := 0;
    TimeOut := True;
    if PowerDown then
      WakeUpFromSleep
    else
      WatchDogReset;
  end;
end;

procedure TProcessor.OnPortAChanged(APin: TPin);
begin
  if APin.Direction = pdRead then
  begin
    FSkipPortWrite := True;
    Flag[b0PORTA, APin.Index] := APin.State;
    if not Running then
      OnAsyncMemoryChange.Call(Self);
    FSkipPortWrite := False;

    if ExtClockSrc and (APin.Index = 4) then
    begin
      if ExtClockFallingEdge = APin.State then
        ProcessTimer(1, True);
    end;
  end;
end;

procedure TProcessor.OnPortBChanged(APin: TPin);
begin
  if APin.Direction = pdRead then
  begin
    if (APin.Index = 0) then
    begin
      if InteruptRisingEdge = APin.State then
        ExternalInterruptFlag := True;
    end
    else if (APin.Index >= 4) and (APin.Index <= 7) then
      PortBInterruptChangeFlag := True;

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

procedure TProcessor.LoadFromLST(AFileData: TStrings);
var
  Counter: Integer;
  CodePos: Cardinal;
  Instruction: DWORD;
begin
  ClearProgramMemory;
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

procedure TProcessor.SaveProgram(AFileName: String);
var
  FileStream: TFileStream;
  LineInstruction: TLineInstruction;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    for LineInstruction in FProgramMem do
      FileStream.Write(LineInstruction.Instruction, SizeOf(TInstruction));
  finally
    FileStream.Free;
  end;
end;

procedure TProcessor.LoadProgram(AFileName: String; AGeneratedCode: TStrings);
const
  StartLine = 11;
var
  FileStream: TFileStream;
  I: Integer;
  InstructionType: TInstructionType;
  LabelList: array [TProgramMemPos] of String;
  LastLabel: Integer;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    for I := 0 to ProgramMemorySize - 1 do
    begin
      FileStream.Read(FProgramMem[I].Instruction, SizeOf(TInstruction));
      FProgramMem[I].Line := StartLine + I;
    end;
  finally
    FileStream.Free;
  end;

  AGeneratedCode.Add('         ; ---');
  AGeneratedCode.Add('         ; ' + ExtractFileName(AFileName));
  AGeneratedCode.Add('         ; ---');
  AGeneratedCode.Add('         ; Automagically generated from Binary-Data');
  AGeneratedCode.Add('');
  AGeneratedCode.Add('         list c=132');
  AGeneratedCode.Add('         device 16F84');
  AGeneratedCode.Add('');
  AGeneratedCode.Add('         org 0');
  AGeneratedCode.Add('');

  LastLabel := 1;
  for I := 0 to TProcessor.ProgramMemorySize - 1 do
  begin
    InstructionType := FInstructionArray[Code[I].Instruction];
    if ipPC in InstructionInfo[InstructionType].Params then
    begin
      LabelList[ExtractProgramCounter(Code[I].Instruction)] := 'Label' + IntToStr(LastLabel);
      AGeneratedCode.Add('         ' + Instructioninfo[InstructionType].Name + ' Label' + IntToStr(LastLabel));
      Inc(LastLabel);
    end
    else
      AGeneratedCode.Add('         ' + TCompiler.FormatInstruction(Code[I].Instruction));
  end;

  for I := 0 to TProcessor.ProgramMemorySize - 1 do
    if not LabelList[I].IsEmpty then
    begin
      AGeneratedCode[I + StartLine - 1] := LabelList[I] +
        AGeneratedCode[I + StartLine - 1].Substring(Min(LabelList[I].Length, 8));
    end;

  while AGeneratedCode[AGeneratedCode.Count - 1] = '         ' + TProcessor.InstructionInfo[itNOP1].Name do
    AGeneratedCode.Delete(AGeneratedCode.Count - 1);

end;

procedure TProcessor.ClearProgramMemory;
begin
  FillByte(FProgramMem, SizeOf(FProgramMem), 0);
end;

procedure TProcessor.ClearROM;
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
  FOverloadFactor := 1;
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
  if FEEPROMDelay = 0 then
  begin
    EEPROM[FileMap[b0EEADR]] := FileMap[b0EEDATA];
    EEWriteDoneInterruptFlag := true;
    EEWriteControl := false;
    FEEPROMDelay := -1;
  end
  else if FEEPROMDelay > 0 then
  begin
    FEEPROMDelay := FEEPROMDelay - 1;
  end;

  if PowerDown then
  begin
    if EEWriteDoneInterruptFlag or ExternalInterruptFlag or PortBInterruptChangeFlag then
    begin
      WakeUpFromSleep;
      ProcessInstruction(CurrentInstruction);
    end;
    AdvanceCycles;
    ProcessTimer;
  end
  else
  begin
    if GlobalInterruptEnable then
    begin
      if EEWriteDoneInterruptEnable and EEWriteDoneInterruptFlag or
         Timer0InterruptEnable and Timer0InterruptFlag or
         ExternalInterruptEnable and ExternalInterruptFlag or
         PortBInterruptChangeEnable and PortBInterruptChangeFlag then
      begin
        GlobalInterruptEnable := False;
        PushStack(FProgramCounter);
        FProgramCounter := InterruptEntryAddress;
      end;
    end;
    ProcessInstruction(CurrentInstruction);
  end
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
const
  MaxTimeBehind = 0.05; // 50 ms
begin
  FOverloadFactor := 1;
  while TimeBehind > 0 do
  begin
    StepIn;
    if Breakpoint[CurrentInstruction.Line] or
       FHelpBreakpointEnabled and (FProgramCounterStackPos = FHelpBreakpointDepth) then
    begin
      Stop;
      Break;
    end;
    if TimeBehind > MaxTimeBehind * FSpeedFactor then
    begin
      if not PowerDown then
        FOverloadFactor := FCyclesFromStart / TargetCycles;
      ResetSyncTime;
      Break;
    end;
  end;
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

procedure TProcessor.InstructionCLRWDT(AInstruction: TInstruction);
begin
  FWatchDogTimer := 0;
  TimeOut := False;
  PowerDown := False;
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

procedure TProcessor.InstructionSLEEP(AInstruction: TInstruction);
begin
  if Timer0InterruptFlag or ExternalInterruptFlag or PortBInterruptChangeFlag then
    Exit;
  TimeOut := False;
  PowerDown := True;
  FWatchDogTimer := 0;
  if PreScalerAssignment = paWatchdog then
    FPreScaler := 0;
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

