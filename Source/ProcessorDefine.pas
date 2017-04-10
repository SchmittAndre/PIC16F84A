unit ProcessorDefine;

{$M+}

interface

uses
  Classes, windows, SysUtils, Dialogs;

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
    RegisterBankOffset = $80;
    RAMStart = $0C;
    RAMEnd = $4F;
    RAMSize = RAMEnd - RAMStart + 1;
    RAMMappedStart = RAMStart + RegisterBankOffset;
    RAMMappedEnd = RAMEnd + RegisterBankOffset;
    RAMFullSize = $100;
    // EEPRom
    ROMSize = $40;
    // Timing
    OperationFrequeny = 20e6;
    OperationTime = 1 / OperationFrequeny;
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
    TInstruction = $0 .. $3FFF;
    TProgramMemPos = 0 .. ProgramMemorySize - 1;
    TProgramMemPointer = 0 .. ProgramMemorySize * 2 - 1;
    TRAMPointer = $0 .. $FF;
    TROMPointer = 0 .. ROMSize - 1;
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

    TInstructionInfo = record
      Name: ShortString;
      SignificantBits: Byte;
      Instruction: TInstruction;
    end;

    TInstructionMethod = procedure (AInstruction: TInstruction) of object;

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

    {$REGION Instructions}
    InstructionInfo: array [TInstructionType] of TInstructionInfo = (
      (Name: 'ADDWF';  SignificantBits: 6  ; Instruction: $0700),
      (Name: 'ANDWF';  SignificantBits: 6  ; Instruction: $0500),
      (Name: 'CLRF';   SignificantBits: 7  ; Instruction: $0180),
      (Name: 'CLRW';   SignificantBits: 7  ; Instruction: $0100),
      (Name: 'COMF';   SignificantBits: 6  ; Instruction: $0900),
      (Name: 'DECF';   SignificantBits: 6  ; Instruction: $0300),
      (Name: 'DECFSZ'; SignificantBits: 6  ; Instruction: $0B00),
      (Name: 'INCF';   SignificantBits: 6  ; Instruction: $0A00),
      (Name: 'INCFSZ'; SignificantBits: 6  ; Instruction: $0F00),
      (Name: 'IORWF';  SignificantBits: 6  ; Instruction: $0400),
      (Name: 'MOVF';   SignificantBits: 6  ; Instruction: $0800),
      (Name: 'MOVWF';  SignificantBits: 7  ; Instruction: $0080),
      (Name: 'NOP';    SignificantBits: 14 ; Instruction: $0000),
      (Name: 'NOP';    SignificantBits: 14 ; Instruction: $0020),
      (Name: 'NOP';    SignificantBits: 14 ; Instruction: $0040),
      (Name: 'NOP';    SignificantBits: 14 ; Instruction: $0060),
      (Name: 'RLF';    SignificantBits: 6  ; Instruction: $0D00),
      (Name: 'RRF';    SignificantBits: 6  ; Instruction: $0C00),
      (Name: 'SUBWF';  SignificantBits: 6  ; Instruction: $0200),
      (Name: 'SWAPF';  SignificantBits: 6  ; Instruction: $0E00),
      (Name: 'XORWF';  SignificantBits: 6  ; Instruction: $0600),
      (Name: 'BCF';    SignificantBits: 4  ; Instruction: $1000),
      (Name: 'BSF';    SignificantBits: 4  ; Instruction: $1400),
      (Name: 'BTFSC';  SignificantBits: 4  ; Instruction: $1800),
      (Name: 'BTFSS';  SignificantBits: 4  ; Instruction: $1C00),
      (Name: 'ADDLW';  SignificantBits: 5  ; Instruction: $3E00),
      (Name: 'ANDLW';  SignificantBits: 6  ; Instruction: $3900),
      (Name: 'CALL';   SignificantBits: 3  ; Instruction: $2000),
      (Name: 'CLRWDT'; SignificantBits: 14 ; Instruction: $0064),
      (Name: 'GOTO';   SignificantBits: 3  ; Instruction: $2800),
      (Name: 'IORLW';  SignificantBits: 6  ; Instruction: $3800),
      (Name: 'MOVLW';  SignificantBits: 4  ; Instruction: $3000),
      (Name: 'RETFIE'; SignificantBits: 14 ; Instruction: $0009),
      (Name: 'RETLW';  SignificantBits: 4  ; Instruction: $3400),
      (Name: 'RETURN'; SignificantBits: 14 ; Instruction: $0008),
      (Name: 'SLEEP';  SignificantBits: 14 ; Instruction: $0063),
      (Name: 'SUBLW';  SignificantBits: 5  ; Instruction: $3C00),
      (Name: 'XORLW';  SignificantBits: 6  ; Instruction: $3A00)
    );
    {$ENDREGION}

  private type
    TLineInstruction = record
      Instruction: TInstruction;
      Line: Cardinal;
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

    // EEPROM
    FROM: array [TROMPointer] of Byte;

    // Function Pointer
    FMethods: array [TInstructionType] of TInstructionMethod;
    FKeepProgramCounter: Boolean;

    // State
    FRunning: Boolean;
    FCycles: Int64;
    FFrequency: Int64;
    FStartTime: Int64;
    FOverride: Single;

    function GetCurrentInstruction: TInstruction;
    function GetDataMem(P: TROMPointer): Byte;
    function GetMemory(AType: TMemoryType; APos: Cardinal): Byte;
    function GetProgramCounterStack(P: TProgramCounterStackPos): TProgramCounter;
    function GetProgramInstruction(P: TProgramCounter): TInstruction;
    function GetProgramMem(P: TProgramMemPointer): Byte;
    function GetRAM(P: TRAMPointer): Byte;
    function GetRegisterBank0(S: TRegisterBank0): Byte;
    function GetRegisterBank1(S: TRegisterBank1): Byte;
    function GetValid(AType: TMemoryType; APos: Cardinal): Boolean;

    function NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;
    function CanNormalize(ARAMPointer: TRAMPointer): Boolean;

    procedure KeepProgramCounter;

  public
    constructor Create;

    procedure LoadProgram(AFileData:TStrings);
    procedure Initialize;
    procedure ResetROM;

    procedure Start(ARestart: Boolean = True);
    procedure Stop;

    procedure WaitForSync;
    procedure ResetSyncTime;

    procedure DoStep;

    procedure CatchUp;
    function UpToDate: Boolean;

    property ProgramMem[P: TProgramMemPointer]: Byte read GetProgramMem;
    property ProgramInstruction[P: TProgramCounter]: TInstruction read GetProgramInstruction;
    property CurrentInstruction: TInstruction read GetCurrentInstruction;
    property RegisterBank0[S: TRegisterBank0]: Byte read GetRegisterBank0;
    property RegisterBank1[S: TRegisterBank1]: Byte read GetRegisterBank1;
    property RAM[P: TRAMPointer]: Byte read GetRAM;
    property ROM[P: TROMPointer]: Byte read GetDataMem;
    property ProgramCounterStack[P: TProgramCounterStackPos]: TProgramCounter read GetProgramCounterStack;

    property Valid[AType: TMemoryType; APos: Cardinal]: Boolean read GetValid;
    property Memory[AType: TMemoryType; APos: Cardinal]: Byte read GetMemory;

    property Running: Boolean read FRunning;

  published
    {$REGION --- Byte-Oriented File Register Operations --- }
    // TODO: procedure ADDWF(AInstruction: TInstruction);
    // TODO: procedure ANDWF(AInstruction: TInstruction);
    // TODO: procedure CLRF(AInstruction: TInstruction);
    // TODO: procedure CLRW(AInstruction: TInstruction);
    // TODO: procedure COMF(AInstruction: TInstruction);
    // TODO: procedure DECF(AInstruction: TInstruction);
    // TODO: procedure DECFSZ(AInstruction: TInstruction);
    // TODO: procedure INCF(AInstruction: TInstruction);
    // TODO: procedure INCFSZ(AInstruction: TInstruction);
    // TODO: procedure IORWF(AInstruction: TInstruction);
    // TODO: procedure MOVF(AInstruction: TInstruction);
    // TODO: procedure MOVWF(AInstruction: TInstruction);
    procedure NOP({%H-}AInstruction: TInstruction);
    // TODO: procedure RLF(AInstruction: TInstruction);
    // TODO: procedure RRF(AInstruction: TInstruction);
    // TODO: procedure SUBWF(AInstruction: TInstruction);
    // TODO: procedure SWAPF(AInstruction: TInstruction);
    // TODO: procedure XORWF(AInstruction: TInstruction);
    {$ENDREGION}

    {$REGION --- Bit-Oriented File Register Operations --- }
    // TODO: procedure BCF(AInstruction: TInstruction);
    // TODO: procedure BSF(AInstruction: TInstruction);
    // TODO: procedure BTFSC(AInstruction: TInstruction);
    // TODO: procedure BTFSS(AInstruction: TInstruction);
    {$ENDREGION}

    {$REGION --- Literal and Control Operations --- }
    // TODO: procedure ADDLW(AInstruction: TInstruction);
    // TODO: procedure ANDLW(AInstruction: TInstruction);
    // TODO: procedure CALL(AInstruction: TInstruction);
    // TODO: procedure CLRWDT(AInstruction: TInstruction);
    // TODO: procedure GOTO(AInstruction: TInstruction);
    // TODO: procedure IORLW(AInstruction: TInstruction);
    // TODO: procedure MOVLW(AInstruction: TInstruction);
    // TODO: procedure RETFIE(AInstruction: TInstruction);
    // TODO: procedure RETLW(AInstruction: TInstruction);
    // TODO: procedure RETURN(AInstruction: TInstruction);
    // TODO: procedure SLEEP(AInstruction: TInstruction);
    // TODO: procedure SUBLW(AInstruction: TInstruction);
    // TODO: procedure XORLW(AInstruction: TInstruction);
    {$ENDREGION}
  end;

implementation

{ TProcessor }

class constructor TProcessor.Create;
var
  T: TInstructionType;
  I: TInstruction;
begin
  FillByte(FInstructionArray, SizeOf(FInstructionArray), 0);
  for T := Low(T) to High(T) do
  begin
    for I := InstructionInfo[T].Instruction to
      InstructionInfo[T].Instruction + 1 shl (14 - InstructionInfo[T].SignificantBits) - 1 do
    begin
      FInstructionArray[InstructionInfo[T].Instruction or I] := T;
    end;
  end;
end;

function TProcessor.GetDataMem(P: TROMPointer): Byte;
begin
  Result := FROM[P];
end;

function TProcessor.GetCurrentInstruction: TInstruction;
begin
  Result := ProgramInstruction[FProgramCounter];
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
      Result := ProgramCounterStack[APos div 2] shl APos mod 2 and $FF;
  end;
  except
    on ERangeError do
      Result := 0;
  end;
end;

function TProcessor.GetProgramCounterStack(P: TProgramCounterStackPos): TProgramCounter;
begin
  Result := FProgramCounterStack[P];
end;

function TProcessor.GetProgramInstruction(P: TProgramCounter): TInstruction;
begin
  Result := FProgramMem[P mod ProgramMemorySize].Instruction;
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
  Result := FRAM[NormalizeRAMPointer(TRAMPointer(S))];
end;

function TProcessor.GetRegisterBank1(S: TRegisterBank1): Byte;
begin
  Result := FRAM[NormalizeRAMPointer(TRAMPointer(S))];
end;

function TProcessor.GetValid(AType: TMemoryType; APos: Cardinal): Boolean;
begin
  try
    if (AType = mtRAM) and not CanNormalize(TRAMPointer(APos)) then
      Exit(False);
  except
    on ERangeError do
      Exit(False);
  end;
  Result := True;
end;

function TProcessor.NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;
begin
  if not CanNormalize(ARAMPointer) then
    raise Exception.CreateFmt('Cannot process RAM-Pointer: 0x%.2x', [ARAMPointer]);
  case ARAMPointer of
  RAMStart .. RAMEnd:
    Result := ARAMPointer;
  RAMMappedStart .. RAMMappedEnd:
    Result := ARAMPointer - RegisterBankOffset;
  end;
end;

function TProcessor.CanNormalize(ARAMPointer: TRAMPointer): Boolean;
begin
  Result := (ARAMPointer <= RAMEnd) or (ARAMPointer >= RegisterBankOffset) and (ARAMPointer <= RAMMappedEnd);
end;

procedure TProcessor.KeepProgramCounter;
begin
  FKeepProgramCounter := True;
end;

procedure TProcessor.NOP(AInstruction: TInstruction);
begin
  // no operation
end;

constructor TProcessor.Create;
var
  T: TInstructionType;
  M: TMethod;
begin
  M.Data := Self;
  for T := Low(T) to High(T) do
  begin
    M.Code := MethodAddress(InstructionInfo[T].Name);
    if M.Code <> nil then
      FMethods[T] := TInstructionMethod(M);
  end;
  QueryPerformanceFrequency(FFrequency);
end;

procedure TProcessor.LoadProgram(AFileData: TStrings);
var
  Counter: Integer;
  Line: Cardinal;
  Instruction: DWORD;
begin
  for Counter := 0 to AFileData.Count - 1 do
  begin
    if not AFileData[Counter].StartsWith(' ') then
    begin
      Line := StrToDWord('$' + AFileData[Counter].Substring(0, 4));
      Instruction := StrToDWord('$' + AFileData[Counter].Substring(5, 4));
      try
        FProgramMem[Line].Instruction := TInstruction(Instruction);
      except
        on ERangeError do
          raise Exception.CreateFmt('Unknown Instruction! 0x%.2h', [Instruction]);
      end;
      FProgramMem[Line].Line := Counter + 1;
    end;
  end;
end;

procedure TProcessor.Initialize;
begin
  FProgramCounter := 0;
  FProgramCounterStackPos := 0;
  FillByte(FProgramCounterStack, SizeOf(FProgramCounterStack), 0);
  FillByte(FRAM, SizeOf(FRAM), 0);
  FCycles := 0;
end;

procedure TProcessor.ResetROM;
begin
  FillByte(FROM, SizeOf(FROM), 0);
end;

procedure TProcessor.Start(ARestart: Boolean = True);
begin
  if ARestart then
    Initialize;
  QueryPerformanceCounter(FStartTime);
  FRunning := True;
end;

procedure TProcessor.Stop;
begin
  FRunning := False;
end;

procedure TProcessor.WaitForSync;
begin

end;

procedure TProcessor.ResetSyncTime;
begin

end;

procedure TProcessor.DoStep;
var
  T: TInstructionType;
begin
  T := FInstructionArray[ProgramInstruction[FProgramCounter]];

  if Assigned(FMethods[T]) then
    FMethods[T](ProgramInstruction[FProgramCounter])
  else
    raise ENotImplemented.CreateFmt('Processor-Instruction "%s" not implemented!', [InstructionInfo[T].Name]);

  if FKeepProgramCounter then
    FKeepProgramCounter := False
  else
  begin
    if FProgramCounter = High(FProgramCounter) then
      FProgramCounter := 0
    else
      Inc(FProgramCounter);
  end;

  Inc(FCycles);
end;

procedure TProcessor.CatchUp;
begin
  while not UpToDate do
    DoStep;
end;

function TProcessor.UpToDate;
begin

end;

end.

