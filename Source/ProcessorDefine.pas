unit ProcessorDefine;


interface

uses
  Classes, windows, SysUtils;

type

  { TProcessor }

  TProcessor = class
  public const
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
    // EEPRom
    ROMSize = $40;

  public type
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

    TProgramCounter = $0 .. $1FFF;
    TProgramCounterStackPos = 0 .. StackSize - 1;
    TInstruction = $0 .. $3FFF;
    TProgramMemPos = 0 .. ProgramMemorySize - 1;
    TProgramMemPointer = 0 .. ProgramMemorySize * 2 - 1;
    TRAMPointer = $0 .. $FF;
    TROMPointer = 0 .. ROMSize - 1;

  public const
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

  private type
    TLineInstruction = record
      Instruction: TInstruction;
      Line: Cardinal;
    end;

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
    function GetDataMem(P: TROMPointer): Byte;
    function GetProgramMem(P: TProgramMemPointer): Byte;
    function GetRAM(P: TRAMPointer): Byte;
    function GetRegisterBank0(S: TRegisterBank0): Byte;
    function GetRegisterBank1(S: TRegisterBank1): Byte;

    function NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;
    function CanNormalize(ARAMPointer: TRAMPointer): Boolean;

  public
    procedure LoadProgram(AFileData: TStrings);
    procedure Initialize;
    procedure ResetROM;

    procedure DoStep;

    property ProgramMem[P: TProgramMemPointer]: Byte read GetProgramMem;
    property RegisterBank0[S: TRegisterBank0]: Byte read GetRegisterBank0;
    property RegisterBank1[S: TRegisterBank1]: Byte read GetRegisterBank1;
    property RAM[P: TRAMPointer]: Byte read GetRAM;
    property ROM[P: TROMPointer]: Byte read GetDataMem;

  end;

implementation

{ TProcessor }

function TProcessor.GetDataMem(P: TROMPointer): Byte;
begin
  Result := FROM[P];
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

function TProcessor.NormalizeRAMPointer(ARAMPointer: TRAMPointer): TRAMPointer;
begin
  if not CanNormalize(ARAMPointer) then
    raise Exception.CreateFmt('Cannot process RAM-Pointer: 0x.2x', [ARAMPointer]);
  case ARAMPointer of
  RAMStart .. RAMEnd:
    Result := ARAMPointer;
  RAMMappedStart .. RAMMappedEnd:
    Result := ARAMPointer - RegisterBankOffset;
  end;
end;

function TProcessor.CanNormalize(ARAMPointer: TRAMPointer): Boolean;
begin
  Result := (ARAMPointer < RAMEnd) or (ARAMPointer >= RegisterBankOffset) and (ARAMPointer < RAMMappedEnd);
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
end;

procedure TProcessor.ResetROM;
begin
  FillByte(FROM, SizeOf(FROM), 0);
end;

procedure TProcessor.DoStep;
begin

end;

end.

