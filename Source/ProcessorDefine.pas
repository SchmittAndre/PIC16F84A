unit ProcessorDefine;


interface

uses
  Classes, SysUtils;

type

  TProgramConter = Word;

  TBank0 = (
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

  TBank1 = (
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

  { TProcessor }

  TProcessor = class
  private type
    TInstruction = record
      Command: Word;
      Line: Cardinal;
    end;
  private
    FProgramCounter: TProgramConter;
    FProgramCounterStack: array [0 .. 7] of TProgramConter;
    FProgamMemory: array [0 .. $3FF] of TInstruction;
    FRam: array [$0C .. $4F] of Byte;
    FRegisterBank0: array [TBank0] of Byte;
    FRegisterBank1: array [TBank1] of Byte;
    FDataMemory: array [0 .. 63] of Byte;
  public
    procedure FlashProgramMemory(AFileData:TStringList);

  end;



implementation

{ TProcessor }

procedure TProcessor.FlashProgramMemory(AFileData: TStringList);
var
  Counter: Integer;
  HelpLineNumber: Cardinal;
  help: string;
begin
  for Counter := 0 to AFileData.Count - 1 do
  begin
    if not AFileData[Counter].StartsWith(' ') then
    begin
      HelpLineNumber := StrToDWord('$' + AFileData[Counter].Substring(0, 4));
      FProgamMemory[HelpLineNumber].Command := StrToDWord('$' + AFileData[Counter].Substring(5, 4)) ;
      FProgamMemory[HelpLineNumber].Line := Counter + 1;
    end;
  end;
end;

end.

