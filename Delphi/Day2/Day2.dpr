program Day2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Vcl.Graphics,
  Classes,
  SysUtils;

type
  TInstruction = record
    Direction: String;
    Quantity: Int64;
  end;
  TInstructionArray = TArray<TInstruction>;

function LoadInstructions(FileName: String): TInstructionArray;
begin
  var Input := LoadStrings(FileName);
  SetLength(Result, Length(Input));
  for var i := Low(Result) to High(Result) do
  begin
    var Parts := Input[i].Split([' ']);
    Result[i].Direction := Parts[0];
    Result[i].Quantity := Parts[1].ToInt64;
  end;
end;

function Day2_1(Input: TInstructionArray): String;
begin
  var Horz := 0;
  var Depth := 0;
  for var Instruction in Input do
    if Instruction.Direction = 'forward' then
      Inc(Horz, Instruction.Quantity)
    else if Instruction.Direction = 'up' then
      Dec(Depth, Instruction.Quantity)
    else if Instruction.Direction = 'down' then
      Inc(Depth, Instruction.Quantity);

  Result := (Horz * Depth).ToString;
end;

function Day2_2(Input: TInstructionArray): String;
begin
  var Horz := 0;
  var Depth := 0;
  var Aim := 0;
  for var Instruction in Input do
    if Instruction.Direction = 'forward' then
    begin
      Inc(Horz, Instruction.Quantity);
      Inc(Depth, Instruction.Quantity * Aim);
    end
    else if Instruction.Direction = 'up' then
      Dec(Aim, Instruction.Quantity)
    else if Instruction.Direction = 'down' then
      Inc(Aim, Instruction.Quantity);

  Result := (Horz * Depth).ToString;
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := LoadInstructions('Day2.test.txt');
  Result := Day2_1(Input);
  Validate(Result, '150');
  Result := Day2_2(Input);
  Validate(Result, '900');

  WriteLn(#10'Final');
  Input := LoadInstructions('Day2.input.txt');
  Result := Day2_1(Input);
  Validate(Result, '2272262');
  Result := Day2_2(Input);
  Validate(Result, '2134882034');

  WriteLn(#10'Hit it');
  ReadLn;
end.
