program Day16;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

  TBitStream = class
  strict private
    FData: String;
    FDataPos: Integer;
    FBuffer: Byte;
    FBufferSize: Integer;
    FSize: Integer;
    FPosition: Integer;
    procedure NextChar;
  public
    constructor Create(HexString: String);
    property Position: Integer read FPosition;
    property Size: Integer read FSize;
    function Read(Bits: Integer): Integer;
    function Eof: Boolean;
  end;

  TPacket = class
    Version: Integer;
    TypeId: Integer;
    function VersionSum: Integer; virtual;
    function Value: BigInt; virtual; abstract;
  end;

  TPacketArray = TArray<TPacket>;

  TLiteral = class(TPacket)
    FValue: UInt64;
    function Value: BigInt; override;
  end;

  TLengthType = (TotalLength, SubPackets);

  TOperator = class(TPacket)
    LengthType: TLengthType;
    Subs: TPacketArray;
    function VersionSum: Integer; override;
    function Value: BigInt; override;
  end;

const
  TYPE_LITERAL = 4;
  TYPE_OP_SUM = 0;
  TYPE_OP_PRD = 1;
  TYPE_OP_MIN = 2;
  TYPE_OP_MAX = 3;
  TYPE_OP_GT = 5;
  TYPE_OP_LT = 6;
  TYPE_OP_EQ = 7;


function ReadPacket(const Bits: TBitStream): TPacket;
begin
  var Version := Bits.Read(3);
  var TypeId := Bits.Read(3);
  //WriteLn('Type ', TypeId, ', version, ', Version);
  if TypeId = TYPE_LITERAL then
  begin
    // Type 4 is a literal, which contains a number in nibbles of 5 bits,
    // each nibble starting with a continuation bit
    var Literal := TLiteral.Create;
    var Nibble := 0;
    repeat
      Nibble := Bits.Read(5);
      Literal.FValue := (Literal.FValue shl 4);
      Literal.FValue := Literal.FValue or (Nibble and $F);
    until Nibble shr 4 = 0;

    //WriteLn('Literal value ', Literal.Value);

    Result := Literal;
  end
  else
  begin
    var Op := TOperator.Create;
    Op.LengthType := TLengthType(Bits.Read(1));
    if Op.LengthType = TLengthType.TotalLength then
    begin
      var RunLength := Bits.Read(15);
      //WriteLn('Runlength: ', RunLength);
      var EndPos := Bits.Position + RunLength;
      while Bits.Position < EndPos do
      begin
        SetLength(Op.Subs, Length(Op.Subs)+1);
        Op.Subs[High(Op.Subs)] := ReadPacket(Bits);
      end;
      Assert(Bits.Position = EndPos, 'Expected runlength should be met');
    end
    else // TLengthType.SubPackets
    begin
      SetLength(Op.Subs, Bits.Read(11));
      //WriteLn('Sub-packets: ', Length(Op.Subs));
      for var i := Low(Op.Subs) to High(Op.Subs) do
        Op.Subs[i] := ReadPacket(Bits);
    end;
    Result := Op;
  end;
  Result.Version := Version;
  Result.TypeId := TypeId;
end;

function ReadVersionSum(const s: String): BigInt;
begin
  //WriteLn(s);
  var Bits := TBitStream.Create(s);
  var Root := ReadPacket(Bits);
  Result := Root.VersionSum;
end;

function ReadValue(const s: String): BigInt;
begin
  //WriteLn(s);
  var Bits := TBitStream.Create(s);
  var Root := ReadPacket(Bits);
  Result := Root.Value;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := ReadVersionSum(Inputs[0]);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := ReadValue(Inputs[0]);
end;

var
  Input: TStringArray;
  Result: Int64;
{ TBitStream }

constructor TBitStream.Create(HexString: String);
begin
  FData := HexString;
  FSize := FData.Length * 4; // Each character represents a nibble, 4 bits of data
  FBufferSize := 0;
end;

function TBitStream.Eof: Boolean;
begin
  Result := FPosition >= FSize;
end;

procedure TBitStream.NextChar;
begin
  Assert(FBufferSize = 0, 'No bits left in buffer');
  if FDataPos >= Length(FData) then
    raise Exception.Create('Reading beyond end of stream');

  Inc(FDataPos);
  FBuffer := StrToInt('$'+FData[FDataPos]);
  FBufferSize := SizeOf(FBuffer) * 4;
end;

function TBitStream.Read(Bits: Integer): Integer;
begin
  Result := 0;
  var Remaining := Bits;
  while Remaining > 0 do
  begin
    if FBufferSize = 0 then
      NextChar;

    var BitsToRead := Remaining;
    if Remaining > FBufferSize then
      BitsToRead := FBufferSize;

    // Shift the buffer and apply a mask, to get _just_ the bits to read.
    var BitsToAdd :=
      (FBuffer shr (FBufferSize - BitsToRead))
      and ((1 shl BitsToRead) - 1);

    // Shift result to add more bits
    Result := Result shl BitsToRead;
    // Merge the two
    Result := Result or BitsToAdd;

    Dec(Remaining, BitsToRead);
    Dec(FBufferSize, BitsToRead);
    Inc(FPosition, BitsToRead);
  end;
end;

{ TPacket }

function TPacket.VersionSum: Integer;
begin
  Result := Version;
end;

{ TOperator }

function TOperator.Value: BigInt;
begin
  case TypeId of
    TYPE_OP_SUM:
    begin
      Result := 0;
      for var Sub in Subs do
        Inc(Result, Sub.Value);
    end;
    TYPE_OP_PRD:
    begin
      Result := 1;
      for var Sub in Subs do
        Result := Result * Sub.Value;
    end;
    TYPE_OP_MIN:
    begin
      Result := Subs[0].Value;
      for var s := Low(Subs)+1 to High(Subs) do
        Result := Min(Result, Subs[s].Value);
    end;
    TYPE_OP_MAX:
    begin
      Result := Subs[0].Value;
      for var s := Low(Subs)+1 to High(Subs) do
        Result := Max(Result, Subs[s].Value);
    end;
    TYPE_OP_GT:
      Result := Ord(Subs[0].Value > Subs[1].Value);
    TYPE_OP_LT:
      Result := Ord(Subs[0].Value < Subs[1].Value);
    TYPE_OP_EQ:
      Result := Ord(Subs[0].Value = Subs[1].Value);
  end;
end;

function TOperator.VersionSum: Integer;
begin
  Result := inherited;
  for var Sub in Subs do
    Inc(Result, Sub.VersionSum);
end;

{ TLiteral }

function TLiteral.Value: BigInt;
begin
  Result := FValue;
end;

begin
  WriteLn('Tests');
  ValidateNr(ReadVersionSum('8A004A801A8002F478'), 16);
  ValidateNr(ReadVersionSum('620080001611562C8802118E34'), 12);
  ValidateNr(ReadVersionSum('C0015000016115A2E0802F182340'), 23);
  ValidateNr(ReadVersionSum('A0016C880162017C3686B18A3D4780'), 31);

  ValidateNr(ReadValue('C200B40A82'), 3);
  ValidateNr(ReadValue('04005AC33890'), 54);
  ValidateNr(ReadValue('880086C3E88112'), 7);
  ValidateNr(ReadValue('CE00C43D881120'), 9);
  ValidateNr(ReadValue('D8005AC2A8F0'), 1);
  ValidateNr(ReadValue('F600BC2D8F'), 0);
  ValidateNr(ReadValue('9C005AC2F8F0'), 0);
  ValidateNr(ReadValue('9C0141080250320F1802104A08'), 1);

  WriteLn(#10'Final');
  Input := LoadStrings('Day16.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 981);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  for var i := 1 to Iterations do
    Result := Solve2(Input);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 299227024091);

  WriteLn(#10'Hit it');
  ReadLn;
end.
