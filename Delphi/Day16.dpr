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
  end;

  TPacketArray = TArray<TPacket>;

  TLiteral = class(TPacket)
    Value: UInt64;
  end;

  TLengthType = (TotalLength, SubPackets);

  TOperator = class(TPacket)
    LengthType: TLengthType;
    Subs: TPacketArray;
    function VersionSum: Integer; override;
  end;

const
  TYPE_LITERAL = 4;


function ReadPackets(const Bits: TBitStream): TPacketArray;
begin
  //Result :=
end;

function ReadPacket(const Bits: TBitStream): TPacket;
begin
  var Version := Bits.Read(3);
  var TypeId := Bits.Read(3);
  WriteLn('Type ', TypeId, ', version, ', Version);
  if TypeId = TYPE_LITERAL then
  begin
    // Type 4 is a literal, which contains a number in nibbles of 5 bits,
    // each nibble starting with a continuation bit
    var Literal := TLiteral.Create;
    var Nibble := 0;
    repeat
      Nibble := Bits.Read(5);
      Literal.Value := (Literal.Value shl 4);
      Literal.Value := Literal.Value or (Nibble and $F);
    until Nibble shr 4 = 0;

    WriteLn('Literal value ', Literal.Value);

    Result := Literal;
  end
  else
  begin
    var Op := TOperator.Create;
    Op.LengthType := TLengthType(Bits.Read(1));
    if Op.LengthType = TLengthType.TotalLength then
    begin
      var RunLength := Bits.Read(15);
      WriteLn('Runlength: ', RunLength);
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
      WriteLn('Sub-packets: ', Length(Op.Subs));
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
  WriteLn(s);
  var Bits := TBitStream.Create(s);
  var Root := ReadPacket(Bits);
  Result := Root.VersionSum;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := ReadVersionSum(Inputs[0]);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  //Result := Solve(Inputs, 5);
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

function TOperator.VersionSum: Integer;
begin
  Result := inherited;
  for var Sub in Subs do
    Inc(Result, Sub.VersionSum);
end;

begin
  WriteLn('Tests');
  ValidateNr(ReadVersionSum('8A004A801A8002F478'), 16);
  ValidateNr(ReadVersionSum('620080001611562C8802118E34'), 12);
  ValidateNr(ReadVersionSum('C0015000016115A2E0802F182340'), 23);
  ValidateNr(ReadVersionSum('A0016C880162017C3686B18A3D4780'), 31);

  WriteLn(#10'Final');
  Input := LoadStrings('Day16.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 609);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  for var i := 1 to Iterations do
    Result := Solve2(Input);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 2925);

  WriteLn(#10'Hit it');
  ReadLn;
end.
