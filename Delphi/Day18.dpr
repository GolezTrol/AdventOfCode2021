program Day18;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Character,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

  TBase = class
    Depth: Integer;
    Parent: TBase;
    constructor Create(Parent: TBase);
    function Magnitude: BigInt; virtual; abstract;
  end;
  TRegular = class(TBase)
    Value: Integer;
    function Magnitude: BigInt; override;
  end;
  TPair = class(TBase)
    Left, Right: TBase;
    function Magnitude: BigInt; override;
    destructor Destroy; override;
  end;

  TNumber = class
    Pair: TPair;
    function ToString: String;
    function Magnitude: BigInt;
  end;

function SubToStr(const s: TBase): String;
begin
  if s is TPair then
    Result := '['+SubToStr(TPair(s).Left)+','+SubToStr(TPair(s).Right)+']'
  else if s is TRegular then
    Result := Result + TRegular(s).Value.ToString
  else
    raise Exception.Create('Boom');
end;

function ReadNumber(const s: String): TNumber;
var
  p: Integer;

  function Read(const MustBe: Char = #0): Char;
  begin
    Result := s[p];
    if (MustBe <> #0) and (MustBe <> Result) then
      raise Exception.Create('Expected ' + MustBe + ' but got ' + Result + ' at position ' + p.ToString);
    if Result = #0 then
      raise Exception.Create('Unexpected end of input');
    Inc(p);
  end;

  procedure ReadPair(Pair: TPair);
    function ReadSubNumber: TBase;
    begin
      if s[p] = '[' then
      begin
        Result := TPair.Create(Pair);
        ReadPair(TPair(Result));
      end
      else
      begin
        Result := TRegular.Create(Pair);
        var Value := 0;
        repeat
          var C := Read;
          Value := Value * 10 + Ord(c) - Ord('0');
        until not s[p].IsNumber;
        TRegular(Result).Value := Value;
      end;
    end;
  begin
    Read('[');
    Pair.Left := ReadSubNumber;
    Read(',');
    Pair.Right := ReadSubNumber;
    Read(']');
  end;

begin
  p := 1;
  Result := TNumber.Create;
  Result.Pair := TPair.Create(nil);
  ReadPair(Result.Pair);
end;

{ TBase }

constructor TBase.Create(Parent: TBase);
begin
  Self.Parent := Parent;
end;

{ TPair }

destructor TPair.Destroy;
begin
  inherited;
  Left.Free;
  Right.Free;
end;

function TPair.Magnitude: BigInt;
begin
  Result := 3 * Left.Magnitude + 2 * Right.Magnitude;
end;

{ TRegular }

function TRegular.Magnitude: BigInt;
begin
  Result := Value;
end;

{ Tests }

procedure TestRead(const Expected: String);
begin
  var n := ReadNumber(Expected);
  Validate(n.ToString, Expected);
  n.Free;
end;

procedure TestMagnitude(const s: String; const Expected: BigInt);
begin
  var n := ReadNumber(s);
  ValidateNr(n.Magnitude, Expected);
end;

procedure TestExplode(const s, Expected: String);
begin
  var n := ReadNumber(s);
  Validate(n.ToString, Expected);
end;

{ TNumber }

function TNumber.Magnitude: BigInt;
begin
  Result := Pair.Magnitude;
end;

function TNumber.ToString: String;
begin
  Result := SubToStr(Pair);
end;

begin
  WriteLn('Test reading');

  TestRead('[1,2]');
  TestRead('[[1,2],3]');
  TestRead('[9,[8,7]]');
  TestRead('[[1,9],[8,5]]');
  TestRead('[[[[1,2],[3,4]],[[5,6],[7,8]]],9]');
  TestRead('[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]');
  TestRead('[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]');

  TestMagnitude('[[1,2],[[3,4],5]]', 143);
  TestMagnitude('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]', 1384);
  TestMagnitude('[[[[1,1],[2,2]],[3,3]],[4,4]]', 445);
  TestMagnitude('[[[[3,0],[5,3]],[4,4]],[5,5]]', 791);
  TestMagnitude('[[[[5,0],[7,4]],[5,5]],[6,6]]', 1137);
  TestMagnitude('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]', 3488);

  TestExplode('[[[[[9,8],1],2],3],4]', '[[[[0,9],2],3],4]');
  TestExplode('[7,[6,[5,[4,[3,2]]]]]', '[7,[6,[5,[7,0]]]]');
  TestExplode('[[6,[5,[4,[3,2]]]],1]', '[[6,[5,[7,0]]],3]');
  TestExplode('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]');
  TestExplode('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[7,0]]]]');

  WriteLn(#10'Hit it');
  ReadLn;
end.
