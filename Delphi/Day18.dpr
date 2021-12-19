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

function NumToStr(const s: TBase): String;
begin
  if s is TPair then
    Result := '['+NumToStr(TPair(s).Left)+','+NumToStr(TPair(s).Right)+']'
  else if s is TRegular then
    Result := Result + TRegular(s).Value.ToString
  else
    raise Exception.Create('Boom');
end;

function ReadNumber(const s: String): TPair;
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
  Result := TPair.Create(nil);
  ReadPair(Result);
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
  Validate(NumToStr(n), Expected);
  n.Free;
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

  WriteLn(#10'Hit it');
  ReadLn;
end.
