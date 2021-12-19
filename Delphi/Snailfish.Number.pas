unit Snailfish.Number;

interface

type
  BigInt = Int64;

  Number = interface
    ['{9BA013C2-34C1-4DCF-937E-664924EB3FFC}']
    function ToString: String;
    function Magnitude: BigInt;
  end;

  function ReadNumber(const s: String): Number;

implementation

uses
  SysUtils,
  System.Character;

type
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

  TNumber = class(TInterfacedObject, Number)
    Pair: TPair;
    function ToString: String; override;
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

function ReadNumber(const s: String): Number;
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
  var Num := TNumber.Create;
  Num.Pair := TPair.Create(nil);
  ReadPair(Num.Pair);
  Result := Num;
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

{ TNumber }

function TNumber.Magnitude: BigInt;
begin
  Result := Pair.Magnitude;
end;

function TNumber.ToString: String;
begin
  Result := SubToStr(Pair);
end;

end.
