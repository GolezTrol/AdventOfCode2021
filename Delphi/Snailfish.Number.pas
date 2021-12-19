unit Snailfish.Number;

interface

uses
  Snailfish.Number.Intf,
  SysUtils,
  System.Character;

type
  TBase = class
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

  TToken = record
    Sym: Char;
    Val: Integer;
  end;
  TTokenArray = array of TToken;

const
  TOKEN_NONE = #0;
  TOKEN_NUM = '0';
  TOKEN_SUB = 's';

type
  TNumber = class(TInterfacedObject, Number)
    Pair: TPair;
    function ToString: String; override;
    function Magnitude: BigInt;
    procedure Reduce;
    procedure Add(Right: Number);
  end;

  TTokenUtils = record
    class function Tokenize(const S: String): TTokenArray; static;
    class function Serialize(const Tokens: TTokenArray): String; static;
    class function ExplodeOnce(var Tokens: TTokenArray): Boolean; static;
    class function ReduceOnce(var Tokens: TTokenArray): Boolean; static;
    class procedure Compress(var Tokens: TTokenArray); static;
    class function SplitOnce(var Tokens: TTokenArray): Boolean; static;
    class function Reduce(var Tokens: TTokenArray): Boolean; static;
    class function Add(const A, B: TTokenArray): TTokenArray; static;
  end;

function ReadNumber(const s: String): Number;

implementation


function SubToStr(const s: TBase): String;
begin
  if s is TPair then
    Result := '['+SubToStr(TPair(s).Left)+','+SubToStr(TPair(s).Right)+']'
  else if s is TRegular then
    Result := Result + TRegular(s).Value.ToString
  else
    raise Exception.Create('Boom');
end;

function ReadRootPair(const s: String): TPair;
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

function ReadNumber(const s: String): Number;
begin
  var Num := TNumber.Create;
  Num.Pair := ReadRootPair(s);
  Num.Reduce;
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

procedure TNumber.Add(Right: Number);
begin
  var r := TTokenUtils.Add(
    TTokenUtils.Tokenize(ToString),
    TTokenUtils.Tokenize(Right.ToString)
  );
  TTokenUtils.Reduce(r);
  Pair.Free;
  Pair := ReadRootPair(TTokenUtils.Serialize(r));
end;

function TNumber.Magnitude: BigInt;
begin
  Result := Pair.Magnitude;
end;

procedure TNumber.Reduce;
begin
  var t := TTokenUtils.Tokenize(ToString);
  if TTokenUtils.Reduce(t) then
  begin
    Pair.Free;
    Pair := ReadRootPair(TTokenUtils.Serialize(t));
  end;
end;

function TNumber.ToString: String;
begin
  Result := SubToStr(Pair);
end;

{ TTokenUtils }

class function TTokenUtils.Add(const A, B: TTokenArray): TTokenArray;
  function SymToTokenArray(const Sym: Char): TTokenArray;
  begin
    var Token: TToken;
    Token.Sym := Sym;
    Token.Val := 0;
    Result := [Token];
  end;
begin
  Result := SymToTokenArray('[') + A + SymToTokenArray(',') + B + SymToTokenArray(']');
end;

class procedure TTokenUtils.Compress(var Tokens: TTokenArray);
begin
  var i := 0;
  for var j := Low(Tokens) to High(Tokens) do
  begin
    if Tokens[j].Sym <> TOKEN_NONE then
    begin
      if i < j then
        Tokens[i] := Tokens[j];
      Inc(i);
    end;
  end;
  SetLength(Tokens, i);
end;

class function TTokenUtils.ExplodeOnce(var Tokens: TTokenArray): Boolean;
begin
  Result := False;
  var Depth := 0;
  for var t := Low(Tokens) to High(Tokens) do
  begin
    var Token := Tokens[t];
    if Token.Sym = '[' then
    begin
      Inc(Depth);
      var IsSimple := (Tokens[t+1].Sym = TOKEN_NUM) and (Tokens[t+3].Sym = TOKEN_NUM);

      if (Depth > 4) and IsSimple then
      begin
        // Try to explode to the left
        for var x := t downto Low(Tokens) do
        begin
          if Tokens[x].Sym = TOKEN_NUM then
          begin
            Inc(Tokens[x].Val, Tokens[t+1].Val);
            Break;
          end;
        end;
        // Try to explode to the right
        for var x := t+4 to High(Tokens) do
        begin
          if Tokens[x].Sym = TOKEN_NUM then
          begin
            Inc(Tokens[x].Val, Tokens[t+3].Val);
            Break;
          end;
        end;
        Tokens[t].Sym := TOKEN_NUM;
        Tokens[t].Val := 0;
        // Delete the obsolete items. Pair = 5 tokens. Replace the first, delete the rest.
        Tokens := Copy(Tokens, 0, t+1) + Copy(Tokens, t + 5);
        Exit(True); //We exploded
      end;
    end;
    if Token.Sym = ']' then
      Dec(Depth);
  end;
end;

class function TTokenUtils.Reduce(var Tokens: TTokenArray): Boolean;
begin
  Result := False;
  while TTokenUtils.ReduceOnce(Tokens) do Result := True;
end;

class function TTokenUtils.ReduceOnce(var Tokens: TTokenArray): Boolean;
begin
  Result := ExplodeOnce(Tokens) or SplitOnce(Tokens);
end;

class function TTokenUtils.Serialize(const Tokens: TTokenArray): String;
begin
  Result := '';
  for var Token in Tokens do
    if Token.Sym = TOKEN_NONE then
      // Skip
    else if Token.Sym = TOKEN_NUM then
      Result := Result + Token.Val.ToString
    else
      Result := Result + Token.Sym;
end;

class function TTokenUtils.SplitOnce(var Tokens: TTokenArray): Boolean;
begin
  Result := False;
  for var i := Low(Tokens) to High(Tokens) do
  begin
    if (Tokens[i].Sym = TOKEN_NUM) and (Tokens[i].Val > 9) then
    begin
      // Replace the number at i with a pair
      var Left := Tokens[i].Val div 2;
      var Right := Left;
      Inc(Right, Tokens[i].Val and 1); // Round up = add one if the odd bit was set
      Tokens :=
        Copy(Tokens, 0, i) +
        Tokenize('['+Left.ToString+','+Right.ToString+']') +
        Copy(Tokens, i+1, Length(Tokens));

      Exit(True);
    end;
  end;
end;

class function TTokenUtils.Tokenize(const S: String): TTokenArray;
begin
  var Token: TToken;
  Token.Sym := TOKEN_NONE;
  Token.Val := 0;
  var p := 0;
  var i := 0;
  SetLength(Result, Length(s)); // Guaranteed to be enough
  while p < Length(s) do
  begin
    Inc(p);
    if S[p].IsNumber then
    begin
      // Gather digits for the number
      Token.Sym := TOKEN_NUM;
      Token.Val := Token.Val * 10 + Ord(S[p]) - Ord('0');
    end
    else
    begin
      if Token.Sym = TOKEN_NUM then
      begin
        // If the token just built was a number, add it and reset the value for next time
        Result[i] := Token;
        Inc(i);
        Token.Val := 0;
      end;
      // Add this character
      Token.Sym := S[p];
      Result[i] := Token;
      Inc(i);
    end;
  end;
  SetLength(Result, i); // Truncate unused part
end;

end.
