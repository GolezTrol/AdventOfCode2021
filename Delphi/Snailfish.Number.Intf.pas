unit Snailfish.Number.Intf;

interface

type
  BigInt = Int64;

  Number = interface
    ['{9BA013C2-34C1-4DCF-937E-664924EB3FFC}']
    function ToString: String;
    function Magnitude: BigInt;
    procedure Add(Right: Number);
  end;

implementation

end.
