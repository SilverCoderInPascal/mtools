unit umathutils;

{$mode ObjFPC}{$H+}

interface

function EaseInOutSine(t: Double): Double;

implementation

uses Math;

// Ref: https://nicmulvaney.com/easing
function EaseInOutSine(t: Double): Double;
begin
  Result := 0.5 * (1 - Cos(Pi * EnsureRange(t, 0.0, 1.0)));
end;

end.

