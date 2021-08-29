unit DelphiCompatibility;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  //TRectHelper = record helper for TRect
  //  function Width: Integer;
  //  function Height: Integer;
  //end;

  PSize = ^TSize;
  TSize = record
    cx: FixedInt;
    cy: FixedInt;
  public
    constructor Create(P : TSize); overload;
    constructor Create(const X, Y : Integer); overload;
    // operator overloads
    class operator =(const Lhs, Rhs : TSize) : Boolean;
    class operator <>(const Lhs, Rhs : TSize): Boolean;
    class operator +(const Lhs, Rhs : TSize): TSize;
    class operator -(const Lhs, Rhs : TSize): TSize;

    // methods
    function Add(const Point: TSize): TSize;
    function Distance(const P2 : TSize) : Double;
    function IsZero : Boolean;
    function Subtract(const Point: TSize): TSize;

    // properties
    property Width: Integer read cx write cx;
    property Height: Integer read cy write cy;
  end;

implementation

//function TRectHelper.Height: Integer;
//begin
//  Result := Bottom - Top;
//end;
//
//function TRectHelper.Width: Integer;
//begin
//  Result := Right - Left;
//end;

{ tagSIZE, TSize }
constructor TSize.Create(P: TSize);
begin
  cx := P.cx;
  cy := P.cy;
end;

constructor TSize.Create(const X, Y: Integer);
begin
  cx := X;
  cy := Y;
end;

class operator TSize.+(const Lhs, Rhs: TSize): TSize;
begin
  Result.cx := Lhs.cx + Rhs.cx;
  Result.cy := Lhs.cy + Rhs.cy;
end;

function TSize.Add(const Point: TSize): TSize;
begin
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
end;

function TSize.Distance(const P2: TSize): Double;
begin
  // We Add 0.0 int the expression to cast all operations to floating point.
  // This avoids overflow in signed integer operations.
  Result := Sqrt(Sqr(0.0+ Self.cx - P2.cx) + Sqr(0.0 + Self.cy - P2.cy));
end;

function TSize.IsZero: Boolean;
begin
  Result := (cx = 0) and (cy = 0);
end;

class operator TSize.=(const Lhs, Rhs: TSize): Boolean;
begin
  Result := (Lhs.cx = Rhs.cx) and (Lhs.cy = Rhs.cy);
end;

class operator TSize.<>(const Lhs, Rhs: TSize): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

class operator TSize.-(const Lhs, Rhs: TSize): TSize;
begin
  Result.cx := Lhs.cx - Rhs.cx;
  Result.cy := Lhs.cy - Rhs.cy;
end;

function TSize.Subtract(const Point: TSize): TSize;
begin
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
end;

end.

